library(shiny)
library(captchaReceitaAudio)
library(magrittr)
library(tidyverse)
library(shinyBS)
addResourcePath("dados", "/home/athos/Documentos/R Projetos/captchaReceitaAudio/data")

#########################################################################################
ui <- shinyUI(fluidPage(

  titlePanel("Classificador de CAPTCHAS"),
  bsButton("novo_captcha", "Novo Captcha", icon("refresh"), block = FALSE),
  uiOutput("captchas_sumario_contador", style = "display:inline-block; width: 50%;margin:5px 0 0 20px", class = "lead"),
  hr(),
  fluidRow(
    column(width = 6,
           plotOutput("captcha_img", height = 200),
           uiOutput("captcha_aud"),
           plotOutput("grafico_vis_corte")
    ),
    column(width = 6,
           fluidRow(
             column(width = 6,
                    textInput("captcha_resposta", "Captcha", placeholder = "Preencha com as 6 letras"),
                    checkboxInput("captcha_corte_ok", "Letras cortadas corretamente"),
                    actionButton("enviar_captcha", label = "Enviar captcha", icon("check"))
             ),
             column(width = 6,
                    radioButtons("captcha_defeitos", "Captcha com defeito",
                                 choices = c("Sem som", "Difícil/impossível de decifrar",
                                             "Letras cortadas incorretamente"),
                                 selected = character(0)),
                    bsButton("enviar_defeito", label = "Reportar defeito", icon("exclamation-triangle"), style = "danger")
             )
           ),
           fluidRow(
             column(width = 6, style = "padding:5px 0",
                    bsAlert("status")
             )
           ),
           h4("Prévia:"),
           tableOutput("previa")
    )
  ),
  fluidRow(
    column(width = 6,
           verbatimTextOutput("debug")
    ),
    column(width = 2,
           plotOutput("captchas_sumario_grafico")
    )
  )


)
)

#########################################################################################
sortear_captcha <- function(data) {
  data %>%
    dplyr::filter(resposta %>% is.na) %>%
    sample_n(1) %$%
    captcha_id
}

limpar_inputs <- function(session, captcha_sugerido = character(0)) {
  updateTextInput(session, inputId = "captcha_resposta", value = captcha_sugerido)
  updateRadioButtons(session, inputId = "captcha_defeitos", selected = character(0))
  updateCheckboxInput(session, "captcha_corte_ok", value = FALSE)
}

atualiza_captchas_sumario <- function(data, input, output) {
  output$captchas_sumario_grafico <- renderPlot({
    ggplot(data) +
      geom_bar(aes(x = resposta, fill = resposta), show.legend = FALSE) +
      theme_minimal() +
      theme(axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
  }, height = 200)

  output$captchas_sumario_contador <- renderUI({
    n_oks <- sum(data$resposta %in% "OK")
    n_captchas <- nrow(data)
    p_oks <- scales::percent(n_oks/n_captchas)
    p(sprintf("%s de %s captchas respondidos (%s).",
              n_oks, n_captchas, p_oks))
  })
}

#########################################################################################
server <- shinyServer(function(input, output, session) {

  load(file = "../data/captchas.RData")
  atualiza_captchas_sumario(captchas, input, output)
  captcha_da_vez <- reactiveValues()
  captcha_da_vez$arq <- sortear_captcha(captchas)

  observeEvent(input$novo_captcha, {
    load("../data/captchas.RData")
    captcha_da_vez$arq <- sortear_captcha(captchas)
  })

  df_com_letras_identificadas <- reactive({
    identificar_letras(sprintf("../data/captchas/%s.wav", captcha_da_vez$arq))
  })

  output$debug <- renderPrint({
    list(arquivo = captcha_da_vez$arq,
         getwd = getwd(),
         captcha_resposta = input$captcha_resposta,
         RDSs = data.frame(rds = list.files("../data/captchas_resposta")),
         captcha_defeitos = input$captcha_defeitos,
         captcha_corte_ok = input$captcha_corte_ok)
  })

  output$captcha_img <- renderImage({
    arq_img <- sprintf("../data/captchas/%s.png", captcha_da_vez$arq)

    list(src = arq_img,
         contentType = 'image/png',
         width = 500,
         alt = "Imagem do captcha")
  })

  output$captcha_aud <- renderUI({
    arq_aud <- sprintf("dados/captchas/%s.wav", captcha_da_vez$arq)
    tags$audio(src = arq_aud, type = "audio/wav", controls = NA)
  })

  output$grafico_vis_corte <- renderPlot({
    visualizar_corte(df_com_letras_identificadas())
  })

  letras <- eventReactive(input$captcha_resposta, {
    letras <- strsplit(input$captcha_resposta, "")[[1]] %>% magrittr::extract(. != "")
    letras <- c(letras, rep("", 6 - length(letras)))[1:6]
  })

  df_com_letras_cortadas <- reactive({
    cortar_som_em_letras(df_com_letras_identificadas())
  })

  df_com_resposta <- reactive({
    df_com_letras_cortadas() %>%
      mutate(resposta = letras(),
             final_diff = sprintf("Letra %s", order(final_diff)))
  })

  output$previa <- renderTable({
    df_com_resposta() %>% dplyr::select(-som_letra)
  })

  captcha_valido <- eventReactive(input$enviar_captcha, {
    (stringi::stri_length(input$captcha_resposta) == 6) && (letras() %>% stringi::stri_detect_regex("[0-9a-zA-Z]") %>% all)
  })

  observeEvent(input$enviar_defeito, {
    if(input$captcha_defeitos %>% length %>% equals(0) %>% not){
      load("../data/captchas.RData")

      captchas[captchas$captcha_id %in% captcha_da_vez$arq, "resposta"] <- input$captcha_defeitos
      save(captchas, file = "../data/captchas.RData")

      closeAlert(session, alertId = "status_msg")
      createAlert(session, "status", alertId = "status_msg", title = NULL, style = "info",
                  content = sprintf("Defeito reportado (arquivo %s).", captcha_da_vez$arq), append = FALSE)


      atualiza_captchas_sumario(captchas, input, output)
      captcha_da_vez$arq <- sortear_captcha(captchas)
    }
  })

  observeEvent(input$enviar_captcha, {
    updateCheckboxInput(session, "captcha_corte_ok", value = FALSE)
    if(captcha_valido() & input$captcha_corte_ok) {
      load("../data/captchas.RData")

      captchas[captchas$captcha_id %in% captcha_da_vez$arq, "resposta"] <- "OK"
      save(captchas, file = "../data/captchas.RData")

      createAlert(session, "status", alertId = "status_msg", title = "Sucesso!", style = "success",
                  content = "Captcha enviado!", append = FALSE)

      atualiza_captchas_sumario(captchas, input, output)
      saveRDS(df_com_resposta(), file = sprintf("../data/captchas_resposta/%s.rds", captcha_da_vez$arq))
      captcha_da_vez$arq <- sortear_captcha(captchas)
    } else {
      if(!input$captcha_corte_ok) {
        createAlert(session, "status", alertId = "status_msg", title = "Erro", style = "warning",
                    content = "Por favor confirme se as letras foram cortadas corretamente.", append = FALSE)
      } else {
        createAlert(session, "status", alertId = "status_msg", title = "Erro", style = "danger",
                    content = "Captcha inválido.", append = FALSE)
      }
    }
  })

  observeEvent(captcha_da_vez$arq, {
    captcha_sugerido <- df_com_letras_cortadas() %$% som_letra %>% map(decifra_letra) %>% reduce(paste0)
    limpar_inputs(session, captcha_sugerido)

  })

  observeEvent(input$captcha_resposta, {closeAlert(session, alertId = "status_msg")})


})

shinyApp(ui = ui, server = server)

