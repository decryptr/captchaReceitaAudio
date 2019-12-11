#' Visualizar imagem do captcha.
#'
#' Mostra o arquivo PNG do captcha.
#'
#' Funciona bem com o objeto retornado pela funcao \code{\link{baixar_imgs_audios}}: \code{<obj>$imagem$request$output$path}
#'
#' @param arq_png character. caminho/do/png/do/captcha.png que se deseja visualizar.
#'
#' @export
visualizar_imagem <- function(arq_png) {
  img <- arq_png %>%
    arquivo() %>%
    png::readPNG()
  grid::grid.newpage()
  grid::grid.raster(img)
}

#' Plota onda sonora com os cortes identificados.
#'
#' Gera um grafico da onda sonora com as partes identificadas como letras em destaque.
#' Serve para verificar se as 6 letras foram identificadas corretamente.
#'
#' @param df_com_letras_identificadas objeto data.frame retornado pela funcao \code{\link{identificar_letras}}.
#'
#' @export
#' @import ggplot2
visualizar_corte <- function(df_com_letras_identificadas) {
  ggplot(df_com_letras_identificadas %>%
           dplyr::mutate(eh_letra = final_diff %% 2 != 0), aes(x = tempo)) +
    geom_line(aes(y = scale(som), alpha = eh_letra), show.legend = FALSE) +
    geom_line(aes(y = scale(w0), colour = "w0"), show.legend = FALSE) +
    geom_line(aes(y = scale(final), colour = "final"), show.legend = FALSE) +
    geom_line(aes(y = scale(final_diff), colour = "final_diff"), show.legend = FALSE) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    labs(x = "", y = "")
}


#' Aplicacao Shiny Classificador de Captchas
#'
#' @param arq arquivo
#'
#'@export
app_classificador <- function(arq = NULL) {
  requireNamespace("shiny", quietly = TRUE)
  if (is.null(arq)) arq <- system.file("classificador/app.R",
                                       package = 'captchaReceitaAudio')
  shiny::runApp(arq)
}
