#' Insere novos captchas no data.frame 'captchas'.
#'
#' @param dirs vetor de characters com os diretorios contendo os .wav e .png dos captchas.
#' @param nome_do_bd character Nome do data.frame. O padrao eh 'captcha'.
#'
#' @export
insere_captchas <- function(dirs, nome_do_bd = "captchas") {
  captchas_novos <- purrr::map(dirs, ~ list.files(.x, pattern = "\\.(wav|png)$")) %>%
    purrr::reduce(c) %>%
    stringi::stri_replace_first_regex("\\.(wav|png)$", "") %>%
    unique %>%
    dplyr::data_frame(captcha_id = ., resposta = as.character(NA))

  path_do_bd <- sprintf("data/%s.RData", nome_do_bd)
  load(file = path_do_bd)
  captchas <- bind_rows(captchas,
                        captchas_novos %>% anti_join(captchas, by = c("captcha_id", "resposta"))) %>%
    arrange(resposta) %>%
    distinct(captcha_id, .keep_all = TRUE)
  save(captchas, file = path_do_bd)
}


#' Cria o data.frame 'captchas_resposta'.
#'
#' Cria o data.frame 'captchas_resposta' com a estrutura predefinida.
#'
#' @param nome_do_bd character Nome do data.frame. O padrao eh 'captchas_resposta'.
#'
#' @export
cria_data_frame_captchas_resposta <- function(nome_do_bd = "captchas_resposta") {
  captchas_resposta <- dplyr::data_frame(arq_aud = "", final_diff = "", som_letra = list(""), resposta = "")[NULL,]
  save(captchas_resposta, file = sprintf("data/%s.RData", nome_do_bd))
}

#' Insere novos captchas com resposta no data.frame 'captchas_resposta'.
#'
#' @param dir character. Diretorio contendo os .rds captchas com resposta.
#'
#' @export
insere_captchas_resposta <- function(dir = "data/captchas_resposta") {
  load(file = "data/captchas_resposta.RData")
  captchas_resposta_novos <- list.files(dir, pattern = "\\.rds$") %>%
    setdiff(captchas_resposta$arq_aud %>% stringi::stri_extract_first_regex("[0-9]{14}.+rds$")) %>%
    purrr::map(~readRDS(sprintf("%s/%s", dir, .x))) %>%
    purrr::map(~.x %>% mutate(arq_aud = as.character(arq_aud))) %>%
    purrr::reduce(bind_rows)
  captchas_resposta %<>%
    dplyr::bind_rows(captchas_resposta_novos) %>%
    dplyr::mutate(resposta = resposta %>% tolower) %>%
    dplyr::distinct(arq_aud, final_diff, .keep_all = TRUE)
  save(captchas_resposta, file = "data/captchas_resposta.RData")
}

