#' Cria o data.frame 'captchas'.
#'
#' Cria o data.frame 'captchas' com a estrutura predefinida para ser preenchido com a resposta posteriormente.
#'
#' @param dir character. Pasta onde o arquivo .rds será salvo.
#'
#' @export
criar_df_captchas <- function(dir = 'data-raw') {
  if (!file.exists(dir)) dir.create(dir, recursive = TRUE)
  captchas <- dplyr::data_frame(captcha_id = "", resposta = "")[NULL,]
  readr::save_rds(captchas, file = sprintf("%s/%s.rds", dir, nome_do_bd))
}

#' Carrega o sinal .wav no R.
#'
#' @param arq_aud character. Caminho/do/audio/do/captcha.wav
#'
#' @export
ler_audio <- function(arq_aud) {
  arq_aud <- arquivo(arq_aud)
  tuneR::readWave(arq_aud)@left
}

#' Identifica letras nas ondas sonoras.
#'
#' Identifica qual parte do sinal da onda sonora do arquivo .wav corresponde as letras do captcha.
#'
#' @param arq_aud character. Caminho/do/audio/do/captcha.wav
#'
#' @export
#' @import magrittr
identificar_letras <- function(arq_aud) {
  x <- ler_audio(arq_aud)
  df <- tibble::tibble(arq_aud = arq_aud, som = x, tempo = 1:length(x))
  corte <- 1
  treze_partes <- FALSE
  while(!treze_partes & corte < 150) {
    df <- df %>%
      dplyr::mutate(w0 = som[c(2:150 - 1, tempo)] %in% som[c(1:300, n() - 1:400)] %>% RcppRoll::roll_sum(150),
             final = as.numeric(w0 > corte)[c(2:50 - 1, tempo)] %>% RcppRoll::roll_min(50),
             final_diff = c(0, abs(diff(final))) %>% cumsum())
    treze_partes <- df$final_diff %>% max() %>% magrittr::equals(12)
    corte <- corte + 5
  }
  return(df)
}

#' Valida corte das 6 letras.
#'
#' Verifica se o corte das 6 letras feitas pela funcao \code{\link{identificar_letras}} foi feita corretamente.
#'
#' @param df_com_letras_identificadas objeto data.frame retornado pela funcao \code{\link{identificar_letras}}.
#'
#' @export
corte_eh_valido <- function(df_com_letras_identificadas) {
  tem_6_letras <- df_com_letras_identificadas$final_diff %>%
    dplyr::n_distinct() %>%
    magrittr::equals(13)

  if(tem_6_letras) {
    tamanho_dos_sons_ok <- df_com_letras_identificadas %>%
      dplyr::filter(final_diff %% 2 != 0) %>%
      dplyr::group_by(final_diff) %>%
      dplyr::summarise(tamanho_do_som = n()) %>%
      with(tamanho_do_som) %>%
      magrittr::is_greater_than(1000) %>%
      all()
    return(tamanho_dos_sons_ok)
  } else {
    return(FALSE)
  }
}

#' Corta o som em partes correspondentes a letras.
#'
#' Retorna um data.frame cujas linhas sao informacoes relacinadas as letras.
#' O corte eh baseado nas identificacoes feitas pela funcao \code{\link{identificar_letras}}.
#'
#' @param df_com_letras_identificadas objeto data.frame retornado pela funcao \code{\link{identificar_letras}}.
#'
#' @export
cortar_som_em_letras <- function(df_com_letras_identificadas) {
  df_com_letras_identificadas %>%
    dplyr::filter(final_diff %% 2 != 0) %>%
    dplyr::group_by(arq_aud, final_diff) %>%
    dplyr::do(som_letra = .$som) %>%
    dplyr::ungroup()
}
