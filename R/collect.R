#' Baixa audio do captcha do site da Receita Federal
#'
#' Solicita um captcha e grava em disco seu audio.
#'
#' @param dest character. (opcional) Diretorio em que se deseja salvar os arquivos.
#'
#' @export
download_audio <- function(dest = NULL) {

  url_solicitacao <- 'http://www.receita.fazenda.gov.br/pessoajuridica/cnpj/cnpjreva/cnpjreva_solicitacao2.asp'
  url_gera_captcha <- 'http://www.receita.fazenda.gov.br/pessoajuridica/cnpj/cnpjreva/captcha/gerarCaptcha.asp'
  url_audio <- 'http://www.receita.fazenda.gov.br/pessoajuridica/cnpj/cnpjreva/captcha/gerarSom.asp'

  #  solicitacao <- httr::GET(url_solicitacao)
  #  data_hora <- stringr::str_replace_all(lubridate::now(), "[^0-9]", "")
  if(is.null(dest)) {
    dest <- tempfile(pattern = 'captcha', fileext = '.wav')
  } else {
    dest <- tempfile(pattern = 'captcha', tmpdir = dest, fileext = '.wav')
  }

  wd_aud <- httr::write_disk(dest, overwrite = TRUE)
  wd_img <- httr::write_disk(dest, overwrite = TRUE)
  httr::GET(url_gera_captcha, wd_img)
  audio <- httr::GET(url_audio, wd_aud)

#  while (as.numeric(audio$headers[['content-length']]) < 1) {
#    msg <- sprintf('Aconteceu algum problema. Tentando novamente em %d segundos...', sleep)
#    message(msg)
#    Sys.sleep(3)
#    imagem <- httr::GET(url_gera_captcha, wd_img)
#    audio <- httr::GET(url_audio, wd_aud)
#  }
  return(audio)
}


#' Baixa imagem do captcha do site da Receita Federal
#'
#' Solicita um captcha e grava em disco.
#'
#' @param dest character. (opcional) Diretorio em que se deseja salvar os arquivos.
#'
#' @export
download <- function(dest = NULL) {
#revisar
  url_solicitacao <- 'http://www.receita.fazenda.gov.br/pessoajuridica/cnpj/cnpjreva/cnpjreva_solicitacao2.asp'
  url_gera_captcha <- 'http://www.receita.fazenda.gov.br/pessoajuridica/cnpj/cnpjreva/captcha/gerarCaptcha.asp'
  if(is.null(dest)) {
    dest <- tempfile(pattern = 'captcha', fileext = '.jpeg')
  } else {
    dest <- tempfile(pattern = 'captcha', tmpdir = dest, fileext = '.jpeg')
  }

  wd_img <- httr::write_disk(dest, overwrite = TRUE)
  imagem <- httr::GET(url_gera_captcha, wd_img)
  return(imagem)
}

#' Laço para baixar imagens e audios dos captchas.
#'
#' Funcao auxiliar para rodar a funcao \code{baixa_img_audio} repetidamente.
#'
#' Possivelmente ira dar Timeout no meio.
#'
#' @param nlim integer. Numero limite de captchas para baixar.
#' @param esperar numeric. Tempo em segundos de espera entre uma chamada da funcao \code{baixa_img_audio} e outra.
#' @param dir character. (opcional) Diretorio em que se deseja salvar os arquivos.
#' @param verbose boolean. Se TRUE (default), informa em qual iteracao esta.
#'
#' @export
baixar_imgs_audios <- function(nlim = 1000L, esperar = 2.5, dir = "data-raw/captchas", verbose = TRUE) {
  dir.create("data-raw/captchas", showWarnings = FALSE, recursive = TRUE)
  for(i in 1:nlim) {
    download_audio(dir)
    Sys.sleep(esperar)
    if(verbose) cat(i, "de", nlim, "\n")
  }
}

