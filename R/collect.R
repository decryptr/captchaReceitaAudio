#' Baixa imagem e audio do captcha do site da Receita Federal
#'
#' Solicita um captcha e grava em disco sua imagem e seu audio.
#'
#' @param dir character. (opcional) Diretorio em que se deseja salvar os arquivos.
#' @param sleep numeric. Tempo esperado para tentar novamente caso ocorra algum problema no download.
#' @export
baixa_img_audio <- function(dir = "data-raw/captchas", sleep = 3) {
  if (!file.exists(dir)) dir.create(dir, recursive = TRUE)

  url_solicitacao <- 'http://www.receita.fazenda.gov.br/pessoajuridica/cnpj/cnpjreva/cnpjreva_solicitacao2.asp'
  url_gera_captcha <- 'http://www.receita.fazenda.gov.br/pessoajuridica/cnpj/cnpjreva/captcha/gerarCaptcha.asp'
  url_audio <- 'http://www.receita.fazenda.gov.br/pessoajuridica/cnpj/cnpjreva/captcha/gerarSom.asp'

  solicitacao <- httr::GET(url_solicitacao)
  data_hora <- stringr::str_replace_all(lubridate::now(), "[^0-9]", "")
  if (is.null(dir)) dir <- tempdir()
  arq <- tempfile(pattern = data_hora, tmpdir = dir)

  wd_aud <- httr::write_disk(paste0(arq, ".wav"), overwrite = TRUE)
  wd_img <- httr::write_disk(paste0(arq, ".png"), overwrite = TRUE)
  imagem <- httr::GET(url_gera_captcha, wd_img)
  audio <- httr::GET(url_audio, wd_aud)

  while (as.numeric(audio$headers[['content-length']]) < 1) {
    msg <- sprintf('Aconteceu algum problema. Tentando novamente em %d segundos...', sleep)
    message(msg)
    Sys.sleep(3)
    imagem <- httr::GET(url_gera_captcha, wd_img)
    audio <- httr::GET(url_audio, wd_aud)
  }
  return(list(imagem = imagem, audio = audio))
}

#' Laço para baixar imagens e audios dos captchas.
#'
#' Funcao auxiliar para rodar a funcao \code{baixa_img_audio} repetidamente.
#'
#' Possivelmente ira dar Timeout no meio.
#'
#' @param nlim integer. Numero limite de captchas para baixar.
#' @param esperar numeric. Tempo em segundos de espera entre uma chamada da funcao \code{baixa_img_audio} e outra.
#' @param verbose boolean. Se TRUE (default), informa em qual iteracao esta.
#' @param dir character. (opcional) Diretorio em que se deseja salvar os arquivos.
#'
#' @export
baixar_imgs_audios <- function(nlim = 1000L, esperar = 2.5, dir = "data/captchas", verbose = TRUE) {
  for(i in 1:nlim) {
    baixa_img_audio(dir)
    Sys.sleep(esperar)
    if(verbose) cat(i, "de", nlim, "\n")
  }
}

