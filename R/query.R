#' dp_q
#' @param x numerico
#' @param probs quantis
dp_q <- function(x, probs = c(0, 0.25)) {
  quantis <- quantile(x = seq.int(length(x)), probs = probs, type = 1)
  x[seq(quantis[1], quantis[2])] %>% sd()
}

#' max_q
#' @param x numerico
#' @param probs quantis
max_q <- function(x, probs = c(0, 0.25)) {
  quantis <- quantile(x = seq.int(length(x)), probs = probs, type = 1)
  x[seq(quantis[1], quantis[2])] %>% max()
}

#' ataque2_f
#' @param x numerico
ataque2_f <- function(x) {
  x_max <- x %>% which.max()
  x[(x_max + 10):length(x)] %>% which.max()
}

#' ataque3_f
#' @param x numerico
ataque3_f <- function(x) {
  x_max <- x %>% which.max()
  x[1:max(x_max - 10, 1)] %>% which.max()
}

montar_treino_um <- function(x) {
  d_letras <- x %>% identificar_letras()
  if (corte_eh_valido(d_letras)) {
    d_letras %>%
      dplyr::filter(final_diff %in% c(1, 3, 5, 7, 9, 11)) %>%
      dplyr::group_by(final_diff) %>%
      dplyr::do(som = .$som) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(y = d_letras$arq_aud[1] %>%
                      basename() %>%
                      tools::file_path_sans_ext() %>%
                      stringr::str_split('', simplify = TRUE) %>%
                      as.character() %>%
                      tolower()) %>%
      dplyr::select(y, som) %>%
      dplyr::mutate(som_df = purrr::map(som, monta_df)) %>%
      dplyr::select(y, som_df) %>%
      tidyr::unnest(som_df)
  } else {
    tibble::tibble()
  }
}

#' montar_treino
#'
#' Montar base de treino
#'
#' @param arqs vetor de arquivos
#'
#' @export
montar_treino <- function(arqs) {
  arqs %>%
    tibble::enframe('id', 'arq') %>%
    dplyr::mutate(audio_letras = purrr::map(arq, montar_treino_um)) %>%
    dplyr::select(id, audio_letras) %>%
    tidyr::unnest(audio_letras)
}

#' Monta uma base de dados de treino para cada letra.
#'
#' @param som_letra vetor de inteiros. Pedaco de audio correspondente a um caracter do captcha.
#'
#' @export
monta_df <- function(som_letra) {
  som_letra_abs <- abs(som_letra)
  df <- tibble::data_frame(
    comprimento = som_letra %>% length,
    ataque = som_letra_abs %>% which.max(),
    ataque2 = som_letra_abs %>% ataque2_f(),
    ataque3 = som_letra_abs %>% ataque3_f(),

    som_letra_dp_q1 = som_letra %>% dp_q(probs = c(0.00, 0.25)),
    som_letra_dp_q2 = som_letra %>% dp_q(probs = c(0.25, 0.50)),
    som_letra_dp_q3 = som_letra %>% dp_q(probs = c(0.50, 0.75)),

    som_letra_abs_dp_q1 = som_letra_abs %>% dp_q(probs = c(0.00, 0.25)),
    som_letra_abs_dp_q2 = som_letra_abs %>% dp_q(probs = c(0.25, 0.50)),
    som_letra_abs_dp_q3 = som_letra_abs %>% dp_q(probs = c(0.50, 0.75)),
    som_letra_abs_dp =    som_letra_abs %>% sd,

    som_letra_max_q1 = som_letra %>% max_q(probs = c(0.00, 0.25)),
    som_letra_max_q2 = som_letra %>% max_q(probs = c(0.25, 0.50)),
    som_letra_max_q3 = som_letra %>% max_q(probs = c(0.50, 0.75)),

    som_letra_abs_max_q1 = som_letra_abs %>% max_q(probs = c(0.00, 0.25)),
    som_letra_abs_max_q2 = som_letra_abs %>% max_q(probs = c(0.25, 0.50)),
    som_letra_abs_max_q3 = som_letra_abs %>% max_q(probs = c(0.50, 0.75)),

    som_letra_dp_p2 = som_letra %>% max_q(probs = c(0.35, 0.40)),
    som_letra_abs_dp_p2 = som_letra_abs %>% max_q(probs = c(0.35, 0.40)),
    som_letra_max_p2 = som_letra %>% max_q(probs = c(0.35, 0.40))
  )
  df
}

#' Decifra uma letra.
#'
#' Decifra uma letra individual a partir de um pedaco de audio correspondente a um caracter.
#'
#' @param som_letra vetor de inteiros. Pedaco de audio correspondente a um caracter do captcha.
#'
#' @export
decifra_letra <- function(som_letra) {
  df <- monta_df(som_letra)
  suppressWarnings(suppressMessages(predict(modelo, df, "raw")))
}


#' Predizer
#'
#' @param arq arquivo para predizer
#'
#' @export
predizer <- function(arq) {
  UseMethod('predizer')
}

#' Predizer response
#'
#' @param arq arquivo para predizer
#'
#' @export
predizer.response <- function(arq) {
  arq$request$output$path %>%
    identificar_letras() %>%
    cortar_som_em_letras() %>%
    dplyr::mutate(result = purrr::map_chr(som_letra, ~as.character(decifra_letra(.x)))) %>%
    with(result) %>%
    paste(collapse = '')
}

#' Predizer response
#'
#' @param arq arquivo para predizer
#'
#' @export
predizer.character <- function(arq) {
  arq %>%
    identificar_letras() %>%
    cortar_som_em_letras() %>%
    dplyr::mutate(result = purrr::map_chr(som_letra, ~as.character(decifra_letra(.x)))) %>%
    with(result) %>%
    paste(collapse = '')
}
