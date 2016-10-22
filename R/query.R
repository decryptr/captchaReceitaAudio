#' dp_q
dp_q <- function(x, probs = c(0, 0.25)) {
  quantis <- quantile(x = seq.int(length(x)), probs = probs, type = 1)
  x[seq(quantis[1], quantis[2])] %>% sd
}

#' max_q
max_q <- function(x, probs = c(0, 0.25)) {
  quantis <- quantile(x = seq.int(length(x)), probs = probs, type = 1)
  x[seq(quantis[1], quantis[2])] %>% max
}

#' ataque2_f
ataque2_f <- function(x) {
  x_max <- x %>% which.max
  x[(x_max + 10):length(x)] %>% which.max
}

#' ataque3_f
ataque3_f <- function(x) {
  x_max <- x %>% which.max
  x[1:max(x_max - 10, 1)] %>% which.max
}

#' Decifra uma letra.
#'
#' Decifra uma letra individual a partir de um pedaco de audio correspondente a um caracter.
#'
#' @param som_letra vetor de inteiros. Pedaco de audio correspondente a um caracter do captcha.
#'
#' @export
decifra_letra <- function(som_letra) {
  som_letra_abs <- som_letra %>% abs

  df <- tibble::data_frame(
    comprimento = som_letra %>% length,
    ataque = som_letra_abs %>% which.max,
    ataque2 = som_letra_abs %>% ataque2_f,
    ataque3 = som_letra_abs %>% ataque3_f,

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
  predict(modelo, df, "raw")
}

predizer <- function(arq) {
  UseMethod('predizer')
}

predizer.response <- function(arq) {
  arq$request$output$path %>%
    identificar_letras() %>%
    cortar_som_em_letras() %>%
    dplyr::mutate(result = purrr::map_chr(som_letra, ~as.character(decifra_letra(.x)))) %>%
    with(result) %>%
    paste(collapse = '')
}

predizer.character <- function(arq) {
  arq %>%
    identificar_letras() %>%
    cortar_som_em_letras() %>%
    dplyr::mutate(result = purrr::map_chr(som_letra, ~as.character(decifra_letra(.x)))) %>%
    with(result) %>%
    paste(collapse = '')
}
