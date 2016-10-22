#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
magrittr::`%>%`

arquivo <- function(x) {
  UseMethod('arquivo')
}

arquivo.response <- function(x) {
  arq <- x$request$output$path
  if (!file.exists(arq)) stop('Arquivo não encontrado.')
  arq
}

arquivo.character <- function(x) {
  if (!file.exists(x)) stop('Arquivo não encontrado.')
  x
}
