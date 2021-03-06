% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualise.R
\name{visualizar_imagem}
\alias{visualizar_imagem}
\title{Visualizar imagem do captcha.}
\usage{
visualizar_imagem(arq_png)
}
\arguments{
\item{arq_png}{character. caminho/do/png/do/captcha.png que se deseja visualizar.}
}
\description{
Mostra o arquivo PNG do captcha.
}
\details{
Funciona bem com o objeto retornado pela funcao \code{\link{baixar_imgs_audios}}: \code{<obj>$imagem$request$output$path}
}
