
#' Datos Covid México
#' @export
# hello <- function() {
#   print("Hello, world!")
# }

DatosCOVID19 <- function() {
  library(readxl)
  library(tidyverse)
  library(RCurl)

  source("R/funciones_base.R")

  datos_covid <- read.csv(descargar_datos_abiertos(),
                          encoding = "UTF-8",
                          stringsAsFactors = T )

  print("Hello, datos!")

}


#DatosAbiertosCOVID19::DatosCOVID19()
