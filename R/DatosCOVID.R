
#' Datos Covid México
#' @export
# hello <- function() {
#   print("Hello, world!")
# }

DatosCOVID19 <- function() {
  library(readxl)
  library(tidyverse)
  library(RCurl)

  datos_covid <- read.csv(descargar_datos_abiertos(),
                          encoding = "UTF-8",
                          stringsAsFactors = T ) %>%
    recodifica_variables(.)

  print("Hello, datos!")
}


#DatosAbiertosCOVID19::DatosCOVID19()
