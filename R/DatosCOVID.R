#' Datos Covid México
#' @export

DatosCOVID19 <- function() {
  library(readxl)
  library(tidyverse)
  library(RCurl)

  datos_covid <- read.csv(descargar_datos_abiertos(),
                          encoding = "UTF-8",
                          stringsAsFactors = T ) %>%
    recodifica_variables(.) %>%
    recodifica_poblaciones(.)


  return(datos_covid)
}

