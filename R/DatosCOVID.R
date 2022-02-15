#' Datos Covid México
#' @export

DatosCOVID19 <- function() {

  pacman::p_load(readxl,tidyverse , RCurl,data.table)
  source("funciones_base.R")

  datos_covid <- fread("curl http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip | funzip") %>%
    recodifica_variables(.) %>%
    recodifica_poblaciones(.)

  return(datos_covid)

}

