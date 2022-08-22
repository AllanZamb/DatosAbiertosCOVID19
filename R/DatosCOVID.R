#' Datos Covid México
#' @export

DatosCOVID19 <- function(RESULTADO_FINAL= T,
                         UM = F,
                         PACIENTE_DATOS = T,
                         COMPLICACION = F,
                         COMORBILIDADES = F,
                         LABORATORIO = F,
                         SOCIO_CULT_DEMO = F,
                         POBLACIONES = F,
                         ANIOS = c(2020:2022)
){


  if(isTRUE(RESULTADO_FINAL)){
    variables <- c("CLASIFICACION_FINAL", "FECHA_ACTUALIZACION" ) }

  if(isTRUE(UM)){
    variables <- append(variables, c("ORIGEN", "SECTOR" ,"ENTIDAD_UM" )) }


  if (isTRUE(PACIENTE_DATOS)) {
    variables <- append(variables, c("FECHA_INGRESO",
                                     "FECHA_SINTOMAS",
                                     "FECHA_DEF",
                                     "SEXO",
                                     "ENTIDAD_RES",
                                     "MUNICIPIO_RES",
                                     "ENTIDAD_NAC",
                                     "EDAD" ))}

  if (isTRUE(COMPLICACION)) {
    variables <- append(variables, c("TIPO_PACIENTE",
                                     "UCI",
                                     "INTUBADO",
                                     "NEUMONIA"  ))}


  if (isTRUE(COMORBILIDADES)) {
    variables <- append(variables, c("EMBARAZO",
                                     "DIABETES",
                                     "EPOC",
                                     "ASMA",
                                     "INMUSUPR",
                                     "HIPERTENSION",
                                     "OTRA_COM",
                                     "CARDIOVASCULAR",
                                     "OBESIDAD",
                                     "RENAL_CRONICA",
                                     "TABAQUISMO",
                                     "OTRO_CASO"     ))}

  if (isTRUE(LABORATORIO)) {
    variables <- append(variables, c("TOMA_MUESTRA_LAB",
                                     "RESULTADO_LAB",
                                     "TOMA_MUESTRA_ANTIGENO" ,
                                     "RESULTADO_ANTIGENO"  ))}

  if (isTRUE(SOCIO_CULT_DEMO)) {
    variables <- append(variables, c("NACIONALIDAD",
                                     "PAIS_NACIONALIDAD",
                                     "PAIS_ORIGEN",
                                     "HABLA_LENGUA_INDIG",
                                     "INDIGENA",
                                     "MIGRANTE"    ))}


  if (POBLACIONES %in% c("MUNICIPIO", "municipio", "muni")) {
    poblaciones <- 1
  }else if(POBLACIONES %in% c("ENTIDAD", "ESTADO", "ENT", "entidad", "estado", "est" )){
    poblaciones <- 2
  }else{
    poblaciones <- F
  }

  vars <- c(UM,
  COMPLICACION,
  COMORBILIDADES,
  LABORATORIO,
  SOCIO_CULT_DEMO, ANIOS)





  pacman::p_load(readxl,tidyverse , RCurl,data.table, magrittr)

  # datos_covid <- fread("curl http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip | funzip",select =variables ) %>%
  #   recodifica_variables(., poblaciones)

  # datos_covid <- data.table::fread(descargar_datos_abiertos(),
  #                                  select =variables ) %>%
  #   recodifica_variables(., poblaciones, vars)

  datos_covid <- lapply(descargar_datos_abiertos(),
                        fread, select = variables ) %>%
    rbindlist(.) %>%
    recodifica_variables(., poblaciones, vars)



  return(datos_covid)

}
