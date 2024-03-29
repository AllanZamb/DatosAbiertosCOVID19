################################################################################
################## ~ FUNCIONES BASE DATOS ABIERTOS COVID-19 ~ ##################
################################################################################

#FUNCION PRINCIPAL
# base_covid <-function(){
#   library(readxl)
#   library(tidyverse)
#   library(RCurl)
#   library(data.table)
#
#   source("funciones_base.R")
#   # datos_covid <- read.csv(descargar_datos_abiertos(),
#   #                         encoding = "UTF-8",
#   #                         stringsAsFactors = F )
#   # datos_covid <- data.table::fread(descargar_datos_abiertos(),
#   #                         encoding = "UTF-8",
#   #                         stringsAsFactors = F )
#
#
#   # datos_covid <- fread("curl http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip | funzip") %>%
#   #   recodifica_variables (.)
#     #recodifica_poblaciones (.)
#   return(datos_covid)
# }

# FUNCION 1 : DESCARGA LA BASE DE DATOS ABIERTOS

#Con esta funcion nos aseguramos de siempre tener la versión del día o actualizada de la base de datos abiertos.

descargar_datos_abiertos <- function(){
  #Comporbamos si ya existe la base de datos de hoy... Sí ya la descargamos, no volvemos a descargarla
  #Pero si no la tenemos actualizada o no se encuentra, la descargamos


  if (!dir.exists("datos_abiertos")){
    dir.create("datos_abiertos")
  } else {
    print("Directorio existente, descargando base...")
  }


  if ((file.exists("datos_abiertos/COVID19MEXICO2020.csv") |
       file.exists("datos_abiertos/COVID19MEXICO2021.csv")|
       file.exists("datos_abiertos/COVID19MEXICO2022.csv")) &
      Sys.Date() == as.Date(file.info("datos_abiertos/COVID19MEXICO2022.csv")$atime)){
    print("Ya tenemos la base de hoy, ¿Volvemos a descargar? S/N")
    rdl<-readline()

    if (rdl %in% c("s","S","si", "SI")) {
      print("Descargando base...")

      #Sí, ya le tenemos, pero la volvemos a actualizar...
      download.file(url ="http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip",
                    destfile='datos_abiertos/datos_abiertos_covid19_2022.zip',
                    method='libcurl')

      download.file(url ="https://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/historicos/2020/COVID19MEXICO2020.zip",
                    destfile='datos_abiertos/datos_abiertos_covid19_2020.zip',
                    method='libcurl')

      download.file(url ="https://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/historicos/2021/COVID19MEXICO2021.zip",
                    destfile='datos_abiertos/datos_abiertos_covid19_2021.zip',
                    method='libcurl')

      #Descomprimimos la carpeta
      if (Sys.info()["sysname"] == "Windows"){


        datos_abiertos_act <- unzip('datos_abiertos/datos_abiertos_covid19_2022.zip',
                                exdir = "datos_abiertos",
                                overwrite = TRUE)
        datos_abiertos_2020 <- unzip('datos_abiertos/datos_abiertos_covid19_2020.zip',
                                exdir = "datos_abiertos",
                                overwrite = TRUE)
        datos_abiertos_2021 <- unzip('datos_abiertos/datos_abiertos_covid19_2021.zip',
                                exdir = "datos_abiertos",
                                overwrite = TRUE)

      }else{
        #MAC
        #Descomprimimos la carpeta
        datos_abiertos_act <- unzip('datos_abiertos/datos_abiertos_covid19_2022.zip',
                                exdir = "datos_abiertos/", overwrite = TRUE)

        datos_abiertos_2020 <- unzip('datos_abiertos/datos_abiertos_covid19_2020.zip',
                                exdir = "datos_abiertos/",
                                overwrite = TRUE)
        datos_abiertos_2021 <- unzip('datos_abiertos/datos_abiertos_covid19_2021.zip',
                                exdir = "datos_abiertos/",
                                overwrite = TRUE)

      }

      #Eliminamos el zip
      file.remove("datos_abiertos/datos_abiertos_covid19_2020.zip")
      file.remove("datos_abiertos/datos_abiertos_covid19_2021.zip")
      file.remove("datos_abiertos/datos_abiertos_covid19_2022.zip")



      actual_f<-paste0("datos_abiertos/COVID19MEXICO" ,
             lubridate::year(as.Date(substring(gsub("[^0-9]", "", datos_abiertos_act),1,2), format ="%y")),
             ".csv")
      file.rename(datos_abiertos_act, actual_f)



      #return( c(datos_abiertos_2020,datos_abiertos_2021, actual_f))

    }else if (rdl %in% c("n","N","no", "NO")){
      #No, sólo carga la base que ya está descargada
      print("Cargando base...")
      return(paste0("datos_abiertos/" , unique(list.files("datos_abiertos/", pattern = ".csv"))))
    }

  }else{

    print("No tenemos la base actualizada o alguna base está desactualizada o inexistente...")
    #No la tenemos, y descargamos los datos abiertos
    print("Desargando base...")

    download.file(url ="http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip",
                  destfile='datos_abiertos/datos_abiertos_covid19_2022.zip',
                  method='libcurl')

    download.file(url ="https://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/historicos/2020/COVID19MEXICO2020.zip",
                  destfile='datos_abiertos/datos_abiertos_covid19_2020.zip',
                  method='libcurl')

    download.file(url ="https://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/historicos/2021/COVID19MEXICO2021.zip",
                  destfile='datos_abiertos/datos_abiertos_covid19_2021.zip',
                  method='libcurl')



    #Descomprimimos la carpeta
    if (Sys.info()["sysname"] == "Windows"){


      datos_abiertos_act <- unzip('datos_abiertos/datos_abiertos_covid19_2022.zip',
                                   exdir = "datos_abiertos",
                                   overwrite = TRUE)
      datos_abiertos_2020 <- unzip('datos_abiertos/datos_abiertos_covid19_2020.zip',
                                   exdir = "datos_abiertos",
                                   overwrite = TRUE)
      datos_abiertos_2021 <- unzip('datos_abiertos/datos_abiertos_covid19_2021.zip',
                                   exdir = "datos_abiertos",
                                   overwrite = TRUE)

    }else{
      #MAC
      #Descomprimimos la carpeta
      datos_abiertos_act <- unzip('datos_abiertos/datos_abiertos_covid19_2022.zip',
                                  exdir = "datos_abiertos/", overwrite = TRUE)


      datos_abiertos_2020 <- unzip('datos_abiertos/datos_abiertos_covid19_2020.zip',
                                   exdir = "datos_abiertos/",
                                   overwrite = TRUE)
      datos_abiertos_2021 <- unzip('datos_abiertos/datos_abiertos_covid19_2021.zip',
                                   exdir = "datos_abiertos/",
                                   overwrite = TRUE)

    }

    #Eliminamos el zip
    file.remove("datos_abiertos/datos_abiertos_covid19_2020.zip")
    file.remove("datos_abiertos/datos_abiertos_covid19_2021.zip")
    file.remove("datos_abiertos/datos_abiertos_covid19_2022.zip")



    actual_f<-paste0("datos_abiertos/COVID19MEXICO" ,
                     lubridate::year(as.Date(substring(gsub("[^0-9]", "", datos_abiertos_act),1,2), format ="%y")),
                     ".csv")
    file.rename(datos_abiertos_act, actual_f)

    return( c(datos_abiertos_2020,datos_abiertos_2021, actual_f))

  }


}


# FUNCION 2 : RECODIFICA LA BASE

recodifica_variables <- function(x){
  x %>% mutate(FECHA_INGRESO = as.Date(FECHA_INGRESO, format = "%Y-%m-%d"),
               FECHA_ACTUALIZACION = as.Date(FECHA_ACTUALIZACION, format = "%Y-%m-%d"),
               FECHA_DEF = as.Date(FECHA_DEF, format = "%Y-%m-%d"),
               FECHA_SINTOMAS = as.Date(FECHA_SINTOMAS, format = "%Y-%m-%d"),

               ORIGEN = recode(ORIGEN,
                               "1" = "USMER",
                               "2" = "FUERA DE USMER"),

               ENTIDAD_UM = funcion_recodificar (ENTIDAD_UM),

               ENTIDAD_NAC = funcion_recodificar (ENTIDAD_NAC),

               ENTIDAD_RES = funcion_recodificar(ENTIDAD_RES),

               SECTOR = recode(SECTOR,
                               "1" = "CRUZ ROJA",
                               "2" = "DIF",
                               "3" = "ESTATAL",
                               "4" = "IMSS",
                               "5" = "IMSS-BIENESTAR",
                               "6" = "ISSSTE",
                               "7" = "MUNICIPAL",
                               "8" = "PEMEX",
                               "9" = "PRIVADA",
                               "10" = "SEDENA",
                               "11" = "SEMAR",
                               "12" = "SSA",
                               "13" ="UNIVERSITARIO",
                               "14" = "NO ESPECIFICADO",
                               "99" = "NO ESPECIFICADO"),
               SEXO = recode(SEXO,
                             "1" = "MUJER",
                             "2" = "HOMBRE",
                             "99" = "NO ESPECIFICADO"),

               TIPO_PACIENTE = recode(TIPO_PACIENTE,
                                      "1" = "AMBULATORIO",
                                      "2" = "HOSPITALIZADO",
                                      "99" = "NO ESPECIFICADO"),

               #Creamos la variable para grupos de edad
               GRUPO_EDAD = factor(case_when(EDAD > 69 ~ '>70',
                                             EDAD >= 60  & EDAD <= 69 ~ '60 - 69',
                                             EDAD >= 50  & EDAD <= 59 ~ '50 - 59',
                                             EDAD >= 40  & EDAD <= 49 ~ '40 - 49',
                                             EDAD >= 30  & EDAD <= 39 ~ '30 - 39',
                                             EDAD >= 20  & EDAD <= 29 ~ '20 - 29',
                                             EDAD >= 10  & EDAD <= 19 ~ '10 - 19',
                                             EDAD < 9 ~ '<9')),

               INTUBADO = funcion_recodificar_si_no(INTUBADO),

               NEUMONIA = funcion_recodificar_si_no(NEUMONIA),

               UCI = funcion_recodificar_si_no(UCI),

               NACIONALIDAD = recode(NACIONALIDAD,
                                     "1" = "MEXICANA",
                                     "2" = "EXTRANJERA",
                                     "99" = "NO ESPECIFICADO"),

               INDIGENA = funcion_recodificar_si_no(INDIGENA),

               MIGRANTE = funcion_recodificar_si_no(MIGRANTE),

               HABLA_LENGUA_INDIG = funcion_recodificar_si_no(HABLA_LENGUA_INDIG),

               EMBARAZO = funcion_recodificar_si_no(EMBARAZO),

               DIABETES = funcion_recodificar_si_no(DIABETES),

               EPOC = funcion_recodificar_si_no(EPOC),

               ASMA = funcion_recodificar_si_no(ASMA),

               INMUSUPR = funcion_recodificar_si_no(INMUSUPR),

               HIPERTENSION = funcion_recodificar_si_no(HIPERTENSION),

               OTRA_COM = funcion_recodificar_si_no(OTRA_COM),

               CARDIOVASCULAR = funcion_recodificar_si_no(CARDIOVASCULAR),

               OBESIDAD = funcion_recodificar_si_no(OBESIDAD),

               TABAQUISMO = funcion_recodificar_si_no(TABAQUISMO),

               RENAL_CRONICA = funcion_recodificar_si_no(RENAL_CRONICA),

               OTRO_CASO = funcion_recodificar_si_no(OTRO_CASO),

               TOMA_MUESTRA_ANTIGENO = funcion_recodificar_si_no(TOMA_MUESTRA_ANTIGENO),

               TOMA_MUESTRA_LAB = funcion_recodificar_si_no(TOMA_MUESTRA_LAB),

               RESULTADO_LAB = funcion_recodificar_resultado(RESULTADO_LAB),

               RESULTADO_ANTIGENO = funcion_recodificar_resultado(RESULTADO_ANTIGENO),

               CLASIFICACION_FINAL = recode(CLASIFICACION_FINAL,
                                            "1" = "CONFIRMADO POR ASOCIACION CLINICA",
                                            "2" = "CONFIRMADO POR DICTAMINACION",
                                            "3" = "POSITIVO A SARS-COV-2",
                                            "4" = "INVALIDO POR LABORATORIO",
                                            "5" = "NO REALIZADO POR LABORATORIO",
                                            "6" = "SOSPECHOSO",
                                            "7" = "NEGATIVO A SARS-COV-2") )
}

recodifica_variables <- function(datos_covid, poblaciones, vars){

  UM = vars[1]
  COMPLICACION = vars[2]
  COMORBILIDADES = vars[3]
  LABORATORIO = vars[4]
  SOCIO_CULT_DEMO = vars[5]


  datos_covid <-datos_covid %>% mutate(
    FECHA_INGRESO = as.Date(FECHA_INGRESO, format = "%Y-%m-%d"),
    FECHA_ACTUALIZACION = as.Date(FECHA_ACTUALIZACION, format = "%Y-%m-%d"),


    ##### DATOS DEL PACIENTE ####
    FECHA_DEF = as.Date(FECHA_DEF, format = "%Y-%m-%d"),
    FECHA_SINTOMAS = as.Date(FECHA_SINTOMAS, format = "%Y-%m-%d"),


    # ENTIDAD_NAC = funcion_recodificar (ENTIDAD_NAC),
    #
    # ENTIDAD_RES = funcion_recodificar(ENTIDAD_RES),


    SEXO = recode(SEXO,
                  "1" = "MUJER",
                  "2" = "HOMBRE",
                  "99" = "NO ESPECIFICADO"),

    EDAD = as.numeric(EDAD),



    #Creamos la variable para grupos de edad
    GRUPO_EDAD = factor(case_when(EDAD > 69 ~ '>70',
                                  EDAD >= 60  & EDAD <= 69 ~ '60 - 69',
                                  EDAD >= 50  & EDAD <= 59 ~ '50 - 59',
                                  EDAD >= 40  & EDAD <= 49 ~ '40 - 49',
                                  EDAD >= 30  & EDAD <= 39 ~ '30 - 39',
                                  EDAD >= 20  & EDAD <= 29 ~ '20 - 29',
                                  EDAD >= 10  & EDAD <= 19 ~ '10 - 19',
                                  EDAD < 9 ~ '<9')),


    CLASIFICACION_FINAL = recode(CLASIFICACION_FINAL,
                                 "1" = "CONFIRMADO POR ASOCIACION CLINICA",
                                 "2" = "CONFIRMADO POR DICTAMINACION",
                                 "3" = "POSITIVO A SARS-COV-2",
                                 "4" = "INVALIDO POR LABORATORIO",
                                 "5" = "NO REALIZADO POR LABORATORIO",
                                 "6" = "SOSPECHOSO",
                                 "7" = "NEGATIVO A SARS-COV-2") )


  if(COMORBILIDADES == T){
    datos_covid <- datos_covid %>%
      mutate( EMBARAZO = funcion_recodificar_si_no(EMBARAZO),

              DIABETES = funcion_recodificar_si_no(DIABETES),

              EPOC = funcion_recodificar_si_no(EPOC),

              ASMA = funcion_recodificar_si_no(ASMA),

              INMUSUPR = funcion_recodificar_si_no(INMUSUPR),

              HIPERTENSION = funcion_recodificar_si_no(HIPERTENSION),

              OTRA_COM = funcion_recodificar_si_no(OTRA_COM),

              CARDIOVASCULAR = funcion_recodificar_si_no(CARDIOVASCULAR),

              OBESIDAD = funcion_recodificar_si_no(OBESIDAD),

              TABAQUISMO = funcion_recodificar_si_no(TABAQUISMO),

              RENAL_CRONICA = funcion_recodificar_si_no(RENAL_CRONICA),

              OTRO_CASO = funcion_recodificar_si_no(OTRO_CASO))

  }


  if(COMPLICACION == T){
    datos_covid <- datos_covid %>%
      mutate( TIPO_PACIENTE = recode(TIPO_PACIENTE,
                                     "1" = "AMBULATORIO",
                                     "2" = "HOSPITALIZADO",
                                     "99" = "NO ESPECIFICADO"),

              INTUBADO = funcion_recodificar_si_no(INTUBADO),

              NEUMONIA = funcion_recodificar_si_no(NEUMONIA),

              UCI = funcion_recodificar_si_no(UCI))

  }

  if(LABORATORIO == T){
    datos_covid <- datos_covid %>%
      mutate( TOMA_MUESTRA_ANTIGENO = funcion_recodificar_si_no(TOMA_MUESTRA_ANTIGENO),

              TOMA_MUESTRA_LAB = funcion_recodificar_si_no(TOMA_MUESTRA_LAB),

              RESULTADO_LAB = funcion_recodificar_resultado(RESULTADO_LAB),

              RESULTADO_ANTIGENO = funcion_recodificar_resultado(RESULTADO_ANTIGENO))
  }

  if(SOCIO_CULT_DEMO == T){
    datos_covid <- datos_covid %>%
      mutate( NACIONALIDAD = recode(NACIONALIDAD,
                                    "1" = "MEXICANA",
                                    "2" = "EXTRANJERA",
                                    "99" = "NO ESPECIFICADO"),

              INDIGENA = funcion_recodificar_si_no(INDIGENA),

              MIGRANTE = funcion_recodificar_si_no(MIGRANTE),

              HABLA_LENGUA_INDIG = funcion_recodificar_si_no(HABLA_LENGUA_INDIG))


    #"PAIS_NACIONALIDAD",
    #"PAIS_ORIGEN",
  }

  if(poblaciones == 1){

    poblaciones <-read.csv("https://raw.githubusercontent.com/AllanZamb/DatosAbiertosCOVID19/main/poblaciones/Poblaciones_INE_Muni.csv", encoding = "UTF-8")


    datos_covid_ <- datos_covid %>%
      left_join(., poblaciones , by = c("ENTIDAD_RES" = "CVE_ENT" ,
                                        "MUNICIPIO_RES" = "CVE_MUN")) %>%
      left_join(., poblaciones %>%
                  select(CVE_ENT, NACIMIENTO=DESC_ENT) %>%
                  filter(!duplicated(.)) , by = c("ENTIDAD_NAC"= "CVE_ENT" )) %>%
      select(-ENTIDAD_NAC, -ENTIDAD_RES, -MUNICIPIO_RES) %>%
      rename(ENTIDAD_RES=DESC_ENT, MUNICIPIO_RES=DESC_MUN, ENTIDAD_NAC = NACIMIENTO, POB_MUNI_RES=POBTOT ) %>%

      left_join(., poblaciones %>%
                  group_by(DESC_ENT ) %>%
                  summarise(POB_ENT_RES = sum(POBTOT)), by = c("ENTIDAD_RES"= "DESC_ENT"))

    return(datos_covid_)


  }else if (poblaciones == 2){

    poblaciones <-read.csv("https://raw.githubusercontent.com/AllanZamb/DatosAbiertosCOVID19/main/poblaciones/Poblaciones_INE_Muni.csv", encoding = "UTF-8")


    datos_covid_ <- datos_covid %>%
      left_join(., poblaciones , by = c("ENTIDAD_RES" = "CVE_ENT" ,
                                        "MUNICIPIO_RES" = "CVE_MUN")) %>%
      left_join(., poblaciones %>%
                  select(CVE_ENT, NACIMIENTO=DESC_ENT) %>%
                  filter(!duplicated(.)) , by = c("ENTIDAD_NAC"= "CVE_ENT" )) %>%
      select(-ENTIDAD_NAC, -ENTIDAD_RES, -MUNICIPIO_RES) %>%
      rename(ENTIDAD_RES=DESC_ENT, MUNICIPIO_RES=DESC_MUN, ENTIDAD_NAC = NACIMIENTO, POB_MUNI_RES=POBTOT ) %>%

      left_join(., poblaciones %>%
                  group_by(DESC_ENT ) %>%
                  summarise(POB_ENT_RES = sum(POBTOT)), by = c("ENTIDAD_RES"= "DESC_ENT")) %>%
      select(-POB_MUNI_RES)

    return(datos_covid_)


  }else{

    poblaciones_INE <-read.csv("https://raw.githubusercontent.com/AllanZamb/DatosAbiertosCOVID19/main/poblaciones/Poblaciones_INE_Muni.csv", encoding = "UTF-8")


    datos_covid_ <- datos_covid %>%
      left_join(., poblaciones_INE , by = c("ENTIDAD_RES" = "CVE_ENT" ,
                                            "MUNICIPIO_RES" = "CVE_MUN")) %>%

      left_join(., poblaciones_INE %>%
                  select(CVE_ENT, NACIMIENTO=DESC_ENT) %>%
                  filter(!duplicated(.)) , by = c("ENTIDAD_NAC"= "CVE_ENT" )) %>%


      select(-ENTIDAD_NAC, -ENTIDAD_RES, -MUNICIPIO_RES, -POBTOT) %>%
      rename(ENTIDAD_RES=DESC_ENT, MUNICIPIO_RES=DESC_MUN, ENTIDAD_NAC = NACIMIENTO)


  }
}

# FUNCION 3 : RECODIFICA LA VARIABLE ESTADOS

funcion_recodificar <- function(x){
  recode(x,
         "1" = "AGUASCALIENTES",
         "2" = "BAJA CALIFORNIA",
         "3" = "BAJA CALIFORNIA SUR",
         "4" = "CAMPECHE",
         "5" = "COAHUILA",
         "6" = "COLIMA",
         "7" = "CHIAPAS",
         "8" = "CHIHUAHUA",
         "9" = "CIUDAD DE MEXICO",
         "10" = "DURANGO",
         "11" = "GUANAJUATO",
         "12" = "GUERRERO",
         "13" = "HIDALGO",
         "14" = "JALISCO",
         "15" = "MEXICO",
         "16" = "MICHOACAN",
         "17" = "MORELOS",
         "18" = "NAYARIT",
         "19" = "NUEVO LEON",
         "20" = "OAXACA",
         "21" = "PUEBLA",
         "22" = "QUERETARO",
         "23" = "QUINTANA ROO",
         "24" = "SAN LUIS POTOSI",
         "25" = "SINALOA",
         "26" = "SONORA",
         "27" = "TABASCO",
         "28" = "TAMAULIPAS",
         "29" = "TLAXCALA",
         "30" = "VERACRUZ",
         "31" = "YUCATAN",
         "32" = "ZACATECAS",
         "36" = "ESTADOS UNIDOS MEXICANOS",
         "97" = "NO APLICA",
         "98" = "SE IGNORA",
         "99" = "NO ESPECIFICADO")
}


# FUNCION 4 :  RECODIFICA SI NO Y DEMAS

funcion_recodificar_si_no <- function(x){
  recode(x,
         "1" = "SI",
         "2" = "NO",
         "97" = "NO APLICA",
         "98" = "SE IGNORA",
         "99" = "NO ESPECIFICADO")
}


# FUNCION 5 : RECODIFICA LA VARIABLE RESULTADO

funcion_recodificar_resultado <- function(x){
  recode(x,
         "1" = "POSITIVO A SARS-COV-2",
         "2" = "NO POSITIVO A SARS-COV-2",
         "3" = "RESULTADO PENDIENTE",
         "4" = "RESULTADO NO ADECUADO",
         "97" = "NO APLICA (CASO SIN MUESTRA)")
}


# FUNCION 6 : RECODIFICA LA VARIABLE MUNICIPIOS

# Con esta funcición recodificamos la variables de los municipios por nombre
#Realizamos la carga del catálogo y mediante left_join unimos la tabla, creando las nuevas variables.
recodifica_poblaciones <- function(x){
  poblaciones <- read.csv("https://raw.githubusercontent.com/AllanZamb/DatosAbiertosCOVID19/main/poblaciones/POBLACIONES_ENTIDADES_2021.csv") %>%
    rename(ENTIDAD_RES = 1)

  #catalogo_municipios <- read_excel("CATALOGO_MUNICIPIOS.xlsx",sheet = 1) %>%
  catalogo_municipios <- read.csv("https://raw.githubusercontent.com/AllanZamb/DatosAbiertosCOVID19/main/poblaciones/POBLACIONES_MUNICIPIOS_2021.csv") %>%
    mutate(CLAVE_MUNICIPIO = as.integer(CLAVE_MUNICIPIO),
           CLAVE_ENTIDAD = as.integer(CLAVE_ENTIDAD)) %>%
    left_join(.,poblaciones, by = c("CLAVE_MUNICIPIO"="MUNICIPIO_RES",
                                    "CLAVE_ENTIDAD"="CLAVE_ENTIDAD")) %>%
    left_join( x,  ., by= c("ENTIDAD_RES"="ENTIDAD",
                            "MUNICIPIO_RES"="CLAVE_MUNICIPIO"))
}

