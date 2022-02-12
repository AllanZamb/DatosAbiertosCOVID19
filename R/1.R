COVID <- read.csv("COVID19MEXICO (muestra).csv", encoding = "UTF-8")


#INCIDENCIA DE CASOS EN ENTIDADES DURANTE LA SEMANA EPIDEMIOLOGICA 4 y 5 (CASOS ACTIVOS)

prioritarios <- c("ZACATECAS","COLIMA",
                  "BAJA CALIFORNIA SUR",
                  "CIUDAD DE MEXICO","SINALOA")

INCIDENCIA_DIA<-COVID %>%
  mutate(FECHA_SINTOMAS = as.Date(FECHA_SINTOMAS)) %>%

  group_by(ENTIDAD_RES,
           FECHA_SINTOMAS,
           POBLACION) %>%
  summarise(CASOS = n()) %>%

  mutate(INCIDENCIA = (CASOS / POBLACION * 1000000)) %>%
  filter(FECHA_SINTOMAS >= "2022-01-23" & FECHA_SINTOMAS <= "2022-02-05" ) %>%
  filter(ENTIDAD_RES %in% prioritarios )


INCIDENCIA_DIA %>%
  ggplot()+
  geom_bar(aes(x=FECHA_SINTOMAS, y= INCIDENCIA, fill = ENTIDAD_RES), stat = "identity", position = "dodge")+
  geom_line(aes(x=FECHA_SINTOMAS, y= INCIDENCIA, color = ENTIDAD_RES))




