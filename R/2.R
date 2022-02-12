unique(COVID$GRUPO_EDAD)
names(COVID)
orden_correcto_Edades <- c("<9", "10 - 19", "20 - 29",
                           "30 - 39", "40 - 49",
                           "50 - 59", "60 - 69", ">70" )

COVID %>%
  ggplot()+
  geom_bar(aes(x=factor(GRUPO_EDAD, levels = orden_correcto_Edades),
               y = ..count.., fill = SEXO), position = "stack")



COVID %>%
  ggplot()+
  geom_bar(aes(x=GRUPO_EDAD,
               y = ..count.., fill = CLASIFICACION_FINAL), position = "stack")
