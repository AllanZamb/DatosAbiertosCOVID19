library(lubridate)
library(aweek)
library(tidyverse)
library(dint)


datos<-read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>% 
  select(date, continent, new_cases_smoothed, reproduction_rate, new_cases_per_million) %>% 
  filter(continent %in% c("Europe", "North America"),
         reproduction_rate < 1.5) %>% 
  mutate(epiweek = epiweek(date),
         year=year(date)) %>% 
  mutate(date2=get_date(week = epiweek,year = year)) %>% 
  mutate(date3= ifelse(date2 > Sys.Date(), 
                       (as.character(date2 %m-% years(1))) , 
                       (as.character(date2) ))) %>% 
  mutate(date4 = as.Date(date3)) %>% 
  
  group_by(date4, continent) %>% 
  summarise(reproduction_rate = mean(reproduction_rate, na.rm = T), 
            new_cases_per_million = sum(new_cases_per_million, na.rm = T))




europa_inicios<-c("2020-07-06", "2021-02-15", "2021-07-05", "2021-09-27")

#Generamos los puntos para europa
puntost_europa<-datos %>% 
  filter(continent %in% c("Europe")) %>% 
  filter(date4 %in% as.Date(europa_inicios))


datos %>% 
  filter(continent %in% c("Europe")) %>% 
  ggplot()+
  geom_line(aes(x = date4, y = (reproduction_rate), color = continent), size =1.3) +
  
  geom_line(aes(x = date4, y = scale(new_cases_per_million), color = continent), size =1.3, color = "blue") +
  
  geom_point(data = pt_europa, aes(x=date4, y = 1))+
  geom_hline(yintercept = 1, linetype=3, 
             color = "black", size=1) +
  
  scale_x_date(breaks = "1 month", minor_breaks = "1 week")+
  
  geom_vline(xintercept = as.Date(europa_inicios))+
  

  theme_bw()+
  theme(text=element_text(size=10,hjust = 0.5),
        
        axis.text.x = element_text(angle=90,
                                   hjust = 1, 
                                   size = 10),
        
        legend.text=element_text(size=12),
        
        axis.text.y = element_text(hjust = 1, 
                                   size = 12),
        
        plot.title = element_text(hjust = 0.5))

  
  

