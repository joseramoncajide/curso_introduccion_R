# install.packages("ggmap", type = "source")
library(ggmap)
library(hrbrthemes)
# devtools::install_github("Rforecastio", "hrbrmstr")
library(Rforecastio)

Sys.setenv("FORECASTIO_API_KEY" = "c39232b69c6d8f1f0564c59ef2bb8996")

now <- get_current_forecast(kschool[2], kschool[1])
temp <- round( (now$currently$temperature -  32) / 1.8, 0)

kschool <- geocode("Calle José Picón, 31 28028 MADRID", source = "google")

ggmap(get_map(kschool, zoom = 19, maptype = "satellite")) +
  geom_point(data=kschool, size = 7, shape = 13, color = "red") +
  labs(title="Curso Básico de Introducción a R", subtitle= "KSchool, 22 Mayo a 12 de Junio", caption = paste(temp, "º en MADRID")) +
  xlab("") +
  ylab("") +
  theme_ipsum_rc(grid='Y', axis=F, ticks = F) +
  theme( legend.position = "bottom", axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(), axis.title.y=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank())




#install.packages("tidyverse")
#install.packages("plotly")
#install.packages("forecast")
library(tidyverse)
world.pop <- readxl::read_excel('./data/WPP2015_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.XLS', sheet = 1, skip = 16)

world.pop <- readxl::read_excel('./data/WPP2015_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.XLS', sheet = 1, skip = 16) %>% 
  filter(Index == "1") %>% 
  select(matches("\\d")) %>% 
  gather('year', 'population')

world.pop.ts <- ts(world.pop$population, start = c(1950), end = c(2015))
plot(world.pop.ts)
fit <- forecast::auto.arima(world.pop.ts)
library(plotly); library(ggplot2)
ggplotly(autoplot(forecast::forecast(fit)))


