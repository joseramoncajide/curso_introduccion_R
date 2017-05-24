##########################################################################
# Jose Cajide - @jrcajide
# Curso de introducción a R
##########################################################################


# Creación de data frames -------------------------------------------------

df <-
  data.frame(
    month = month.name[1:12],
    sesiones = round(rnorm(12, mean = 400, sd = 10), 2),
    transacciones = round(rnorm(12, mean = 20, sd = 4), 2)
  )
str(df)

# Vector de datos de una variable

df[, c('sesiones')]
df[, 2]
df$sesiones

# Subsetting
df[c(1, 3), ]

df[, c("month", "transacciones")]
df[c("month", "transacciones")]


pocas_sesiones <- df$sesiones < 400
df[pocas_sesiones, ]
df[df$sesiones < 400, ]



# cargar datos ------------------------------------------------------------

data()
datasets::cars
df <- datasets::cars
plot(df)

# Algunos paquetes traen conjuntos de datos
# install.packages('gapminder')
library(gapminder)
gapminder

# Motivación
library(tidyverse)
gapminder %>% 
  ggplot(aes(year, lifeExp, group = country, color = continent)) +
  geom_line(alpha = 1/3) 

rm(gapminder)

# importación de datos ----------------------------------------------------
# install.packages('tidyverse')
library(tidyverse)

gapminder <- read_csv("data/gapminder.csv")

# Ejercicio:
# Carga el archivo data/gapminder.txt
# ?read_delim
gapminder <- read_delim()


head(gapminder)
tail(gapminder, 2)
str(gapminder)
summary(gapminder)
summary(gapminder$continent)
gapminder$country
unique(gapminder$continent)
length(unique(gapminder$continent))
table(gapminder$continent)

# Motivación:
mean(gapminder[gapminder$continent == "Africa", "gdpPercap"]$gdpPercap, na.rm = T)
# Y para Asia? Y

# Incluido en tidyverse
# install.packages('dplyr')
# library("dplyr")

# select()
year_country_gdp <- select(gapminder,year,country,gdpPercap)

# Usando pipe %>%

mean(1:20)

1:20 %>% mean()

head(gapminder)

gapminder %>% head

year_country_gdp <- gapminder %>% select(year,country,gdpPercap)

gapminder %>% select(year,country,gdpPercap)
gapminder %>% select(-continent, -lifeExp, -pop)

# filter()
year_country_gdp_euro <- gapminder %>%
  filter(continent=="Europe") %>%
  select(year,country,gdpPercap)

# group_by() sobre una variable

gdp_bycontinents <- gapminder %>%
  group_by(continent) %>%
  summarize(mean_gdpPercap=mean(gdpPercap))


# ¿Qué países tienen la mayor y menor esperanza de vida? 

lifeExp_bycountry <- gapminder %>%
  group_by(country) %>%
  summarize(mean_lifeExp=mean(lifeExp)) %>% 
  filter(mean_lifeExp == min(mean_lifeExp) | mean_lifeExp == max(mean_lifeExp))

# Ejercicio:
# ¿Y sólo en Europa?

lifeExp_bycountry <- gapminder %>% 
  ...



# group_by() sobre más de una variable

gdp_bycontinents_byyear <- gapminder %>%
  group_by(continent,year) %>%
  summarize(mean_gdpPercap=mean(gdpPercap))

# summarise_each()
# Partimos de:

gapminder %>%
  filter(year %in% c(1952, 2007)) %>%
  group_by(continent,year) %>%
  summarize(mean_gdpPercap=mean(gdpPercap),
            sd_gdpPercap=sd(gdpPercap),
            mean_pop=mean(pop),
            sd_pop=sd(pop))

gapminder %>%
  filter(year %in% c(1952, 2007)) %>%
  group_by(continent, year) %>%
  summarise_each(funs(mean, median), gdpPercap, pop)

# count() y n()

# Cuantas observaciones hay por continente?

gapminder %>% 
  count(continent)

gapminder %>%
  group_by(continent) %>%
  summarise(n = n())

# Tabla de frecuencias
table(gapminder$continent)

gapminder %>%
  group_by(continent) %>%
  tally()

# n_distinct()

gapminder %>%
  group_by(continent) %>%
  summarize(n = n(),
            n_countries = n_distinct(country))


# mutate()

gapminder %>% 
  mutate(gdp = pop * gdpPercap)


# arrange

gapminder %>%
  filter(year == 2007) %>%
  arrange(lifeExp)

gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(lifeExp))


# rename

gapminder %>%
  rename(life_exp = lifeExp,
         gdp_percap = gdpPercap)

# reordenar variables

gapminder %>%
filter(country == "Burundi", year > 1996) %>% 
  select(yr = year, lifeExp, gdpPercap) %>% 
  select(gdpPercap, everything())


# Incremento de la esperanza de vida desde 1952

gapminder %>% 
  group_by(country) %>% 
  filter(country == "Burundi") %>% 
  select(country, year, lifeExp) %>% 
  mutate(lifeExp_gain = lifeExp - first(lifeExp))


# Convertir columnasen filas con gather()
library(tidyr)

# https://docs.google.com/spreadsheets/d/1NTXQNoY8V0H_EZ_peFmnH1ZcGlxCPhwl2VmJNpiACMU/edit#gid=1098192593

library(googlesheets)
gap_wide <- read_csv("https://docs.google.com/spreadsheets/d/1NTXQNoY8V0H_EZ_peFmnH1ZcGlxCPhwl2VmJNpiACMU/pub?output=csv")
# gap_wide <- read.csv('data/gapminder_wide.csv')
head(gap_wide)

gap_long <- gap_wide %>% 
  gather(key   = obstype_year,
         value = obs_values)
head(gap_long)
tail(gap_long)

# Debemos indicar las columnas a transformar
gap_long <- gap_wide %>% 
  gather(key   = obstype_year,
         value = obs_values,
         3:38)  # ó -1:-2
head(gap_long)
tail(gap_long)

# Alternativa
gap_long <- gap_wide %>% 
  gather(key   = obstype_year,
         value = obs_values,
         dplyr::starts_with('pop'),
         dplyr::starts_with('lifeExp'),
         dplyr::starts_with('gdpPercap'))
head(gap_long)
tail(gap_long)

# ¿Que ocurre con la variable 'obstype_year'?

gap_long <- gap_wide %>% 
  gather(key   = obstype_year,
         value = obs_values,
         -continent, -country) %>%
  separate(obstype_year,
           into = c('obs_type','year'),
           sep="_")
head(gap_long)
tail(gap_long)

# spread()
gap_normal <- gap_long %>% 
  spread(obs_type, obs_values)

head(gap_normal)
head(gapminder)
