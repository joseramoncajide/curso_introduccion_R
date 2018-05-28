##########################################################################
# Jose Cajide - @jrcajide
# Curso de introducción a R: 
# Master Data Science: Tidyr
##########################################################################


library(tidyverse)
library(tidyr)

gapminder <- read_csv("data/gapminder.csv")

gap_wide <- read_csv("data/gapminder_wide.csv")
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

gapminder <- gap_normal 


ggplot(gapminder, aes(x = continent, y = lifeExp)) +
  geom_boxplot(outlier.colour = "hotpink") +
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4)

library(modelr)

gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)


nz <- filter(gapminder, country == "New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")

nz %>% 
  add_residuals(nz_mod) %>% 
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")

# How can we easily fit that model to every country?

by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

by_country

by_country$data[[1]]

country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

# Option 1
models <- map(by_country$data, country_model)

by_country <- by_country %>% 
  mutate(model = map(data, country_model))
by_country

?add_residuals

add_residuals(nz, nz_mod)

by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_country

resids <- unnest(by_country, resids)
resids

resids %>% 
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

resids %>% 
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 / 3) + 
  facet_wrap(~continent)

# Model quality
broom::glance(nz_mod)

glance <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
glance

glance %>% 
  arrange(r.squared)

glance %>% 
  ggplot(aes(continent, r.squared)) + 
  geom_jitter(width = 0.5)

bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>% 
  right_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()
