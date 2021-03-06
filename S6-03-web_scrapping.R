##########################################################################
# Jose Cajide - @jrcajide
# Curso de introducción a R: Webscraping
##########################################################################

rm(list=ls()) 
cat("\014")

library(tidyverse)
library(rvest)
library(googleLanguageR)
library(stringr)


#----------------------------------------------------------------------------
# stringr
#----------------------------------------------------------------------------

"28.028 Madrid" %>% str_to_upper()
"28.028 Madrid" %>% str_split(" ")
"28.028 Madrid" %>% str_split(boundary("word"))
"28.028 Madrid" %>% str_count(boundary("word"))


"28.028 Madrid" %>% str_sub(0, 6)
"28.028 Madrid" %>% str_replace('\\.', '')
"28.028 Madrid" %>% str_sub(0, 6) %>% str_replace('\\.', '') %>% as.factor()


#----------------------------------------------------------------------------
# rvest
#----------------------------------------------------------------------------


url_madrid <- "http://resultados.elpais.com/elecciones/2011/municipales/12/28/79.html"
html_madrid <- read_html(url_madrid)

partidos <- html_madrid %>% html_nodes(".nombrePartido") %>% html_text()
concejales <- html_madrid %>% html_nodes(".tipoNumeroElectos") %>% html_text() %>% as.numeric
votos <- html_madrid %>% html_nodes(".tipoNumeroVotos") %>% html_text() %>% as.numeric

madrid <- data_frame(partidos, concejales, votos)
madrid


madrid <- html_madrid %>% html_node("#tablaVotosPartidos") %>% html_table()
names(madrid) <- c("partidos", "concejales", "votos", "porcentaje")


library(treemap)
library(viridis)

treemap(madrid, 
        index=c("partidos"), 
        vSize="votos", 
        type="index",
        border.lwds=.3,
        border.col="#FFFFFF",
        palette=viridis(15))



#----------------------------------------------------------------------------
# All together now!
#----------------------------------------------------------------------------

url_libro <- "https://www.amazon.es/El-monstruo-colores-Cuentos-flamboyant/product-reviews/8493987743/ref=cm_cr_getr_d_show_all?showViewpoints=1&pageNumber=1&reviewerType=all_reviews"

browseURL(url_libro)

html_libro <- url_libro %>% 
  read_html()

opiniones <- html_libro %>% 
  html_nodes(".review-text") %>% 
  html_text()

head(opiniones, 3)

# Ejercicio: obten la puntuación dada por cada usuario.
# Deberás obtener sólo las opiniones correspondientes a las anteriores opiniones. Estas se encuentran dentro de una etiqueta HTML con una clase "a-icon-star" que a su vez están dentro de un div con id "cm_cr-review_list"
estrellas <- html_libro %>% 
  html_nodes("#cm_cr-review_list") %>% 
  html_nodes(".a-icon-star") %>% 
  html_text() %>% 
  str_sub(0, 3) %>% 
  str_replace(',', '.') %>% 
  as.numeric()


# Modificamos la URL para simplificar. Movemos pageNumber al final 

url <- "https://www.amazon.es/El-monstruo-colores-Cuentos-flamboyant/product-reviews/8493987743/ref=cm_cr_getr_d_show_all?showViewpoints=1&reviewerType=all_reviews&pageNumber="

num_paginas <- 2
opiniones_amazon <- NULL

for (j in 1: num_paginas){
  
  html <- read_html(paste0(url, j)) 
  
  opinion <- html %>% 
    html_nodes(".review-text") %>% 
    html_text()
  
  estrellas <- html %>% 
    html_nodes("#cm_cr-review_list") %>% 
    html_nodes(".a-icon-star") %>% 
    html_text() %>% 
    str_sub(0, 3) %>% 
    str_replace(',', '.') %>% 
    as.numeric()
  
  opiniones_amazon <- rbind(opiniones_amazon, data.frame('pagina'=j, 'opinion'=opinion, 'estrellas' = estrellas))
}

str(opiniones_amazon)

opiniones_amazon$opinion <- as.character(opiniones_amazon$opinion)

# opiniones_amazon <- read_csv('https://raw.githubusercontent.com/joseramoncajide/curso_introduccion_R/master/data/opiniones_amazon.csv')

# Calcular el sentimiento de cada opinión

gl_auth('google_cloud_platform.json')

sentimiento <- lapply(opiniones_amazon$opinion, function(t) gl_nlp(t))

puntuaciones_producto <- sapply(sentimiento, function(t) t$documentSentiment$score)

opiniones_amazon$puntuacion <- puntuaciones_producto

opiniones_amazon$sentimiento <- 'Neutro'
opiniones_amazon$sentimiento <- ifelse(opiniones_amazon$puntuacion > .2, "Positivo", opiniones_amazon$sentimiento)
opiniones_amazon$sentimiento <- ifelse(opiniones_amazon$puntuacion < -.2, "Negativo", opiniones_amazon$sentimiento)

opiniones_amazon %>% arrange(desc(sentimiento)) %>% View()

#----------------------------------------------------------------------------
# Un poco de estadística
#----------------------------------------------------------------------------

cor(opiniones_amazon$estrellas, opiniones_amazon$puntuacion)

modelo <- lm(puntuacion ~ estrellas, data=opiniones_amazon)

summary( modelo )

# ¿En que opiniones hay más discordancia entre el voto del usuario y el sentimiento asignado?

errores_modelo <- resid(modelo)

errores_destacados <- boxplot(errores_modelo, plot = F)$out

names(errores_destacados)

opiniones_amazon[names(errores_destacados), ]
