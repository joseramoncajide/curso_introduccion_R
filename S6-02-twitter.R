##########################################################################
# Jose Cajide - @jrcajide
# Curso de introducción a R: Apis - Twitter
##########################################################################

rm(list=ls()) 
cat("\014")

install.packages(c("devtools", "rjson", "bit64", "httr"),  repos="https://cran.rstudio.com")
devtools::install_github("geoffjentry/twitteR")

library(twitteR)

# https://apps.twitter.com/

api_key <- "FIfsGBnsvDY8cMUE4NtZzA3rQ"
api_secret <- "PvkuFQX6WXHCladsHrnT0l1nNtNhylQrLRMewmmNTA47YWWhkZ"
access_token <- "194127650-LgEWlrQxTX7li7qBwIIHbN9jEbsMNE9s6ULN6yMs"
access_token_secret <- "TqooOn58tDJ8duYrWsul7YtXX2ZOu75rRBD4iUpLeOB8B"

# Autenticacion
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

termino_a_buscar <- "#Zidane"
numero_tweets <- 100

tweets <- searchTwitter(termino_a_buscar, 
                        n=numero_tweets, 
                        lang="es", 
                        resultType="recent")

texto_tweets <- lapply(tweets, function(t) t$getText())

limpia_texto_tweets <- function (tweet) {

  # eliminamos las entidades
  tweet = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', tweet)
  # eliminamos usuarios
  tweet = gsub('@\\w+', '', tweet)
  # eliminamos símbolos de puntuación
  tweet = gsub('[[:punct:]]', '', tweet)
  # eliminamos números
  tweet = gsub('[[:digit:]]', '', tweet)
  # eliminamos los enlaces
  tweet = gsub('http\\w+', '', tweet)
  # eliminamos espacios en blaco
  tweet = gsub('[ \t]{2,}', '', tweet)
  tweet = gsub('^\\s+|\\s+$', '', tweet)
  # eliminamos emojis
  tweet = gsub('<.*>', '', enc2native(tweet))
  # texto en minúsculas
  tweet = tolower(tweet)
  
  return(tweet)
}

texto_tweets <- lapply(texto_tweets, function(t) limpia_texto_tweets(t))

texto_tweets <- unlist(texto_tweets, use.names=FALSE)

library(syuzhet)

sentimiento <- get_sentiment(texto_tweets, method="afinn", language = 'spanish')

sentimiento_df <- data_frame(tweet = texto_tweets,
           sentimiento = sentimiento)

sentimiento_df <- sentimiento_df %>% 
  mutate(sentimiento = case_when(
    sentimiento > 0 ~ "positivo",
    sentimiento < 0 ~ "negativo",
    TRUE ~ "neutro"
))

sentimiento_df %>% 
  group_by(sentimiento) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=sentimiento, y=n, fill=sentimiento)) + 
  geom_bar(stat = 'identity') + 
  theme_minimal()



# emociones ---------------------------------------------------------------

emociones <- get_nrc_sentiment(texto_tweets, language = 'spanish')

library(tidyr)
emociones <- emociones %>% 
  as_data_frame() %>% 
  gather(emocion, n) %>% 
  group_by(emocion) %>% 
  summarise(n=sum(n))

ggplot(emociones, aes(x=reorder(emocion,n), y=n, fill=emocion)) +
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  theme_minimal()

