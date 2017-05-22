##########################################################################
# Jose Cajide - @jrcajide
# Curso de introducción a R
##########################################################################

library(shiny)
library(plotly)

library(tidyverse)

world.pop <- readxl::read_excel('data/WPP2015_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.XLS', sheet = 1, skip = 16) %>% 
  filter(Index == "1") %>% 
  select(matches("\\d")) %>% 
  gather('year', 'population')




world.pop.ts <- ts(world.pop$population, start = c(1950), end = c(2015))

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("event")
)

server <- function(input, output) {
  
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    ggplotly(autoplot(forecast::forecast(fit)))
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Selecciona un punto del gráfico" else d
  })
}

shinyApp(ui, server)