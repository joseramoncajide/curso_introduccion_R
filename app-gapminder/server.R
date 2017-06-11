library(tidyverse)

gDat <- read_csv(file = "data/gapminder.csv")

gDat$country <- as.factor(gDat$country)

shinyServer(function(input, output) {
  
  # Drop-down selection box generated from Gapminder dataset
  output$choose_country <- renderUI({
    selectInput("country_from_gapminder", "Country", as.list(levels(gDat$country)))
  })
  
  one_country_data <- reactive({
    if(is.null(input$country_from_gapminder)) {
      return(NULL)
    }
    gDat %>% filter(country == input$country_from_gapminder & year >= input$year_range[1] & year <= input$year_range[2])
  })
  
  output$gapminder_table <- renderTable({ 
    one_country_data()
  })
  output$output_country <- renderText({
    if (is.null(input$country_from_gapminder)){
      return(NULL)
    }
    paste("Country selected", input$country_from_gapminder)
  })
  output$ggplot_gdppc_vs_country <- renderPlot({
    if(is.null(one_country_data())) {
      return(NULL)
    }
    p <-  ggplot(one_country_data(), aes(x = gdpPercap, y =  lifeExp))
    p + geom_point() + geom_smooth()
  })
})