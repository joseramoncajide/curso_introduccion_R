library(tidyverse)

gDat <- read_csv(file = "data/gapminder.csv", col_types = list(country = col_factor(levels = NULL)) )

server <- function(input, output) {
  
  # Desplagable
  output$choose_country <- renderUI({
    selectInput("country_from_gapminder", "Selecciona un país:", as.list(levels(gDat$country)))
  })
  
  # Filtrado de datos según la selección del usuario en el desplegable y las fechas
  one_country_data <- reactive({
    if(is.null(input$country_from_gapminder)) {
      return(NULL)
    }
    gDat %>% filter(country == input$country_from_gapminder 
                    & year >= input$year_range[1] 
                    & year <= input$year_range[2])
  })
  
  # Tabla de datos:
  output$gapminder_table <- renderTable({ 
    one_country_data()
  })
  
  # Mensaje de país seleccionado:
  output$output_country <- renderText({
    if (is.null(input$country_from_gapminder)){
      return(NULL)
    }
    paste("Mostrando datos de ", input$country_from_gapminder)
  })
  
  # Gráfico
  output$ggplot_lifeExp <- renderPlot({
    if(is.null(one_country_data())) {
      return(NULL)
    }
    p <-  ggplot(one_country_data(), aes(x = year, y =  lifeExp)) + geom_point()
    p
  })

}