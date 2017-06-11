shinyUI(fluidPage(
  titlePanel("App - Gapminder"),
  
  sidebarLayout(
    sidebarPanel("Controles del usuario",
                 uiOutput("choose_country"),
                 sliderInput("year_range", 
                             label = "Range of years:",
                             min = 1952, max = 2007, value = c(1955, 2005), format = "####")
    ),
    mainPanel("Espacio para el gráfico",
              textOutput("output_country"),
              plotOutput("ggplot_gdppc_vs_country"),
              tableOutput("gapminder_table")              
    )
  )
))