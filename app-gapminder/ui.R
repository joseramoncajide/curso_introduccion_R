ui <- fluidPage(
  titlePanel("Evolución de la experanza media de vida (lifeExp)"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("choose_country"),
      sliderInput("year_range", label = "Elije un periodo:", min = 1952, max = 2007, value = c(1955, 2005),format = "####")
    ),
    mainPanel(
      h3(textOutput("output_country")),
      plotOutput("ggplot_lifeExp"),
      tableOutput("gapminder_table")
    )
  )
)