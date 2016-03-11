
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Practicing Importance Sampling"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Selecciona el parametro Lambda de la función Exponencial para la que aproximaremos la integral"),
      sliderInput("m",
                  "Lambda para exponencial",
                  min = 0,
                  max = 5,
                  value = 1),
      numericInput("lowsim",
                   "Simulaciones empiezan desde",
                   min = 0,
                   max = 100000,
                   value = 10000),
      numericInput("uppsim",
                   "Simulaciones terminan en",
                   min = 0,
                   max = 1000000,
                   value = 100000),
      numericInput("by",
                   "Simulaciones avanzan de",
                   min = 0,
                   max = 100000,
                   value = 10000),
      numericInput("lambda",
                   "Lambda para exponencial truncada",
                   min = 0,
                   max = 5,
                   value = 0.5),
      numericInput("alpha",
                   "Parametro alpha de la distribución Beta",
                   min = 0,
                   max = 10,
                   value = 1),
      numericInput("beta",
                   "Parametro beta de la distribución Beta",
                   min = 0,
                   max = 10,
                   value = 2)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      h1(paste0("Comparativo de Montecarlo con y sin Importance Sampling con diversas Funciones")),
      
      tabsetPanel(
        tabPanel("Montecarlo Tradicional", plotOutput("distPlot")),
        tabPanel("Montecarlo Importance Samplingcon Exponencial Truncada", plotOutput("distPlot1")),
        tabPanel("Montecarlo Importance Sampling con Beta", plotOutput("distPlot2"))
      ),
      textOutput("text1"),
      h1(paste0("Las velocidades de convergencia son diferentes, se presentan el numero de simulaciones y el tamaño de las simulaciones para cada uno de los metodos")),
      
      plotOutput("distPlot3")
    )
  )
))
