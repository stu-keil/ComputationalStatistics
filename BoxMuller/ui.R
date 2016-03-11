
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Nombre: Stephane Keil Rios - CVU: 160559 -  Tarea 2 - Estadistica Computacional - Metodo de Box-Muller para generación de numeros aleatorios con distribución normal"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(position = "right",
    sidebarPanel(
      numericInput("num.aleatorios",
                  "Numero de aleatorios a generar:",
                  min = 0,
                  max = 100000,
                  value = 1000,
                  step = 1000
                  )
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("distPlot1"),
      plotOutput("distPlot2")
    )
  )
))
