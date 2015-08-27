
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.comhh
#

library(shiny)


shinyUI(fluidPage(

  # Application title
  titlePanel("Nombre: Stephane Keil Rios - CVU: 160559 -  Tarea 1 - Estadistica Computacional - Metodo de la función inversa de una función exponencial"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("n",
                  "Seleccionar numero de aleatorios a generar:",
                  min = 0,
                  max = 100000,
                  value = 1000
      ),
      
      sliderInput("lambda",
                  "Seleccionar parametro Lambda:",
                  min = 0.000000000000001,
                  max = 10,
                  value = 1#Valor por defecto
                  #condicion que si lambda = 0 truene
                  )
    ),
    

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("distPlot1"),
      dataTableOutput("view")
    )
  )
))
