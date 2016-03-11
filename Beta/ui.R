
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Facebook de la Beta"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Selecciona los parametros alpha y beta de una función Beta muy usada en Estadistica Bayesiana por la propiedad de conjugabilidad (su distribucion a posteriori es una Beta con parametros que on transformaciiones lineales de primer grado de la distribución Beta a priori)"),
      
      numericInput("alpha",
                  "Parametro Alpha de la Distribución Beta",
                  min = 0,
                  max = 100,
                  value = 1),
      numericInput("beta",
                   "Parametro Beta de la Distribución Beta",
                   min = 0,
                   max = 100,
                   value = 1)
    ),
    
  
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("text1"),
      textOutput("text2"),
       plotOutput("distPlot"),
      plotOutput("distPlot1")
    )
    
  )
))
