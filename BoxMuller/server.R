
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  unif1 <- reactive(runif(input$num.aleatorios,0,1))
  unif2 <- reactive(runif(input$num.aleatorios,0,2*pi))
  
  output$distPlot <- renderPlot({

    
    x <- sqrt(-2*log(unif1()))*cos(unif2())
    y <- sqrt(-2*log(unif1()))*sin(unif2())
    par(mfrow=c(1,3))
    plot(density(x),col='skyblue',lwd=2, main = "Density for the first set of random numbers", ylim = c(0,0.4))
    plot(density(y),col='darkgray',lwd=2, main = "Density for the second set of random numbers", ylim = c(0,0.4))
    xnorm   <- seq(-4,4,length=1000)
    ynorm   <- dnorm(xnorm,mean=0, sd=1)
    plot(xnorm,ynorm, type="l",col = 'peru', lwd=2, main = "Density of the standard normal distribution", ylim = c(0,0.4))

    

  })
  output$distPlot1 <- renderPlot({
    
    
    x <- sqrt(-2*log(unif1()))*cos(unif2())
    y <- sqrt(-2*log(unif1()))*sin(unif2())
    par(mfrow=c(1,1))
    plot(density(x),col='skyblue',lwd=2, main = "Density for the first set of random numbers", ylim = c(0,0.4) )
    lines(density(y),col='darkgray',lwd=2, main = "Density for the second set of random numbers")
    xnorm   <- seq(-4,4,length=1000)
    ynorm   <- dnorm(xnorm,mean=0, sd=1)
    lines(xnorm,ynorm, type="l",col = 'peru', lwd=2, main = "Density of the standard normal distribution")
    
    
    
  })

})

