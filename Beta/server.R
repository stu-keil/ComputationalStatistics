
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  unif <- reactive(sort(runif(1000)))
  
  output$text1 <- renderText({ 
    paste0("La media o Esperanza Matematica se expresa como alpha / (alpha + beta) = ",input$alpha/(input$alpha+input$beta))
  })
  output$text2 <- renderText({ 
    var <- (input$alpha*input$beta)/((input$alpha+input$beta)^2*(input$alpha+input$beta+1))
    stddev <- sqrt(var)
    paste0("La varianza o segundo momento se expresa como alpha*beta / (alpha + beta)^2(alpha+beta+1) = ",
          var," y su desviacion estandar es ",stddev,"\\n\\n\\n")
  })
  output$distPlot <- renderPlot({
    
    plot(unif(),dbeta(unif(),input$alpha,input$beta),col = 'darkgray',  xlab="Rango de 0 a 1", ylab= "fx", 
         main = paste0("Esta es la funcion de densidad de probabilidad de Beta(",input$alpha,",",input$beta,")"),type="l")
    abline(v=input$alpha/(input$alpha+input$beta),lty = 1, col = 21, lwd =4)
    abline(v=(input$alpha/(input$alpha+input$beta))+sqrt((input$alpha*input$beta)/((input$alpha+input$beta)^2*(input$alpha+input$beta+1))),lty = 2, col = "red", lwd =2)
    abline(v=(input$alpha/(input$alpha+input$beta))-sqrt((input$alpha*input$beta)/((input$alpha+input$beta)^2*(input$alpha+input$beta+1))),lty = 2, col = "red", lwd =2)
  })
  output$distPlot1 <- renderPlot({
    
    plot(unif(),pbeta(unif(),input$alpha,input$beta),col = 'skyblue',  xlab="Rango de 0 a 1", ylab= "Fx", 
         main = paste0("Esta es la funcion cumulativa de probabilidad de Beta(",input$alpha,",",input$beta,")"),type="l")
    abline(v=input$alpha/(input$alpha+input$beta),lty = 1, col = 21, lwd =4)
    abline(v=(input$alpha/(input$alpha+input$beta))+sqrt((input$alpha*input$beta)/((input$alpha+input$beta)^2*(input$alpha+input$beta+1))),lty = 2, col = "red", lwd =2)
    abline(v=(input$alpha/(input$alpha+input$beta))-sqrt((input$alpha*input$beta)/((input$alpha+input$beta)^2*(input$alpha+input$beta+1))),lty = 2, col = "red", lwd =2)
    
    
  })

})

