
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


shinyServer(function(input, output) {

  u <- reactive(runif(input$n))#Creo
  x <- reactive((1/input$lambda)*log(1/(1-u())))
  
  output$distPlot <- renderPlot({
    
    
    dat <- data.frame(Value=x())
    ggplot(dat, aes(x=Value)) +
      geom_histogram(aes(y=..density..), binwidth= .05, colour="black", fill="white") +
      stat_function(fun = function(x) input$lambda*exp(-input$lambda*x),colour = "blue") +ggtitle("Histograma de frecuencias de X") + 
      theme(plot.title = element_text(lineheight=2, face="bold"))+xlab("X")+ylab("Frecuencia de X")
    #par(mfrow=c(1,2))
    #hist(x(), breaks = 50, col = 'green', border = 'white', main = "Histograma de frecuencias de X", xlab = "X", ylab = "Frecuencia de X")
    #hist(x(), breaks = 50,freq= FALSE, col = 'blue', border = 'white', main = "Histograma de frecuencias de X", xlab = "X", ylab = "Función de Densidad de Probabilidad")
    #lines(density(x()),col=2,lwd=2)
    #legend(1,1,c("Histograma normalizado","Función de densidad"),lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red"),cex = 0.75)
  })
  output$distPlot1 <- renderPlot({
   
    dat1 <- data.frame(cbind(sort(x()),sort(u())))
    ggplot(dat1,aes(x=X1,y=X2))+geom_line(colour="red", linetype="dashed", size=1.5)+ggtitle("U(X)= F(X) Función de distribución de X") +
      ylab("Función de distribución acumulada de X") + xlab("X")+
      theme(plot.title = element_text(lineheight=2, face="bold"))
    
    #hist(u(), breaks = 50,freq= FALSE, col = 'darkgray', border = 'white')
    #lines(density(u()),col=2,lwd=2)
    #plot(sort(u()),sort(u()),type="l")
    
    
  })
  
  output$view <- renderDataTable({
   
   
    data <- as.data.frame(cbind(x(),u()))
    names(data) <- c("X","Probabilidad de X<=x")
    data  
  })

})
