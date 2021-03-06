
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(reshape)
library(gridExtra)

shinyServer(function(input, output) {

  m=reactive(input$m)
  simulaciones <- reactive(c(seq(input$lowsim,input$uppsim,input$by)))
  lambda=reactive(input$lambda)
  truncate=2
  a_beta=reactive(input$alpha)
  b_beta=reactive(input$beta)
  
  my_func <- function(x,m = 1){
    m*exp(-m*x)
  }
  analyticalintegration_0_2 <- function(lower=0,upper=2,m=1){
    return (exp(-lower*m)-exp(-upper*m))
  }
  montecarlo_tartare <- function(nsim=10000,alpha=0.05,fun=mifunc,a=0,b=1,...){
    valores.uniformes <- runif(nsim,a,b)
    mi.func <- mean((b-a)*fun(valores.uniformes,...))
    zdensityonalpha <- qnorm(alpha/2, lower.tail = FALSE)
    sample_var <- var(fun(valores.uniformes))
    interval.lower <- mi.func-zdensityonalpha*sqrt(sample_var/nsim)
    interval.upper <- mi.func+zdensityonalpha*sqrt(sample_var/nsim)
    return (data.frame(point_estimate = mi.func, int.low = interval.lower,int.upp = interval.upper))
  }  

  rExp_truncada <- function(nsim, lambda=1,truncate=2){
    return(-(1/lambda)*log(1-((1-exp(-truncate*lambda))*runif(nsim))))
  }
  
  montecarlo_importance <- function(nsim=10000,alpha=0.05,fun=mifunc,a=0,b=1,lambda=1,truncate=2,...){
    valores.exponenciales <- rExp_truncada(nsim,lambda,truncate)
    mi.func <- mean(fun(valores.exponenciales,...)/(dexp(valores.exponenciales,lambda)/(1-exp(-truncate*lambda))))
    zdensityonalpha <- qnorm(alpha/2, lower.tail = FALSE)
    sample_var <- var(fun(valores.exponenciales))
    interval.lower <- mi.func-zdensityonalpha*sqrt(sample_var/nsim)
    interval.upper <- mi.func+zdensityonalpha*sqrt(sample_var/nsim)
    return (data.frame(point_estimate = mi.func, int.low = interval.lower,int.upp = interval.upper))
  }  
  
  montecarlo_importance_beta <- function(nsim=10000,alpha=0.05,fun=mifunc,a=0,b=1,a_beta=1,b_beta=2,...){
    valores.beta <- 2*rbeta(nsim,a_beta,b_beta)
    mi.func <- mean(2*fun(valores.beta,...)/(dbeta(valores.beta/2,a_beta,b_beta)))
    zdensityonalpha <- qnorm(alpha/2, lower.tail = FALSE)
    sample_var <- var(fun(valores.beta))
    interval.lower <- mi.func-zdensityonalpha*sqrt(sample_var/nsim)
    interval.upper <- mi.func+zdensityonalpha*sqrt(sample_var/nsim)
    return (data.frame(point_estimate = mi.func, int.low = interval.lower,int.upp = interval.upper))
  }  
  
  
  
  res_montecarlo_raw <- reactive(sapply(simulaciones(),montecarlo_tartare,0.05,my_func,0,2,m=m()))
  errores <- reactive(analyticalintegration_0_2(0,2,m()) - sapply(res_montecarlo_raw()[1,],c)) 
  vector1 <- reactive(errores())
  #plot(1:length(simulaciones),errores,type="l")
  res_montecarlo_importance <- reactive(sapply(simulaciones(),montecarlo_importance,0.05,my_func,0,2,lambda(),truncate,m=m()))
  errores_importance <- reactive(analyticalintegration_0_2(0,2,m()) - sapply(res_montecarlo_importance()[1,],c)) 
  vector2 <- reactive(errores_importance())
  #plot(1:length(simulaciones),errores_importance,type="l")
  res_montecarlo_importance_beta <- reactive(sapply(simulaciones(),montecarlo_importance_beta,0.05,my_func,0,2,a_beta(),b_beta(),m=m()))
  errores_importance_beta <- reactive(analyticalintegration_0_2(0,2,m()) - sapply(res_montecarlo_importance_beta()[1,],c)) 
  vector3 <- reactive(errores_importance_beta())
  #plot(1:length(simulaciones),errores_importance_beta,type="l")
  
  
  output$text1 <- renderText({ 
    paste0("El valor de la integral deberia ser ",analyticalintegration_0_2(0,2,m()))
  })
  
  
  
  output$distPlot <- renderPlot({
   
    plot(sapply(res_montecarlo_raw()[1,],c),type="l",xlab="Indice de Simulación",ylab="Valor de la aproximación",main="Aproximación de la Integral con Montecarlo Crudo",ylim=c(do.call(min,res_montecarlo_raw()[2,]),do.call(max,res_montecarlo_raw()[3,])))
    lines(sapply(res_montecarlo_raw()[2,],c),type="l",lty=2,col="red")
    lines(sapply(res_montecarlo_raw()[3,],c),type="l",lty=2,col="red")
    abline(h =analyticalintegration_0_2(0,2,m()), untf = FALSE)
  })
  output$distPlot1 <- renderPlot({
    
    plot(sapply(res_montecarlo_importance()[1,],c),type="l",xlab="Indice de Simulación",ylab="Valor de la aproximación",main="Aproximación de la Integral con una Exponencial Truncada en 2",ylim=c(do.call(min,res_montecarlo_raw()[2,]),do.call(max,res_montecarlo_raw()[3,])))
    lines(sapply(res_montecarlo_importance()[2,],c),type="l",lty=2,col="red")
    lines(sapply(res_montecarlo_importance()[3,],c),type="l",lty=2,col="red")
    abline(h =analyticalintegration_0_2(0,2,m()), untf = FALSE)
    
  })
  output$distPlot2 <- renderPlot({
    
    plot(sapply(res_montecarlo_importance_beta()[1,],c),type="l",xlab="Indice de Simulación",ylab="Valor de la aproximación",main="Aproximación de la Integral con una Beta",ylim=c(do.call(min,res_montecarlo_raw()[2,]),do.call(max,res_montecarlo_raw()[3,])))
    lines(sapply(res_montecarlo_importance_beta()[2,],c),type="l",lty=2,col="red")
    lines(sapply(res_montecarlo_importance_beta()[3,],c),type="l",lty=2,col="red")
    abline(h =analyticalintegration_0_2(0,2,m()), untf = FALSE)
    
  })
  output$distPlot3 <- renderPlot({
    simul <- simulaciones()
    df <- data.frame(simul,Montecarlo_Crudo=vector1(),Importance_Sampling_Exp_Truncada=vector2(),Importance_Sampling_Beta=vector3())
    df1 <- melt(df,id.vars = 1,variable.name = "Tipo de aproximación")
    df1 <- cbind(rep(1:length(simul),3),df1)
    names(df1) = c("indice",names(df1)[2:4])
    plot1 <- ggplot(df1, aes(indice,value)) + geom_line(aes(colour = variable)) + theme(legend.position="top") + xlab("Indice de simulacion") +ylab("Error al valor real") +ggtitle("Comparativo del error de aproximación de la integral para diferentes tamaños de simulaciones")
    plot2 <- ggplot(df1, aes(simul,value)) + geom_line(aes(colour = variable)) + theme(legend.position="top") + xlab("Tamaño de simulacion") +ylab("Error al valor real") +ggtitle("Comparativo del error de aproximación de la integral para diferentes tamaños de simulaciones")
    grid.arrange(plot1, plot2, ncol=2)
    
  })

})

