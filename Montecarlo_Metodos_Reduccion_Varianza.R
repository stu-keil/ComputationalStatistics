####### 
library(plyr)

mc.intervals <- function(Phi, N, x.dens=runif, alpha = 0.05){
  #N es un vector con todos los samplesizes
  #alpha es el intervalo de confianza 1-alpha
  #Sample es una funcion que me regresa N intentos de X
  #Phi es la funcion que quiero aproximar

  result.list <- lapply(N, function(nsim){
  
    X <- sapply(nsim, FUN = x.dens)#Evaluan la funcion de muestreo x 
    phi.x <- sapply(X,Phi)#evaluo c
    estimacion.montecarlo <- mean(phi.x)
    S2 <- var(phi.x)
    quant <- qnorm(alpha/2, lower.tail = FALSE)
    int.upper <- estimacion.montecarlo + sqrt(S2/nsim)*quant
    int.lower <- estimacion.montecarlo - sqrt(S2/nsim)*quant
    return(data.frame(N=nsim, Estimate=estimacion.montecarlo, LI = int.lower, UI =int.upper))
  })
  result.table <- ldply(result.list)
  return (result.table)
  
}


set.seed(110104)
Phi <- function(x) 2*sqrt(4-x^2)
lower.limit <- 0
upper.limit <- 2
x.dens <- function(nsim) runif(nsim, 0, 2)
N <- seq(1000,30000,1000)#Veces que corro metodo montecarlo
data <- mc.intervals(Phi,N,x.dens)
plot(data)



?rep
?replicate
#necesitas meterle la g
#t(wi) %*%
#sum(w*x)
x.dens <- function(nsim) {
  x<-rexp
}
#######Control Variates

mc.intervals <- function(Phi, N, x.dens=runif, alpha = 0.05,control=list()){
  #N es un vector con todos los samplesizes
  #alpha es el intervalo de confianza 1-alpha
  #Sample es una funcion que me regresa N intentos de X
  #Phi es la funcion que quiero aproximar
  
  result.list <- lapply(N, function(nsim){
    
    X <- sapply(nsim, FUN = x.dens)#Evaluan la funcion de muestreo x 
    phi.x <- sapply(X,Phi)#evaluo c
    estimacion.montecarlo <- mean(phi.x)
    S2 <- var(phi.x)
    ##Control viene con tres cosas- funcion C especifica contril variate -  la media - el coefficiente de correlacion 
    if(length(control)>0){
      C <- sapply()
    }
    quant <- qnorm(alpha/2, lower.tail = FALSE)
    int.upper <- estimacion.montecarlo + sqrt(S2/nsim)*quant
    int.lower <- estimacion.montecarlo - sqrt(S2/nsim)*quant
    return(data.frame(N=nsim, Estimate=estimacion.montecarlo, LI = int.lower, UI =int.upper))
  })
  result.table <- ldply(result.list)
  return (result.table)
  
}



59.6*13.26*0.01
