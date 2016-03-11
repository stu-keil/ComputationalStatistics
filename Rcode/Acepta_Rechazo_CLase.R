### Aceptacion-Rechazo

f <- function(x){
  (2/sqrt(2*pi))*exp(-x^2/2)### Normal Distribution con un 2 que la gace algo rara, buquemos una funcion que la domine en el intervalo 0,5
}
g <- function(x) exp(-x)#Exponencial con Lamba igual a 1
Mg <- function(x) M*exp(-x)

x   <- seq(-5,5,length=1000)
y   <- dnorm(x,mean=0, sd=0.5)
plot(x,y, type="l", lwd=1,ylim=c(0,2), xlim=c(-5,5))

plot(f, add= TRUE, col = 'peru', ylim=c(0,2), xlim=c(-5,5))
plot(g, add=TRUE, col='red', ylim=c(0,2), xlim=c(-5,5))

h <- function(x){
  f(x)/g(x)
}
plot(h, add= TRUE, col = 'skyblue', xlim=c(0,4), ylim=c(0,2))

M <- 2
plot(Mg, add=TRUE, col='red', ylim=c(0,2), xlim=c(-5,5))
Mh <- function(x){
  f(x)/M*g(x)
}
plot(Mh, add=TRUE, col='red', ylim=c(0,2), xlim=c(-5,5))

# Aceptación y rechazo para generar
genera_una <- function(...){
  maxiter <- 10000
  iter <- 1
  while(TRUE & iter < maxiter){
    # 1) Exponencial(lambda = 1)
    Y <- rexp(1, 1)
    U <- runif(1)
    # 2) Aceptar o rechazar
    if(U <= f(Y)/(M*g(Y))){
      X <- Y # Z es el valor absoluto de una normal
      break
    }
    iter <- iter + 1
  }
  X
}

genera_muchas <- function(n = 1){
  sapply(1:n, genera_una)
}
?hist
hist(genera_muchas(10000),freq=FALSE)
plot(f,add = TRUE, ylim=c(0,2), xlim=c(0,5))


genera_normales <- function(n = 1){
  sample(c(-1,1), size = n, replace = T)*genera_muchas(n = n)
}

hist(genera_normales(10000),freq=FALSE)
plot(f, add= TRUE, col = 'peru', ylim=c(0,2), xlim=c(-5,5))
