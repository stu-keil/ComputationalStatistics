##Teorema del Limite Centra y algunas distribuciones


# SIMULACION, ALGUNOS METODOS Y
# APLICACIONES
# Hugo Carrasco

##Metodo de la funcion inversa CDF - si genero numeros aleatorios uniformes y les aplico la inversa de la CDF genero numeros aleatorios en esa distribucion

N <- 10000
l <- qexp(runif(N))
hist(sort(l),breaks = 1000,freq=FALSE, col = "red", main="Exponential Distribution")


##Direct number generation is faster
N <- 10^8
system.time(qnorm(runif(N)))
# user  system elapsed 
# 1.67    0.00    1.70 
system.time(rnorm(N)) 
# user  system elapsed 
# 1.50    0.00    1.51 


par(mfrow = c(1,1))
random <- cbind(runif(1000),runif(1000))
plot(random[,1],random[,2])



####Exponential Distribution

simulations <- 10000
x <- list()
for (i in 1:simulations){
  x[[i]] <- rexp(1000,3)
}

par(mfrow = c(2,1))
hist(x[[1]],breaks = 100,freq=FALSE, col = "red", main="Exponential Distribution")
hist(sapply(x,mean),breaks = 100,freq=FALSE, col = "red", main="Mean of the sample mean")
abline(v=mean(sapply(x,mean)),lty = 6, col = 21, lwd =3)

mean(x[[1]])#dberia ser 1/lambda un tercio
var(x[[1]])#Deberia de ser

####Uniform Distribution

simulations <- 10000
x <- list()
for (i in 1:simulations){
  x[[i]] <- runif(12,0,1)
}

par(mfrow = c(2,1))
hist(x[[1]],breaks = 100,freq=FALSE, col = "red", main="Uniform Distribution")
hist(sapply(x,mean),breaks = 100,freq=FALSE, col = "red", main="Mean of the sample mean")
abline(v=mean(sapply(x,mean)),lty = 6, col = 21, lwd =3)


hist(sapply(x,sum),breaks = 100,freq=FALSE, col = "red", main="Mean of the sample mean")
mean(sapply(x,sum))
var(sapply(x,sum))

hist(sapply(x,sum)-6,breaks = 100,freq=FALSE, col = "red", main="Mean of the sample mean")


mean(x[[1]])#dberia ser 1/2
var(x[[1]])#Deberia de ser 1/12


#####Beta Distribution


####Beta Distribution

simulations <- 10000
x <- list()
for (i in 1:simulations){
  x[[i]] <- rbeta(1000,3,1)
}

par(mfrow = c(2,1))
hist(x[[1]],breaks = 100,freq=FALSE, col = "red", main="Beta Distribution")
hist(sapply(x,mean),breaks = 100,freq=FALSE, col = "red", main="Mean of the sample mean")
abline(v=mean(sapply(x,mean)),lty = 6, col = 21, lwd =3)

mean(x[[1]])#dberia ser a/a+b o 0.75
var(x[[1]])#Deberia ser ab/(a+b)^2(a+b+1) 
3/(16*5)

#Quiero generar una serie de aleatorios pertenecientes a una distribucion normal de media 4 y desviacion estandar 2
x <- rnorm(1000,0,1)
y <- 2*x + 4
plot(density(y))


unif1 <- runif(50,0,1)
unif2 <- runif(50,0,2*pi)
class(unif1)


xnorm   <- seq(-4,4,length=1000)
ynorm   <- dnorm(xnorm,mean=0, sd=1)
plot(xnorm,ynorm, type="l",col = 'peru', lwd=2)

?legend
install.packages('Rtools')

shinyapps::setAccountInfo(name='stukeil', token='BA06134ED1D87DDD82A35CE4702134D9', secret='ZndhIUxGaVaw9QHyh5fO/QYleHybGWZjJlNoyZxr')

library(shinyapps)
shinyapps::deployApp("C:/Users/Stephane/Desktop/ITAM/EstadisticaComputacional/CompuStat/Beta")

getwd()
