library(foreign)
library(mvtnorm)
library(lubridate)
library(dplyr)
install.packages('mixtools')
library(mixtools)


setwd("C:/Users/Stephane/Desktop/ITAM/EstadisticaComputacional/Proyecto_Final")
dir()
filename <- "2010-02.csv"

dtype <- c("factor","numeric","numeric","numeric","character","character","numeric","character","character")
ecobici <- read.csv(filename,header=TRUE,colClasses = dtype)
ecobici[[5]] <- ymd(ecobici[[5]])
ecobici[[8]] <- ymd(ecobici[[8]])
ecobici[[6]]<- hms(ecobici[[6]])
ecobici[[9]]<- hms(ecobici[[9]])
str(ecobici)
second(ecobici[[9]])
wday(ecobici[[8]])


ecobici %>% select(Fecha_Retiro) 
?wday
par(mfrow=c(1,1))
plot(table(hour(ecobici$Hora_Retiro[which(wday(ecobici$Fecha_Retiro) %in% c(2:6))])))
plot(table(hour(ecobici$Hora_Retiro[which(wday(ecobici$Fecha_Retiro) %in% c(1,7))])))
plot(table(hour(ecobici$Hora_Arribo)))
par(mfrow=c(1,2))
plot(table(hour(ecobici$Hora_Arribo[which(wday(ecobici$Fecha_Arribo) %in% c(2:6))])))
plot(table(hour(ecobici$Hora_Arribo[which(wday(ecobici$Fecha_Arribo) %in% c(1,7))])))

aggregate()

?aggregate
dir("./Datos")
filename2 <-"./Datos/2015-10.csv"
dtype <- c("factor","numeric","character","character","character","character","numeric","character","character")
ecobici2 <- read.csv(filename2,header=TRUE,colClasses = dtype)
ecobici2$Bici <- as.numeric(ecobici2$Bici)
ecobici2$Ciclo_Estacion_Retiro <- as.numeric(ecobici2$Ciclo_Estacion_Retiro)


plot(table(hour(ecobici2$Hora_Retiro[which(wday(ecobici2$Fecha_Retiro) %in% c(2:6))])))


#####2012

setwd("C:/Users/Stephane/Desktop/ITAM/EstadisticaComputacional/Proyecto_Final/Datos/2012")
filename <- dir(".")
 

dtype <- c("factor","numeric","numeric","numeric","character","character","numeric","character","character")
tmp <- lapply(filename,read.csv, header = TRUE, colClasses = dtype)

ecobici <- do.call(rbind,tmp)
str(ecobici)
ecobici[[5]] <- ymd(ecobici[[5]])
ecobici[[8]] <- ymd(ecobici[[8]])
ecobici[[6]]<- hms(ecobici[[6]])
ecobici[[9]]<- hms(ecobici[[9]])

plot(table(hour(ecobici$Hora_Retiro[which(wday(ecobici$Fecha_Retiro) %in% c(2:6))])+minute(ecobici$Hora_Retiro[which(wday(ecobici$Fecha_Retiro) %in% c(2:6))])/60))
plot(table(hour(ecobici$Hora_Retiro[which(wday(ecobici$Fecha_Retiro) %in% c(1,7))])+minute(ecobici$Hora_Retiro[which(wday(ecobici$Fecha_Retiro) %in% c(1,7))])/60))

plot(table(hour(ecobici$Hora_Arribo[which(wday(ecobici$Fecha_Arribo) %in% c(2:6))])+minute(ecobici$Hora_Arribo[which(wday(ecobici$Fecha_Arribo) %in% c(2:6))])/60))
plot(table(hour(ecobici$Hora_Arribo[which(wday(ecobici$Fecha_Arribo) %in% c(1,7))])+minute(ecobici$Hora_Arribo[which(wday(ecobici$Fecha_Arribo) %in% c(1,7))])/60))

setwd("C:/Users/Stephane/Desktop/ITAM/EstadisticaComputacional/Proyecto_Final/Datos/2012")
filename <- "2012-02.csv"
  dir(".")
dtype <- c("factor","numeric","numeric","numeric","character","character","numeric","character","character")
wday(ecobici$Fecha_Retiro)
ecobici <- read.csv(filename,header=TRUE,colClasses = dtype)
ecobici[[5]] <- ymd(ecobici[[5]])
ecobici[[8]] <- ymd(ecobici[[8]])
ecobici[[6]]<- hms(ecobici[[6]])
ecobici[[9]]<- hms(ecobici[[9]])


head(ecobici)
ecobici_wd <- ecobici[which(wday(ecobici$Fecha_Retiro) %in% c(2:6)),]
head(ecobici_wd)
ecobici_wd_morn <- ecobici_wd[which(hour(ecobici_wd$Hora_Retiro) %in% c(6:12)),]
head(ecobici_wd_morn)
salidas <- ecobici_wd_morn %>% select(Ciclo_Estacion_Retiro) %>% group_by(Ciclo_Estacion_Retiro) %>% summarise(salidas=n())
llegadas <- ecobici_wd_morn %>% select(Ciclo_Estacion_Arribo) %>% group_by(Ciclo_Estacion_Arribo) %>% summarise(llegadas=n())
str(salidas) 
  
matriz <-data.frame(sort(unique(c(ecobici$Ciclo_Estacion_Retiro,ecobici$Ciclo_Estacion_Arribo))))
names(matriz) <- "estacion"
matriz <- left_join(matriz, salidas, by =c("estacion"="Ciclo_Estacion_Retiro"))
matriz <- left_join(matriz, llegadas, by =c("estacion"="Ciclo_Estacion_Arribo"))

#########EM Algorithm
View(matriz)
x <- matriz[,c(2,3)]
class(x)
model <- kmeans(x,4)
plot(x[,1],x[,2],col = miscols,ylab="llegadas",xlab="salidas")
colores <- c("red","blue","green","orange","purple")
miscols <- colores[model$cluster]
model

wssplot <- function(data, nc=15, seed=25051982){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))#Caclula la varianza del conjunto completo
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}##Por cada #clusters calcula la varianza intra cluster
  plot(1:nc, wss, type="b", xlab="Numero de Grupos",
       ylab="Suma de CUadrados Intra-Cluster",main="Selección del numero de clusters")}
wssplot(x)

aggregate(x, by=list(cluster=model$cluster), sd)

# initial values
pi1<-0.2
pi2<-0.8
mu1<-100
mu2<-1000
sigma1<-100
sigma2<-1000
loglik<- rep(NA, 1000)
loglik[1]<-0
loglik[2]<-mysum(pi1*(log(pi1)+log(dnorm(x,mu1,sigma1))))+mysum(pi2*(log(pi2)+log(dnorm(x,mu2,sigma2))))

mysum <- function(x) {
  sum(x[is.finite(x)])
}
logdnorm <- function(x, mu, sigma) {
  mysum(sapply(x, function(x) {logdmvnorm(x, mu, sigma)}))  
}
tau1<-0
tau2<-0
#k<-1
k<-2

# loop
while(abs(loglik[k]-loglik[k-1]) >= 0.00001) {
  # E step
  tau1<-pi1*dnorm(x,mean=mu1,sd=sigma1)/(pi1*dnorm(x,mean=mu1,sd=sigma1)+pi2*dnorm(x,mean=mu2,sd=sigma2))
  tau2<-pi2*dnorm(x,mean=mu2,sd=sigma2)/(pi1*dnorm(x,mean=mu1,sd=sigma1)+pi2*dnorm(x,mean=mu2,sd=sigma2))
  tau1[is.na(tau1)] <- 0.5
  tau2[is.na(tau2)] <- 0.5
  
  # M step
  pi1<-mysum(tau1)/length(x)
  pi2<-mysum(tau2)/length(x)
  
  mu1<-mysum(tau1*x)/mysum(tau1)
  mu2<-mysum(tau2*x)/mysum(tau2)
  
  sigma1<-mysum(tau1*(x-mu1)^2)/mysum(tau1)
  sigma2<-mysum(tau2*(x-mu2)^2)/mysum(tau2)
  
  
  loglik[k+1]<-mysum(tau1*(log(pi1)+logdnorm(x,mu1,sigma1)))+mysum(tau2*(log(pi2)+logdnorm(x,mu2,sigma2)))
  k<-k+1
}
###########
###########
 #####Intento 2

#load library for multivariate normal
library(mvtnorm)
x <- matriz[,c(2,3)]
x <- x[complete.cases(x),]
summary(x)
plot(x)
x <- cbind(rnorm(100,-5,1),rnorm(100,-5,1))
x <- rbind(x,cbind(rnorm(100,5,1),rnorm(100,5,1)))
#setup grid for plotting
xpts <- seq(from=0,to=25000,length.out=1000)
ypts <- seq(from=0,to=25000,length.out=1000)

#initial parameter estimates (chosen to be deliberately bad)
theta <- list(
  tau=c(0.5,0.5),
  mu1=c(5000,5000),
  mu2=c(15000,15000),
  sigma1=matrix(c(1000000,0,0,1000000),ncol=2),
  sigma2=matrix(c(1000000,0,0,1000000),ncol=2)
)

#E step: calculates conditional probabilities for latent variables
E.step <- function(theta)
  t(apply(cbind(
    theta$tau[1] * dmvnorm(x,mean=theta$mu1,sigma=theta$sigma1),
    theta$tau[2] * dmvnorm(x,mean=theta$mu2,sigma=theta$sigma2)
  ),1,function(x) x/sum(x)))
#M step: calculates the parameter estimates which maximise Q
M.step <- function(T) list(
  tau= apply(T,2,mean),
  mu1= apply(x,2,weighted.mean,T[,1]),
  mu2= apply(x,2,weighted.mean,T[,2]),
  sigma1= cov.wt(x,T[,1])$cov,
  sigma2= cov.wt(x,T[,2])$cov)

#function to plot current data
plot.em <- function(theta){
  mixture.contour <- outer(xpts,ypts,function(x,y) {
    theta$tau[1]*dmvnorm(cbind(x,y),mean=theta$mu1,sigma=theta$sigma1) + theta$tau[2]*dmvnorm(cbind(x,y),mean=theta$mu2,sigma=theta$sigma2)
  })
  contour(xpts,ypts,mixture.contour,nlevels=5,drawlabel=FALSE,col="red",xlab="Eruption time (mins)",ylab="Waiting time (mins)",main="Waiting time vs Eruption time of the Old Faithful geyser")
  points(x)
}

#plot initial contours
iter <- 1
png(filename=paste("em",formatC(iter,width=4,flag="0"),".png",sep=""))
plot.em(theta)
dev.off()

#run EM and plot
for (iter in 2:30){
  print(paste0("Iteracion: ",iter))
  T <- E.step(theta)
  theta <- M.step(T)
  png(filename=paste("em",formatC(iter,width=4,flag="0"),".png",sep=""))
  plot.em(theta)
  dev.off()
}
theta




###### Para 3 clusters
library(mvtnorm)
x <- matriz[,c(2,3)]
x <- x[complete.cases(x),]
summary(x)
plot(x)
par(mfrow=c(1,1))
x <- cbind(rnorm(100,-5,1),rnorm(100,-5,1))
x <- rbind(x,cbind(rnorm(100,5,1),rnorm(100,5,1)))
x <- rbind(x,cbind(rnorm(100,-5,1),rnorm(100,5,1)))
x <- rbind(x,cbind(rnorm(100,5,1),rnorm(100,-5,1)))

#setup grid for plotting
xpts <- seq(from=0,to=25000,length.out=1000)
ypts <- seq(from=0,to=25000,length.out=1000)

n_clusters <- 3
#initial parameter estimates (chosen to be deliberately bad)
initialize
theta <- list(
  tau=c(1/3,1/3,1/3),
  mu1=c(5000,5000),
  mu2=c(15000,15000),
  mu3=c(10000,10000),
  sigma1=matrix(c(1000000,0,0,1000000),ncol=2),
  sigma2=matrix(c(1000000,0,0,1000000),ncol=2),
  sigma3=matrix(c(1000000,0,0,1000000),ncol=2)
)

#E step: calculates conditional probabilities for latent variables
E.step <- function(theta)
  t(apply(cbind(
    theta$tau[1] * dmvnorm(x,mean=theta$mu1,sigma=theta$sigma1),
    theta$tau[2] * dmvnorm(x,mean=theta$mu2,sigma=theta$sigma2),
    theta$tau[3] * dmvnorm(x,mean=theta$mu3,sigma=theta$sigma3)
  ),1,function(x) x/sum(x)))
#M step: calculates the parameter estimates which maximise Q
M.step <- function(T) list(
  tau= apply(T,2,mean),
  mu1= apply(x,2,weighted.mean,T[,1]),
  mu2= apply(x,2,weighted.mean,T[,2]),
  mu3= apply(x,2,weighted.mean,T[,3]),
  sigma1= cov.wt(x,T[,1])$cov,
  sigma2= cov.wt(x,T[,2])$cov,
  sigma3= cov.wt(x,T[,3])$cov)

#function to plot current data
plot.em <- function(theta){
  mixture.contour <- outer(xpts,ypts,function(x,y) {
    theta$tau[1]*dmvnorm(cbind(x,y),mean=theta$mu1,sigma=theta$sigma1) + theta$tau[2]*dmvnorm(cbind(x,y),mean=theta$mu2,sigma=theta$sigma2)
  })
  contour(xpts,ypts,mixture.contour,nlevels=5,drawlabel=FALSE,col="red",xlab="Eruption time (mins)",ylab="Waiting time (mins)",main="Waiting time vs Eruption time of the Old Faithful geyser")
  points(x)
}

#plot initial contours
iter <- 1
png(filename=paste("em",formatC(iter,width=4,flag="0"),".png",sep=""))
plot.em(theta)
dev.off()

#run EM and plot
for (iter in 2:30){
  print(paste0("Iteracion: ",iter))
  T <- E.step(theta)
  theta <- M.step(T)
  png(filename=paste("em",formatC(iter,width=4,flag="0"),".png",sep=""))
  plot.em(theta)
  dev.off()
}
theta


########### N clusters N atributos
library(mvtnorm)
x <- matriz[,c(2,3)]
x <- x[complete.cases(x),]
summary(x)
plot(x)

#setup grid for plotting
xpts <- seq(from=0,to=25000,length.out=1000)
ypts <- seq(from=0,to=25000,length.out=1000)

n_clusters <- 4
#initial parameter estimates (chosen to be deliberately bad)
initialize <- function(n_clusters,data){
  theta <- list(tau=rep(1/n_clusters,n_clusters))
  for(i in 1:n_clusters){
    for(j in 1:ncol(data)){
      theta[[paste0("mu",i)]] <- c(theta[[paste0("mu",i)]],runif(1,min(data[j,]),max(data[j,])))
    }
  }
  for(i in 1:n_clusters){
    arr <- numeric()
    for(j in 1:ncol(data)){
      for(k in 1:ncol(data)){
        if(j==k){
          arr <- c(arr,runif(1,0,var(data)))
        }
        else{
          arr <- c(arr,0)
        }
      }
    }
    theta[[paste0("sigma",i)]] <- matrix(arr,ncol=ncol(x))
  }
  return(list=theta)
}
#E step: calculates conditional probabilities for latent variables
E.step <- function(theta){
  temp <- numeric(0)
  for(i in 1:length(theta$tau)){
    temp <- cbind(temp,theta$tau[i] * dmvnorm(x,mean=theta[[paste0("mu",i)]],sigma=theta[[paste0("sigma",i)]]))
  }
  T <- t(apply(temp,1,function(x) x/sum(x)))
  return(T)
}
#M step: calculates the parameter estimates which maximise Q
M.step <- function(T){
  result <- list(tau=apply(T,2,mean))
  for(i in 1:n_clusters){
    result[[paste0("mu",i)]] <- apply(x,2,weighted.mean,T[,i])
  }
  for(i in 1:n_clusters){
    result[[paste0("sigma",i)]] <- cov.wt(x,T[,i])$cov
  }
  return(result)
} 
  
theta <- initialize(n_clusters,x)
#run EM and plot
for (iter in 1:30){
  print(paste0("Iteracion: ",iter))
  T <- E.step(theta)
  theta <- M.step(T)
}
theta


clusters <- function(T){
  return(apply(T,1,which.max))
}
centers <- function(theta){
  centers <- numeric(0)
  for(i in 1:n_clusters){
   centers<-  rbind(centers,theta[[paste0("mu",i)]]) 
  }
  return(centers)
}
matriz <- matriz[which(complete.cases(matriz)),]
matriz[["clusters"]] <- clusters(T)
centers <- centers(theta)
