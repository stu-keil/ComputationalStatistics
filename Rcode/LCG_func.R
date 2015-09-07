LCG.setvalues <- function(x0= 4711,lala,b= 4321983212333411,m= 2^31-1){
  x0 <<-x0
  lala <<- lala
  b <<- b
  m<<-m
}
LCG.random.gen <- function(n){
  
  random.vector = rep(0,n+1)
  random.vector[1] = x0
  for(i in 2:n){
    
    random.vector[i] = (lala*random.vector[i-1] + b) %% m
  }
  return(random.vector[-1]/m)
}


LCG.setvalues(3434,13445,4321983212333411,2^31-1)
randomunif <- LCG.random.gen(1000)
realrandom <- runif(1000)
par(mfrow=c(1,2))
plot(randomunif[1:length(randomunif)-1],randomunif[2:length(randomunif)])
plot(realrandom[1:length(realrandom)-1],realrandom[2:length(realrandom)])



library(ggplot2)
library(gridExtra)

grid.arrange(
qplot(randomunif[-1000], randomunif[-1]),
qplot(realrandom[-1000], realrandom[-1]),
ncol=2
)
?"grid.arrange"
####Pruebas para verificar que tan aleatorios son mis numeros
summary(randomunif) 
sd(randomunif)


k=100
ei <- rep(1000/k, times=k)
oi <- table(trunc(k*randomunif)/k) # Easy way to compute the oi
chi2 <- sum((oi-ei)^2/ei)
pchisq(chi2, df = k-1)


k=100
ei <- rep(1000/k, times=k)
oi <- table(trunc(k*realrandom)/k) # Easy way to compute the oi
chi2 <- sum((oi-ei)^2/ei)
pchisq(chi2, df = k-1)