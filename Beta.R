
par(mfrow=c(3,3))
plot(dbeta(sort(runif(10000)),0.5,0.5))
plot(dbeta(sort(runif(10000)),0.8,0.2))
plot(dbeta(sort(runif(10000)),1,1))
plot(dbeta(sort(runif(10000)),2,2))
plot(dbeta(sort(runif(10000)),5,1))
plot(dbeta(sort(runif(10000)),1,5))
plot(dbeta(sort(runif(10000)),5,3))
plot(dbeta(sort(runif(10000)),20,12))
plot(dbeta(sort(runif(10000)),20,20))

