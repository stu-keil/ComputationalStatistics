LCG.setvalues <- function(x0= 4711,a= 13445,b= 4321983212333411,m= 2^31-1){
  a <<- a
  b <<- b
  x0 <<-x0
  m<<-m
}
LCG.random.gen <- function(n){
  
  random.vector = rep(0,n+1)
  random.vector[1] = x0
  for(i in 2:n){
    
    random.vector[i] = (a*random.vector[i-1] + b) %% m
  }
  return(random.vector[-1]/m)
}


LCG.setvalues(3434)
randomunif <- LCG.random.gen(1000)
realrandom <- runif(1000)
par(mfrow=c(1,2))
plot(randomunif[1:length(randomunif)-1],randomunif[2:length(randomunif)])
plot(realrandom[1:length(realrandom)-1],realrandom[2:length(realrandom)])
