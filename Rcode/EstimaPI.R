###Pi estimation
library(ggplot2)


set.seed(110104) # Good practice!
nsim <- 1000 # The number of points we will generate
xcoord <- runif(nsim) # We store a random sequence for each coordinate
ycoord <- runif(nsim)
dist.orig <- sqrt(xcoord^2+ycoord^2) # Distance to (0,0)
hits <- dist.orig < 1 # Number of points lying below the circle of radius 1
area <- sum(hits)/nsim # Fraction below
qplot(xcoord, ycoord, colour=hits) 
4*area# Final Estimate




estimatePI <- function(nsim){

set.seed(110104) # Good practice!
#nsim <- 100000 # The number of points we will generate
xcoord <- runif(nsim) # We store a random sequence for each coordinate
ycoord <- runif(nsim)
dist.orig <- sqrt(xcoord^2+ycoord^2) # Distance to (0,0)
hits <- dist.orig < 1 # Number of points lying below the circle of radius 1
area <- sum(hits)/nsim # Fraction below
qplot(xcoord, ycoord, colour=hits)
return(4*area)# Final Estimate
}

N = seq(1000 ,100000,1000)
estimation <- sapply(N,estimatePI)
qplot(N,estimation,type="l")+geom_smooth()
estimation
 