library(ggplot2)
set.seed(110104)
# Function to simulate ------
rGeom.aux <- function(p){ # Simulate one draw
  U <- runif(1)
  # We now use a while loop to find to which partition U belongs to.
  k <- 1
  max.iter <- 10e5 # Security parameter to avoid infinite loops.
  # Good practice even when not necessary.
  Fk <- p
  while(U > Fk & k <= max.iter){
    k <- k + 1
    Fk <- Fk + (1-p)^(k-1)*p
  }
  return(k-1)
}
rGeom <- function(nsim, p){ # Simulate several draws
  replicate(nsim, rGeom.aux(p=p))
}
# Comparison with theoretical values ------
nsim <- 10000
p <- 0.15
geom.seq <- rGeom(nsim, p) # nsim simulations of Geom(p)
qplot(sort(geom.seq))

result = rbind(c(mean(geom.seq),(1-p)/p),c(sd(geom.seq),sqrt((1-p)/(p*p))))
dimnames(result) = list(c("Mean", "Standard error"),c("Aproximation by simulation", "Expected parameters")) # column names 
result


max.obs <- max(geom.seq) # Max value to print in tables
observed <- table(factor(geom.seq, levels=0:max.obs)) # Observed frequencies
probs <- (1-p)^(0:max.obs)*p
expected <- round(probs*nsim)
compare <- data.frame(data.frame(observed), Expected=expected) # Observed vs expected
compare$Freq

k=100
chi2 <- sum((compare$Freq[1:50]-compare$Expected[1:50])^2/compare$Expected[1:50])
pchisq(chi2, df = k-1)
