set.seed(110104)
library(ggplot2)
nsim <- 1000
lambda <- 4
rExp <- function(nsim, lambda){
  return((-1/lambda)*log(1-runif(nsim)))
}
dat <- data.frame(Value=rExp(nsim, lambda))
ggplot(dat, aes(x=Value)) +
  geom_histogram(aes(y=..density..), binwidth= .2, colour="black", fill="white") +
  stat_function(fun = function(x) lambda*exp(-lambda*x),colour = "blue")


u <- runif(100)
x <- rexp(100)
dat1 <- data.frame(cbind(sort(x),sort(u)))
                   
dat1$
