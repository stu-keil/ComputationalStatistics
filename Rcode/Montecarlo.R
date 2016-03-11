#Montecarlo


##Testing

nsim <- 1000
x <- runif(1000,0,2)
y <- sqrt(4-(x)^2)
df <- as.data.frame(cbind(x,y))
class(df)
plot(df[order(df$x),]$x,df[order(df$x),]$y,type="l")





montecarlo <- function(nsim=10000,alpha=0.05,fun=mifunc,a=0,b=1){
  valores.uniformes <- runif(nsim,a,b)
  mi.func <- mean(fun(valores.uniformes))
  zdensityonalpha <- qnorm(alpha/2, lower.tail = FALSE)
  sample_var <- var(fun(valores.uniformes))
  interval <- zdensityonalpha*sqrt(sample_var/nsim)
  return (list(point_estimate = mi.func, intervalo = interval))
}

mifunc <- function(x){2*sqrt(4-(x^2))}
integral.lim.sup <- 2
integral.lim.inf <- 0
N = 9000:10000
alpha = 0.05

df <- as.data.frame(do.call(rbind,lapply(N,montecarlo,alpha=alpha,fun=mifunc,a=integral.lim.inf,b=integral.lim.sup)))
plot(N,as.numeric(df$point_estimate), type = "l",xlab="Numero Simulaciones",ylab = "Valor Aproximado",main="Simulaciones de montecarlo para aproximación de integrales")
lines(N,as.numeric(df$point_estimate)+as.numeric(df$intervalo), type="l", col = "red")
lines(N,as.numeric(df$point_estimate)-as.numeric(df$intervalo), type="l", col = "red")
legend(x = "top", c("Intervalos de confianza","Aproximación de Integral"),lty=c(1,1), lwd=c(2,2,2),col=c("red","black"),cex = 0.75)
str(df)
?do.call

?plot

