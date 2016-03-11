typeof(iris)
class(iris)
str(iris)
datos <- iris
data$Species
class(data$Species)
unclass(data$Species)
as.character(data$Species)
table(data$Species)
table(trunc(datos$Sepal.Length),datos$Species)


mifactor <- factor(c("dia","tarde","noche"), levels=c(c("dia","tarde","noche")))
table(mifactor)
unclass(mifactor)
getwd()
setwd("C:\\Users\\Stephane\\Desktop\\ITAM\\R\\RLearning")
write.csv(data, "iris.csv")
data2 <- read.csv("iris.csv")
row.names(data2)
names(data2)
row.names(data2)[1] <- "Juan Gabriel"
data2
data2[2,1]

plot(data2$Sepal.Length,data2$Sepal.Width)
attach(data2)
plot(Petal.Length,Petal.Width)
#Hay un objeto tipo Modelo - regresiones lineales 
model <- Petal.Length ~ Petal.Width + Sepal.Length
class(model)
typeof(model)
str(model)

res <- lm(model)
class(res)
par(mfrow= c(2,2))
plot(res, ask = FALSE)

par(mfrow= c(1,1))
plot(Petal.Length,predict(res))
predict(res, data.frame(Petal.Width=3 , Sepal.Length=4))
res.table <- data.frame(reales = Petal.Length, Predichos = predict(res))
data <- cbind(data, Petal.Length=predict(res))
data <- data[,1:5]


mimatrix<- matrix(runif(100) , ncol=10, nrow=10)
mimatrix


plot(mifactor)
data2
colnames(data2)

x <- runif(100)
mean(x)
var(x)
x.scaled <- scale(x)
var(x.scaled)

#Queremos escalar todas nuestras variables
covariancematrix <- var(iris[,1:4])
dat.std <- apply(iris[,1:4],2, scale)
var(dat.std)##Todas tienen media 0
summary(dat.std)

mivarianza <- (t(dat.std) %*% dat.std) / (nrow(dat.std)-1)
eigen(mivarianza)$values/4

pp <- prcomp(iris[,1:4],center=TRUE,scale= TRUE)
names(pp)
pp$center
pp$scale
