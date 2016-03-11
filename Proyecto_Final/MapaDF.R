####Mapa


getwd()
setwd("../.././shp/")
dir()
shape <- readShapePoints("./shp/.shp")
mask <- readOGR(".", "Mask")
colonias <- readOGR(".", "ColoniasDF")
calles <- readOGR(".", "CallesMask")
print(proj4string(colonias))
plot(colonias, axes=TRUE, border="gray")
??readOGR


str(colonias)


coords = cbind(dat$longitud, dat$latitud)
sp = SpatialPoints(coords)
spdf = SpatialPointsDataFrame(coords, dat)
coordinates(dat) = ~dat$longitud + dat$latitud
points(dat$longitud, dat$latitud, col = dat$delegacion, cex = .6)
head(colonias@data)
unique(colonias@data$MUN_NAME)
length(colonias@data$MUN_NAME)
coloniasecobici = colonias[which(colonias@data$MUN_NAME %in% c("CUAUHTÉMOC","MIGUEL HIDALGO")),]
plot(colonias)
plot(mask, axes=TRUE, border="gray")
lines(calles, axes=TRUE)
points(prueba$longitud, prueba$latitud, col =miscols,pch=19,  cex = 1.5)
legend('bottomleft', legend = c("Perifericas","Trafico Local","Enlaces Urbanos","Zonas de Trabajo") , 
       pch=19, col=c("red","blue","green","orange"), cex=1)
colores <- c("red","blue","green","orange","purple","yellow","black","grey")
miscols <- colores[as.factor(prueba$clusters)]

??pch
prueba <- left_join(dat, matriz, by =c("id"="estacion"))
View(prueba)

