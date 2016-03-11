install.packages('devtools')
install.packages('rgdal')
install.packages('shiny')
devtools::install_github("hrbrmstr/asam")
install.packages(c('leaflet','sp','dplyr'))
install.packages('rjson')
library(shiny)


library(leaflet)
library(asam)
library(sp)
library(dplyr)
options(encoding = "utf-8")

mapbox_public_token <- "pk.eyJ1Ijoic3RlcGhhbmVrZWlsIiwiYSI6ImNpZ3ppM283ejB3Nzl3OW01eDIyNmt5bTIifQ.p197sfq7QzHg6MGvv1KRMA" #Sys.getenv("MAPBOX_PUBLIC_TOKEN")
mapbox_map_id <- "mapbox.pirates"#Sys.getenv("PIRATE_MAP_ID")
mapbox_url <- "https://a.tiles.mapbox.com/v4/%s/{z}/{x}/{y}.png?access_token=%s"
mapbox_tiles_template <- sprintf(mapbox_url, mapbox_map_id, mapbox_public_token)

x_marker <- icons("https://lh3.googleusercontent.com/-NAuoya-XTXc/AAAAAAAAAAI/AAAAAAAAABU/y2oHqg3EceA/photo.jpg",
                  iconHeight=32, iconWidth=32,
                  iconAnchorX=16, iconAnchorY=16)

## leaflet() %>%
##   addTiles(mapbox_tiles_template) %>%
##   setView(lng=-50.9249, lat=45.68929, zoom=3) %>%
##     addMarkers(-70.2667, 43.6667, icon=x_marker)

datos <- read.csv('../../ecobiciestaciones.csv')

#class(datos$latitud)
#class(datos$longitud)
#datos[,7:8]
glimpse(datos)

# dat <- subset(asam_shp,
#               DateOfOcc > as.Date("2015-01-01") &
#                 grepl("pirate", Aggressor, ignore.case=TRUE))
# could also do data.frame(dat)

dat <- bind_cols(filter(datos, latitud= !is.na(latitud), longitud =  !is.na(longitud)))

writeOGR()

popup_template <- '<div style="background:#f3e0b5; padding:10px">
<b>Num Cicloestacion:</b> %s<br/>
<b>Colonia:</b> %s<br/>
<b>Calle:</b> %s</div>'



pirate_pops <- sprintf(popup_template,
                       dat$id,
                       rep("hola",268),
                       dat$id)

leaflet() %>%
  addTiles(mapbox_tiles_template) %>%
  setView(lng=-99.16848, lat=19.43293, zoom=15) %>%
  addMarkers(dat$longitud, dat$latitud,  popup=pirate_pops,icon=x_marker)