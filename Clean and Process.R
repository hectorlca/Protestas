### Load Libraries ###

library(plyr)
library(dplyr)
library(ggplot2)
library(highcharter)
library(ggmap)
library(lubridate)
library(leaflet)

### Log into google for Geocode API ###

register_google(key = 'AIzaSyDMhK5bODup91UE9w9O-LIKyeDOMKR6atk',
                account_type = "premium", day_limit = 100000)


### Load Data ###

protestas <- read.csv ("data/protestas.csv", stringsAsFactors = FALSE)
protestas$fecha <- dmy(protestas$fecha)


#####################################
############# GEOCODE ###############

coords <- geocode(protestas$ubicacion)

########################################

protestas2 <- cbind(protestas, coords)
protestas2$location <- paste0(as.character(protestas2$lat), ",", as.character(protestas2$lon))

write.csv(protestas2, "data/protestasconcoords.csv", row.names = FALSE)


##### COMENZAR LEYENDO DESDE AQUI #####
protestas <- read.csv("data/protestasconcoords.csv")

##### Filter out Non-Honduran Coordinates ####

protestas$ubicacioncorrecta <- ifelse(protestas$lat > 13 
                                      & protestas$lat < 16 
                                      & protestas$lon > -89.3 
                                      & protestas$lon < -83.1, 
                                      yes = "correcto", no = "incorrecto")

write.csv(protestas, "data/Protestas con Ubicaciones.csv", row.names = FALSE)


### Summarize Protests by Day ###

datsum <- group_by(protestas, fecha) %>%
  summarise(count = n())


#### A pretty higchart ###

highchart() %>% hc_add_theme(hc_theme_smpl()) %>%
  
  hc_xAxis(categories = datsum$fecha) %>%
  hc_add_series(data = datsum$count,
                name = "Protestas",
                color = "gray") %>%
    hc_chart(type = "line") %>%
    hc_title(
    text = "Numero de Protestas Diarias",
    style = list(
      color = "#2d323a",
      fontSize = "27px",
      fontFamily = "Franklin Gothic Medium Cond",
      style = "bold"
    )
  ) %>%
  hc_subtitle(
    text = "Fuente: Reporte Policial Diario",
    style = list(
      color = "#525c6b",
      fontSize = "17px",
      fontFamily = "Times New Roman",
      style = "bold"
    )
  )

#### Simple Map ###

leaflet() %>%
  setView(lng = -86.6,
       lat = 14.63,
      zoom = 8) %>%
addProviderTiles("Esri.WorldStreetMap") %>%
  addCircles(
  lat = protestas$coord.lon,
  lng = protestas$coords.lat,
 #radius = (dat$total*50),
  stroke = FALSE,
  color = "#de2d26",
  fillOpacity = 0.6
)



























hn <- readShapeLines (fn="gis/HND_adm0", 
                     proj4string=CRS("+proj=longlat +datum=WGS84") )



