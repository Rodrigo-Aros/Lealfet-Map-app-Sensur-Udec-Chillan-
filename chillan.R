# Los datos son recogidos por  app Sensur, para el desarrollo de Mapa Participativo en Campus Chillán Universidad de Concepción.
# https://saas.sensorurbano.cl/welcome


library(htmltools)
library(leafsync)
library(leaflet)
library(sf)
library(leaflet.extras)
library(mapview)
library(tidyverse)
library(ggmap)
library(leafem)
library(rstudioapi)
library(ggplot2)
library(ggmap)
library(tibble)
library(tidyverse)
library(knitr)
library(printr)
}

#Configuro working directory

setwd("C:/Users/Rodrigo Aros/Desktop/Proyecto_UDEC")
getwd()

# Cargo, limpio y ordeno base de datos Sensur -----------------------------

{
data<- readxl::read_xlsx("ids.xlsx")
data["ID Parte"] <- as.numeric(data$`ID Parte`)

#paso 1 separar ids unicos
separados<- data %>% 
  separate(RP_IDS, paste0("RP_IDS",1:3),sep=';')

#paso 2 armar df ordenado por IDS unicos

df_ordenado<- separados %>%
  gather(variable, valor, RP_IDS1, RP_IDS2, RP_IDS3)


df_ordenado["valor"]<- as.numeric(df_ordenado$valor)

df_ordenado <- df_ordenado %>% 
  filter(valor <= 243)

# cargo base de datos para cruce
df_cruce <- readxl::read_xlsx("df_cruce.xlsx")
df_cruce["ID Parte"] <- as.numeric(df_cruce$`ID Parte`)

df_cruce <- df_cruce[,c(2,9:17)]
names(df_cruce)
df_cruce <- df_cruce[,c(1, 9:10)]


test<- df_ordenado %>% 
  left_join(df_cruce, by=c("valor" = "ID Parte")) # join para obtener datos finales a utilizar
test <- test %>% 
  drop_na(Latitud,Longitud)
}

names(test)[12:13] <- c("lat", "lon") #cambio nombres lat lon

# Subset por puntos de Lugares y Actividad para LEAFLET -------------------------------------------

{
a <- test %>% 
  filter(Label=="Sociabilización")

b <- test %>% 
  filter(Label=="Recreación")

cc <- test %>% 
  filter(Label=="Estudio")

d <- test %>% 
  filter(Label=="Alimentación")

ee <- test %>% 
  filter(Label=="Deportes")

f <- test %>% 
  filter(Label=="Deteriorado")

g <- test %>% 
  filter(Label=="Inseguro")

h <- test %>% 
  filter(Label=="Patrimonial")

ii <- test %>% 
  filter(Label=="Favorito")
}

# Creo popups con info de cada subset de ptos  ----------------------------------------------------

{
popupa <- paste0("<b>", "ID: ", "</b>", as.character(a$`ID Parte`), 
                 "<br>", "<b>", "Genero: ", "</b>", as.character(a$Género),
                 "<br>", "<b>", "fecha: ", "</b>", a$Fecha, 
                 "<br>", "<b>", "Etiqueta: ", "</b>", a$Label,
                 "<br>", "<b>", "Nombre: ", "</b>", a$Nombre,  "<br>")

popupb <- paste0("<b>", "ID: ", "</b>", as.character(b$`ID Parte`), 
                 "<br>", "<b>", "Genero: ", "</b>", as.character(b$Género),
                 "<br>", "<b>", "fecha: ", "</b>", b$Fecha, 
                 "<br>", "<b>", "Etiqueta: ", "</b>", b$Label,
                 "<br>", "<b>", "Nombre: ", "</b>", b$Nombre,  "<br>")

popupcc <- paste0("<b>", "ID: ", "</b>", as.character(cc$`ID Parte`), 
                  "<br>", "<b>", "Genero: ", "</b>", as.character(cc$Género),
                  "<br>", "<b>", "fecha: ", "</b>", cc$Fecha, 
                  "<br>", "<b>", "Etiqueta: ", "</b>", cc$Label,
                  "<br>", "<b>", "Nombre: ", "</b>", cc$Nombre,  "<br>")

popupee <- paste0("<b>", "ID: ", "</b>", as.character(ee$`ID Parte`), 
                  "<br>", "<b>", "Genero: ", "</b>", as.character(ee$Género),
                  "<br>", "<b>", "fecha: ", "</b>", ee$Fecha, 
                  "<br>", "<b>", "Etiqueta: ", "</b>", ee$Label,
                  "<br>", "<b>", "Nombre: ", "</b>", ee$Nombre,  "<br>")

popupf <- paste0("<b>", "ID: ", "</b>", as.character(f$`ID Parte`), 
                 "<br>", "<b>", "Genero: ", "</b>", as.character(f$Género),
                 "<br>", "<b>", "fecha: ", "</b>", f$Fecha, 
                 "<br>", "<b>", "Etiqueta: ", "</b>", f$Label,
                 "<br>", "<b>", "Nombre: ", "</b>", f$Nombre,  "<br>")

popupg <- paste0("<b>", "ID: ", "</b>", as.character(g$`ID Parte`), 
                 "<br>", "<b>", "Genero: ", "</b>", as.character(g$Género),
                 "<br>", "<b>", "fecha: ", "</b>", g$Fecha, 
                 "<br>", "<b>", "Etiqueta: ", "</b>", g$Label,
                 "<br>", "<b>", "Nombre: ", "</b>", g$Nombre,  "<br>")

popuph <- paste0("<b>", "ID: ", "</b>", as.character(h$`ID Parte`), 
                 "<br>", "<b>", "Genero: ", "</b>", as.character(h$Género),
                 "<br>", "<b>", "fecha: ", "</b>", h$Fecha, 
                 "<br>", "<b>", "Etiqueta: ", "</b>", h$Label,
                 "<br>", "<b>", "Nombre: ", "</b>", h$Nombre,  "<br>")

popupii <- paste0("<b>", "ID: ", "</b>", as.character(ii$`ID Parte`), 
                 "<br>", "<b>", "Genero: ", "</b>", as.character(ii$Género),
                 "<br>", "<b>", "fecha: ", "</b>", ii$Fecha, 
                 "<br>", "<b>", "Etiqueta: ", "</b>", ii$Label,
                 "<br>", "<b>", "Nombre: ", "</b>", ii$Nombre,  "<br>")
}

# MAPA con Leaflet App SENSUR CAMPUS CHILLAN UDEC -------------------------

{
mapa1 <- leaflet(data = test)
mapa1 <- leaflet(data=test) %>% 
  addTiles(group = "OSM (default)") %>% 
  setView(-72.08027, -36.59806, 15) %>% 
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Carto") %>% 
  addCircleMarkers(data = a, lat = a$lat, lng = a$lon, group = "Sociabilización",
                   radius = 5,
                   popup = popupa,
                   color = "blue",
                   weight = 3,
                   fill = "blue") %>% 
  addCircleMarkers(data = b, lat = b$lat, lng = b$lon, group = "Recreación",
                   radius = 5,
                   popup = popupb,
                   color = "red",
                   weight = 3,
                   fill = "red") %>% 
  addCircleMarkers(data = cc, lat = cc$lat, lng = cc$lon, group = "Estudio",
                   radius = 5,
                   popup = popupcc,
                   color = "green",
                   weight = 3,
                   fill = "green") %>% 
  addCircleMarkers(data = ee, lat = ee$lat, lng = ee$lon, group = "Deportes",
                   radius = 5,
                   popup = popupee,
                   color = "black",
                   weight = 3,
                   fill = "black") %>%
  addCircleMarkers(data = f, lat = f$lat, lng = f$lon, group = "Deteriorado",
                   radius = 5,
                   popup = popupf,
                   color = "black",
                   weight = 3,
                   fill = "brown") %>%
  addCircleMarkers(data = g, lat = g$lat, lng = g$lon, group = "Inseguro",
                   radius = 5,
                   popup = popupg,
                   color = "red",
                   weight = 3,
                   fill = "white") %>%
  addCircleMarkers(data = h, lat = h$lat, lng = h$lon, group = "Patrimonial",
                   radius = 5,
                   popup = popuph,
                   color = "orange",
                   weight = 3,
                   fill = "orange") %>% 
  addCircleMarkers(data = ii, lat = ii$lat, lng = ii$lon, group = "Favorito",
                   radius = 5,
                   popup = popupii,
                   color = "violet",
                   weight = 3) %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Carto"),
    overlayGroups = c("Sociabilización", "Recreación", "Estudio", "Deportes", "Deteriorado",
                      "Inseguro", "Patrimonial", "Favorito"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  addMiniMap(mapa1,
             position = "bottomright",
             width = 100,
             height = 100,
             collapsedWidth = 19,
             collapsedHeight = 19,
             zoomLevelOffset = -5) %>% 
  garnishMap(mapa1, addScaleBar, addMouseCoordinates, position = "bottomleft")

mapa1
}

# Mapa de CALOR con leaflet UDEC Chillan-----------------------------------------------------------

{
mapa2 <- leaflet() %>%  
  addTiles(group = "OSM (default)") %>% 
  setView(-72.08027, -36.59806, 15) %>% 
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Carto") %>% 
  addHeatmap(data = a, lat = a$lat, lng = a$lon, group = "Sociabilización",
             gradient = "RdBu") %>% 
  addHeatmap(data = b, lat = b$lat, lng = b$lon, group = "Recreación",
             gradient = "Spectral") %>% 
  addHeatmap(data = cc, lat = cc$lat, lng = cc$lon, group = "Estudio",
             gradient = "YlOrRd") %>% 
  addHeatmap(data = ee, lat = ee$lat, lng = ee$lon, group = "Deportes") %>% 
  addHeatmap(data = f, lat = f$lat, lng = f$lon, group = "Deteriorado") %>% 
  addHeatmap(data = g, lat = g$lat, lng = g$lon, group = "Inseguro") %>% 
  addHeatmap(data = h, lat = h$lat, lng = h$lon, group = "Patrimonial") %>% 
  addHeatmap(data = ii, lat = ii$lat, lng = ii$lon, group = "Favorito") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Carto"),
    overlayGroups = c("Sociabilización", "Recreación", "Estudio",
                      "Deportes", "Deteriorado", "Inseguro",
                      "Patrimonial", "Favorito"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  addMiniMap(mapa1,
             position = "bottomright",
             width = 100,
             height = 100,
             collapsedWidth = 19,
             collapsedHeight = 19,
             zoomLevelOffset = -5) %>% 
  garnishMap(mapa1, addScaleBar, addMouseCoordinates, position = "bottomleft")

mapa2
}

# Salida en html ----------------------------------------------------------

htmlwidgets::saveWidget(mapa1, "Plan_Maestro_Chillan.html")
htmlwidgets::saveWidget(mapa2, "heatmap_Udec_Chillan.html")

prueba <- latticeview(mapa1,mapa2)

htmltools::save_html(prueba, "test.html")

