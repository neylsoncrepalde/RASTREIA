# JUVENTUDES
# Plotando as regiões do interior selecionadas
# Neylson Crepalde
###############################################
library(leaflet)
library(htmltools)

getwd()

##############
# DIVINOPOLIS
##############
locais = data.frame(lat = c(-20.15450, -20.14235), lon=c(-44.90044, -44.87476))

mymap <- leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik",
                   options = tileOptions(minZoom=13, maxZoom=16)) %>% #"freeze" the mapwindow to max and min zoomlevel
  setView(-44.8916,-20.1451, zoom=13) %>%
  addMarkers(lng=locais$lon,
                   lat=locais$lat,
                   popup = paste0("<b>Região selecionada</b><br><b>Critério:</b><br>Homicídios de jovens na região")
  )
mymap

save_html(mymap, 'juventudes_divinopolis.html')



