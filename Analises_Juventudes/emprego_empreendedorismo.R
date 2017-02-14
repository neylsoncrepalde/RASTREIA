# Emprego e Empreendedorismo
# RASTREIA - Monitoramento e Avaliação
# Script: Neylson Crepalde
##############################################
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
################################
library(data.table)
library(bit64)
library(DT)
library(magrittr)
library(descr)
library(dplyr)
library(lubridate)
library(ggplot2)

setwd("C:/Users/x6905399/Documents/RASTREIA/JUVENTUDES")

dados = fread('emprego_empreendedorismo.csv', encoding="UTF-8") %>% 
  as.data.frame(., stringsAsFactors=F) %>% .[-2,]

names(dados)

# Monitorando a aplicação de questionários
freq(dados[,4],plot=F)

tabela = freq(dados[,4], plot=F) %>% as.data.frame(., stringsAsFactors=F)
tabela$pertotal = (tabela[,1] * 100) / 50
tabela = mutate(tabela,
                pertotal2 = pertotal %>% as.character %>% paste0(., '%'),
                nomes = rownames(tabela))
names(tabela) = c('Frequência','Percentual','Percentual do total',
                  'Percentual do total (50)', 'Nome do Consultor')
datatable(tabela[-nrow(tabela),c(5,1,4)])

#Fluxo das aplicacoes de questionarios

#corrigindo erro
dados[170,5] = '13/02/2017'
dados[83,5] = '10/02/2017'
dados[149,5] = '06/02/2017'
dados[150,5] = '06/02/2017'

datas = as.data.frame(table(dados[,5]), stringsAsFactors = F)
datas$Var1 = datas$Var1 %>% dmy %>% as_date
limits = c(20170101,20170215) %>% ymd %>% as_date

ggplot(datas, aes(x=Var1, y=Freq))+geom_line(lwd=1)+
  scale_x_date(date_minor_breaks = '1 week', date_breaks = '1 week',
               date_labels = '%d/%m', limits = limits)+
  labs(x='',y='Número de questionários')

########################
#Plotando volume de aplicação nos locais 
library(maptools)
library(maps)
library(tidyr)

#extraindo as regiões do serviço

dados$servico = dados$Serviço 
dados = dados %>% separate(servico, c('parte1','regiao'), ' / ')
names(dados)

dados$regiao %>% freq(., plot=F)
dados2 = dados %>% separate(regiao, c('r1','r2','r3'), ' e ')
regioes_desag = rbind(dados2$r1, dados2$r2, dados2$r3)
locais = freq(regioes_desag, plot=F) %>% as.data.frame(., stringsAsFactors=F) %>%
  .[-c(nrow(.), nrow(.)-1),]
locais = mutate(locais,
                nomes = rownames(locais),
                lat = c(-19.939045,-19.912857,-20.027582,-19.907773,-19.828723,-19.951469,
                        -19.945829,-20.039762,-19.962187,-19.965879,-19.916447),
                lon = c(-43.919928,-43.893466,-44.228331,-43.882842,-43.925793,-44.117417,
                        -43.963345,-44.216812,-43.947033,-44.014811,-43.885229))
locais

#plota o mapa
library(sp)
#minas = readShapePoly('31MUE250GC_SIR.shp')
#map(minas, col="#191919", fill=TRUE, bg="#000000", lwd=0.08)
#RMBH = subset(minas, minas$CD_GEOCMU == 3106200, fit.bbox=T)
#map(RMBH, col="#191919", fill=TRUE, bg="#000000", lwd=0.08)

#adiciona os pontos
#points(locais$lon, locais$lat, pch = 19, 
#       col=adjustcolor('#ffd42a',.6), 
#       cex=locais$Frequência/40)
#title('Região Metropolitana de Belo Horizonte',col.main="white", cex.main=1)

#plotando com ggmap
library(ggmap)
BH = get_map(location = c(lon=-44.06,lat=-19.92), zoom = 11)
mapPoints <- ggmap(BH)+geom_point(data=locais, shape=21, aes(x = lon, y = lat,size=`% Válido`),
                                  colour='black',fill=adjustcolor('red',.5))+labs(title='Economias locais na RMBH')
mapPoints

# plotando mapa interativo com googleVis
#library(plotGoogleMaps)
#RMBH@polygons %<>% as.SpatialPolygons.PolygonsList
#map = plotGoogleMaps(RMBH@polygons[[1]],
#                     zoom = 11, clickable = T)
#plot(map)
##############################
#library(googleVis)
#help("gvisGeoMap")
#datagvis = data.frame(locationvar = paste0(locais$lat,':',locais$lon), hovervar=locais$nomes,
#                      numvar = locais$`% Válido`, stringsAsFactors = F)
#map = gvisGeoChart(datagvis, "locationvar", sizevar = 'numvar', hovervar = 'hovervar',
#                   options = list(region='BR',resolution='provinces',
#                                            width=1000, height=600))
#plot(map)

##################################
# Com Leaflet
##################################
library(rgdal)
library(leaflet)

minas = readShapePoly('31MUE250GC_SIR.shp')
#map(minas, col="#191919", fill=TRUE, bg="#000000", lwd=0.08)
RMBH = subset(minas, minas$CD_GEOCMU == 3106200, fit.bbox=T)
palette <- colorBin(c('#fee0d2',  #an example color scheme. you can substitute your own colors
                      '#fcbba1',
                      '#fc9272',
                      '#fb6a4a',
                      '#ef3b2c',
                      '#cb181d',
                      '#a50f15',
                      '#67000d'), 
                    bins = c(0, 5, 8, 10, 12, 14, 18, 24, 26))

popup1 <- paste0("<span style='color: #7f0000'><strong>Número de empreendimentos</strong></span>",
                 "<br><span style='color: salmon;'><strong>District: </strong></span>", 
                 locais$`% Válido`)
mymap <- leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik",
                   options = tileOptions(minZoom=10, maxZoom=16)) %>% #"freeze" the mapwindow to max and min zoomlevel
  #CONTINUA!!!
  
  
  
  
  
