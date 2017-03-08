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
library(plotly)

setwd("C:/Users/x6905399/Documents/RASTREIA/JUVENTUDES")

dados = fread('emprego_empreendedorismo.csv', encoding="UTF-8") %>% 
  as.data.frame(., stringsAsFactors=F)
dados[,4][dados[,4] == 'Moisés Ezequiel Macedo'] = 'Moisés Ezequiel Macêdo'
dados[,4][dados[,4] == 'FREDERICO MENDES'] = 'FREDERICO MENDES DE CARVALHO'
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
grep('2012', dados[,5])
dados[76,5] = "06/01/2017"
dados[148,5] = '06/02/2017'
dados[149,5] = '06/02/2017'


dados[170,5] = '13/02/2017'
dados[83,5] = '10/02/2017'

dados[150,5] = '06/02/2017'
dados[213,5] = '08/03/2017'
dados[216,5] = '01/03/2017'
dados[266,5] = '11/02/2017'

freq(dados[,5],plot=F)


datas = dados[,1] %>% dmy_hms %>% as_date %>%
  table %>% as.data.frame(., stringsAsFactors=F)
  
limits = c(20170128,20170225) %>% ymd %>% as_date
names(datas) = c('Data','Questionários')
datas$Data %<>% as_date

g = ggplot(datas, aes(x=Data, y=Questionários))+geom_line(lwd=1)+
  scale_x_date(date_minor_breaks = '1 week', date_breaks = '1 week',
               date_labels = '%d/%m', limits = limits)+
  labs(x='Data de lançamento',y='Número de questionários')

ggplotly(g)


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
                lat = c(-19.939045,-19.912857,-20.027582,-19.764419,
                        -19.907773,-19.828723,-19.951469,
                        
                        -19.945829,-19.830853,
                        -20.039762,-19.962187,-19.809883,-19.916447),
                
                lon = c(-43.919928,-43.893466,-44.228331,-44.145917,
                        -43.882842,-43.925793,-44.117417,
                        
                        -43.963345,-44.150248,
                        -44.216812,-43.947033,-43.887616,-43.885229))
locais

#plota o mapa
#library(sp)
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
#library(ggmap)
#BH = get_map(location = c(lon=-44.06,lat=-19.92), zoom = 11)
#mapPoints <- ggmap(BH)+geom_point(data=locais, shape=21, aes(x = lon, y = lat,size=`% Válido`),
#                                  colour='black',fill=adjustcolor('red',.5))+labs(title='RMBH')
#mapPoints

##################################
# Com Leaflet
##################################
library(leaflet)
mymap <- leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik",
                   options = tileOptions(minZoom=10, maxZoom=16)) %>% #"freeze" the mapwindow to max and min zoomlevel
  setView(-44.06,-19.92, zoom=10) %>%
  addCircleMarkers(lng=locais$lon,
                   lat=locais$lat,
                   radius = locais$`% Válido`,
                   color = 'blue',
                   fillColor = 'blue',
                   popup = paste0("<b>",locais$nomes,"</b><br>Número de Empreendimentos:<br><b>",locais$Frequência,"</b>")
  )

print(mymap)

#############################################
# Proporções de empreendimentos por categoria

universo = fread('percentual_empreendimentos.csv', dec = ',') %>% 
  as.data.frame(., stringsAsFactors=F) %>% .[-22,]
names(universo)

# proporções no universo
ggplot(universo, aes(x=`Descrição CNAE`, y=`Regiões JUVENTUDES`))+
  geom_bar(stat='identity', fill='#ffd42a')+
  labs(title='Proporções de tipos de empreendimentos no universo', x='',y='')+
  scale_x_discrete(limits=rev(levels(factor(universo$`Descrição CNAE`))))+
  coord_flip()

# proporções na amostra
names(dados[1:10])
freq(dados[,6], plot=F)

ggplot(dados, aes(`1.1 | Qual é o setor econômico do empreendimento?`))+
  geom_bar(fill='#ffd42a')+
  labs(title='Proporções de tipos de empreendimentos na amostra', x='',y='')+
  coord_flip()

### Vagas de trabalho existentes nos empreendimentos
### Separar tipos de trabalho por região
### após limpeza e transformações para integer, multiplicar nome por número disponível


# Gerando wordclouds com o pacote wordclou2
library(tm)
library(wordcloud)
pal2 = brewer.pal(8,'Dark2')

dificuldades = dados[,48] %>% tolower %>% 
  removePunctuation %>% removeWords(., stopwords('pt'))

corpus = Corpus(VectorSource(enc2native(dificuldades)))

#preparando o df para wordcloud2
tdm.word = TermDocumentMatrix(corpus) %>% as.matrix
dim(tdm.word)
tdm.df = data.frame(words = rownames(tdm.word),
                    freq = apply(tdm.word,1,sum))
View(tdm.df)
library(wordcloud2)
wordcloud2(tdm.df, shuffle = F, rotateRatio = .8, )

letterCloud(tdm.df, word = "RASTREIA", wordSize = 2)


# Preparando para o dendograma
tdm <- TermDocumentMatrix(corpus) %>% as.matrix
tdm <- removeSparseTerms(tdm, sparse = 0.97)
df <- as.data.frame(inspect(tdm))
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")


par(mfrow=c(1,2))
wordcloud(enc2native(dificuldades), min.freq=2,max.words=100, random.order=F, colors=pal2)
plot(fit.ward2, xlab = '',ylab = '',main = 'Dendograma')
par(mfrow=c(1,1))

