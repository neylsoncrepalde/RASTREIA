# RASTREIA
# Levantamento da Rede de Instituições Parceiras
# Script: Neylson Crepalde e Wesley Matheus
##############################################
# Multiple plot function
#
# ggplot objects can be passed in ..., o# RASTREIA
# Levantamento da Rede de Instituições Parceiras
# Script: Neylson Crepalde e Wesley Matheus
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
#############################################
library(ggplot2)
library(data.table)
library(bit64)
library(DT)
library(magrittr)
library(descr)
library(lubridate)

#Carregando os bancos de dados
setwd('C:/Users/x6905399/Documents/RASTREIA/JUVENTUDES')
redes = fread('rede_instituicoes_parceiras.csv', encoding="UTF-8") %>% 
  as.data.frame(., stringsAsFactors=F)
coletivos = fread('coletivos.csv', encoding="UTF-8") %>% 
  as.data.frame(., stringsAsFactors=F)
referencias = fread('referencias_comunitarias.csv', encoding="UTF-8") %>% 
  as.data.frame(., stringsAsFactors=F) %>% .[-1,]

redes$tipo = 'Instituição'
coletivos$tipo = 'Coletivo'
referencias$tipo = 'Referência'

names(redes)[1:10]
names(coletivos)[1:10]
names(referencias)[1:10]

#View(redes)
names(redes)
redes[,4][redes[,4] == "Jaqueline"] = "Jaqueline Silva"
redes[,4][redes[,4] == "paula neres"] = "Paula Neres"
#View(coletivos)
names(coletivos)
coletivos[,4][coletivos[,4] == "Jaqueline"] = "Jaqueline Silva"
names(referencias)
referencias[,4][referencias[,4] == "Jaqueline"] = "Jaqueline Silva"

#Monitorando questionários
freq(redes[,4], plot=F)
freq(coletivos[,4], plot=F)
freq(referencias[,4], plot=F)

names(coletivos)[3] = 'Serviço'
entregas = rbind(redes[,c(1:4,54)], coletivos[,c(1:4,54)], referencias[,c(1:4,21)])
entregas$publica = 'Outra'

for (i in 1:nrow(redes)){
  result <- ifelse(redes[i,6] == 'Privada', 'Privada', 'Pública')
  entregas$publica[i] <- result
}



tabela = freq(entregas[,4], plot=F) %>% as.data.frame(., stringsAsFactors=F)
tabela$pertotal = (tabela[,1] * 100) / 25
tabela$pertotal2 = paste0(as.character(tabela$pertotal),'%')
names(tabela) = c('Total de Entregas', 'Percentual','Percentual piso','Percentual do piso (25)')

datatable(tabela[-nrow(tabela),c(1,4)])

#Verificando entregas por tipo de consulta
library(tidyr)

tabela_tipo = entregas %>% dplyr::group_by(`Nome do Mobilizador(a):`) %>% 
  count(tipo) %>% spread(., tipo, n)
for (var in 2:ncol(tabela_tipo)){
  for (row in 1:nrow(tabela_tipo)){
    if (is.na(tabela_tipo[row,var])){
      tabela_tipo[row,var] <- 0
    }
  }
}
datatable(tabela_tipo)

tabela_pubpri = entregas %>% dplyr::group_by(`Nome do Mobilizador(a):`) %>% 
  count(publica) %>% spread(., publica, n)
for (var in 2:ncol(tabela_pubpri)){
  for (row in 1:nrow(tabela_pubpri)){
    if (is.na(tabela_pubpri[row,var])){
      tabela_pubpri[row,var] <- 0
    }
  }
}
datatable(tabela_pubpri)


#Plotando questionários por dia
datas_de_entrega = c(redes[,1],coletivos[,1],referencias[,1]) %>% dmy_hms %>% as_date
freq(datas_de_entrega, plot=F)

datas = as.data.frame(table(datas_de_entrega), stringsAsFactors = F)
datas$datas_de_entrega = datas$datas_de_entrega %>% ymd %>% as_date
limits = c(20170201,20170218) %>% ymd %>% as_date
names(datas) = c('Datas de Entrega','Formulários Enviados')

g = ggplot(datas, aes(x=`Datas de Entrega`, y=`Formulários Enviados`))+geom_line(lwd=1)+
  scale_x_date(date_minor_breaks = '1 day', date_breaks = '2 days',
               date_labels = '%d/%m', limits = limits)+
  labs(x='Data de entrega do',y='Número de questionários')
library(plotly)
ggplotly(g)

############################
# Plotando mapa das regiões
head(entregas)
entregas$servico = entregas$Serviço 
entregas = entregas %>% separate(servico, c('parte1','regiao'), ' / ')
names(entregas)

entregas$regiao %>% freq(., plot=F)
entregas2 = entregas %>% separate(regiao, c('r1','r2','r3','r4'), ' e ')
regioes_desag = c(entregas2$r1, entregas2$r2, entregas2$r3, entregas2$r4)
for (i in 1:length(regioes_desag)){
  if (is.na(regioes_desag[i])){
    next
  }  
  if (regioes_desag[i] == 'Santa Rita'){
    regioes_desag[i] <- 'Santa Rita de Cássia'
    #print('sim')
  }
}
locais = freq(regioes_desag, plot=F) %>% as.data.frame(., stringsAsFactors=F) %>%
  .[-c(nrow(.), nrow(.)-1),]
locais$Locais = rownames(locais)
locais = mutate(locais,
                nomes = rownames(locais),
                lat = c(-19.939045,-19.912857,-20.027582,-19.948919,-19.802896,
                        -19.907773,-19.828723,-19.916681,#jardim teresópolis
                        -19.945829,-19.858375,-19.863601, #papagaio
                        -19.836808,-19.962187,-19.953818,-19.916447),
                lon = c(-43.919928,-43.893466,-44.228331,-43.940996,-43.997121,
                        -43.882842,-43.925793,-43.934493, #jardim teresópolis
                        -43.963345,-44.024482,-43.898408, #papagaio
                        -43.937718,-43.947033,-43.941464,-43.885229))
locais




###################################################################
# Perfil das instituições
############################################
names(redes)
names(coletivos)
names(referencias)

#extraindo as regiões do serviço
dados$servico = dados$Serviço 
dados = dados %>% separate(servico, c('parte1','regiao'), ' / ')
names(dados)

dados$regiao %>% freq(., plot=F)
dados2 = dados %>% separate(regiao, c('r1','r2','r3'), ' e ')
regioes_desag = rbind(dados2$r1, dados2$r2, dados2$r3)
locais = freq(regioes_desag, plot=F) %>% as.data.frame(., stringsAsFactors=F) %>%
  .[-c(nrow(.), nrow(.)-1),]
#locais = mutate(locais,
#                nomes = rownames(locais),
#                lat = c(-19.939045,-19.912857,-20.027582,-19.907773,-19.828723,-19.951469,
#                        -19.945829,-20.039762,-19.962187,-19.965879,-19.916447),
#                lon = c(-43.919928,-43.893466,-44.228331,-43.882842,-43.925793,-44.117417,
#                        -43.963345,-44.216812,-43.947033,-44.014811,-43.885229))
#locais

##### PAREI AQUI!!!!!
instituicoes = c(redes[,8], coletivos[,8])
regiao = 
regiao

inst = data.frame(redes[,c(8,13,14,16,17,19)])
names(inst) = c('Nome da instituição','Bairro', 'Cidade',
                'Telefone','E-mail','Responsável')
datatable(inst)

# Atividades
instituicoes = c(redes[,8], coletivos[,8], referencias[,6])
atividades = c(redes[,38], coletivos[,37], referencias[,13])
ativ_realiz = data.frame(instituicoes, atividades, stringsAsFactors = F)
names(ativ_realiz) = c("Instituição/Coletivo/Referência","Principais serviços oferecidos")
datatable(ativ_realiz)



####################################
# Jovens Atendidos
library(dplyr)
names(redes)

ja1 = freq(table(redes[,29])) %>% as.data.frame %>% .[-nrow(.),] %>%
  mutate(numero = as.numeric(rownames(.))) # 15-18
ja2 = freq(table(redes[,31])) %>% as.data.frame %>% .[-nrow(.),] %>% 
  mutate(numero = as.numeric(rownames(.)))# 19-24
ja3 = freq(table(redes[,33])) %>% as.data.frame %>% .[-nrow(.),] %>% 
  mutate(numero = as.numeric(rownames(.)))# 25-29

total1 = ja1$Frequência*ja1$numero
total2 = ja2$Frequência*ja2$numero
total3 = ja3$Frequência*ja3$numero
total = sum(total1,total2,total3)

(sum(total1)/total)*100

jovens_atendidos = data.frame(idade=c('15-18','19-24','25-29','Total'),
                              atendidos=c(sum(total1), sum(total2), sum(total3),
                                          total),
                              percentuais=c((sum(total1)/total)*100,
                                            (sum(total2)/total)*100,
                                            (sum(total3)/total)*100,
                                            100))
names(jovens_atendidos) = c('Faixa Etária','Quantidade de atendidos','Percentuais')
datatable(jovens_atendidos)

# DIFICULDADES DOS JOVENS
redes[,39]
library(tm)
library(wordcloud)
names(redes)
pal2 = brewer.pal(8,'Dark2')

dificuldades = redes[,39] %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords('pt'))  %>% removeWords(., 'jovens')

wordcloud(dificuldades, min.freq=2,max.words=100, random.order=F, colors=pal2)
  
corpus = Corpus(VectorSource(dificuldades))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.94)
df <- as.data.frame(inspect(tdm))
dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2, xlab = '',ylab = '',main = 'Dendograma'); rect.hclust(fit.ward2, k=7, border='#ffd42a')

par(mfrow=c(1,2))
par(mfrow=c(1,1))

#######
# Demandas dos jovens

demandas = redes[,40] %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords('pt'))  %>% removeWords(., 'jovens')

dem_nao_atend = redes[,41] %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords('pt'))  %>% removeWords(., 'jovens')

par(mfrow=c(1,2))
wordcloud(demandas, min.freq=2,max.words=100, random.order=F, colors=pal2)
title(xlab='Principais demandas')
wordcloud(dem_nao_atend, min.freq=2,max.words=100, random.order=F, colors=pal2)
title(xlab='Demandas não atendidas')
par(mfrow=c(1,1))

################################################
# Parcerias e participaçao política

freq(redes[,46],plot=F)
freq(redes[,47],plot=F)
freq(redes[,48],plot=F)
freq(redes[,49],plot=F)

tab4 = table(redes[,46]) %>% as.data.frame(.,stringsAsFactors=F)
tab5 = table(redes[,48]) %>% as.data.frame(.,stringsAsFactors=F)

g4 = ggplot(tab4, aes(x=Var1,y=Freq))+geom_bar(stat='identity',fill='#ffd42a')+
  labs(x='',y='',title='A instituição participa de rede local?')
g5 = ggplot(tab5, aes(x=Var1,y=Freq))+geom_bar(stat='identity',fill='#ffd42a')+
  labs(x='',y='',title='Participa de espaços de discussão política?')

multiplot(g4,g5, cols=2)

parcerias = redes[,c(8,47,49)]
names(parcerias) = c('Instituição','Rede local','Espaços de discussão política')
datatable(parcerias)


#################################
# Rede de cooperação entre instituições




# Parcerias que poderiam ser feitas
freq(redes[,45], plot=F)

# Avaliação de parcerias
freq(redes[,50],plot=F) # Como avalia a parceria?
tab3 = table(redes[,50]) %>% as.data.frame(., stringsAsFactors=F)
tab3$Var1 = as.factor(tab3$Var1) %>% as.factor
ggplot(tab3, aes(x=ordered(Var1, levels=c('Péssimo','Ruim','Médio','Bom')),
                 y=Freq))+geom_bar(stat='identity',fill='#ffd42a')+
  labs(x='',y='',title='Como avalia o trabalho em parceria?')+coord_flip()

freq(redes[,51],plot=F) # Porque?

avalia_parceria = redes[,c(8,50,51)]
names(avalia_parceria) = c('Instituição', 'Como você avalia o trabalho em parceria?','Porque?')
datatable(avalia_parceria)


# Interesse em parceria com JUVENTUDES
freq(redes[,52],plot=F) #interesse?
freq(redes[,53],plot=F) #possibilidades
interesse = table(redes[,52]) %>% as.data.frame(., stringsAsFactors=F)
possibilidades = strsplit(redes[,53], ', ', fixed=T) %>% unlist

g1 = ggplot(interesse, aes(x=Var1,y=Freq))+geom_bar(stat='identity',fill='#ffd42a')+
  labs(title='Existe interesse em parceria com JUVENTUDES?',x='',y='')
g2 = ggplot(NULL, aes(possibilidades))+geom_bar(fill='#ffd42a')+
  labs(x='',y='',title='Possibilidades de parceria com JUVENTUDES')+
  coord_flip()


###################################################
# Rede de parceiros
# Vars 42, 43, 44, 8
# Tem que limpar os dados!!! :(
freq(redes[,42],plot=F)
freq(redes[,43],plot=F)
freq(redes[,44],plot=F)

View(redes[,42:44])

rede_cooperacao = data.frame(redes[,c(8,42:44)])

#write.table(rede_cooperacao, 'rede_cooperacao_bruta.csv', sep=',', row.names = F,
#            fileEncoding = 'UTF-8')
names(rede_cooperacao) = c('Instituição','Inst. Gov. Parceiras',
                           'ONGs parceiras','Coletivos Parceiros')
rede_cooperacao[,1] %<>% tolower
rede_cooperacao[,2] %<>% tolower
rede_cooperacao[,3] %<>% tolower
rede_cooperacao[,4] %<>% tolower
View(rede_cooperacao)

library(tidyr)
gov = rede_cooperacao[,c(1,2)]
ong = rede_cooperacao[,c(1,3)]
coletivos = rede_cooperacao[,c(1,4)]

# Rede com inst gov
View(gov)
gov = gov %>% mutate(corrigido = strsplit(gov[,2], ', ')) %>% unnest(corrigido)
gov = gov %>% mutate(corrigido2 = strsplit(corrigido, '; ')) %>% unnest(corrigido2)
gov = gov %>% mutate(corrigido3 = strsplit(corrigido2, ';')) %>% unnest(corrigido3)
gov$corrigido3 %<>% removePunctuation

rede_gov = cbind(gov$Instituição,gov$corrigido3)
library(igraph)
g_gov = graph_from_edgelist(rede_gov)
indeg = degree(g_gov, mode='in')
plot(g_gov, vertex.size=indeg, vertex.label.cex=(indeg+1)/6, 
     vertex.label.color=adjustcolor('blue',.7), edge.arrow.size=.3)
title(main = 'Principais requisitados')
outdeg = degree(g_gov, mode='out')
plot(g_gov, vertex.size=outdeg, vertex.label.cex=(outdeg+1)/10, 
     vertex.label.color=adjustcolor('blue',.7), edge.arrow.size=.3)
title(main = 'Instituições mais ativas')

###################################
# rede com inst NAO gov
# programar...
ong = ong %>% mutate(corrigido = strsplit(ong[,2], ', ')) %>% unnest(corrigido)
ong = ong %>% mutate(corrigido2 = strsplit(corrigido, '; ')) %>% unnest(corrigido2)
ong = ong %>% mutate(corrigido3 = strsplit(corrigido2, ';')) %>% unnest(corrigido3)
ong$corrigido3 %<>% removePunctuation

View(rede_ong)

rede_ong = cbind(ong$Instituição,ong$corrigido3)
mudar = which(rede_ong[,2] == '')
rede_ong[,2][mudar] = 'nenhum'

g_ong = graph_from_edgelist(rede_ong)
indeg = degree(g_ong, mode='in')
plot(g_ong, vertex.size=indeg, vertex.label.cex=(indeg+1)/5, 
     vertex.label.color=adjustcolor('blue',.7), edge.arrow.size=.3)
title(main = 'Principais requisitados')
outdeg = degree(g_ong, mode='out')
plot(g_ong, vertex.size=outdeg, vertex.label.cex=(outdeg+1)/7, 
     vertex.label.color=adjustcolor('blue',.7), edge.arrow.size=.3)
title(main = 'Instituições mais ativas')

########################
# rede coletivos
coletivos = coletivos %>% mutate(corrigido = strsplit(coletivos[,2], ', ')) %>% unnest(corrigido)
coletivos = coletivos %>% mutate(corrigido2 = strsplit(corrigido, '; ')) %>% unnest(corrigido2)
coletivos = coletivos %>% mutate(corrigido3 = strsplit(corrigido2, ';')) %>% unnest(corrigido3)
coletivos$corrigido3 %<>% removePunctuation

rede_coletivos = cbind(coletivos$Instituição,coletivos$corrigido3)
mudar = which(rede_coletivos[,2] == '')
rede_coletivos[,2][mudar] = 'nenhum'
View(rede_coletivos)


g_coletivos = graph_from_edgelist(rede_coletivos)
g_coletivos = delete.vertices(g_coletivos, 'nenhum')
indeg = degree(g_coletivos, mode='in')
plot(g_coletivos, vertex.size=indeg, vertex.label.cex=(indeg+1)/3, 
     vertex.label.color=adjustcolor('blue',.7), edge.arrow.size=.3)
title(main = 'Principais requisitados')
outdeg = degree(g_coletivos, mode='out')
plot(g_coletivos, vertex.size=outdeg, vertex.label.cex=(outdeg+1)/6, 
     vertex.label.color=adjustcolor('blue',.7), edge.arrow.size=.3)
title(main = 'Instituições mais ativas')


#######################################################################
# Paramos aqui!




r to plotlist (as a list of ggplot objects)
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
#############################################
library(ggplot2)
library(data.table)
library(bit64)
library(DT)
library(magrittr)
library(descr)
library(lubridate)

#Carregando os bancos de dados
setwd('C:/Users/x6905399/Documents/RASTREIA/JUVENTUDES')
redes = fread('rede_instituicoes_parceiras.csv', encoding="UTF-8") %>% 
  as.data.frame(., stringsAsFactors=F)
coletivos = fread('coletivos.csv', encoding="UTF-8") %>% 
  as.data.frame(., stringsAsFactors=F)
referencias = fread('referencias_comunitarias.csv', encoding="UTF-8") %>% 
  as.data.frame(., stringsAsFactors=F) %>% .[-1,]

redes$tipo = 'Instituição'
coletivos$tipo = 'Coletivo'
referencias$tipo = 'Referência'

names(redes)[1:10]
names(coletivos)[1:10]
names(referencias)[1:10]

#View(redes)
names(redes)
redes[,4][redes[,4] == "Jaqueline"] = "Jaqueline Silva"
redes[,4][redes[,4] == "paula neres"] = "Paula Neres"
#View(coletivos)
names(coletivos)
coletivos[,4][coletivos[,4] == "Jaqueline"] = "Jaqueline Silva"
names(referencias)
referencias[,4][referencias[,4] == "Jaqueline"] = "Jaqueline Silva"

#Monitorando questionários
freq(redes[,4], plot=F)
freq(coletivos[,4], plot=F)
freq(referencias[,4], plot=F)

names(coletivos)[3] = 'Serviço'
entregas = rbind(redes[,c(1:4,54)], coletivos[,c(1:4,54)], referencias[,c(1:4,21)])
entregas$publica = 'Outra'

for (i in 1:nrow(redes)){
  result <- ifelse(redes[i,6] == 'Privada', 'Privada', 'Pública')
  entregas$publica[i] <- result
}



tabela = freq(entregas[,4], plot=F) %>% as.data.frame(., stringsAsFactors=F)
tabela$pertotal = (tabela[,1] * 100) / 25
tabela$pertotal2 = paste0(as.character(tabela$pertotal),'%')
names(tabela) = c('Total de Entregas', 'Percentual','Percentual piso','Percentual do piso (25)')

datatable(tabela[-nrow(tabela),c(1,4)])

#Verificando entregas por tipo de consulta
library(tidyr)

tabela_tipo = entregas %>% dplyr::group_by(`Nome do Mobilizador(a):`) %>% 
  count(tipo) %>% spread(., tipo, n)
for (var in 2:ncol(tabela_tipo)){
  for (row in 1:nrow(tabela_tipo)){
    if (is.na(tabela_tipo[row,var])){
      tabela_tipo[row,var] <- 0
    }
  }
}
datatable(tabela_tipo)

tabela_pubpri = entregas %>% dplyr::group_by(`Nome do Mobilizador(a):`) %>% 
  count(publica) %>% spread(., publica, n)
for (var in 2:ncol(tabela_pubpri)){
  for (row in 1:nrow(tabela_pubpri)){
    if (is.na(tabela_pubpri[row,var])){
      tabela_pubpri[row,var] <- 0
    }
  }
}
datatable(tabela_pubpri)


#Plotando questionários por dia
datas_de_entrega = c(redes[,1],coletivos[,1],referencias[,1]) %>% dmy_hms %>% as_date
freq(datas_de_entrega, plot=F)

datas = as.data.frame(table(datas_de_entrega), stringsAsFactors = F)
datas$datas_de_entrega = datas$datas_de_entrega %>% ymd %>% as_date
limits = c(20170201,20170218) %>% ymd %>% as_date
names(datas) = c('Datas de Entrega','Formulários Enviados')

g = ggplot(datas, aes(x=`Datas de Entrega`, y=`Formulários Enviados`))+geom_line(lwd=1)+
  scale_x_date(date_minor_breaks = '1 day', date_breaks = '2 days',
               date_labels = '%d/%m', limits = limits)+
  labs(x='Data de entrega do',y='Número de questionários')
library(plotly)
ggplotly(g)

############################
# Plotando mapa das regiões
head(entregas)
entregas$servico = entregas$Serviço 
entregas = entregas %>% separate(servico, c('parte1','regiao'), ' / ')
names(entregas)

entregas$regiao %>% freq(., plot=F)
entregas2 = entregas %>% separate(regiao, c('r1','r2','r3','r4'), ' e ')
regioes_desag = c(entregas2$r1, entregas2$r2, entregas2$r3, entregas2$r4)
for (i in 1:length(regioes_desag)){
  if (is.na(regioes_desag[i])){
    next
  }  
  if (regioes_desag[i] == 'Santa Rita'){
    regioes_desag[i] <- 'Santa Rita de Cássia'
    #print('sim')
  }
}
locais = freq(regioes_desag, plot=F) %>% as.data.frame(., stringsAsFactors=F) %>%
  .[-c(nrow(.), nrow(.)-1),]
locais$Locais = rownames(locais)
locais = mutate(locais,
                nomes = rownames(locais),
                lat = c(-19.939045,-19.912857,-20.027582,-19.948919,-19.802896,
                        -19.907773,-19.828723,-19.916681,#jardim teresópolis
                        -19.945829,-19.858375,-19.863601, #papagaio
                        -19.836808,-19.962187,-19.953818,-19.916447),
                lon = c(-43.919928,-43.893466,-44.228331,-43.940996,-43.997121,
                        -43.882842,-43.925793,-43.934493, #jardim teresópolis
                        -43.963345,-44.024482,-43.898408, #papagaio
                        -43.937718,-43.947033,-43.941464,-43.885229))
locais




#####################################
# Perfil das instituições
names(redes)

freq(redes[,8])  #nome instituicao
freq(redes[,9])  #possui CNPJ
freq(redes[,10]) #ano de inicio

inst = data.frame(redes[,c(8,10,13,14,16,17,18,19)])
names(inst) = c('Nome da instituição','Início das atividades','Bairro', 'Cidade',
                'Telefone','E-mail','Facebook','Responsável')
datatable(inst)

# Atividades
atividades = data.frame(redes[,c(8,38)])
names(atividades) = c("Instituição","Principais serviços oferecidos")
datatable(atividades)

# Responsável

resp = data.frame(redes[,c(19,21,25,26,27)])
names(resp) = c('Responsável','Gênero','É morador da região?',
                'Há quanto tempo mora?','Há quanto tempo trabalha na região?')
datatable(resp)

freq(redes[,26])
freq(redes[,27])

tab1 = table(redes[,26]) %>% as.data.frame(., stringsAsFactors=F)
g1 = ggplot(tab1, aes(x=Var1, y=Freq))+
  geom_bar(stat='identity')+
  labs(x='',y='',title='Há quanto tempo mora na região?')+
  coord_flip()
tab2 = table(redes[,27]) %>% as.data.frame(., stringsAsFactors=F)
g2 = ggplot(tab2, aes(x=Var1, y=Freq))+
  geom_bar(stat='identity')+
  labs(x='',y='',title='Há quanto tempo trabalha na região?')+
  coord_flip()

multiplot(g1,g2,cols=2)


####################################
# Jovens Atendidos
library(dplyr)
names(redes)

ja1 = freq(table(redes[,29])) %>% as.data.frame %>% .[-nrow(.),] %>%
  mutate(numero = as.numeric(rownames(.))) # 15-18
ja2 = freq(table(redes[,31])) %>% as.data.frame %>% .[-nrow(.),] %>% 
  mutate(numero = as.numeric(rownames(.)))# 19-24
ja3 = freq(table(redes[,33])) %>% as.data.frame %>% .[-nrow(.),] %>% 
  mutate(numero = as.numeric(rownames(.)))# 25-29

total1 = ja1$Frequência*ja1$numero
total2 = ja2$Frequência*ja2$numero
total3 = ja3$Frequência*ja3$numero
total = sum(total1,total2,total3)

(sum(total1)/total)*100

jovens_atendidos = data.frame(idade=c('15-18','19-24','25-29','Total'),
                              atendidos=c(sum(total1), sum(total2), sum(total3),
                                          total),
                              percentuais=c((sum(total1)/total)*100,
                                            (sum(total2)/total)*100,
                                            (sum(total3)/total)*100,
                                            100))
names(jovens_atendidos) = c('Faixa Etária','Quantidade de atendidos','Percentuais')
datatable(jovens_atendidos)

# DIFICULDADES DOS JOVENS
redes[,39]
library(tm)
library(wordcloud)
names(redes)
pal2 = brewer.pal(8,'Dark2')

dificuldades = redes[,39] %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords('pt'))  %>% removeWords(., 'jovens')

wordcloud(dificuldades, min.freq=2,max.words=100, random.order=F, colors=pal2)
  
corpus = Corpus(VectorSource(dificuldades))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.94)
df <- as.data.frame(inspect(tdm))
dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2, xlab = '',ylab = '',main = 'Dendograma'); rect.hclust(fit.ward2, k=7, border='#ffd42a')

par(mfrow=c(1,2))
par(mfrow=c(1,1))

#######
# Demandas dos jovens

demandas = redes[,40] %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords('pt'))  %>% removeWords(., 'jovens')

dem_nao_atend = redes[,41] %>% tolower %>% removePunctuation %>%
  removeWords(., stopwords('pt'))  %>% removeWords(., 'jovens')

par(mfrow=c(1,2))
wordcloud(demandas, min.freq=2,max.words=100, random.order=F, colors=pal2)
title(xlab='Principais demandas')
wordcloud(dem_nao_atend, min.freq=2,max.words=100, random.order=F, colors=pal2)
title(xlab='Demandas não atendidas')
par(mfrow=c(1,1))

################################################
# Parcerias e participaçao política

freq(redes[,46],plot=F)
freq(redes[,47],plot=F)
freq(redes[,48],plot=F)
freq(redes[,49],plot=F)

tab4 = table(redes[,46]) %>% as.data.frame(.,stringsAsFactors=F)
tab5 = table(redes[,48]) %>% as.data.frame(.,stringsAsFactors=F)

g4 = ggplot(tab4, aes(x=Var1,y=Freq))+geom_bar(stat='identity',fill='#ffd42a')+
  labs(x='',y='',title='A instituição participa de rede local?')
g5 = ggplot(tab5, aes(x=Var1,y=Freq))+geom_bar(stat='identity',fill='#ffd42a')+
  labs(x='',y='',title='Participa de espaços de discussão política?')

multiplot(g4,g5, cols=2)

parcerias = redes[,c(8,47,49)]
names(parcerias) = c('Instituição','Rede local','Espaços de discussão política')
datatable(parcerias)


#################################
# Rede de cooperação entre instituições




# Parcerias que poderiam ser feitas
freq(redes[,45], plot=F)

# Avaliação de parcerias
freq(redes[,50],plot=F) # Como avalia a parceria?
tab3 = table(redes[,50]) %>% as.data.frame(., stringsAsFactors=F)
tab3$Var1 = as.factor(tab3$Var1) %>% as.factor
ggplot(tab3, aes(x=ordered(Var1, levels=c('Péssimo','Ruim','Médio','Bom')),
                 y=Freq))+geom_bar(stat='identity',fill='#ffd42a')+
  labs(x='',y='',title='Como avalia o trabalho em parceria?')+coord_flip()

freq(redes[,51],plot=F) # Porque?

avalia_parceria = redes[,c(8,50,51)]
names(avalia_parceria) = c('Instituição', 'Como você avalia o trabalho em parceria?','Porque?')
datatable(avalia_parceria)


# Interesse em parceria com JUVENTUDES
freq(redes[,52],plot=F) #interesse?
freq(redes[,53],plot=F) #possibilidades
interesse = table(redes[,52]) %>% as.data.frame(., stringsAsFactors=F)
possibilidades = strsplit(redes[,53], ', ', fixed=T) %>% unlist

g1 = ggplot(interesse, aes(x=Var1,y=Freq))+geom_bar(stat='identity',fill='#ffd42a')+
  labs(title='Existe interesse em parceria com JUVENTUDES?',x='',y='')
g2 = ggplot(NULL, aes(possibilidades))+geom_bar(fill='#ffd42a')+
  labs(x='',y='',title='Possibilidades de parceria com JUVENTUDES')+
  coord_flip()


###################################################
# Rede de parceiros
# Vars 42, 43, 44, 8
# Tem que limpar os dados!!! :(
freq(redes[,42],plot=F)
freq(redes[,43],plot=F)
freq(redes[,44],plot=F)

View(redes[,42:44])

rede_cooperacao = data.frame(redes[,c(8,42:44)])

#write.table(rede_cooperacao, 'rede_cooperacao_bruta.csv', sep=',', row.names = F,
#            fileEncoding = 'UTF-8')
names(rede_cooperacao) = c('Instituição','Inst. Gov. Parceiras',
                           'ONGs parceiras','Coletivos Parceiros')
rede_cooperacao[,1] %<>% tolower
rede_cooperacao[,2] %<>% tolower
rede_cooperacao[,3] %<>% tolower
rede_cooperacao[,4] %<>% tolower
View(rede_cooperacao)

library(tidyr)
gov = rede_cooperacao[,c(1,2)]
ong = rede_cooperacao[,c(1,3)]
coletivos = rede_cooperacao[,c(1,4)]

# Rede com inst gov
View(gov)
gov = gov %>% mutate(corrigido = strsplit(gov[,2], ', ')) %>% unnest(corrigido)
gov = gov %>% mutate(corrigido2 = strsplit(corrigido, '; ')) %>% unnest(corrigido2)
gov = gov %>% mutate(corrigido3 = strsplit(corrigido2, ';')) %>% unnest(corrigido3)
gov$corrigido3 %<>% removePunctuation

rede_gov = cbind(gov$Instituição,gov$corrigido3)
library(igraph)
g_gov = graph_from_edgelist(rede_gov)
indeg = degree(g_gov, mode='in')
plot(g_gov, vertex.size=indeg, vertex.label.cex=(indeg+1)/6, 
     vertex.label.color=adjustcolor('blue',.7), edge.arrow.size=.3)
title(main = 'Principais requisitados')
outdeg = degree(g_gov, mode='out')
plot(g_gov, vertex.size=outdeg, vertex.label.cex=(outdeg+1)/10, 
     vertex.label.color=adjustcolor('blue',.7), edge.arrow.size=.3)
title(main = 'Instituições mais ativas')

###################################
# rede com inst NAO gov
# programar...
ong = ong %>% mutate(corrigido = strsplit(ong[,2], ', ')) %>% unnest(corrigido)
ong = ong %>% mutate(corrigido2 = strsplit(corrigido, '; ')) %>% unnest(corrigido2)
ong = ong %>% mutate(corrigido3 = strsplit(corrigido2, ';')) %>% unnest(corrigido3)
ong$corrigido3 %<>% removePunctuation

View(rede_ong)

rede_ong = cbind(ong$Instituição,ong$corrigido3)
mudar = which(rede_ong[,2] == '')
rede_ong[,2][mudar] = 'nenhum'

g_ong = graph_from_edgelist(rede_ong)
indeg = degree(g_ong, mode='in')
plot(g_ong, vertex.size=indeg, vertex.label.cex=(indeg+1)/5, 
     vertex.label.color=adjustcolor('blue',.7), edge.arrow.size=.3)
title(main = 'Principais requisitados')
outdeg = degree(g_ong, mode='out')
plot(g_ong, vertex.size=outdeg, vertex.label.cex=(outdeg+1)/7, 
     vertex.label.color=adjustcolor('blue',.7), edge.arrow.size=.3)
title(main = 'Instituições mais ativas')

########################
# rede coletivos
coletivos = coletivos %>% mutate(corrigido = strsplit(coletivos[,2], ', ')) %>% unnest(corrigido)
coletivos = coletivos %>% mutate(corrigido2 = strsplit(corrigido, '; ')) %>% unnest(corrigido2)
coletivos = coletivos %>% mutate(corrigido3 = strsplit(corrigido2, ';')) %>% unnest(corrigido3)
coletivos$corrigido3 %<>% removePunctuation

rede_coletivos = cbind(coletivos$Instituição,coletivos$corrigido3)
mudar = which(rede_coletivos[,2] == '')
rede_coletivos[,2][mudar] = 'nenhum'
View(rede_coletivos)


g_coletivos = graph_from_edgelist(rede_coletivos)
g_coletivos = delete.vertices(g_coletivos, 'nenhum')
indeg = degree(g_coletivos, mode='in')
plot(g_coletivos, vertex.size=indeg, vertex.label.cex=(indeg+1)/3, 
     vertex.label.color=adjustcolor('blue',.7), edge.arrow.size=.3)
title(main = 'Principais requisitados')
outdeg = degree(g_coletivos, mode='out')
plot(g_coletivos, vertex.size=outdeg, vertex.label.cex=(outdeg+1)/6, 
     vertex.label.color=adjustcolor('blue',.7), edge.arrow.size=.3)
title(main = 'Instituições mais ativas')


#######################################################################
# Paramos aqui!




