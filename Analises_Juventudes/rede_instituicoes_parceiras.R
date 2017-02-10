# RASTREIA
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

setwd('C:/Users/x6905399/Documents/RASTREIA/JUVENTUDES')
redes = fread('rede_instituicoes_parceiras.csv', encoding="UTF-8") %>% 
  as.data.frame(., stringsAsFactors=F)

View(redes)
names(redes)

redes[,4][redes[,4] == "Jaqueline"] = "Jaqueline Silva"

#Monitorando questionários
freq(redes[,4], plot=F)

tabela = freq(redes[,4], plot=F) %>% as.data.frame(., stringsAsFactors=F)
tabela$pertotal = (tabela[,1] * 100) / 25
datatable(tabela[-nrow(tabela),c(1,3)])


#Plotando questionários por dia
freq(redes[,5], plot=F)
datas = as.data.frame(table(redes[,5]), stringsAsFactors = F)
datas$Var1 = datas$Var1 %>% dmy %>% as_date
limits = c(20170106,20170215) %>% ymd %>% as_date

ggplot(datas, aes(x=Var1, y=Freq))+geom_line()+
  scale_x_date(date_minor_breaks = '1 week', date_breaks = '1 week',
               date_labels = '%d/%m', limits = limits)+
  labs(x='',y='Número de questionários')

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






