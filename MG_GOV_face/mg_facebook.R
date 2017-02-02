#Análise da página Governo do Estado de MG
#Script: Neylson Crepalde
#Diretor de Monitoramento e Avaliação - SEDESE

library(data.table)
library(magrittr)
library(descr)
library(ggplot2)
library(wordcloud)
library(RTextTools)
library(lubridate)

setwd('C:/Users/x6905399/Documents/MG_GOV_face')
dados = fread('page_416907625028840_2017_02_01_13_19_33_comments.tab',
              sep='\t')

summary(dados$comment_like_count)
ggplot(dados, aes(comment_like_count))+geom_histogram()

################################

library(tm)
library(wordcloud)
library(magrittr)

coments = dados$comment_message

dataset_tm = c()
for(i in coments){
  erro <- try(tolower(i), silent=TRUE)
  if ('try-error' %in% class(erro)){
    next
  } else{
    dataset_tm <- c(dataset_tm, tolower(i))
  }
}



dataset_tm <- dataset_tm %>% removePunctuation %>% removeWords(., stopwords('pt'))
head(dataset_tm)

grep('runs', dataset_tm)

dataset_tm <- gsub('polãcia', 'polícia', dataset_tm)
dataset_tm <- gsub('seguranãa', 'segurança', dataset_tm)
dataset_tm <- gsub('nomeaã‡ãƒo', 'nomeção', dataset_tm)
dataset_tm <- gsub('notãcia', 'notícia', dataset_tm)
dataset_tm <- gsub('nomeaãão', 'nomeação', dataset_tm)
dataset_tm <- gsub('populaãão', 'população', dataset_tm)
dataset_tm <- gsub('pãblica', 'pública', dataset_tm)
dataset_tm <- gsub('parabãns', 'parabéns', dataset_tm)
dataset_tm <- gsub('dãficit', 'déficit', dataset_tm)
dataset_tm <- gsub('ãltimo', 'último', dataset_tm)
dataset_tm <- gsub('injustiãa', 'injustiça', dataset_tm)
dataset_tm <- gsub('fãruns', 'fóruns', dataset_tm)

###########################
# Nuvem de palavras

pal <- brewer.pal(8,"Dark2")

wordcloud(dataset_tm, min.freq = 5, random.order = F, colors = pal, max.words = 100)

###############################
# Clusterização hierárquica

corpus = Corpus(VectorSource(dataset_tm))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.90)
df <- as.data.frame(inspect(tdm))
dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2, main = 'Clusterização Hierárquica')

rect.hclust(fit.ward2, k=6)

###################################
# Rede semântica

library(igraph)
matriz <- as.matrix(df)
g <- graph_from_incidence_matrix(matriz)
is.bipartite(g)
g
plot(g, vertex.size=4, vertex.label=V(g)$name, vertex.color=as.numeric(V(g)$type))
g2 <- bipartite_projection(g, which = "FALSE")
deg = degree(g2)
plot(g2, edge.width=log(E(g2)$weight)/40, vertex.label.cex=deg/25,
     edge.color=adjustcolor("grey60", .4),
     vertex.label.color=adjustcolor("red", .7),
     vertex.shape="none")
title('Rede semântica - Facebook')

#############################################################
# Análise de sentimentos
