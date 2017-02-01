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

setwd('C:/Users/Administrator/Documents/BigData/Governo de Minas')
dados = fread('page_416907625028840_2017_02_01_13_19_33_comments.tab',
                 header=T, sep='\t) %>% as.data.frame(., stringsAsFactors=F)

names(dados)
freq(dados$post_by, plot=F)
summary(dados$comment_like_count)
ggplot(dados, aes(comment_like_count))+geom_histogram()

#por data ########

dados$datas_comentarios = dados$comment_published %>% as_date %>% ymd
head(dados$datas_comentarios)
######## Verificar

#Análises de texto
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

pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]

wordcloud(dataset_tm, min.freq = 5, random.order = F, colors = pal, max.words = 100)

corpus = Corpus(VectorSource(dataset_tm))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.92)
df <- as.data.frame(inspect(tdm))
dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2)

rect.hclust(fit.ward2, k=7)

library(igraph)
matriz <- as.matrix(df)
g <- graph_from_incidence_matrix(matriz)
is.bipartite(g)
g
plot(g, vertex.size=4, vertex.label=V(g)$name, vertex.color=as.numeric(V(g)$type))
g2 <- bipartite_projection(g, which = "FALSE")
deg = degree(g2)
plot(g2, edge.width=log(E(g2)$weight)/10, vertex.label.cex=deg/30,
     edge.color=adjustcolor("grey60", .4),
     vertex.label.color=adjustcolor("blue", .7),
     vertex.shape="none")

#############################################################
# Análise de sentimentos
