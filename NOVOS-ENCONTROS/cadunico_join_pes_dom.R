# Vinculando PESSOAS a DOMICILIOS
# CADUNICO
# Script: Neylson Crepalde
#################################

setwd("~/Documentos/CADUNICO")

library(data.table)
library(bit64)
library(descr)
library(dplyr)
library(tidyr)
library(magrittr)

CADPES <- fread('CADPES.csv') %>% as.data.frame(.,stringsAsFactors=F)
gc()

#Separando variáveis de interesse, a saber, identificação 
names(CADPES)
#Variáveis 1,2,5,6,34
variaveis <- names(CADPES)
selec <- c(1,2,5,6,34)
selecao <- CADPES[,selec]
names(CADPES)[selec]

# Lendo os selecionados da Emater 2 e separando as variáveis de interesse
emater2 <- fread("selecionados_acao2.csv") %>% as.data.frame(., stringsAsFactors=F)
names(emater2)
manter <- c(1,2,3,4,22,8:17,67:69)
emater2 <- emater2[,manter]

#Juntando tudo
emater.merge <- left_join(emater2, selecao, by="cod_familiar_fam")
View(emater.merge)
#write.csv2(emater.merge, "selecionados_acao2_pes.csv", 
#           row.names = F, fileEncoding = "UTF-8")
##
### OK!

######################
#Agora juntando a acao1
emater1 <- fread("selecionados_acao1.csv") %>% as.data.frame(., stringsAsFactors=F)
names(emater1)
manter <- c(1,2,3,4,22,8:17,66:69)
emater1 <- emater1[,manter]
emater.merge1 <- left_join(emater1, selecao, by="cod_familiar_fam")

#write.csv2(emater.merge1, "selecionados_acao1_pes.csv", 
#           row.names = F, fileEncoding = "UTF-8")
### OK!!!
