# CADUNICO
# Neylson
##############

setwd("E:/Bases/CADUNICO")

library(data.table)
library(bit64)
library(descr)
library(dplyr)
library(tidyr)
library(magrittr)

alto_jequi <- fread("Alto Jequitinhonha.csv") %>% as.data.frame(.,stringsAsFactors=F)
medio_baixo <- fread('Médio e Baixo Jequitinhonha.csv') %>% as.data.frame(.,stringsAsFactors=F)
mucuri <- fread('Mucuri.csv') %>% as.data.frame(.,stringsAsFactors=F)
norte <- fread('Norte.csv') %>% as.data.frame(.,stringsAsFactors=F)
vale_rio_doce <- fread('Vale do Rio Doce.csv') %>% as.data.frame(.,stringsAsFactors=F)

CADUNICO <- rbind(alto_jequi,medio_baixo,mucuri,norte,vale_rio_doce); rm(alto_jequi,medio_baixo,mucuri,norte,vale_rio_doce)
gc()

freq(CADUNICO$fx_rfpc,plot=F)

CAD = CADUNICO[CADUNICO$fx_rfpc == 1, ] # Separando famílias com renda até R$85,00

dic <- fread('E:/dicionariodomicilio.csv')
View(dic)



# Variáveis importantes
# MERGE - cod_familiar_fam [,3]
# Faixa de renda = fx_rfpc [, 23]
# Idade (nascimento) tá no PES. Tem que dar merge

