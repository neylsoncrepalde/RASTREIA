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
library(lubridate)
library(ggplot2)

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
# 0 - 17 e 65 < x -> 00 (puxa os dois e vê)
# Agregar variável ano da meta (planilha Jessika)

# vendo a data do cadastramento

datas <- CADUNICO[,4] %>% ymd %>% as_date
datas.df <- table(datas) %>% as.data.frame(.,stringsAsFactors=F)
limits = c(20140101,20170101)
limits %<>% ymd %>% as_date
ggplot(datas.df, aes(x=as_date(datas), y=Freq))+geom_line()+
  scale_x_date(date_minor_breaks = '1 year', date_breaks = '1 year',
               date_labels = '%Y', limits = limits)+
  scale_y_continuous(limits = c(0, 1000))

