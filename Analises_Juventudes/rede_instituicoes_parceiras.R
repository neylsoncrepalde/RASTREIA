# RASTREIA
# Levantamento da Rede de Instituições Parceiras
# Script: Neylson Crepalde e Wesley Matheus
##############################################

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

#Monitorando questionários
freq(redes[,4], plot=F)

#Plotando questionários por dia
freq(redes[,5], plot=F)
datas = as.data.frame(table(redes[,5]), stringsAsFactors = F)
datas$Var1 = datas$Var1 %>% dmy %>% as_date
limits = c(20170106,20170209) %>% ymd %>% as_date

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










