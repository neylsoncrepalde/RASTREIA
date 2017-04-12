#####################################
# Problemas  - ações com água
# RASTREIA
# Neylson Crepalde
#####################################

library(descr)
library(dplyr)
library(tidyr)
library(magrittr)
library(xtable)

setwd('C:/Users/x6905399/Documents/RASTREIA/NOVOS ENCONTROS/problemas_agua')

#SECIR
##########
secir = read.csv2('problemas_agua_secir.csv',
                  stringsAsFactors = F)
View(secir) # Com altos problemas
names(secir)[20:21] = c('Em operação? 1=sim, 0=não','Se não, porque? sem análise de água=1, água imprópria para consumo humano=2, tubulação não entregue=3, impossibiliader de caraterizar quantitativa e qualitativamente=4, Sem energização=5')
names(secir)[26] = c("Solução proposta")
secir %<>% .[1:28,]
###############

#IGAM
###############
igam = read.csv2('problemas_agua_igam.csv', stringsAsFactors = F)
View(igam)
names(igam)[20:21] = c('Em operação? 1=sim, 0=não','Se não, porque? sem análise de água=1, água imprópria para consumo humano=2, tubulação não entregue=3, impossibiliader de caraterizar quantitativa e qualitativamente=4, Sem energização=5')
names(igam)[26] = c("Solução proposta")
###############

#COPANOR
###############
copanor = read.csv2('problemas_agua_copanor.csv', stringsAsFactors = F)
View(copanor)
names(copanor)[20:21] = c('Em operação? 1=sim, 0=não','Se não, porque? sem análise de água=1, água imprópria para consumo humano=2, tubulação não entregue=3, impossibiliader de caraterizar quantitativa e qualitativamente=4, Sem energização=5')
names(copanor)[26] = c("Solução proposta")
###############


#SEDINOR
###############
sedinor = read.csv2('problemas_agua_sedinor.csv',stringsAsFactors = F)
View(sedinor)
names(sedinor)[20:21] = c('Em operação? 1=sim, 0=não','Se não, porque? sem análise de água=1, água imprópria para consumo humano=2, tubulação não entregue=3, impossibiliader de caraterizar quantitativa e qualitativamente=4, Sem energização=5')
names(sedinor)[26] = c("Solução proposta")
sedinor %<>% .[1:219,]
###############

dados = rbind(copanor, sedinor, igam, secir)

##############################################

### ANÁLISES:

##########Poços fora de operação
class(dados$`Em operação? 1=sim, 0=não`)

dados$`Em operação? 1=sim, 0=não`[dados$`Em operação? 1=sim, 0=não` == "Não"] = "0"
dados$`Em operação? 1=sim, 0=não`[dados$`Em operação? 1=sim, 0=não` == "R$ 0,00"] = "0"
freq(dados$`Em operação? 1=sim, 0=não`,plot=F)

tab1 = freq(dados$Município[dados$`Em operação? 1=sim, 0=não`=="0"],plot=F) %>% 
  as.data.frame(., stringsAsFactors=F) %>% mutate(nome = rownames(.)) %>% 
  arrange(desc(Frequência)) 
tab1 %<>% .[-c(1,2),] 
tab1 %<>% .[,-2]

xtab1 = xtable(tab1, caption = "Poços fora de operação", label='poc-operacao', 
               digits = 2, align = T)
print(xtab1, include.rownames = F, caption.placement = 'top', 
      tabular.environment = "longtable", floating = F)


### Porque está fora de operação?
class(dados$`Se não, porque? sem análise de água=1, água imprópria para consumo humano=2, tubulação não entregue=3, impossibiliader de caraterizar quantitativa e qualitativamente=4, Sem energização=5`)
dados$`Se não, porque? sem análise de água=1, água imprópria para consumo humano=2, tubulação não entregue=3, impossibiliader de caraterizar quantitativa e qualitativamente=4, Sem energização=5` %<>% gsub(';',',',.)

porque_nao_opera <- dados %>% select(Município, `Se não, porque? sem análise de água=1, água imprópria para consumo humano=2, tubulação não entregue=3, impossibiliader de caraterizar quantitativa e qualitativamente=4, Sem energização=5`) %>%
  unnest(strsplit(`Se não, porque? sem análise de água=1, água imprópria para consumo humano=2, tubulação não entregue=3, impossibiliader de caraterizar quantitativa e qualitativamente=4, Sem energização=5`, ','))

porque_nao_opera %<>% .[,-2]
names(porque_nao_opera)[2] = "Porque não opera"
class(porque_nao_opera$`Porque não opera`)
porque_nao_opera %<>% na.omit
View(porque_nao_opera)


pno_corrigido = data.frame()
for (row in 1:nrow(porque_nao_opera)){
  
  if (porque_nao_opera[row,2] == "1"){
    pno_corrigido[row,1] = porque_nao_opera[row,1]
    pno_corrigido[row,2] = porque_nao_opera[row,2]
  } 
  if (porque_nao_opera[row,2] == "2"){
    pno_corrigido[row,1] = porque_nao_opera[row,1]
    pno_corrigido[row,2] = porque_nao_opera[row,2]
  }
  if (porque_nao_opera[row,2] == "3"){
    pno_corrigido[row,1] = porque_nao_opera[row,1]
    pno_corrigido[row,2] = porque_nao_opera[row,2]
  }
  if (porque_nao_opera[row,2] == "5"){
    pno_corrigido[row,1] = porque_nao_opera[row,1]
    pno_corrigido[row,2] = porque_nao_opera[row,2]
  } else{
      next
    }
}
pno_corrigido %<>% na.omit

tabela2 <- freq(pno_corrigido$V2, plot=F) %>% as.data.frame(., stringsAsFactors=F)
xtab2 <- xtable(tabela2, caption = "Motivos de não operação", label='poc-operacao', 
                digits = 2)
print(xtab2, include.rownames = T, caption.placement = 'top', 
      tabular.environment = "longtable", floating = F)


# Separando cada motivo por município:
motivo1 = freq(pno_corrigido$V1[pno_corrigido$V2 == 1],plot=F) 
motivo1 = motivo1 %>% as.data.frame(.,stringsAsFactors=F) %>%
  mutate(Município = rownames(motivo1)) %>% arrange(desc(Frequência)) %>% .[-1,]
xmotivo1 = xtable(motivo1, caption = "Municípios sem análise de água", label='motivo1', 
                  digits = 2)
print(xmotivo1, include.rownames = F, caption.placement = 'top', 
      tabular.environment = "longtable", floating = F)



motivo2 = freq(pno_corrigido$V1[pno_corrigido$V2 == 2],plot=F) 
motivo2 = motivo2 %>% as.data.frame(.,stringsAsFactors=F) %>%
  mutate(Município = rownames(motivo2)) %>% arrange(desc(Frequência)) %>% .[-1,]
xmotivo2 = xtable(motivo2, 
                  caption = "Municípios com poços com água imprópria para o consumo",
                  label='motivo2', 
                  digits = 2)
print(xmotivo2, include.rownames = F, caption.placement = 'top', 
      tabular.environment = "longtable", floating = F)



motivo3 = freq(pno_corrigido$V1[pno_corrigido$V2 == 3],plot=F) 
motivo3 = motivo3 %>% as.data.frame(.,stringsAsFactors=F) %>%
  mutate(Município = rownames(motivo3)) %>% arrange(desc(Frequência)) %>% .[-1,]
xmotivo3 = xtable(motivo3, 
                  caption = "Municípios com poços com tubulação não entregue",
                  label='motivo3', 
                  digits = 2)
print(xmotivo3, include.rownames = F, caption.placement = 'top', 
      tabular.environment = "longtable", floating = F)


motivo4 = freq(pno_corrigido$V1[pno_corrigido$V2 == 5],plot=F) 
motivo4 = motivo4 %>% as.data.frame(.,stringsAsFactors=F) %>%
  mutate(Município = rownames(motivo4)) %>% arrange(desc(Frequência)) %>% .[-1,]
xmotivo4 = xtable(motivo4, 
                  caption = "Municípios com poços sem energização",
                  label='motivo4', 
                  digits = 2)
print(xmotivo4, include.rownames = F, caption.placement = 'top', 
      tabular.environment = "longtable", floating = F)
####################################################################


#####Problemas fundiários
freq(dados$Existe.Problema.Fundiário...sim...1..não...0., plot=F)
# 91.8288 de missings. Sem chance.
####################################################################


### Licença ambiental - Possui outorga?
which(dados$Outorga.de.Uso..sim...1..não...0. == '')
dados$Outorga.de.Uso..sim...1..não...0.[dados$Outorga.de.Uso..sim...1..não...0. == ''] = NA

outorga <- freq(dados$Outorga.de.Uso..sim...1..não...0., plot=F) %>% 
  as.data.frame(., stringsAsFactors=F)
xoutorga <- xtable(outorga, caption = "Licença ambiental - possui outorga?", label='outorga', 
                   digits = 2)
print(xoutorga, include.rownames = T, caption.placement = 'top', 
      tabular.environment = "longtable", floating = F)

munic_outorga <- cbind(dados$Município, dados$Outorga.de.Uso..sim...1..não...0.) %>%
  as.data.frame(., stringsAsFactors=F) %>% filter(V2 == 0)

semoutorga = freq(munic_outorga$V1, plot=F) 
semoutorga = semoutorga %>% as.data.frame(.,stringsAsFactors=F) %>%
  mutate(Município = rownames(semoutorga)) %>% arrange(desc(Frequência)) %>% .[-1,]
xsemoutorga = xtable(semoutorga, 
                  caption = "Municípios com poços sem outorga",
                  label='semoutorga', 
                  digits = 2)
print(xsemoutorga, include.rownames = F, caption.placement = 'top', 
      tabular.environment = "longtable", floating = F)

##########################################################################

###### Análise das soluções propostas:
View(cbind(dados$`Solução proposta`, dados$Órgão.Entidade))
freq(dados$`Solução proposta`,plot=F)

propostas <- cbind(dados$Município , dados$`Solução proposta`, dados$Órgão.Entidade) %>% 
  as.data.frame(., stringsAsFactors=F)
names(propostas) = c("Município", "Proposta","Órgão")
propostas = propostas %>% unnest(strsplit(Proposta, "\\. "))
names(propostas)[4] = "dividido1"
freq(propostas$dividido1, plot=F)
propostas = propostas %>% unnest(strsplit(dividido1, ", "))
names(propostas)[5] = "dividido2"
tab_propostas = freq(propostas$dividido2, plot=F) 
tab_propostas = tab_propostas %>% as.data.frame(.,stringsAsFactors=F) %>% 
  mutate(Município = rownames(tab_propostas)) %>% arrange(desc(Frequência)) %>% .[-1,]

xtab_propostas = xtable(tab_propostas, caption = "Principais propostas apresentadas pelos Órgãos",
                        label='propostas', 
                        digits = 2)
print(xtab_propostas, include.rownames = F, caption.placement = 'top', 
      tabular.environment = "longtable", floating = F)



