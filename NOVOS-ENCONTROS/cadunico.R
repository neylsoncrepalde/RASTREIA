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
library(lme4)
library(merTools)
library(lmtest)
library(texreg)

alto_jequi <- fread("Alto Jequitinhonha.csv") %>% as.data.frame(.,stringsAsFactors=F)
medio_baixo <- fread('Médio e Baixo Jequitinhonha.csv') %>% as.data.frame(.,stringsAsFactors=F)
mucuri <- fread('Mucuri.csv') %>% as.data.frame(.,stringsAsFactors=F)
norte <- fread('Norte.csv') %>% as.data.frame(.,stringsAsFactors=F)
vale_rio_doce <- fread('Vale do Rio Doce.csv') %>% as.data.frame(.,stringsAsFactors=F)

CADUNICO <- rbind(alto_jequi,medio_baixo,mucuri,norte,vale_rio_doce); rm(alto_jequi,medio_baixo,mucuri,norte,vale_rio_doce)
gc()

freq(CADUNICO$fx_rfpc,plot=F)

dic <- fread('E:/dicionariodomicilio.csv')
dicpes <- fread('E:/dicionariopessoa.csv')
View(dic)
View(dicpes)

#linha 1510739 deu problema
CADPES1 <- fread('pessoa.csv', nrows = 1510737) %>% as.data.frame(.,stringsAsFactors=F)
CADPES2 <- fread('pessoa.csv', skip =  1510737) %>% as.data.frame(.,stringsAsFactors=F)
names(CADPES2) <- names(CADPES1)

CADPES <- rbind(CADPES1,CADPES2); rm(CADPES1, CADPES2)
gc()

#### Já verificamos duplicidade de CPF's #####
#########################
#Juntando as metas de aplicação 2017/2018

metas <- read.csv2('C:/Users/Admin2/Documents/RASTREIA/selecao_publico_cadunico.csv',
                  stringsAsFactors = F, header=F, encoding = 'UTF-8')
names(metas) <- c('cd_ibge','nome_munic','nome_regiao','ano_meta')

CADUNICO <- left_join(CADUNICO, metas) # faz o merge
#View(CADUNICO[1:100,])


########################################################
# Tentando um modelo logístico para verificar quais variáveis possuem
# maior impacto no fato de uma família estar na faixa mais baixa
# de renda

# Valor pago em aluguel e remédios não deu certo
# variavel escoa sanitario nao deu certo

CADUNICO$pobreza <- ifelse(CADUNICO$fx_rfpc == 1, 1, 0)
CADUNICO$cod_local_domic_fam %<>% as.factor
levels(CADUNICO$cod_local_domic_fam) <- c('Urbanas', 'Rurais')
CADUNICO$cod_agua_canalizada_fam %<>% as.factor
levels(CADUNICO$cod_agua_canalizada_fam) <- c('Sim','Não')
CADUNICO$cod_abaste_agua_domic_fam %<>% as.factor
levels(CADUNICO$cod_abaste_agua_domic_fam) <- c('Rede geral de distribuição',
                                                'Poço ou nascente',
                                                'Cisterna',
                                                'Outra forma')
CADUNICO$cod_banheiro_domic_fam %<>% as.factor
levels(CADUNICO$cod_banheiro_domic_fam) <- c('Sim', 'Não')
CADUNICO$cod_iluminacao_domic_fam %<>% as.factor
levels(CADUNICO$cod_iluminacao_domic_fam) <- c('Elétrica com medidor próprio',
                                               'Elétrica com medidor comunitário',
                                               'Elétrica sem medidor',
                                               'Óleo, querosene ou gás',
                                               'Vela',
                                               'Outra forma')



reg <- glm(pobreza ~ cod_local_domic_fam +
             qtd_comodos_dormitorio_fam + cod_agua_canalizada_fam +
             cod_abaste_agua_domic_fam + cod_banheiro_domic_fam +
             cod_iluminacao_domic_fam,
           data = CADUNICO, family = binomial(link='logit'))
summary(reg)

# Tentando um modelo logístico hierárquico
reg_multi <- glmer(pobreza ~ cod_local_domic_fam + (1 | nome_munic) +
                            qtd_comodos_dormitorio_fam + cod_agua_canalizada_fam +
                            cod_abaste_agua_domic_fam + cod_banheiro_domic_fam +
                            cod_iluminacao_domic_fam,
                          data = CADUNICO, family = binomial(link='logit'))
summary(reg_multi)
ICC = var(reg_multi@u) / (var(reg_multi@u)+var(residuals(reg_multi)))
lrtest(reg, reg_multi)
plotREsim(REsim(reg_multi))

#Gerando a tabela com os resultados das regressoes
texreg(list(reg, reg_multi), 
       custom.model.names = c('Logístico', 'Logístico Multinível'),
       center = F, caption.above = T, 
       caption = 'Modelos estatísticos')


#reg_multi_effale <- glmer(pobreza ~ cod_local_domic_fam +
#                     qtd_comodos_dormitorio_fam + cod_agua_canalizada_fam +
#                     cod_abaste_agua_domic_fam + (cod_banheiro_domic_fam | nome_munic) +
#                     cod_iluminacao_domic_fam,
#                   data = CADUNICO, family = binomial(link='logit'))
#summary(reg_multi_effale)

# Verificando as probabilidades
beta2prob <- function(x){
  return((exp(x)-1)*100)
}

beta2prob(coef(reg))
xtable::xtable(as.data.frame(beta2prob(coef(reg))))
beta2prob(reg_multi@beta)

################################
# vendo a data do cadastramento

datas <- CADUNICO[,4] %>% ymd %>% as_date
datas.df <- table(datas) %>% as.data.frame(.,stringsAsFactors=F)
limits = c(20140101,20170101)
limits %<>% ymd %>% as_date
ggplot(datas.df, aes(x=as_date(datas), y=Freq))+geom_line()+
  scale_x_date(date_minor_breaks = '1 year', date_breaks = '1 year',
               date_labels = '%Y', limits = limits)+
  scale_y_continuous(limits = c(0, 1000))
################################


#### Rankeando os municipios
# Juntando o resultado do intercepto aleatorio do modelo multinivel
resultados = coef(reg_multi)
ranking_munic = data.frame(rownames(resultados$nome_munic),resultados$nome_munic[,1])
names(ranking_munic) <- c('nome_munic', 'intercepto_aleatorio')
View(ranking_munic)
CADUNICO <- left_join(CADUNICO, ranking_munic)
View(CADUNICO[1:100,])


#####################################
# Fazendo a seleção

#Seleciona família com renda até R$85,00 per capita E que não
#tenham recebido Bolsa Família
rurais = CADUNICO[CADUNICO$cod_local_domic_fam == 'Rurais',]

selecao_acao1 <- CADUNICO[CADUNICO$fx_rfpc == 1 & CADUNICO$marc_pbf == 0 & CADUNICO$cod_local_domic_fam == 'Rurais',]
selecao_acao1 <- arrange(selecao_acao1, desc(cod_iluminacao_domic_fam),
                         desc(cod_banheiro_domic_fam),
                         desc(intercepto_aleatorio))
View(selecao_acao1)
write.table(selecao_acao1, 'selecionados_acao1.csv', 
            sep=',', row.names = F, fileEncoding = 'UTF-8')

#############################################
# Variáveis importantes
# MERGE - cod_familiar_fam [,3]
# Faixa de renda = fx_rfpc [, 23]


# Idade (nascimento) tá no PES. Tem que dar merge
# 0 - 17 e 65 < x -> 00 (puxa os dois e vê)

##############################################
##### FEITO
#Filtrando os municípios do banco pessoas
#ibge_munics = CADUNICO$cd_ibge %>% unique
#CADPES_ibge = CADPES$cd_ibge
#linhas = c()

#for (i in 1:nrow(CADPES)){
#  for (munic in ibge_munics){
#    if (i == munic){
#      linhas[i] = i
#    } else{
#      linhas[i] = NA
#   }
#  }
#}

#selec_index = function(num, x=CADPES_ibge){
#  retorna_index = function(a, b){
#    if (a == x[b]){
#      return(b)
#    }
#  }
#  lista_indexes = sapply(1:length(x), retorna_index, a=num)
#  lista_indexes = unlist(lista_indexes)
#  return(lista_indexes)
#}

#Teste
#selec_index(2, c(1,2,5,4,6,2,7,6,2,19,2))
#sapply(1:5, selec_index, x = x) %>% unlist

# Pra Valer!!!
#library(parallel)
#no_cores = detectCores()
#cl = makeCluster(no_cores)
#clusterExport(cl, c('ibge_munics', 'CADPES_ibge', 'selec_index'))

#linhas = parSapply(cl, ibge_munics, selec_index) %>% unlist

#stopCluster(cl)


#CADPES_selecao = CADPES[linhas,]

#write.csv2(CADPES_selecao, 'CADPES.csv', row.names = F)

CADPES <-  fread('CADPES.csv') %>% as.data.frame(.,stringsAsFactors=F)

################################################
################################################
################################################
################################################

###############################################
# Ação 2 - Entrega de KIT Alimento e trabalho
rm(reg)
gc()

selecao_acao2 = CADUNICO[CADUNICO$fx_rfpc != 1,]

selecao_acao2$ind_parc_mds_fam[selecao_acao2$ind_parc_mds_fam == 0] = 999

# Filtrando por se tem agua canalizada, depois cat de renda, 
# depois comunidades tradicionais
 
selecao_acao2 = arrange(selecao_acao2, cod_agua_canalizada_fam,
                        fx_rfpc, cod_familia_indigena_fam,
                        ind_familia_quilombola_fam,
                        ind_parc_mds_fam)

View(selecao_acao2)
amostra = selecao_acao2[1:90000,]
freq(amostra$ano_meta,plot=F)
View(amostra)

#write.table(amostra, 'selecionados_acao2.csv', sep=',', row.names = F, 
#            fileEncoding = 'UTF-8')
