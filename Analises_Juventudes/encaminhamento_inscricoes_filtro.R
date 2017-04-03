####################################
# Encaminhamento de Inscrições
# Oficina: CONFEITARIA
# Script: Neylson Crepalde e Marcus Oliveira
# ---RASTREIA---
####################################

library(magrittr)
library(dplyr)
library(tidyr)
library(lubridate)
library(descr)

setwd("C:/Users/x6905399/Documents/RASTREIA/JUVENTUDES/Oficinas")
arquivos <- list.files(pattern = '.csv')

# Agora falta programar o loop
# Para todos os arquivos na pasta tal, execute essas funções
# Aqui tem só para ORGANIZAÇÃO DE EVENTOS


dados <- read.csv("encaminhamento_organizacao_eventos.csv", stringsAsFactors = F, encoding = 'UTF-8')
names(dados) <- c('data_hora','municipio','regiao','instituicao','nome','telefone',
                  'email','posicao','nome1','nascimento','bairro','telefone1','email1',
                  'pub_prioritario','desc_risco_social','previsao_entrada','cursos_2opcao')
names(dados)
######################
# Limpando a posicao

class(dados$posicao)


dados2 <- dados %>% unnest(strsplit(pub_prioritario, ', '))
names(dados2)[18] <- 'pub_prioritario_separado'

dados2$nascimento_numeric <- dados2$nascimento %>% dmy %>% gsub('-','',.) %>% as.numeric

reorder <- dados2 %>% arrange(posicao,
                             pub_prioritario_separado == "Em cumprimento de alternativas penais",
                             pub_prioritario_separado == "Egressos(as) de medidas socioeducativas",
                             nascimento_numeric)

#View(reorder)

nomes_selecionados <- reorder$nome1

nomes_selecionados %<>% unique
nomes_2opcao <- reorder[,c(9,17)]

#write.csv(reorder, "oficinas_ordem_prioridade.csv", row.names = F, fileEncoding = "UTF-8")
#write.csv(nomes_selecionados, "nomes_selecionados.csv", row.names = F, fileEncoding = "UTF-8")
#write.csv(nomes_2opcao, "nomes_segunda_opcao.csv", row.names = F, fileEncoding = "UTF-8")






