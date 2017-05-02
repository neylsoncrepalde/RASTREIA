#############################
### JUVENTUDES
### Encaminhamento aos cursos
### script: Neylson Crepalde
#############################

library(magrittr)
library(dplyr)
library(descr)

setwd("C:/Users/x6905399/Documents/RASTREIA/JUVENTUDES/Oficinas")

dados <- read.csv2("encaminhamento_aos_cursos.csv", stringsAsFactors = F)
#View(dados)
##########################################

### Inserindo os filtros
# Filtrando por posição na ordem de prioridade, em cumprimento de medida socioeducativa,
# egresso de medida socioeducativa e idade

filtrada <- dados %>% arrange(Posição.na.ordem.prioridade,
                              desc(factor(Jovem.está.em.cumprimento.de.medida.)),
                              desc(factor(Egressos.as..de.medidas.socioeducativas)),
                              desc(Idade.em.17.04.2017..Anos.))
View(filtrada)
