# Cruzando inscrições e encaminhamentos
# JUVENTUDES - BETIM
# Script: Neylson Crepalde
######################################

library(data.table)
library(magrittr)
library(descr)
library(dplyr)
library(lubridate)

setwd("C:/Users/x6905399/Documents/RASTREIA/JUVENTUDES/Betim")
list.files()

#inscricoes = fread("Tabulação das Fichas de Inscrição (respostas) - Respostas ao formulário 1.csv",
#                   encoding = "UTF-8") %>% as.data.frame(., stringsAsFactors = F)
#encaminhamentos = fread("Formulário para encaminhamento aos cursos - Betim (respostas) - Respostas ao formulário 1.csv",
#                   encoding = "UTF-8") %>% as.data.frame(., stringsAsFactors = F)

#write.csv(inscricoes, "inscricoes_betim.csv", row.names = F, fileEncoding = "UTF-8")
#write.csv(encaminhamentos, "encaminhamentos_betim.csv", row.names = F, fileEncoding = "UTF-8")

# Lendo o bancão
dados = fread('inscricoes_merge.csv')
View(dados)

freq(dados$`Enc - Curso de Interesse:`)
freq(dados$`Enc - Segunda opção:`)
freq(dados$`Curso (nome)`)
freq(dados$`Enc - Região Intramunicipal`)
freq(dados$`Bairro / Vila`)


which(dados$`Enc - Curso de Interesse:` == dados$`Curso (nome)`)
which(dados$`Enc - Segunda opção:` == dados$`Curso (nome)`)

########## Demanda:
## Rankear as inscrições respeitando os critérios de prioridade estabelecidos
## e as vagas para cada curso

dados$socioeducativa = ifelse(dados$`Enc - Caracterização da(o) jovem nos públicos prioritários: [Em cumprimento de medidas socioeducativas em meio aberto]` == 'Sim', 1, 0)
dados$egresso = ifelse(dados$`Enc - Caracterização da(o) jovem nos públicos prioritários: [Egressos(as) de medidas socioeducativas]` == 'Sim', 1, 0)
dados$nasc.enc = dados$`Enc - Data de Nascimento:` %>% dmy
dados$nasc.insc = dados$`Data de Nascimento` %>% dmy

ranking <- dados %>% arrange(`Enc - Posição na ordem de prioridade:`,
                             desc(socioeducativa), desc(egresso), nasc.enc,
                             nasc.insc)


####################
# Dividindo em regioes e cursos

citrolandia <- ranking[ranking$`Enc - Região Intramunicipal` == "Citrolândia" |
                       ranking$`Enc - Região Intramunicipal` == "",]

teresopolis <- ranking[ranking$`Enc - Região Intramunicipal` == "Jardim Teresópolis e Vila Recreio" |
                         ranking$`Enc - Região Intramunicipal` == "",]

citrolandia <- citrolandia[citrolandia$`Indicação de data e hora` != "",]
teresopolis <- teresopolis[teresopolis$`Indicação de data e hora` != "",]

#Verificando os atendidos entre os prioritários
citrolandia[citrolandia$`Enc - Posição na ordem de prioridade:` != "",] %>% group_by(`Curso (nome)`) %>% count(`Curso (nome)`)
teresopolis[teresopolis$`Enc - Posição na ordem de prioridade:` != "",] %>% group_by(`Curso (nome)`) %>% count(`Curso (nome)`)

### Todos os prioritários foram contemplados!

#Verificando os atendidos entre a demanda espontânea
citrolandia[is.na(citrolandia$`Enc - Posição na ordem de prioridade:`) == T,] %>% group_by(`Curso (nome)`) %>% count(`Curso (nome)`)
teresopolis[is.na(teresopolis$`Enc - Posição na ordem de prioridade:`) == T,] %>% group_by(`Curso (nome)`) %>% count(`Curso (nome)`)


#### Exportando os bancos de dados prontos
cursos = c("Analista de Redes Sociais / Mídias Digitais",
           "Assistente de Produção Cultural",
           "Confeitaria",
           "Desenvolvedor de Aplicativos para Dispositivos Móveis",
           "Editor de projeto visual gráfico (Design Gráfico)",
           "Mecânico de Motocicletas",
           "Organização de Eventos")

for (curso in cursos){
  cat(paste0("citrolandia_", gsub(" ","_",curso), "\n"))
}

citrolandia_Analista_de_Redes_Sociais_Mídias_Digitais = data.frame()
citrolandia_Assistente_de_Produção_Cultural = data.frame()
citrolandia_Confeitaria = data.frame()
citrolandia_Desenvolvedor_de_Aplicativos_para_Dispositivos_Móveis = data.frame()
citrolandia_Editor_de_projeto_visual_gráfico_Design_Gráfico = data.frame()
citrolandia_Mecânico_de_Motocicletas = data.frame()
citrolandia_Organização_de_Eventos = data.frame()

teresopolis_Analista_de_Redes_Sociais_Mídias_Digitais = data.frame()
teresopolis_Assistente_de_Produção_Cultural = data.frame()
teresopolis_Confeitaria = data.frame()
teresopolis_Desenvolvedor_de_Aplicativos_para_Dispositivos_Móveis = data.frame()
teresopolis_Editor_de_projeto_visual_gráfico_Design_Gráfico = data.frame()
teresopolis_Mecânico_de_Motocicletas = data.frame()
teresopolis_Organização_de_Eventos = data.frame()

lista_citrolandia = list(citrolandia_Analista_de_Redes_Sociais_Mídias_Digitais,
                         citrolandia_Assistente_de_Produção_Cultural,
                         citrolandia_Confeitaria,
                         citrolandia_Desenvolvedor_de_Aplicativos_para_Dispositivos_Móveis,
                         citrolandia_Editor_de_projeto_visual_gráfico_Design_Gráfico,
                         citrolandia_Mecânico_de_Motocicletas,
                         citrolandia_Organização_de_Eventos)

lista_teresopolis = list(teresopolis_Analista_de_Redes_Sociais_Mídias_Digitais,
                         teresopolis_Assistente_de_Produção_Cultural,
                         teresopolis_Confeitaria,
                         teresopolis_Desenvolvedor_de_Aplicativos_para_Dispositivos_Móveis,
                         teresopolis_Editor_de_projeto_visual_gráfico_Design_Gráfico,
                         teresopolis_Mecânico_de_Motocicletas,
                         teresopolis_Organização_de_Eventos)


for (i in 1:length(cursos)){
  lista_citrolandia[[i]] = citrolandia[citrolandia$`Curso (nome)` == cursos[i],]
  lista_teresopolis[[i]] = teresopolis[teresopolis$`Curso (nome)` == cursos[i],]
}

nomes_arquivos = c("analista_midias","assis_prod_cult","confeitaria","desenvolvedor",
                   "design","mecanico_motocicletas","organiz_eventos")

for (i in 1:length(cursos)){
  write.csv2(lista_citrolandia[[i]], file=paste0("citrolandia_", nomes_arquivos[i], ".csv"), row.names = F, fileEncoding = "UTF-8")
  write.csv2(lista_teresopolis[[i]], file=paste0("teresopolis_", nomes_arquivos[i], ".csv"), row.names = F, fileEncoding = "UTF-8")
}







