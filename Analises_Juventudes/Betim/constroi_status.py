# -*- coding: utf-8 -*-
"""
Created on Tue May 16 13:53:59 2017
@author: Neylson Crepalde
"""
import pandas as pd
import csv
import os
from unicodedata import normalize

def remover_acentos(txt):
    return normalize('NFKD', txt).encode('ASCII','ignore').decode('ASCII')

#bd = input()

os.chdir("C:/Users/x6905399/Documents/RASTREIA/JUVENTUDES/Betim")
os.listdir()

inscricoes = pd.read_csv("inscricoes_betim.csv", encoding = "utf=8")
encaminhamentos = pd.read_csv("encaminhamentos_betim.csv", encoding = "utf=8")

#Verificando as variáveis de interesse para o MERGE
inscricoes['Nome do Educando (Jovem)']
encaminhamentos['Nome:.1']

inscricoes['nomedoaluno'] = inscricoes['Nome do Educando (Jovem)']
encaminhamentos['nomedoaluno'] = encaminhamentos['Nome:.1']

#Colocando tudo em maiúsculo
for i in range(len(inscricoes)):
    inscricoes['nomedoaluno'][i] = inscricoes['nomedoaluno'][i].upper()

for i in range(len(encaminhamentos)):
    encaminhamentos['nomedoaluno'][i] = encaminhamentos['nomedoaluno'][i].upper()
    
#Retirando acentos
for i in range(len(inscricoes)):
    inscricoes['nomedoaluno'][i] = remover_acentos(inscricoes['nomedoaluno'][i])

for i in range(len(encaminhamentos)):
    encaminhamentos['nomedoaluno'][i] = remover_acentos(encaminhamentos['nomedoaluno'][i])

# Identificando variáveis dos encaminhamentos    
encaminhamentos.columns = 'Enc - ' + encaminhamentos.columns
encaminhamentos.rename(columns = {'Enc - nomedoaluno': 'nomedoaluno'}, inplace = True)

#Fazendo o merge
dados = encaminhamentos.merge(inscricoes, how = 'outer', on = 'nomedoaluno')
dados['nomedoaluno']
dados.to_csv('inscricoes_merge.csv', index=False)

#Limpando
dados['Enc - Curso de Interesse:'].value_counts()
dados['Enc - Segunda opção:'].value_counts()
dados['Curso (nome)'].value_counts()


for i in range(len(dados)):
    if dados['Enc - Segunda opção:'][i] == 'Confeiteiro':
       dados['Enc - Segunda opção:'][i] = 'Confeitaria'
    
    if dados['Enc - Segunda opção:'][i] == 'Analista de Redes Sociais/Mídias Digitais':
       dados['Enc - Segunda opção:'][i] = 'Analista de Redes Sociais / Mídias Digitais'
    
    if dados['Enc - Curso de Interesse:'][i] == 'Editor de Projeto Visual Gráfico':
       dados['Enc - Curso de Interesse:'][i] = 'Editor de projeto visual gráfico (Design Gráfico)'
    
    if dados['Curso (nome)'][i] == 'Mecânica de Motocicletas':
       dados['Curso (nome)'][i] = 'Mecânico de Motocicletas'

    if dados['Curso (nome)'][i] == 'Editor de Projeto Visual Gráfico (Design Gráfico)':
       dados['Curso (nome)'][i] = 'Editor de projeto visual gráfico (Design Gráfico)'
    
    if dados['Curso (nome)'][i] == 'Analista de Redes Sociais':
       dados['Curso (nome)'][i] = 'Analista de Redes Sociais / Mídias Digitais'

    if dados['Curso (nome)'][i] == 'Organização de eventos':
       dados['Curso (nome)'][i] = 'Organização de Eventos'

    if dados['Curso (nome)'][i] == 'Desenvolvedor de aplicativos móveis':
       dados['Curso (nome)'][i] = 'Desenvolvedor de Aplicativos para Dispositivos Móveis'

    if dados['Curso (nome)'][i] == 'Analista de redes sociais':
       dados['Curso (nome)'][i] = 'Analista de Redes Sociais / Mídias Digitais'

    if dados['Curso (nome)'][i] == 'Assistente de produção cultural':
       dados['Curso (nome)'][i] = 'Assistente de Produção Cultural'
       
#Verificando iguais
atendidos_1opcao = []

for i in range(len(dados)):
    if dados['Enc - Curso de Interesse:'][i] == dados['Curso (nome)'][i]:
        atendidos_1opcao.append(i)
print(len(atendidos_1opcao))
print(atendidos_1opcao)

atendidos_2opcao = []

for i in range(len(dados)):
    if dados['Enc - Segunda opção:'][i] == dados['Curso (nome)'][i]:
        atendidos_2opcao.append(i)
print(len(atendidos_2opcao))
print(atendidos_2opcao)









