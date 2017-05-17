# -*- coding: utf-8 -*-
"""
Created on Tue May 16 13:53:59 2017
@author: Neylson Crepalde
"""
import pandas as pd
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

for i in range(len(inscricoes)):
    if inscricoes['nomedoaluno'][i] == 'DAVID PAULO PEREIRA  SOARES':
        inscricoes['nomedoaluno'][i] = 'DAVID PAULO PEREIRA SOARES'
        
    if inscricoes['nomedoaluno'][i] == 'EDSON BARBOZA LOPES':
        inscricoes['nomedoaluno'][i] = 'EDSON BARBOSA LOPES'
        
    if inscricoes['nomedoaluno'][i] == 'ERICK ANTONIO SOARES DOS SANTOS':
        inscricoes['nomedoaluno'][i] = 'ERICK ANTONIO SOARES DOS SANTOS RODRIGUES'
       
    if inscricoes['nomedoaluno'][i] == 'GUILHERME RODRIGUES  F. PAIXAO':
        inscricoes['nomedoaluno'][i] = 'GUILHERME RODRIGUES DE FREITAS PAIXAO'
        
    if inscricoes['nomedoaluno'][i] == 'HELEN CRISTINA NUNES':
        inscricoes['nomedoaluno'][i] = 'HELEN CRISTINA NUNES ALVES'
    
    if inscricoes['nomedoaluno'][i] == 'HELIA CELESTE B. COSTA':
        inscricoes['nomedoaluno'][i] = 'HELIA CELESTE BARBOSA COSTA'
        
    if inscricoes['nomedoaluno'][i] == 'HELIA CELESTE  BARBOSA COSTA':
        inscricoes['nomedoaluno'][i] = 'HELIA CELESTE BARBOSA COSTA'
        
    if inscricoes['nomedoaluno'][i] == 'IRES KARINNE PEREIRA':
        inscricoes['nomedoaluno'][i] = 'IRIS KARINNE PEREIRA DAS GRACAS'
        
    if inscricoes['nomedoaluno'][i] == 'ITTALO FERREIRA GOMES':
        inscricoes['nomedoaluno'][i] = 'ITALO FERREIRA GOMES'
        
    if inscricoes['nomedoaluno'][i] == 'KAMILA CRISTIAN ANACLETO CARDOSO DE SOUZA':
        inscricoes['nomedoaluno'][i] = 'KAMILA CRISTINA ANACLETO CARDOSO DE SOUZA'
        
    if inscricoes['nomedoaluno'][i] == 'KARINE ANDREA TEIXEIRA':
        inscricoes['nomedoaluno'][i] = 'KARINE ANDREIA TEIXEIRA'
        
    if inscricoes['nomedoaluno'][i] == 'KELLY CRISTIANE VERGA DA SILVA':
        inscricoes['nomedoaluno'][i] = 'KELLY CRISTINE VERGA DA SILVA'
        
    if inscricoes['nomedoaluno'][i] == 'MARIA BEATRIZ DOS SANTOS PEREIRA':
        inscricoes['nomedoaluno'][i] = 'MARIA BEATRIZ PEREIRA DOS SANTOS'
        
    if inscricoes['nomedoaluno'][i] == 'NAYANE RAFAELA BARBOSA LAURINO':
        inscricoes['nomedoaluno'][i] = 'NAYARA RAFAELA BARBOSA LAURINDO'
        
    if inscricoes['nomedoaluno'][i] == 'POLLYENE  BATISTA SALUSTINO':
        inscricoes['nomedoaluno'][i] = 'POLLYENE BATISTA SALUSTINO'
        
    if inscricoes['nomedoaluno'][i] == 'ROBSON JUNIOR PERREIRA COSTA':
        inscricoes['nomedoaluno'][i] = 'ROBSON JUNIO PERREIRA COSTA'
        
    if inscricoes['nomedoaluno'][i] == 'THUANE APARECIDA FERNANDES SILVA BARROSO':
        inscricoes['nomedoaluno'][i] = 'THUANE APARECIDA FERNANDES DA SILVA BARROSO'

for i in range(len(encaminhamentos)):
    if encaminhamentos['nomedoaluno'][i] == 'DAVID PAULO PEREIRA  SOARES':
        encaminhamentos['nomedoaluno'][i] = 'DAVID PAULO PEREIRA SOARES'
        
    if encaminhamentos['nomedoaluno'][i] == 'EDSON BARBOZA LOPES':
        encaminhamentos['nomedoaluno'][i] = 'EDSON BARBOSA LOPES'
        
    if encaminhamentos['nomedoaluno'][i] == 'ERICK ANTONIO SOARES DOS SANTOS':
        encaminhamentos['nomedoaluno'][i] = 'ERICK ANTONIO SOARES DOS SANTOS RODRIGUES'
       
    if encaminhamentos['nomedoaluno'][i] == 'GUILHERME RODRIGUES  F. PAIXAO':
        encaminhamentos['nomedoaluno'][i] = 'GUILHERME RODRIGUES DE FREITAS PAIXAO'
        
    if encaminhamentos['nomedoaluno'][i] == 'HELEN CRISTINA NUNES':
        encaminhamentos['nomedoaluno'][i] = 'HELEN CRISTINA NUNES ALVES'
    
    if encaminhamentos['nomedoaluno'][i] == 'HELIA CELESTE B. COSTA':
        encaminhamentos['nomedoaluno'][i] = 'HELIA CELESTE BARBOSA COSTA'
        
    if encaminhamentos['nomedoaluno'][i] == 'HELIA CELESTE  BARBOSA COSTA':
        encaminhamentos['nomedoaluno'][i] = 'HELIA CELESTE BARBOSA COSTA'
        
    if encaminhamentos['nomedoaluno'][i] == 'IRES KARINNE PEREIRA':
        encaminhamentos['nomedoaluno'][i] = 'IRIS KARINNE PEREIRA DAS GRACAS'
        
    if encaminhamentos['nomedoaluno'][i] == 'ITTALO FERREIRA GOMES':
        encaminhamentos['nomedoaluno'][i] = 'ITALO FERREIRA GOMES'
        
    if encaminhamentos['nomedoaluno'][i] == 'KAMILA CRISTIAN ANACLETO CARDOSO DE SOUZA':
        encaminhamentos['nomedoaluno'][i] = 'KAMILA CRISTINA ANACLETO CARDOSO DE SOUZA'
        
    if encaminhamentos['nomedoaluno'][i] == 'KARINE ANDREA TEIXEIRA':
        encaminhamentos['nomedoaluno'][i] = 'KARINE ANDREIA TEIXEIRA'
        
    if encaminhamentos['nomedoaluno'][i] == 'KELLY CRISTIANE VERGA DA SILVA':
        encaminhamentos['nomedoaluno'][i] = 'KELLY CRISTINE VERGA DA SILVA'
        
    if encaminhamentos['nomedoaluno'][i] == 'MARIA BEATRIZ DOS SANTOS PEREIRA':
        encaminhamentos['nomedoaluno'][i] = 'MARIA BEATRIZ PEREIRA DOS SANTOS'
        
    if encaminhamentos['nomedoaluno'][i] == 'NAYANE RAFAELA BARBOSA LAURINO':
        encaminhamentos['nomedoaluno'][i] = 'NAYARA RAFAELA BARBOSA LAURINDO'
        
    if encaminhamentos['nomedoaluno'][i] == 'POLLYENE  BATISTA SALUSTINO':
        encaminhamentos['nomedoaluno'][i] = 'POLLYENE BATISTA SALUSTINO'
        
    if encaminhamentos['nomedoaluno'][i] == 'ROBSON JUNIOR PERREIRA COSTA':
        encaminhamentos['nomedoaluno'][i] = 'ROBSON JUNIO PERREIRA COSTA'
        
    if encaminhamentos['nomedoaluno'][i] == 'THUANE APARECIDA FERNANDES SILVA BARROSO':
        encaminhamentos['nomedoaluno'][i] = 'THUANE APARECIDA FERNANDES DA SILVA BARROSO'




#Fazendo o merge
dados = encaminhamentos.merge(inscricoes, how = 'outer', on = 'nomedoaluno')
dados['nomedoaluno']
dados.to_csv('inscricoes_merge.csv', index=False)

#Limpando
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

dados['Enc - Curso de Interesse:'].value_counts()
dados['Enc - Segunda opção:'].value_counts()
dados['Curso (nome)'].value_counts()
       
    

    
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

#Estabelecendo as vagas.
vagas_citrolandia = {'Prioritárias': {'Confeitaria':12,'Mecânico de Motocicletas':9,
                                 'Organização de Eventos':9,
                                 'Editor de projeto visual gráfico (Design Gráfico)':9,
                                 'Analista de Redes Sociais / Mídias Digitais':9,
                                 'Desenvolvedor de Aplicativos para Dispositivos Móveis':9,
                                 'Assistente de Produção Cultural':9},
                'Demanda Espontânea': {'Confeitaria':8,'Mecânico de Motocicletas':6,
                                 'Organização de Eventos':6,
                                 'Editor de projeto visual gráfico (Design Gráfico)':6,
                                 'Analista de Redes Sociais / Mídias Digitais':6,
                                 'Desenvolvedor de Aplicativos para Dispositivos Móveis':6,
                                 'Assistente de Produção Cultural':6}
                }
vagas_jardim = {'Prioritárias': {'Confeitaria':24,'Mecânico de Motocicletas':15,
                                 'Organização de Eventos':15,
                                 'Editor de projeto visual gráfico (Design Gráfico)':15,
                                 'Analista de Redes Sociais / Mídias Digitais':15,
                                 'Desenvolvedor de Aplicativos para Dispositivos Móveis':15,
                                 'Assistente de Produção Cultural':15},
                'Demanda Espontânea': {'Confeitaria':16,'Mecânico de Motocicletas':10,
                                 'Organização de Eventos':10,
                                 'Editor de projeto visual gráfico (Design Gráfico)':10,
                                 'Analista de Redes Sociais / Mídias Digitais':10,
                                 'Desenvolvedor de Aplicativos para Dispositivos Móveis':10,
                                 'Assistente de Produção Cultural':10}
                }

##############################################################################
# Criando as regras de alocação

dados['status'] = 'Não avaliado'

for i in range(len(dados['status'])):
    #Se for prioritário
    if i < 85:
        #faça estas coisas
        print('Prioritário')
    
    #se for Demanda Espontânea
    elif i >=85:
        #faça estas outras coisas
        print('Demanda Espontânea')
        
        
dados['Enc - Segunda opção:'][:85]



