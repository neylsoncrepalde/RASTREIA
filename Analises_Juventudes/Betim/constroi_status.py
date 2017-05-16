# -*- coding: utf-8 -*-
"""
Created on Tue May 16 13:53:59 2017
@author: Neylson Crepalde
"""
import pandas as pd
import csv
import os

#bd = input()

os.chdir("C:/Users/x6905399/Documents/RASTREIA/JUVENTUDES/Betim")
os.listdir()

inscricoes = pd.read_csv("inscricoes_betim.csv", encoding = "utf=8")
encaminhamentos = pd.read_csv("encaminhamentos_betim.csv", encoding = "utf=8")

inscricoes.columns[6]
encaminhamentos.columns[8]
