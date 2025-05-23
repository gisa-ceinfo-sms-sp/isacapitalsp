###CODIGO BASE PARA APLICAÇÂO DO PLANO DE ANÁLISE UTILIZANDO AMOSTRA COMPLEXA, COM OS DOMÍNIOS DEFINIDOS NO ISA CAPITAL 2023 - SMS-SP###

#install.packages("haven")   # Para ler arquivos .sav
#install.packages("survey")  # O pacote survey realiza a análise de amostras complexas
#install.packages("dplyr")   #biblioteca de funções gerais como o uso de %>%

library(haven)
library(survey)
library(dplyr)

#define a pasta onde estão os arquivos (Subsitua o caminho abaixo "c:/ISA_Capital" pelo local onde o arquivo foi armazenado em sua máquina)
setwd("c:/ISA_Capital") 

#ler o arquivo de dados no formato do SPSS ".SAV"
dados <- read_sav("ISACapital17042025TreinamentoComPesosExcluiVar.SAV")

#exclui os registros com o peso final em branco
dados2 <- dados %>%
  filter(!is.na(pesofinal)) 

#define o desenho do estudo
des <- svydesign(
  id = ~cod_setor,
  strata = ~cod_coord,
  weights = ~pesofinal,
  data = dados2,
  nest = TRUE
)

#faz a tabulação da variável sexo
svytable(~b08_sexo, design = des)

#faça a tabulação de outras variáveis para testar a funcionalidade

