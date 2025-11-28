# Arquivo master contento carregamento de pacotes e funções utilizadas no projeto

# Carregar pacotes
#install.packages("Rilostat")
library(tidyverse)
#library(WDI)        # não utilizado - dados baixados diretamente do repositório WDI
#library(Rilostat)   # não utilizado - dados baixados diretamente do repositório ILO
library(synthdid)
library(xsynthdid)
library(zoo)
library(ggthemes)
library(gtExtras)
library(naniar)

# Carregando funções para o fluxo de análise no ambiente
source("06.Functions.R")

# Carregando os dados
data_model <- load_data()


output_unr <- sdid_run(
  df = data_model,
  unit = "country",
  time = "year",
  outcome = "unr",
  treat = "treat",
  covariates = c("gdp", "inf_d", "exr_ref", "inr", "pos", "coc")
)

output_unr[[13]]
