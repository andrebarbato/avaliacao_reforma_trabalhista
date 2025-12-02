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
library(wesanderson)

# Carregando funções para o fluxo de análise no ambiente
source("06.Functions.R")

# Carregando os dados
data_model <- load_data(
  file = "data/dados.RData",
  range = c(2003:2024)
)


output17 <- sdid_run(
  df = data_model |> filter(!c(country %in% c("Guyana", "St. Lucia", 
                                            "St. Vincent and the Grenadines",
                                            "Trinidad and Tobago"))),
  unit = "country",
  time = "year",
  outcome = "unr",
  treat = "treat",
  covariates = c("inf_d", "v_exr", "inr", "pos", "coc", "upop", "gdp", "gdppc", "labf", "gcf", "fdini")
)

output17[[14]] |> unlist()
