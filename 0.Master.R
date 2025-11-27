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

teste <- sdid_sc(tau.hat) |> 
  dplyr::select(x, y, color) |> 
  tidyr::pivot_wider(
    values_from = y,
    names_from = color
  ) |> 
  dplyr::rename(
    sc = `synthetic control`
  ) |> 
  dplyr::mutate(
    dif = treated - sc,
    bottom = dif - (1.96 * sem),
    upper = dif + (1.96 * sem)
  ) 


x <- mean(teste$dif)
sd <- sd(teste$dif)
n <- length(teste$dif)

sem <- sd / sqrt(n)

z <- qnorm(0.975)

# Intervalo de confiança
ic_inf <- x - z * sem
ic_sup <- x + z * sem

max <- teste$upper |> max()
min <- teste$bottom |> min()
teste |> 
  ggplot(aes(x = x, y = dif)) +
  geom_line(size = 0.75) +
  geom_ribbon(aes(ymin = bottom, ymax = upper), fill = "red", alpha = 0.2) +
  ylim(min*1.5, max*1.5) +  # Substitua -max e max pelos seus valores desejados
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Gap: Tratado - Controle sintético",
    x = "Anos",
    y = "Diferença na taxa de desemprego"
  )

