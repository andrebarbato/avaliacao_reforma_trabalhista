# Arquivo de ETL dos dados utilizados no projeto

# baixando indicadores do World Development Indicators - World Bank (WDI) ------

# Taxa de crescimento do PIB anual
gdp <- WDI::WDI(country = "all", 
                indicator = "NY.GDP.MKTP.KD.ZG") |>
  tidyr::as_tibble()

# Taxa de inflação anual
icp <- WDI::WDI(country = "all", 
                indicator = "FP.CPI.TOTL.ZG") |>
  tidyr::as_tibble()

# Taxa de câmbio
oer <- WDI::WDI(country = "all", 
                indicator = "PA.NUS.FCRF") |>
  tidyr::as_tibble()

# Taxa de juros real
rir <- WDI::WDI(country = "all", 
                indicator = "FR.INR.RINR") |>
  tidyr::as_tibble()

# Taxa de desemprego
unr <- WDI::WDI(country = "all", 
                indicator = "SL.UEM.TOTL.NE.ZS") |>
  tidyr::as_tibble()

# baixando indicadores do International Labour Organization (ILO) ------

