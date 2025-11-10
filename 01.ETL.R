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

# baixando indicadores do World Governance Indicators - World Bank (WGI) ------

coc <- WDI::WDI(country = "all", 
                indicator = "CC.EST") |>
  tidyr::as_tibble()

psv <- WDI::WDI(country = "all", 
                indicator = "PV.EST") |>
  tidyr::as_tibble()

# baixando indicadores do International Labour Organization (ILO) ------

wdi_data <- WDI::WDI_data

wdi_countries <- wdi_data$country |> 
  filter(region != "Aggregates")


wdi_aggregates <- wdi_countries |> 
  filter(region == "Aggregates")

ec <- wdi_countries |> 
  filter(capital == "")
