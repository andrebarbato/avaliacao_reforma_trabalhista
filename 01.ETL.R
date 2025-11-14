# Arquivo de ETL dos dados utilizados no projeto

# baixando indicadores do World Development Indicators - World Bank (WDI) ------

wdi_data <- WDI::WDI_data

wdi_countries <- wdi_data$country

# vetor de indicadores: PIB, Inflação, Tx. Câmbio, Tx. Juros Real,
# Tx. Desemprego, indicadores de governança de corrupção e estabilidade Política e 
# violência e terrorismo
indicators <- c("NY.GDP.MKTP.KD.ZG", "FP.CPI.TOTL.ZG",
                "PA.NUS.FCRF", "FR.INR.RINR", "SL.UEM.TOTL.NE.ZS",
                "CC.EST", "PV.EST")

# baixando os indicadores do WDI
raw_indicators <- WDI::WDI(country = "all", 
                           indicator = indicators) |> 
  tidyr::as_tibble() |> 
  dplyr::rename(gdp = "NY.GDP.MKTP.KD.ZG",
                cpi = "FP.CPI.TOTL.ZG",
                exr = "PA.NUS.FCRF",
                inr = "FR.INR.RINR",
                unr = "SL.UEM.TOTL.NE.ZS",
                coc = "CC.EST",
                pos = "PV.EST")

# trazendo informações dos países
raw_indicators <- raw_indicators |> 
  dplyr::left_join(wdi_countries, by = "iso3c") |> 
  dplyr::select(-c("iso2c.y", "country.y")) |> 
  dplyr::rename(country = country.x,
                iso2c = iso2c.x) 

# Salvando os dados brutos
readr::write_csv(x = raw_indicators,
                 file = "data/raw_indicators.csv")

# Lendo o arquivo de dados brutos
raw_indicators <- readr::read_csv(file = "data/raw_indicators.csv", 
                                  col_names = TRUE)

selected_countries <- c("Bolivia", "Chile", "Colombia", 
                        "Dominican Republic", "Mexico", "Nicaragua",
                        "Trinidad and Tobago", "Brazil")
  
# filtrando países da américa latina e caribe
lac_indicators <- raw_indicators |> 
  dplyr::filter(region == "Latin America & Caribbean",
                year %in% c(2003:2020),
                country %in% selected_countries)

# Checando NAs por país
lac_indicators |>
  dplyr::select(country, gdp, cpi, exr, inr, unr, coc, pos) |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(
    dplyr::across(dplyr::everything(),
                  ~sum(is.na(.)))
    )

# preenchendo os NAs
lac_indicators <- lac_indicators |>
  fill(gdp, .direction = "up") |> 
  fill(cpi, .direction = "up") |> 
  fill(exr, .direction = "up") |> 
  fill(inr, .direction = "up") |> 
  fill(unr, .direction = "up") |> 
  fill(coc, .direction = "up") |> 
  fill(pos, .direction = "up")
  
# Adicionando uma caluna de tratamento para os anos após a reforma
lac_indicators <- lac_indicators |> 
  dplyr::mutate(treat = ifelse(country == "Brazil" & year >= 2018, 1, 0))  
