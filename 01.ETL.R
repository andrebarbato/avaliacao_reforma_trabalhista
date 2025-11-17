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
                #year %in% c(2003:2020),
                country %in% selected_countries)

# tratamento das variáveis para o modelo
lac_indicators <- lac_indicators |> 
  mutate(
    d_gdp = gdp - dplyr::lag(gdp),
    d_cpi = cpi - dplyr::lag(cpi),
    d_unr = unr - dplyr::lag(unr),
    d_coc = coc - dplyr::lag(coc),
    d_pos = pos - dplyr::lag(pos),
    v_exr = exr/dplyr::lag(exr)-1,
    v_inr = inr/dplyr::lag(inr)-1,
  )

# filtro do período
lac_indicators <- lac_indicators |> 
  filter(year %in% c(2003:2023))

# Checando NAs por país
lac_indicators |>
  dplyr::select(country, d_gdp, d_cpi, v_exr, v_inr, d_unr, d_coc, d_pos) |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(
    dplyr::across(dplyr::everything(),
                  ~sum(is.na(.)))
    )

# preenchendo os NAs
lac_indicators <- lac_indicators |>
  fill(d_gdp, .direction = "up") |> 
  fill(d_cpi, .direction = "up") |> 
  fill(v_exr, .direction = "up") |> 
  fill(v_inr, .direction = "up") |> 
  fill(d_unr, .direction = "up") |> 
  fill(d_coc, .direction = "up") |> 
  fill(d_pos, .direction = "up") 
  
# Adicionando uma caluna de tratamento para os anos após a reforma
lac_indicators <- lac_indicators |> 
  dplyr::mutate(treat = ifelse(country == "Brazil" & year >= 2018, 1, 0))  
