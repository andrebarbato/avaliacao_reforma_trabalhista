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
                           indicator = "SL.UEM.TOTL.NE.ZS") |> 
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
                        "Trinidad and Tobago", "Brazil", "Bahamas, The",
                        "St. Lucia", "St. Vincent and the Grenadines", "Guyana")
  
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
  dplyr::mutate(treat = ifelse(country == "Brazil" & year >= 2017, 1, 0))  

# Como segunda opção foram baixados os dados diretamente do site do WDI --------
# https://databank.worldbank.org/source/world-development-indicators

# carregando os dados baixados do WDI
raw_data_wdi <- readr::read_csv(
  file = "data/c0acf3f2-dc67-4a86-86c4-66b97f34d58f_Data.csv"
)

raw_data_wdi <- raw_data_wdi[1:224770,]

lit_selected_countries <- c("Bolivia", "Chile", "Colombia",
                            "Dominican Republic", "Mexico", "Nicaragua",
                            "Trinidad and Tobago", "Brazil", "Bahamas, The",
                            "St. Lucia", "St. Vincent and the Grenadines", 
                            "Guyana")

# carregando os dados baixados do ILO
raw_data_ilo_informality <- readr::read_csv(
  file = "data/EMP_NIFL_SEX_RT_A-20251121T2048.csv"
)

raw_data_ilo_earnings <- readr::read_csv(
  file = "data/EAR_4MTH_SEX_CUR_NB_A-20251122T0306.csv"
)

# formatando as bases do ILO para depois combinar com os dados do WDI
informality <- raw_data_ilo_informality |> 
  dplyr::filter(sex.label != "Other") |> 
  dplyr::mutate(
    `Series Name` = paste0(indicator.label, ": ", sex.label)
  ) |> 
  dplyr::rename(
    `Country Name` = ref_area.label,
    Time = time,
    Value = obs_value
  ) |> 
  dplyr::select(c(`Country Name`, `Series Name`, Time, Value))

earnings <- raw_data_ilo_earnings |>
  dplyr::filter( classif1.label == "Currency: 2021 PPP $",
                 sex.label != "Other") |> 
  dplyr::mutate(
    `Series Name` = paste0(indicator.label, ": ", sex.label)
  ) |> 
  dplyr::rename(
    `Country Name` = ref_area.label,
    Time = time,
    Value = obs_value
  ) |> 
  dplyr::select(c(`Country Name`, `Series Name`, Time, Value))

# combinando as bases do ILO
data_ilo <- dplyr::bind_rows(earnings, informality)

# Combinando bases WDI e ILO
data_sdid <- dplyr::bind_rows(data_ilo, raw_data_wdi)

# tratamento dos dados combinados
data_sdid <- data_sdid |> 
  dplyr::filter(`Country Name` %in% lit_selected_countries) |> 
  dplyr::select(-c(`Country Code`, `Series Code`,`Time Code`)) |> 
  tidyr::pivot_wider(
    names_from = `Series Name`,
    values_from = Value
  ) |> 
  dplyr::rename(
    country = `Country Name`,
    year = Time,
    unr = `Unemployment, total (% of total labor force) (modeled ILO estimate)`,
    unr_f = `Unemployment, female (% of female labor force) (modeled ILO estimate)`,
    unr_m = `Unemployment, male (% of male labor force) (modeled ILO estimate)`,
    gdp = `GDP growth (annual %)`,
    cpi = `Inflation, consumer prices (annual %)`,
    exr = `Official exchange rate (LCU per US$, period average)`,
    exr_ref = `Real effective exchange rate index (2010 = 100)`,
    inr = `Real interest rate (%)`,
    upop = `Urban population growth (annual %)`,
    upop_p = `Urban population (% of total population)`,
    pos = `Political Stability and Absence of Violence/Terrorism: Estimate`,
    coc = `Control of Corruption: Estimate`,
    gini = `Gini index`,
    earn_t = `Average monthly earnings of employees by sex: Total`,
    earn_m = `Average monthly earnings of employees by sex: Male`,
    earn_f = `Average monthly earnings of employees by sex: Female`,
    iel_t = `Informal employment rate by sex (%): Total`,
    iel_m = `Informal employment rate by sex (%): Male`,
    iel_f = `Informal employment rate by sex (%): Female`
    ) |> 
  dplyr::arrange(country,year)

data_sdid <- data_sdid |> 
  dplyr::mutate(treat = ifelse(country == "Brazil" & year >= 2018, 1, 0)) |> 
  dplyr::filter(year %in% c(1996:2024))

# Checando NAs por país
data_sdid |>
  dplyr::select(country, unr, unr_f, unr_m, gini,
                gdp, cpi, exr, exr_ref, inr, upop, upop_p,
                coc, pos) |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(
    dplyr::across(dplyr::everything(),
                  ~sum(is.na(.)))
  )

# Tratamento das variáveis NAs
# Preenchendo NAs por interpolação
data_sdid_clean <- data_sdid |> 
  group_by(country) |> 
  mutate(
    across(everything(),
           ~na.approx(.x, na.rm = FALSE, maxgap = 3))
  )


# Preencher NAs para baixo e para cima
data_sdid_clean <- data_sdid_clean |> 
  group_by(country) |> 
  fill(earn_t, .direction = "downup") |>
  fill(earn_m, .direction = "downup") |>
  fill(earn_f, .direction = "downup") |>
  fill(iel_t, .direction = "downup") |>
  fill(iel_m, .direction = "downup") |>
  fill(iel_f, .direction = "downup") |>
  fill(unr , .direction = "downup") |>
  fill(unr_f , .direction = "downup") |> 
  fill(unr_m , .direction = "downup") |> 
  fill(gdp , .direction = "downup") |> 
  fill(cpi, .direction = "downup") |> 
  fill(exr, .direction = "downup") |> 
  fill(exr_ref, .direction = "downup") |> 
  fill(inr, .direction = "downup") |> 
  fill(upop, .direction = "downup") |> 
  fill(upop_p, .direction = "downup") |> 
  fill(coc, .direction = "downup") |> 
  fill(pos, .direction = "downup") |> 
  fill(gini, .direction = "downup")

data_sdid_clean |>
  # dplyr::select(country, unr, unr_f, unr_m, gini,
  #               gdp, cpi, exr, exr_ref, inr, upop, upop_p,
  #               coc, pos) |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(
    dplyr::across(dplyr::everything(),
                  ~sum(is.na(.)))
  )

