install.packages("imfweo")
library(imfweo)
library(tidyverse)

entities <- imfweo::weo_get_entities()
series <- imfweo::weo_get_series()

imf_weo_data <- imfweo::weo_get()

imf_weo_data <- imf_weo_data |> dplyr::filter(series_id %in% c("LUR",
                                                               "NGDP_RPCH",
                                                               "PCPIEPCH")) 

inr <- readr::read_csv(
  file = "data/dataset_2025-11-17T14_13_07.621562498Z_DEFAULT_INTEGRATION_IMF.STA_MFS_IR_8.0.1.csv",
  col_names = TRUE
  )

exr <- readr::read_csv(
  file = "data/dataset_2025-11-17T14_18_04.083735609Z_DEFAULT_INTEGRATION_IMF.STA_ER_4.0.1.csv",
  col_names = TRUE
  )

inr <- inr |> 
  rename(entity_name = COUNTRY,
         series_name = INDICATOR,
         units = SCALE,
         year = TIME_PERIOD,
         value = OBS_VALUE)

exr <- exr |> 
  rename(entity_name = COUNTRY,
         series_name = INDICATOR,
         units = SCALE,
         year = TIME_PERIOD,
         value = OBS_VALUE)

raw_data <- imf_weo_data |>
  dplyr::bind_rows(inr,exr) |> 
  dplyr::select(-c(FREQUENCY, TYPE_OF_TRANSFORMATION,entity_id, series_id, units))

raw_data <- raw_data |>
  tidyr::pivot_wider(
    names_from = "series_name",
    values_from = "value"
  ) |> 
  dplyr::rename(
    unr = `Unemployment rate`,
    gdp = `Gross domestic product, constant prices`,
    cpi = `Inflation, end of period consumer prices`,
    inr = `Monetary policy-related, Rate, Percent per annum`,
    exr = `Domestic currency per US Dollar`)

selected_countries <- c("Chile", "Colombia", 
                        "Dominican Republic", "Mexico", "Brazil")

raw_data <- raw_data |> 
  filter(entity_name %in% selected_countries,
         year %in% c(2003:2024))

raw_data |> dplyr::group_by(entity_name) |> 
  dplyr::summarise(
    dplyr::across(dplyr::everything(),
                  ~sum(is.na(.)))
  )

# preenchendo os NAs
raw_data <- raw_data |>
  fill(inr, .direction = "up") 

raw_data <- raw_data |> 
  mutate(
    d_gdp = gdp - dplyr::lag(gdp),
    d_cpi = cpi - dplyr::lag(cpi),
    d_unr = unr - dplyr::lag(unr),
    v_exr = exr/dplyr::lag(exr)-1,
    v_inr = inr/dplyr::lag(inr)-1,
  )

readr::write_csv(raw_data, 
                 file = "data/raw_data_imf.csv")

lac_indicators <- readr::read_csv(file = "data/raw_data_imf.csv") |> 
  dplyr::rename(country = entity_name) |> 
  dplyr::mutate(treat = ifelse(country == "Brazil" & year >= 2018, 1, 0)) |> 
  dplyr::filter(year != 2003)


