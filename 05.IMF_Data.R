install.packages("imfweo")
library(imfweo)
library(tidyverse)

entities <- imfweo::weo_get_entities()
series <- imfweo::weo_get_series()

imf_weo_data <- imfweo::weo_get()

imf_weo_data |> dplyr::filter(series_id == c("LUR", "NGDP_RPCH","PCPIEPCH"),
                     year %in% c(1980:2024)) 
