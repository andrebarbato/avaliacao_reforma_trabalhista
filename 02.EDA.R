library(wesanderson)

my_palette <- wesanderson::wes_palette("Darjeeling1", n = 12, type = "continuous")

# Séries de taxa de desemprego
f1 <- data_sdid |>
  #filter(country == "Brazil") |> 
  mutate(isBrazil = (country == "Brazil")) |> 
  ggplot2::ggplot(aes(x = year, y = unr, color = country)) +
  ggplot2::geom_line(aes(linetype = isBrazil), size = 0.75, alpha = 0.7) +
  ggplot2::labs(
    x = "Ano",
    y = "Taxa de desemprego (estimativa OIT)",
    color = ""
  ) +
  ggthemes::theme_fivethirtyeight() +
  ggplot2::theme(axis.title = element_text(),
                 plot.background = element_blank(),
                 panel.background = element_blank(),
                 legend.background = element_blank(),
                 panel.grid.major = element_line(color = "lightgray", 
                                                 size = 0.05),
                 panel.grid.minor = element_line(color = "lightgray", 
                                                 size = 0.05)) +
  ggplot2::scale_linetype_manual(values = c("dashed", "solid"), 
                                 guide = "none") + 
  ggplot2::scale_color_manual(values = my_palette)

ggplot2::ggsave(
  "figs/f1.png",
  f1,
  width = 8.5,
  height = 5.5
)

# Reproduzindo a Figura 1 do artigo # Reproduzindo a Figura 1 do artigo # Reproduzindo a Figura 1 do artigo # Reproduzindo a Figura 1 do artigo # Reproduzindo a Figura 1 do artigo base: 
# Evolução do indicador de controle de corrupção para o Brasil

fig1 <- lac_indicators |> 
  filter(country == "Brazil") |>
  select(year, coc) |> 
  ggplot2::ggplot(aes(x = year, y = coc)) + 
  geom_line(size = 0.75) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  labs(x = "Ano",
       y = "Indicador de controle de corrupção") +
  scale_x_continuous(breaks = seq(min(lac_indicators$year), 
                                  max(lac_indicators$year), by = 2)) +
  ggthemes::theme_stata()

ggplot2::ggsave("figs/fig1.png",
                fig1,
                width = 8.5,
                height = 5.5)

# Reproduzindo a Figura 2 do artigo base: 
# Evolução do indicador de estabilidade política para o Brasil 

fig2 <- lac_indicators |> 
  filter(country == "Brazil") |>
  select(year, pos) |> 
  ggplot2::ggplot(aes(x = year, y = pos)) + 
  geom_line(size = 0.75) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  labs(x = "Ano",
       y = "Indicador de estabilidade política") +
  scale_x_continuous(breaks = seq(min(lac_indicators$year), 
                                  max(lac_indicators$year), by = 2)) +
  ggthemes::theme_stata()

ggplot2::ggsave("figs/fig2.png",
                fig2,
                width = 8.5,
                height = 5.5)

## Taxa de desemprego do Brasil
raw_data_wdi |> 
  dplyr::filter(country == "Trinidad and Tobago") |> 
  dplyr::select(year, unr) |> 
  ggplot2::ggplot(
    aes(x = year, y = unr)
  ) +
  geom_line()

## GDP do Brasil
lac_indicators |> 
  dplyr::filter(country == "Brazil") |> 
  dplyr::select(year, gdp) |> 
  ggplot2::ggplot(
    aes(x = year, y = gdp)
  ) +
  geom_line()

## inflação do Brasil
lac_indicators |> 
  dplyr::filter(country == "Brazil") |> 
  dplyr::select(year, cpi) |> 
  ggplot2::ggplot(
    aes(x = year, y = cpi)
  ) +
  geom_line()

## taxa de cambio do Brasil
lac_indicators |> 
  dplyr::filter(country == "Brazil") |> 
  dplyr::select(year, exr) |> 
  ggplot2::ggplot(
    aes(x = year, y = exr)
  ) +
  geom_line()

## taxa de cambio do Brasil
lac_indicators |> 
  dplyr::filter(country == "Brazil") |> 
  dplyr::select(year, inr) |> 
  ggplot2::ggplot(
    aes(x = year, y = inr)
  ) +
  geom_line()