library(wesanderson)
# Preparando os dados ---------------------------------------------------------
# Checando NAs por país 2003:2024
data_sdid |>
  dplyr::filter(year %in% c(2003:2024)) |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(
    dplyr::across(dplyr::everything(),
                  ~sum(is.na(.)))
  ) |> gt()

# Média antes do tratamento dos NAs
data_sdid |> 
  dplyr::filter(year %in% c(2003:2024)) |> 
  # dplyr::select(-c("earn_t","earn_m", "earn_f", "iel_t", 
  #                  "iel_m", "iel_f", "cpi", "exr", "subt")) |>
  tidyr::pivot_longer(
    !c("country", "year"),
    names_to = "var",
    values_to = "value"
  ) |> 
  group_by(var) |>
  summarise(
    "Mínimo" = round(min(value, na.rm = TRUE), digits = 2),
    "Média" = round(mean(value, na.rm = TRUE), digits = 2),
    "Máximo" = round(max(value, na.rm = TRUE), digits = 2),
    "Desvio Padrão" = round(sd(value, na.rm = TRUE), digits = 2),
    "Núm. Obs." = n(),
    "NA's" = sum(is.na(value))
  ) |> gt() |> 
  tab_header(
    title = md("**Tabela 1. Resumo estatístico dos dados antes da imputação de valores faltantes (2003:2024)**")
  ) |>
  tab_spanner(
    label = md("**Estatísticas Descritivas**"),
    columns = c("Mínimo", "Média", "Máximo", "Desvio Padrão", "Núm. Obs.", "NA's")
  ) |>
  tab_options(
    table.font.names = "Times New Roman",  # Mudando a fonte para Times New Roman
    table.font.size = px(14),                # Alterando o tamanho da fonte da tabela
    table.border.top.color = "black",        # Cor da borda do topo
    column_labels.font.weight = "bold"        # Negrito para os rótulos das colunas
  ) |> 
  cols_label(
    var = "Variável"                         # Renomear coluna
  ) |> 
  fmt_number(
    columns = c("Mínimo", "Média", "Máximo", "Desvio Padrão", "Núm. Obs.", "NA's"), # Formatando os números
    decimals = 2
  ) |> 
  tab_source_note("Fonte: Elaboração Própria") |> 
  gtsave("figs/tab2.png")

# Checando NAs por país depois do tratamento
data_sdid_tratado |>
  dplyr::filter(year %in% c(2009:2024)) |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(
    dplyr::across(dplyr::everything(),
                  ~sum(is.na(.)))
  ) |> gt()

# Média dpois do tratamento dos NAs
data_sdid_tratado |> 
  dplyr::filter(year %in% c(2009:2024)) |> 
  # dplyr::select(-c("earn_t","earn_m", "earn_f", "iel_t", 
  #                  "iel_m", "iel_f", "cpi", "exr", "subt")) |>
  tidyr::pivot_longer(
    !c("country", "year"),
    names_to = "var",
    values_to = "value"
  ) |> 
  group_by(var) |>
  summarise(
    "Mínimo" = round(min(value, na.rm = TRUE), digits = 2),
    "Média" = round(mean(value, na.rm = TRUE), digits = 2),
    "Máximo" = round(max(value, na.rm = TRUE), digits = 2),
    "Desvio Padrão" = round(sd(value, na.rm = TRUE), digits = 2),
    "Núm. Obs." = n(),
    "NA's" = sum(is.na(value))
  ) |> gt() |> 
  tab_header(
    title = md("**Tabela 2. Resumo estatístico dos dados depois da imputação de valores faltantes (2009:2024)**")
  ) |>
  tab_spanner(
    label = md("**Estatísticas Descritivas**"),
    columns = c("Mínimo", "Média", "Máximo", "Desvio Padrão", "Núm. Obs.", "NA's")
  ) |>
  tab_options(
    table.font.names = "Times New Roman",  # Mudando a fonte para Times New Roman
    table.font.size = px(14),                # Alterando o tamanho da fonte da tabela
    table.border.top.color = "black",        # Cor da borda do topo
    column_labels.font.weight = "bold"        # Negrito para os rótulos das colunas
  ) |> 
  cols_label(
    var = "Variável"                         # Renomear coluna
  ) |> 
  fmt_number(
    columns = c("Mínimo", "Média", "Máximo", "Desvio Padrão", "Núm. Obs.", "NA's"), # Formatando os números
    decimals = 2
  ) |> 
  tab_source_note("Fonte: Elaboração Própria") |> 
  gtsave("figs/tab6.png")

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
data_model |> 
  dplyr::filter(country == "Brazil") |> 
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

# Tabela de NAs
data_sdid |> 
  #group_by(country) |>
  miss_var_summary() |> 
  gt() |> 
  gt_theme_pff()

# Tabela de NAs
data_sdid_clean |> 
  #group_by(country) |>
  miss_var_summary() |> 
  gt() |> 
  gt_theme_pff()
