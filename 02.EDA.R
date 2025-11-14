# Reproduzindo a Figura 1 do artigo base: 
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
