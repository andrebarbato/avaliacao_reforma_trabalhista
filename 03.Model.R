# Estimando o modelo SDID

# # criando a matriz 3D de covaráveis
# T0 <- lac_indicators$year |> unique() |> length()
# N0 <- lac_indicators$country |> unique() |> length()
# 
# 
# l_lac_indicators <- lac_indicators |>
#   dplyr::select(country, year, d_gdp, d_cpi, v_exr, v_inr, d_coc, d_pos) |> 
#   tidyr::pivot_longer(
#     !c(country,year),
#     names_to = "covariate",
#     values_to = "value"
#   ) |>
#   dplyr::arrange(covariate, year)
# 
# C0 <- l_lac_indicators$covariate |> unique() |> length()
# 
# a <- array(l_lac_indicators$value,
#   dim = c(N0,T0,C0)
#   )

data_sdid_clean <- data_sdid_clean |> 
  filter(year %in% c(2003:2020))

# Set seed for reproducibility
set.seed(12345)

# Ajustando a variável dependente Taxa de Desemprego para as variáveis independentes
# ou time-varying covariates
data_sdid_clean$adj.unr <- xsynthdid::adjust.outcome.for.x(
  data_sdid_clean,
  unit="country",
  time = "year",
  outcome = "unr",
  treatment = "treat", 
  x=c("gdp", "cpi", "exr_ref", "inr", "coc", "pos", "upop"))

# Prepara a matriz pm considerando a variável independente ajustada
setup <- synthdid::panel.matrices(as.data.frame(data_sdid_clean),
                                 unit = "country",
                                 time = "year",
                                 outcome = "unr",
                                 treatment = "treat")

# Estimate treatment effect using SynthDiD
tau.hat <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
print(summary(tau.hat))

# Calculate standard errors 
se <- sqrt(vcov(tau.hat, method='placebo'))
te_est <- sprintf('Point estimate for the treatment effect: %1.2f', tau.hat)
CI <- sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

# Plot treatment effect estimates
plot(tau.hat,
     trajectory.linetype = 1, 
     line.width=.75,
     trajectory.alpha=.9,
     effect.alpha=.9,
     diagram.alpha=2, onset.alpha=.4, ci.alpha = .3)

plot(tau.hat, se.method='placebo')

ggplot2::ggsave("figs/graf1.png",
                graf1,
                width = 8.5,
                height = 5.5)
# Plot control unit contributions
synthdid_units_plot(tau.hat, se.method='placebo') +
  labs(x = 'Country', y = 'Treatment effect', 
       caption = 'The black horizontal line shows the actual effect; 
       the gray ones show the endpoints of a 95% confidence interval.') 
ggsave('../figures/unit_weights.png')

# Check for pre-treatment parallel trends
plot(tau.hat, overlay=1, se.method='placebo')
plot(tau.hat, overlay=.8, se.method='placebo')
ggsave('../figures/results_simple.png')

# Create spaghetti plot with top 10 control units
top.controls <- synthdid_controls(tau.hat)
plot(tau.hat, spaghetti.units=rownames(top.controls))
ggsave('../figures/results.png')

fe <- feols(adj.unr~treat, 
            data_sdid_clean, 
            cluster = 'country', 
            panel.id = 'country', 
            fixef = c('country', 'year'))

summary(fe)
summary(tau.hat)

tau.sc <- sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did <- did_estimate(setup$Y, setup$N0, setup$T0)
estimates <- list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')

print(unlist(estimates))

synthdid_plot(estimates, se.method='placebo')

synthdid_units_plot(estimates, se.method='placebo')
