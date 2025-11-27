# Estimando o modelo SDID

# Set seed for reproducibility
set.seed(12345)

# Ajustando a variável dependente Taxa de Desemprego para as variáveis independentes
# ou time-varying covariates
data_model$adj.unr <- xsynthdid::adjust.outcome.for.x(
  data_model,
  unit="country",
  time = "year",
  outcome = "unr",
  treatment = "treat", 
  x=c("gdp", "inf_d", "v_exr", "inr", "coc", "pos"))

# Prepara a matriz pm considerando a variável independente ajustada
setup <- synthdid::panel.matrices(as.data.frame(data_model),
                                 unit = "country",
                                 time = "year",
                                 outcome = "adj.unr",
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
     diagram.alpha=2, onset.alpha=.4, ci.alpha = .3) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = element_text(),
                 plot.background = element_rect(fill = "white"),
                 panel.background = element_rect(fill = "white"),
                 legend.background = element_rect(fill = "white"),
                 panel.grid.major = element_line(color = "lightgray", 
                                                 size = 0.05),
                 panel.grid.minor = element_line(color = "lightgray", 
                                                 size = 0.05))

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
