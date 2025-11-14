# Estimando o modelo SDID

# criando a matriz 3D de covaráveis
T0 <- lac_indicators |> unique() |> length()
N0 <- lac_indicators$country |> unique() |> length()
C0 <- 6

l_lac_indicators <- lac_indicators |>
  dplyr::select(country, year, gdp, cpi, exr, inr, coc, pos) |> 
  tidyr::pivot_longer(
    !c(country,year),
    names_to = "covariate",
    values_to = "value"
  ) |> 
  tidyr::pivot_wider(
    names_from = year,
    values_from = value,
  ) |> 
  dplyr::arrange(covariate)

cov <- l_lac_indicators$covariate |> unique()

a <- NULL

abind::abind(a,b, rev.along = 0)
             
for (i in cov) {
  
  segment <- l_lac_indicators |> filter(covariate == i)
  
  b <- array(as.data.frame(segment[3:20]), dim = c(N0,T0))
}

c <- array(,
  dim = c(N0,T0,C0)
  )

# Set seed for reproducibility
set.seed(12345)

setup = synthdid::panel.matrices(as.data.frame(lac_indicators),
                                 unit = "country",
                                 time = "year",
                                 outcome = "unr",
                                 treatment = "treat")

# Estimate treatment effect using SynthDiD
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0, X = a)
print(summary(tau.hat))

# Calculate standard errors 
se = sqrt(vcov(tau.hat, method='placebo'))
te_est <- sprintf('Point estimate for the treatment effect: %1.2f', tau.hat)
CI <- sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

# Plot treatment effect estimates
plot(tau.hat)
plot(tau.hat, se.method='placebo')

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

# Check the number of treatment and control countries to report
num_treated <- length(unique(dt[treat==1]$country))
num_control <- length(unique(dt$country))-num_treated

# Create spaghetti plot with top 10 control units
top.controls = synthdid_controls(tau.hat)[1:10, , drop=FALSE]
plot(tau.hat, spaghetti.units=rownames(top.controls),
     trajectory.linetype = 1, line.width=.75, 
     trajectory.alpha=.9, effect.alpha=.9,
     diagram.alpha=1, onset.alpha=.9, ci.alpha = .3, spaghetti.line.alpha	=.2,
     spaghetti.label.alpha = .1, overlay = 1) + 
  labs(x = 'Period', y = 'unemployment', title = 'Estimation Results', 
       subtitle = paste0(te_est, ', ', CI, '.'), 
       caption = paste0('The number of treatment and control units: ', num_treated, ' and ', num_control, '.'))
ggsave('../figures/results.png')

fe <- feols(unr~treat, lac_indicators, cluster = 'country', panel.id = 'country', 
            fixef = c('country', 'year'))
summary(fe)
summary(tau.hat)

tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')

print(unlist(estimates))

synthdid_plot(estimates, se.method='placebo')

synthdid_units_plot(estimates, se.method='placebo')
