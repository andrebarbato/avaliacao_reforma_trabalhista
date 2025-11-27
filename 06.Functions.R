# Função para rodar o fluxo de estimação ---------------------------------------
sdid_run <- function(df,
                     unit,
                     time,
                     outcome,
                     treat,
                     covariates
                     ) {
  
  t_start <- Sys.time()
  
  # Garantindo a reproducibilidade
  set.seed(123)
  message(paste0("Calculando 1/15...", round(1/15*100, 2), "%"))
  
  # Limpando o efeito das variáveis independentes (X) da variável dependente (Y)
  df$adj.y <- xsynthdid::adjust.outcome.for.x(
    df,
    unit= unit,
    time = time,
    outcome = outcome,
    treatment = treat, 
    x= covariates)
  message(paste0("Calculando 2/15...", round(2/15*100, 2), "%"))
  
  # Prepara a matriz pm considerando a variável independente ajustada
  pm <- synthdid::panel.matrices(as.data.frame(df),
                                 unit = unit,
                                 time = time,
                                 outcome = "adj.y",
                                 treatment = treat)
  message(paste0("Calculando 3/15...", round(3/15*100, 2), "%"))
  
  # Estimate treatment effect using SynthDiD
  tau.hat <- synthdid_estimate(pm$Y, pm$N0, pm$T0)
  message(paste0("Calculando 4/15...", round(4/15*100, 2), "%"))
  
  # Calculate standard errors 
  se <- sqrt(vcov(tau.hat, method='placebo'))
  te_est <- sprintf('Point estimate for the treatment effect: %1.2f', tau.hat)
  CI <- sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
  message(paste0("Calculando 5/15...", round(5/15*100, 2), "%"))
  
  # Plot treatment effect estimates
  plot_treat_effect <- synthdid::synthdid_plot(
    tau.hat,
    treated.name = "Tratado",
    control.name = "Controle sintético",
    trajectory.linetype = 1, 
    line.width=.75,
    trajectory.alpha=.9,
    effect.alpha=.9,
    diagram.alpha=2, 
    onset.alpha=.4, 
    ci.alpha = .3,
    se.method='placebo') +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom"  # Muda a posição da legenda para baixo
    )
  message(paste0("Calculando 6/15...", round(6/15*100, 2), "%"))
  
  # Plot control unit contributions
  control_unit_cont <- synthdid_units_plot(
    tau.hat, 
    se.method='placebo') +
    labs(x = 'Países', y = 'Efeito do tratamento', 
         caption = 'A linha preta horizontal mostra o efeito real; 
       as linhas cinzas mostram os intervaloes a 95% de confiança.') +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = element_text(angle = 90, hjust = 1)  # Rotação dos rótulos do eixo x
    )
  message(paste0("Calculando 7/15...", round(7/15*100, 2), "%"))
  
  # Create spaghetti plot with top 10 control units
  top.controls <- synthdid_controls(tau.hat)
  spaghetti_plot <- plot(
    tau.hat, 
    treated.name = "Tratado",
    control.name = "Controle sintético",
    spaghetti.units= rownames(top.controls),
    trajectory.linetype = 1, 
    line.width=.75,
    trajectory.alpha=.9,
    effect.alpha=.9,
    diagram.alpha=2, 
    onset.alpha=.4, 
    ci.alpha = .3,
    spaghetti.line.width = 0.3,
    spaghetti.label.size = 4,
    spaghetti.line.alpha = 0.6,
    spaghetti.label.alpha = 0.7
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom"  # Muda a posição da legenda para baixo
    )
  message(paste0("Calculando 8/15...", round(8/15*100, 2), "%"))
  
  # calculando os efeitos fixos
  fe <- feols(adj.y ~ treat, 
              df, 
              cluster = unit, 
              panel.id = unit, 
              fixef = c(unit, time))
  message(paste0("Calculando 9/15...", round(9/15*100, 2), "%"))
  
  # Calculando os estimadores de DID e SC e comparando 
  tau.sc <- sc_estimate(pm$Y, pm$N0, pm$T0)
  tau.did <- did_estimate(pm$Y, pm$N0, pm$T0)
  estimates <- list(tau.did, tau.sc, tau.hat)
  names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
  message(paste0("Calculando 10/15...", round(10/15*100, 2), "%"))
  
  # gráfico comparando os estimadores de DID, SC e SDID
  comp_plot <- synthdid_plot(
    estimates, 
    treated.name = "Tratado",
    control.name = "Controle sintético",
    se.method='placebo',
    trajectory.linetype = 1, 
    line.width=.75,
    trajectory.alpha=.9,
    effect.alpha=.9,
    diagram.alpha=2, 
    onset.alpha=.4, 
    ci.alpha = .3) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom"  # Muda a posição da legenda para baixo
    )
  
  message(paste0("Calculando 11/15...", round(11/15*100, 2), "%"))
  
  # Gráfico comparando os pesos dos controles e o erro padrão dos estimadores
  comp_se_unit_plot <- synthdid_units_plot(
    estimates, 
    se.method='placebo')
  message(paste0("Calculando 12/15...", round(12/15*100, 2), "%"))
  
  # Tabela de pesos do controle sintético
  tab_control_weights <- synthdid::synthdid_controls(tau.hat)
  message(paste0("Calculando 13/15...", round(13/15*100, 2), "%"))
  
  # Gerar o gráfico de gap entre o SC e o observado
  sc_gap_plot <- gap_plot(est_sdid = tau.hat, est_sc = tau.sc)
  message(paste0("Calculando 14/15...", round(14/15*100, 2), "%"))
  
  # lista de resultados
  outputSDID <- list(tau.hat, 
                     se, 
                     te_est, 
                     CI, 
                     plot_treat_effect,
                     control_unit_cont,
                     spaghetti_plot,
                     fe,
                     estimates,
                     comp_plot,
                     comp_se_unit_plot,
                     tab_control_weights,
                     sc_gap_plot)
  message(paste0("Calculando 15/15...", round(15/15*100, 2), "%"))
  t_end <- Sys.time()
  
  message(paste("Processo finalizado em", round(t_end - t_start, 2), "minutos!"))
  
  return(outputSDID)
  
}

# Função para recuperar os valores calculados de controle sintético ------------
sdid_sc <- function (estimates, treated.name = "treated", control.name = "synthetic control", 
          spaghetti.units = c(), spaghetti.matrices = NULL, facet = NULL, 
          facet.vertical = TRUE, lambda.comparable = !is.null(facet), 
          overlay = 0, lambda.plot.scale = 3, trajectory.linetype = 1, 
          effect.curvature = 0.3, line.width = 0.5, guide.linetype = 2, 
          point.size = 1, trajectory.alpha = 0.5, diagram.alpha = 0.95, 
          effect.alpha = 0.95, onset.alpha = 0.3, ci.alpha = 0.3, 
          spaghetti.line.width = 0.2, spaghetti.label.size = 2, spaghetti.line.alpha = 0.3, 
          spaghetti.label.alpha = 0.5, se.method = "jackknife", alpha.multiplier = NULL) 
{
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    .ignore <- tryCatch(attachNamespace("ggplot2"), error = function(e) e)
  }
  else {
    stop("Plotting requires the package `ggplot2`. Install it to use this function.")
  }
  if (class(estimates) == "synthdid_estimate") {
    estimates = list(estimates)
  }
  if (is.null(names(estimates))) {
    names(estimates) = sprintf("estimate %d", 1:length(estimates))
  }
  if (is.null(alpha.multiplier)) {
    alpha.multiplier = rep(1, length(estimates))
  }
  if (!is.null(spaghetti.matrices) && length(spaghetti.matrices) != 
      length(estimates)) {
    stop("spaghetti.matrices must be the same length as estimates")
  }
  multiple.frames = length(overlay) > 1
  treated = 1
  control = 2
  groups = factor(c(control, treated), labels = c(control.name, 
                                                  treated.name))
  estimate.factors = factor(1:(length(estimates) + 1), labels = c(treated.name, 
                                                                  names(estimates)))
  facet_factors = if (is.null(facet)) {
    factor(1:length(estimates), labels = names(estimates))
  }
  else {
    factor(facet, levels = 1:length(unique(facet)), labels = unique(facet))
  }
  grid = expand.grid(estimate = 1:length(estimates), overlay = 1:length(overlay))
  plot.descriptions = lapply(1:nrow(grid), function(row) {
    est = estimates[[grid$estimate[row]]]
    over = overlay[grid$overlay[row]]
    se = if (se.method == "none") {
      NA
    }
    else {
      sqrt(vcov(est, method = se.method))
    }
    setup = attr(est, "setup")
    weights = attr(est, "weights")
    Y = setup$Y - contract3(setup$X, weights$beta)
    N0 = setup$N0
    N1 = nrow(Y) - N0
    T0 = setup$T0
    T1 = ncol(Y) - T0
    lambda.synth = c(weights$lambda, rep(0, T1))
    lambda.target = c(rep(0, T0), rep(1/T1, T1))
    omega.synth = c(weights$omega, rep(0, N1))
    omega.target = c(rep(0, N0), rep(1/N1, N1))
    if (!is.null(attr(est, "overlay"))) {
      over = attr(est, "overlay")
    }
    is.sc = all(weights$lambda == 0) || over == 1
    intercept.offset = over * c((omega.target - omega.synth) %*% 
                                  Y %*% lambda.synth)
    obs.trajectory = as.numeric(omega.target %*% Y)
    syn.trajectory = as.numeric(omega.synth %*% Y) + intercept.offset
    spaghetti.trajectories = Y[rownames(Y) %in% spaghetti.units, 
                               , drop = FALSE]
    if (!is.null(spaghetti.matrices)) {
      more.spaghetti.trajectories = spaghetti.matrices[[grid$estimate[row]]]
      if (ncol(more.spaghetti.trajectories) != ncol(Y)) {
        stop("The elements of spaghetti.matrices must be matrices with the same number of columns as Y")
      }
      if (is.null(rownames(more.spaghetti.trajectories))) {
        stop("The elements of the list spaghetti.matrices must have named rows")
      }
      spaghetti.trajectories = rbind(spaghetti.trajectories, 
                                     more.spaghetti.trajectories)
    }
    treated.post = omega.target %*% Y %*% lambda.target
    treated.pre = omega.target %*% Y %*% lambda.synth
    control.post = omega.synth %*% Y %*% lambda.target + 
      intercept.offset
    control.pre = omega.synth %*% Y %*% lambda.synth + intercept.offset
    sdid.post = as.numeric(control.post + treated.pre - 
                             control.pre)
    time = as.numeric(timesteps(Y))
    if (length(time) == 0 || !all(is.finite(time))) {
      time = 1:(T0 + T1)
    }
    pre.time = lambda.synth %*% time
    post.time = lambda.target %*% time
    lines = data.frame(x = rep(time, 2), y = c(obs.trajectory, 
                                               syn.trajectory), color = rep(groups[c(treated, control)], 
                                                                            each = length(time)))
    points = data.frame(x = c(post.time, post.time), y = c(treated.post, 
                                                           sdid.post), color = groups[c(treated, control)])
    did.points = data.frame(x = c(pre.time, pre.time, post.time, 
                                  post.time), y = c(treated.pre, control.pre, control.post, 
                                                    treated.post), color = groups[c(treated, control, 
                                                                                    control, treated)])
    did.segments = data.frame(x = c(pre.time, pre.time), 
                              xend = c(post.time, post.time), y = c(control.pre, 
                                                                    treated.pre), yend = c(control.post, treated.post), 
                              color = groups[c(control, treated)])
    hallucinated.segments = data.frame(x = pre.time, xend = post.time, 
                                       y = treated.pre, yend = sdid.post)
    guide.segments = data.frame(x = c(pre.time, post.time), 
                                xend = c(pre.time, post.time), y = c(control.pre, 
                                                                     control.post), yend = c(treated.pre, sdid.post))
    arrows = data.frame(x = post.time, xend = post.time, 
                        y = sdid.post, yend = treated.post, xscale = max(time) - 
                          post.time, color = groups[control])
    ub.arrows = data.frame(x = post.time, xend = post.time, 
                           y = sdid.post + 1.96 * se, yend = treated.post, 
                           xscale = max(time) - post.time, color = groups[control])
    lb.arrows = data.frame(x = post.time, xend = post.time, 
                           y = sdid.post - 1.96 * se, yend = treated.post, 
                           xscale = max(time) - post.time, color = groups[control])
    spaghetti.lines = data.frame(x = rep(time, nrow(spaghetti.trajectories)), 
                                 y = as.vector(t(spaghetti.trajectories)), unit = rep(rownames(spaghetti.trajectories), 
                                                                                      each = length(time)))
    spaghetti.labels = data.frame(x = rep(time[1], nrow(spaghetti.trajectories)), 
                                  y = as.vector(spaghetti.trajectories[, 1]), unit = rownames(spaghetti.trajectories))
    T0s = attr(est, "T0s")
    if (!is.null(T0s)) {
      vlines = data.frame(xintercept = time[T0s])
    }
    else {
      vlines = data.frame(xintercept = time[T0])
    }
    if (lambda.comparable) {
      height = (max(c(obs.trajectory)) - min(c(obs.trajectory)))/lambda.plot.scale
      bottom = min(c(obs.trajectory)) - height
      ribbons = data.frame(x = time[1:T0], ymin = rep(bottom, 
                                                      T0), ymax = bottom + height * lambda.synth[1:T0], 
                           color = groups[control])
    }
    else {
      height = (max(c(obs.trajectory, syn.trajectory)) - 
                  min(c(obs.trajectory, syn.trajectory)))/lambda.plot.scale
      bottom = min(c(obs.trajectory, syn.trajectory)) - 
        height
      ribbons = data.frame(x = time[1:T0], ymin = rep(bottom, 
                                                      T0), ymax = bottom + height * lambda.synth[1:T0]/max(lambda.synth), 
                           color = groups[control])
    }
    elements = list(lines = lines, points = points, did.segments = did.segments, 
                    did.points = did.points, hallucinated.segments = hallucinated.segments, 
                    guide.segments = guide.segments, arrows = arrows, 
                    lb.arrows = lb.arrows, ub.arrows = ub.arrows, spaghetti.lines = spaghetti.lines, 
                    spaghetti.labels = spaghetti.labels, vlines = vlines, 
                    ribbons = ribbons)
    lapply(elements, function(x) {
      if (nrow(x) > 0) {
        x$frame = over
        x$is.sc = is.sc
        x$estimate = estimate.factors[grid$estimate[row] + 
                                        1]
      }
      x
    })
  })
  one.per.facet = length(unique(facet_factors)) == length(facet_factors)
  concatenate.field = function(field) {
    do.call(rbind, lapply(plot.descriptions, function(desc) {
      element = desc[[field]]
      estimate.factor = element$estimate[1]
      element$facet = facet_factors[as.integer(estimate.factor) - 
                                      1]
      element$show = alpha.multiplier[as.integer(element$estimate) - 
                                        1]
      element$show[element$color == groups[treated]] = 1
      if (!one.per.facet && "color" %in% colnames(element)) {
        color = element$estimate
        color[element$color == groups[treated]] = estimate.factors[1]
        element$color = color
      }
      element
    }))
  }
  conc = lapply(names(plot.descriptions[[1]]), concatenate.field)
  names(conc) = names(plot.descriptions[[1]])
  no.sc = function(x) {
    x[!x$is.sc, ]
  }
  with.frame = function(geom, base.aes, data, ...) {
    new.aes = if (multiple.frames) {
      modifyList(base.aes, aes(frame = frame))
    }
    else {
      base.aes
    }
    do.call(geom, c(list(new.aes, data = data), list(...)))
  }
  p = ggplot() + with.frame(geom_line, aes(x = x, y = y, color = color, 
                                           alpha = trajectory.alpha * show), data = conc$lines, 
                            linetype = trajectory.linetype, size = line.width) + 
    with.frame(geom_point, aes(x = x, y = y, color = color, 
                               alpha = diagram.alpha * show), data = conc$points, 
               shape = 21, size = point.size) + with.frame(geom_point, 
                                                           aes(x = x, y = y, color = color, alpha = diagram.alpha * 
                                                                 show), data = no.sc(conc$did.points), size = point.size) + 
    with.frame(geom_segment, aes(x = x, xend = xend, y = y, 
                                 yend = yend, color = color, alpha = diagram.alpha * 
                                   show), data = no.sc(conc$did.segments), size = line.width) + 
    with.frame(geom_segment, aes(x = x, xend = xend, y = y, 
                                 yend = yend, group = estimate, alpha = 0.6 * diagram.alpha * 
                                   show), data = no.sc(conc$hallucinated.segments), 
               linetype = guide.linetype, size = line.width, color = "black") + 
    with.frame(geom_segment, aes(x = x, xend = xend, y = y, 
                                 yend = yend, group = estimate, alpha = 0.5 * diagram.alpha * 
                                   show), data = no.sc(conc$guide.segments), size = line.width, 
               linetype = guide.linetype, color = "black") + geom_vline(aes(xintercept = xintercept, 
                                                                            alpha = onset.alpha * show), data = conc$vlines, size = line.width, 
                                                                        color = "black") + geom_ribbon(aes(x = x, ymin = ymin, 
                                                                                                           ymax = ymax, group = color, fill = color, alpha = 0.5 * 
                                                                                                             diagram.alpha * show), data = no.sc(conc$ribbons), 
                                                                                                       color = "black", size = line.width, show.legend = FALSE) + 
    geom_curve(aes(x = x, xend = xend, y = y, yend = yend, 
                   alpha = effect.alpha * show), data = conc$arrows, 
               curvature = effect.curvature, color = "black", size = line.width, 
               arrow = arrow(length = unit(0.2, "cm"))) + geom_curve(aes(x = x, 
                                                                         xend = xend, y = y, yend = yend, alpha = ci.alpha * 
                                                                           show), data = conc$ub.arrows, na.rm = TRUE, curvature = effect.curvature, 
                                                                     color = "black", size = line.width, arrow = arrow(length = unit(0.2, 
                                                                                                                                     "cm"))) + geom_curve(aes(x = x, xend = xend, y = y, 
                                                                                                                                                              yend = yend, alpha = ci.alpha * show), data = conc$lb.arrows, 
                                                                                                                                                          na.rm = TRUE, curvature = effect.curvature, color = "black", 
                                                                                                                                                          size = line.width, arrow = arrow(length = unit(0.2, 
                                                                                                                                                                                                         "cm")))
  if (nrow(conc$spaghetti.labels) > 0) {
    p = p + geom_text(aes(x = x, y = y, label = unit, alpha = spaghetti.label.alpha * 
                            show), data = conc$spaghetti.labels, color = "black", 
                      size = spaghetti.label.size) + geom_line(aes(x = x, 
                                                                   y = y, group = unit, alpha = spaghetti.line.alpha * 
                                                                     show), data = conc$spaghetti.lines, color = "black", 
                                                               size = spaghetti.line.width)
  }
  if (!all(conc$lines$facet == conc$lines$facet[1])) {
    if (facet.vertical) {
      p = p + facet_grid(facet ~ ., scales = "free_y")
    }
    else {
      p = p + facet_grid(. ~ facet)
    }
  }
  if (is.null(facet)) {
    p = p + guides(linetype = "none")
  }
  p = tryCatch({
    as.Date(colnames(attr(estimates[[1]], "setup")$Y))
    p + scale_x_continuous(labels = function(time) {
      as.Date(time, origin = "1970-01-01")
    })
  }, error = function(e) {
    p
  })
  return(conc$lines)
}

# Função auxiliar da função synthdid_plot ------------------------------------------
contract3 <- function (X, v) 
{
  stopifnot(length(dim(X)) == 3, dim(X)[3] == length(v))
  out = array(0, dim = dim(X)[1:2])
  if (length(v) == 0) {
    return(out)
  }
  for (ii in 1:length(v)) {
    out = out + v[ii] * X[, , ii]
  }
  return(out)
}

# Função para carregar e preparando os dados para análise  ---------------------

load_data <- function(file = "data/dados.RData") {
 
  load(
    file = file
  )
  
  # preparando para o modelo
  data_model <- data_sdid_tratado |>
    dplyr::select(-c("earn_t", "earn_m", "earn_f", "iel_t", "iel_m", "iel_f",
                     "cpi", "exr", "gini", "subt", "exped")) |>
    dplyr::group_by(country) |>
    dplyr::mutate(
      d_unr = unr - lag(unr),
      d_gdp = gdp - lag(gdp),
      d_inf_d = inf_d - lag(inf_d),
      v_exr = exr_ref / lag(exr_ref) - 1,
      v_inr = inr /lag(inr) - 1,
      d_coc = coc - lag(coc),
      d_pos = pos - lag(pos),
      d_gdppc = gdppc - lag(gdppc),
      v_upop = upop / lag(upop) -1,
      v_labf = labf / lag(labf) -1,
      treat = ifelse(country == "Brazil" & year >= 2018, 1, 0)
    ) |> 
    dplyr::ungroup() |> 
    dplyr::filter(year %in% c(1996:2024))
  
  return(data_model)
}

# Função para plotar o gráfico de GAP entre o SC e o valor observado -----------
gap_plot <- function(est_sdid,
                     est_sc){
  
  #calculando o erro padrão para o SC
  se_sc <- sqrt(vcov(est_sc, method='placebo'))
  
  df <- sdid_sc(est_sdid) |> 
    dplyr::select(x, y, color) |> 
    tidyr::pivot_wider(
      values_from = y,
      names_from = color
    ) |> 
    dplyr::rename(
      sc = `synthetic control`
    ) |> 
    dplyr::mutate(
      dif = treated - sc,
      bottom = dif - (1.96 * se_sc),
      upper = dif + (1.96 * se_sc)
    ) 
  
  min <- min(df$bottom)
  max <- max(df$upper)
  
  g <- df |> 
    ggplot(aes(x = x, y = dif)) +
    geom_line(size = 0.75) +
    geom_ribbon(aes(ymin = bottom, ymax = upper), fill = "red", alpha = 0.2) +
    ylim(min*1.5, max*1.5) +  
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    theme_bw() +
    theme(
      legend.position = "none"
    ) +
    labs(
      title = "Gap: Tratado - Controle sintético",
      x = "Anos",
      y = "Diferença na taxa de desemprego"
    )
  
  return(g)
}
