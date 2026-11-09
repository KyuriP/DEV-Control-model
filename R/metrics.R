# Summary measures

# Developmental periods, as fractions of the simulation.
# For T = 1500, approximately: early 300-450, middle 750-900, late 1275-1425.
default_windows <- list(
  early  = c(0.20, 0.30),
  middle = c(0.50, 0.60),
  late   = c(0.85, 0.95)
)

step_at <- function(T, frac) round(frac * (T - 1)) + 1

# Columns of sim$pi that fall inside a window (fractions of T, or steps).
window_cols <- function(sim, w, in_steps = FALSE) {
  if (in_steps) {
    lo <- w[1]
    hi <- w[2]
  } else {
    lo <- step_at(sim$T, w[1])
    hi <- step_at(sim$T, w[2])
  }
  which(sim$steps >= lo & sim$steps <= hi)
}

# Each agent's average engagement propensity within a window.
window_score <- function(sim, w, in_steps = FALSE) {
  rowMeans(sim$pi[, window_cols(sim, w, in_steps), drop = FALSE])
}

# Between-agent SD at each recorded step, averaged over the window.
window_sd <- function(sim, w) {
  cols <- window_cols(sim, w)
  mean(apply(sim$pi[, cols, drop = FALSE], 2, sd))
}

spearman <- function(x, y) {
  if (sd(x) == 0 || sd(y) == 0) return(NA_real_)
  cor(x, y, method = "spearman")
}

# Mean change, change in between-agent SD, and rank persistence.
dev_metrics <- function(sim, windows = default_windows) {
  s_early  <- window_score(sim, windows$early)
  s_middle <- window_score(sim, windows$middle)
  s_late   <- window_score(sim, windows$late)

  early_mean <- mean(sim$pi[, window_cols(sim, windows$early)])
  late_mean  <- mean(sim$pi[, window_cols(sim, windows$late)])
  early_sd   <- window_sd(sim, windows$early)
  late_sd    <- window_sd(sim, windows$late)

  data.frame(
    early_mean = early_mean,
    late_mean = late_mean,
    delta_mean = late_mean - early_mean,
    early_sd = early_sd,
    late_sd = late_sd,
    sd_gain = late_sd - early_sd,
    rho_early_middle = spearman(s_early, s_middle),
    rho_middle_late = spearman(s_middle, s_late),
    rho_early_late = spearman(s_early, s_late)
  )
}

# Population mean propensity at each recorded step.
mean_curve <- function(sim) colMeans(sim$pi)

# Mean propensity averaged over steps [center - halfwidth, center + halfwidth].
window_mean_at <- function(sim, center, halfwidth) {
  cols <- window_cols(sim, c(center - halfwidth, center + halfwidth), in_steps = TRUE)
  mean(sim$pi[, cols])
}

# Median and interquartile range across replications.
summarize_reps <- function(x) {
  c(median = median(x, na.rm = TRUE),
    q25 = unname(quantile(x, 0.25, na.rm = TRUE)),
    q75 = unname(quantile(x, 0.75, na.rm = TRUE)))
}
