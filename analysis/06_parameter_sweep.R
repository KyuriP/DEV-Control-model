# Simulation 6: are the main patterns robust to parameter values?
# 250 random parameter combinations x 5 replications, M2, shared environment.
# Smaller populations (150 agents, 900 steps) to keep run time down.
# Output: Figure 7, Table S1, results/sim6_*.csv

library(dplyr)
library(tidyr)
source("R/model.R")
source("R/metrics.R")
source("R/plot_style.R")

n_sets <- 250
n_reps <- 5
sweep_N <- 150
sweep_T <- 900

log_runif <- function(n, min, max) exp(runif(n, log(min), log(max)))

set.seed(260819)
sweep_pars <- data.frame(
  set_id = seq_len(n_sets),
  alpha_V = log_runif(n_sets, 0.006, 0.070),
  alpha_C = log_runif(n_sets, 0.0004, 0.0070),
  tau = runif(n_sets, 0.10, 0.30),
  p_reward = runif(n_sets, 0.45, 0.72),
  initial_D = runif(n_sets, -0.60, -0.20),     # V0 - C0
  cost_range = runif(n_sets, 0.25, 0.75)       # C0 - C_min
) %>%
  mutate(V0 = default_pars$V0,
         C0 = V0 - initial_D,
         C_min = C0 - cost_range)

run_set <- function(j) {
  p <- sweep_pars[j, ]
  bind_rows(lapply(seq_len(n_reps), function(r) {
    sim <- simulate_dev_control(
      "value_cost", N = sweep_N, T = sweep_T,
      V0 = p$V0, C0 = p$C0, C_min = p$C_min, tau = p$tau,
      p_reward = p$p_reward, alpha_V = p$alpha_V, alpha_C = p$alpha_C,
      seed = 100000 + j * 100 + r
    )
    cbind(set_id = j, rep = r, dev_metrics(sim))
  }))
}

n_detected <- parallel::detectCores()
if (is.na(n_detected)) n_detected <- 2
n_cores <- if (.Platform$OS.type == "windows") 1 else max(1, min(n_sets, n_detected - 1))
sweep_reps <- bind_rows(parallel::mclapply(seq_len(n_sets), run_set, mc.cores = n_cores))

# Average over replications; keep the replication SD for the supplement.
sweep <- sweep_reps %>%
  group_by(set_id) %>%
  summarize(across(c(delta_mean, sd_gain, rho_middle_late),
                   list(mean = ~ mean(.x, na.rm = TRUE), rep_sd = ~ sd(.x, na.rm = TRUE))),
            .groups = "drop") %>%
  left_join(sweep_pars, by = "set_id") %>%
  mutate(pattern = case_when(
    delta_mean_mean > 0 & sd_gain_mean > 0  ~ "Improvement + differentiation",
    delta_mean_mean > 0 & sd_gain_mean <= 0 ~ "Improvement only",
    delta_mean_mean <= 0 & sd_gain_mean > 0 ~ "Differentiation only",
    TRUE ~ "Neither"
  ))

pattern_levels <- c("Improvement + differentiation", "Improvement only",
                    "Differentiation only", "Neither")

pattern_summary <- sweep %>%
  mutate(pattern = factor(pattern, levels = pattern_levels)) %>%
  count(pattern, .drop = FALSE) %>%
  mutate(proportion = n / sum(n))

sweep_summary <- data.frame(
  measure = c("Mean engagement increases", "Between-agent SD increases", "Both increase",
              "Median change in mean engagement",
              "Median change in between-agent SD",
              "Median middle-to-late rank persistence",
              "Median replication SD, mean change",
              "Median replication SD, SD change",
              "Median replication SD, rank persistence"),
  value = c(mean(sweep$delta_mean_mean > 0, na.rm = TRUE),
            mean(sweep$sd_gain_mean > 0, na.rm = TRUE),
            mean(sweep$delta_mean_mean > 0 & sweep$sd_gain_mean > 0, na.rm = TRUE),
            median(sweep$delta_mean_mean, na.rm = TRUE),
            median(sweep$sd_gain_mean, na.rm = TRUE),
            median(sweep$rho_middle_late_mean, na.rm = TRUE),
            median(sweep$delta_mean_rep_sd, na.rm = TRUE),
            median(sweep$sd_gain_rep_sd, na.rm = TRUE),
            median(sweep$rho_middle_late_rep_sd, na.rm = TRUE))
)

# Which parameters separate the 'both' cases from the rest?
param_contrast <- sweep %>%
  mutate(both = pattern == "Improvement + differentiation") %>%
  group_by(both) %>%
  summarize(n = n(), across(c(alpha_V, alpha_C, tau, p_reward, initial_D, cost_range), median),
            .groups = "drop")

dir.create("results", showWarnings = FALSE)
write.csv(sweep_reps, "results/sim6_sweep_by_replication.csv", row.names = FALSE)
write.csv(sweep, "results/sim6_sweep.csv", row.names = FALSE)
write.csv(pattern_summary, "results/sim6_pattern_summary.csv", row.names = FALSE)
write.csv(sweep_summary, "results/sim6_summary.csv", row.names = FALSE)
write.csv(param_contrast, "results/sim6_parameter_contrast.csv", row.names = FALSE)

# Figure 7 ------------------------------------------------------------------
pattern_cols <- c(
  "Improvement + differentiation" = COL$orange,
  "Improvement only"              = COL$blue,
  "Differentiation only"          = COL$green,
  "Neither"                       = COL$light_gray
)

pct <- setNames(scales::percent(pattern_summary$proportion, accuracy = 1),
                as.character(pattern_summary$pattern))

x_lim <- c(min(sweep$delta_mean_mean, -0.02), max(sweep$delta_mean_mean) * 1.04)
y_lim <- c(min(sweep$sd_gain_mean, -0.02), max(sweep$sd_gain_mean) * 1.05)
x_in <- function(f) x_lim[1] + f * diff(x_lim)
y_in <- function(f) y_lim[1] + f * diff(y_lim)

fig7 <- ggplot(sweep, aes(delta_mean_mean, sd_gain_mean,
                          color = factor(pattern, levels = pattern_levels))) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = COL$gray) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3, color = COL$gray) +
  geom_point(alpha = 0.62, size = 1.45, stroke = 0) +
  scale_color_manual(values = pattern_cols, drop = FALSE) +
  annotate("text", x = x_in(0.97), y = y_in(0.96), label = pct["Improvement + differentiation"],
           hjust = 1, vjust = 1, size = 3.2, fontface = "bold", color = COL$orange) +
  annotate("text", x = x_in(0.97), y = y_in(0.06), label = pct["Improvement only"],
           hjust = 1, vjust = 0, size = 3, fontface = "bold", color = COL$blue) +
  annotate("text", x = x_in(0.03), y = y_in(0.96), label = pct["Differentiation only"],
           hjust = 0, vjust = 1, size = 3, fontface = "bold", color = COL$green) +
  annotate("text", x = x_in(0.03), y = y_in(0.06), label = pct["Neither"],
           hjust = 0, vjust = 0, size = 2.8, color = COL$dark_gray) +
  coord_cartesian(xlim = x_lim, ylim = y_lim, clip = "off") +
  labs(x = "Change in mean EF engagement (late - early)",
       y = "Change in between-agent SD (late - early)") +
  theme_pub()

save_fig(fig7, "Figure7_parameter_robustness", width = 6.2, height = 4.6)
