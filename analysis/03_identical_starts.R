# Simulation 3: do individual differences emerge from identical starts, and do
# they persist? Full model (M2), shared environment.
# The first 1,500 steps of each long run are identical to the Simulation 1 run
# with the same seed (same random stream), so the numbers line up across sections.
# Output: Figure 4, Figure S1, results/sim3_*.csv

library(dplyr)
library(tidyr)
library(patchwork)
source("R/model.R")
source("R/metrics.R")
source("R/plot_style.R")

T_main <- default_pars$T
T_long <- 4 * (T_main - 1) + 1   # four times longer

long_runs <- lapply(seeds, function(s) {
  simulate_dev_control("value_cost", T = T_long, record_every = 5, seed = s)
})

# Restrict a long run to the first T_main steps.
first_part <- function(sim, T_end = T_main) {
  keep <- sim$steps <= T_end
  sim$steps <- sim$steps[keep]
  sim$pi <- sim$pi[, keep, drop = FALSE]
  sim$T <- T_end
  sim
}

main_runs <- lapply(long_runs, first_part)

# Rank persistence within the main simulation --------------------------------
sim3_ranks <- bind_rows(lapply(seq_along(seeds), function(i) {
  cbind(seed = seeds[i], dev_metrics(main_runs[[i]]))
}))

# Rank memory over the long run: correlation between each agent's late-window
# score (85-95% of the main run) and its score in later windows.
late_window <- c(step_at(T_main, 0.85), step_at(T_main, 0.95))
centers <- round(seq(1.10, 4.00, by = 0.20) * (T_main - 1)) + 1
half_width <- round(0.06 * (T_main - 1))

rank_memory <- bind_rows(lapply(seq_along(seeds), function(i) {
  ref <- window_score(long_runs[[i]], late_window, in_steps = TRUE)
  data.frame(
    seed = seeds[i],
    step = centers,
    rho = sapply(centers, function(cc) {
      later <- window_score(long_runs[[i]], c(cc - half_width, cc + half_width), in_steps = TRUE)
      spearman(ref, later)
    })
  )
}))

rank_memory_summary <- rank_memory %>%
  group_by(step) %>%
  summarize(median_rho = median(rho), q25 = quantile(rho, 0.25), q75 = quantile(rho, 0.75),
            .groups = "drop")

dir.create("results", showWarnings = FALSE)
write.csv(sim3_ranks, "results/sim3_rank_persistence_by_replication.csv", row.names = FALSE)
write.csv(rank_memory, "results/sim3_rank_memory_by_replication.csv", row.names = FALSE)
write.csv(rank_memory_summary, "results/sim3_rank_memory_summary.csv", row.names = FALSE)

# Figure 4 ------------------------------------------------------------------
# Panels A-C show the run whose middle-to-late correlation is closest to the
# median over the 20 replications, so the example is typical.
rep_idx <- which.min(abs(sim3_ranks$rho_middle_late - median(sim3_ranks$rho_middle_late)))
run1 <- main_runs[[rep_idx]]

set.seed(seeds[rep_idx])
ids <- sort(sample(default_pars$N, 24))

p_a <- ggplot() +
  geom_line(data = trajectories_df(run1, ids), aes(step, pi, group = agent),
            color = COL$blue, linewidth = 0.3, alpha = 0.3) +
  geom_line(data = data.frame(step = run1$steps, mean_pi = mean_curve(run1)),
            aes(step, mean_pi), color = "dodgerblue4", linewidth = 0.5) +
  step_axis +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Simulation step", y = "EF engagement propensity") +
  theme_pub(8.8)

snapshot <- function(frac, label) {
  col <- which.min(abs(run1$steps - step_at(T_main, frac)))
  data.frame(pi = run1$pi[, col], period = label)
}
snapshots <- bind_rows(snapshot(0.25, "Early"), snapshot(0.55, "Middle"), snapshot(0.90, "Late")) %>%
  mutate(period = factor(period, levels = c("Early", "Middle", "Late")))

p_b <- ggplot(snapshots, aes(pi, color = period)) +
  geom_density(linewidth = 0.32, adjust = 1.1) +
  scale_color_manual(values = c(Early = COL$dark_gray, Middle = COL$blue, Late = COL$orange)) +
  labs(x = "EF engagement propensity", y = "Density") +
  theme_pub(8.8)

scores <- data.frame(middle = window_score(run1, default_windows$middle),
                     late = window_score(run1, default_windows$late)) %>%
  mutate(middle_rank = percent_rank(middle), late_rank = percent_rank(late))

p_c <- ggplot(scores, aes(middle_rank, late_rank)) +
  geom_point(alpha = 0.22, size = 0.7, color = COL$blue) +
  geom_abline(linetype = "dashed", linewidth = 0.3, color = COL$light_gray) +
  annotate("text", x = 0.04, y = 0.96, hjust = 0, vjust = 1, size = 2.7, parse = TRUE,
           label = sprintf("rho == '%.2f'", spearman(scores$middle, scores$late))) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Middle-period rank percentile", y = "Late-period rank percentile") +
  theme_pub(8.8)

p_d <- ggplot(rank_memory_summary, aes(step, median_rho)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = COL$orange, alpha = 0.14) +
  geom_hline(yintercept = 0, linewidth = 0.28, color = COL$light_gray) +
  geom_vline(xintercept = T_main, linetype = "dashed", linewidth = 0.28, color = COL$gray) +
  geom_line(linewidth = 0.5, color = COL$orange) +
  geom_point(size = 0.9, color = COL$orange) +
  coord_cartesian(ylim = c(-0.1, 1)) +
  labs(x = "Simulation step", y = "Correlation with rank at step 1,500") +
  theme_pub(8.8)

fig4 <- ((p_a | p_b) / (p_c | p_d)) + plot_annotation(tag_levels = "A") & panel_tags
save_fig(fig4, "Figure4_identical_starts", width = 7.1, height = 6.4)

# Figure S1: between-agent SD over the long run (first replication) ----------
long1 <- long_runs[[1]]
fig_s1 <- ggplot(data.frame(step = long1$steps, sd_pi = apply(long1$pi, 2, sd)),
                 aes(step, sd_pi)) +
  geom_line(linewidth = 0.3, color = COL$ink) +
  geom_vline(xintercept = T_main, linetype = "dashed", linewidth = 0.3, color = COL$gray) +
  labs(x = "Simulation step", y = "Between-agent SD") +
  theme_pub()

save_fig(fig_s1, "FigureS1_long_run_sd", width = 6.1, height = 3.4)
