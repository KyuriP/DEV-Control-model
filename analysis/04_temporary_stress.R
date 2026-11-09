# Simulation 4: temporary contextual demands (stress).
# A temporary increase in the contextual cost X, compared with a reference run
# with the same seed. X only enters the momentary decision; it does not change
# V or C directly.
# Main condition: half-sine pulse, steps 825-878 (55%-58.5% of the run), peak .20.
# Robustness: pulse peak, duration, and onset.
# Output: Figure 5, Figure S3, results/sim4_*.csv

library(dplyr)
library(tidyr)
library(patchwork)
source("R/model.R")
source("R/metrics.R")
source("R/plot_style.R")

T <- default_pars$T
followup_lag <- round(0.15 * T)     # 225 steps after the pulse ends
halfwidth <- round(0.025 * T)       # +/- 38 steps around that point

main_start <- round(0.55 * T)                       # 825
main_duration <- round(0.585 * T) - main_start + 1  # 54 steps (825-878)
main_peak <- 0.20

# Reference runs (no stress), one per seed.
ref <- lapply(seeds, function(s) simulate_dev_control("value_cost", seed = s))

stress_effects <- function(start, duration, peak) {
  end <- start + duration - 1
  bind_rows(lapply(seq_along(seeds), function(i) {
    x <- sine_pulse(T, start, duration, peak)
    sim <- simulate_dev_control("value_cost", extra_cost = x, seed = seeds[i])
    diff <- mean_curve(sim) - mean_curve(ref[[i]])
    during <- sim$steps >= start & sim$steps <= end
    data.frame(
      seed = seeds[i], start = start, duration = duration, peak = peak,
      max_drop = min(diff[during]),
      lag_effect = window_mean_at(sim, end + followup_lag, halfwidth) -
        window_mean_at(ref[[i]], end + followup_lag, halfwidth),
      late_effect = mean(sim$pi[, window_cols(sim, default_windows$late)]) -
        mean(ref[[i]]$pi[, window_cols(ref[[i]], default_windows$late)])
    )
  }))
}

# Robustness grid -----------------------------------------------------------
grid <- bind_rows(
  expand.grid(start = main_start,
              duration = round(main_duration * c(0.5, 1, 2)),
              peak = c(0.10, 0.20, 0.30)),
  expand.grid(start = round(c(0.25, 0.40, 0.70) * T),
              duration = main_duration, peak = main_peak)
)

sim4 <- bind_rows(lapply(seq_len(nrow(grid)), function(g) {
  stress_effects(grid$start[g], grid$duration[g], grid$peak[g])
}))

sim4_summary <- sim4 %>%
  group_by(start, duration, peak) %>%
  summarize(across(c(max_drop, lag_effect, late_effect),
                   list(median = median,
                        q25 = ~ quantile(.x, 0.25),
                        q75 = ~ quantile(.x, 0.75))),
            .groups = "drop") %>%
  mutate(main = start == main_start & duration == main_duration & peak == main_peak)

dir.create("results", showWarnings = FALSE)
write.csv(sim4, "results/sim4_stress_by_replication.csv", row.names = FALSE)
write.csv(sim4_summary, "results/sim4_stress_summary.csv", row.names = FALSE)

# Figure 5 ------------------------------------------------------------------
main_end <- main_start + main_duration - 1
x_main <- sine_pulse(T, main_start, main_duration, main_peak)

stress1 <- simulate_dev_control("value_cost", extra_cost = x_main, seed = seeds[1])
set.seed(seeds[1])
ids <- sort(sample(default_pars$N, 30))

traj <- bind_rows(
  trajectories_df(ref[[1]], ids) %>% mutate(condition = "Reference"),
  trajectories_df(stress1, ids) %>% mutate(condition = "Temporary stress")
)
means <- bind_rows(
  data.frame(step = ref[[1]]$steps, mean_pi = mean_curve(ref[[1]]), condition = "Reference"),
  data.frame(step = stress1$steps, mean_pi = mean_curve(stress1), condition = "Temporary stress")
)
cond_cols <- c("Reference" = COL$dark_gray, "Temporary stress" = COL$orange)

p_a <- ggplot() +
  annotate("rect", xmin = main_start, xmax = main_end, ymin = -Inf, ymax = Inf,
           fill = COL$orange, alpha = 0.08) +
  geom_line(data = traj, aes(step, pi, group = interaction(condition, agent), color = condition),
            linewidth = 0.14, alpha = 0.13) +
  geom_line(data = means, aes(step, mean_pi, color = condition), linewidth = 0.52) +
  annotate("segment", x = main_start, xend = main_end, y = 0.92, yend = 0.92,
           color = COL$orange, linewidth = 1) +
  annotate("text", x = (main_start + main_end) / 2, y = 0.97, label = "Temporary stress",
           color = COL$orange, size = 3) +
  scale_color_manual(values = cond_cols) +
  step_axis +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Simulation step", y = "EF engagement propensity") +
  theme_pub(8.9)

# Inset: zoom on the period after the stress, same replication and agents.
zoom <- c(1000, 1300)
zoom_traj <- filter(traj, step >= zoom[1], step <= zoom[2])
zoom_means <- filter(means, step >= zoom[1], step <= zoom[2])
zoom_y <- range(zoom_means$mean_pi) + c(-0.04, 0.04)

p_inset <- ggplot() +
  geom_line(data = zoom_traj, aes(step, pi, group = interaction(condition, agent), color = condition),
            linewidth = 0.15, alpha = 0.35) +
  geom_line(data = zoom_means, aes(step, mean_pi, color = condition), linewidth = 0.55) +
  scale_color_manual(values = cond_cols) +
  scale_x_continuous(breaks = seq(zoom[1], zoom[2], 100)) +
  coord_cartesian(xlim = zoom, ylim = zoom_y, expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_pub(7) +
  theme(legend.position = "none",
        panel.border = element_rect(color = COL$gray, fill = NA, linewidth = 0.4),
        plot.background = element_rect(fill = "white", color = NA))

# mark the zoomed region in the main panel
p_a <- p_a +
  annotate("rect", xmin = zoom[1], xmax = zoom[2], ymin = zoom_y[1], ymax = zoom_y[2],
           fill = NA, color = COL$gray, linetype = "dashed", linewidth = 0.3)

fig5 <- p_a + inset_element(p_inset, left = 0.03, bottom = 0.47, right = 0.47, top = 0.97)
save_fig(fig5, "Figure5_temporary_stress", width = 6.7, height = 3.9)

# Figure S3: robustness -----------------------------------------------------
dose <- filter(sim4_summary, start == main_start) %>%
  mutate(duration = factor(paste(duration, "steps"),
                           levels = paste(sort(unique(duration)), "steps")))

p_s3a <- ggplot(dose, aes(peak, lag_effect_median, color = duration)) +
  geom_hline(yintercept = 0, linewidth = 0.25, color = COL$gray) +
  geom_pointrange(aes(ymin = lag_effect_q25, ymax = lag_effect_q75),
                  position = position_dodge(width = 0.02), size = 0.2) +
  geom_line(position = position_dodge(width = 0.02), linewidth = 0.35) +
  scale_color_manual(values = c(COL$blue, COL$orange, COL$purple)) +
  labs(x = "Peak contextual cost", y = "Remaining difference\n225 steps after stress") +
  theme_pub(8.8)

onset <- filter(sim4_summary, duration == main_duration, peak == main_peak)

p_s3b <- ggplot(onset, aes(start, lag_effect_median)) +
  geom_hline(yintercept = 0, linewidth = 0.25, color = COL$gray) +
  geom_pointrange(aes(ymin = lag_effect_q25, ymax = lag_effect_q75),
                  color = COL$orange, size = 0.2) +
  geom_line(color = COL$orange, linewidth = 0.35) +
  labs(x = "Stress onset (simulation step)", y = "Remaining difference\n225 steps after stress") +
  theme_pub(8.8)

fig_s3 <- (p_s3a | p_s3b) + plot_annotation(tag_levels = "A") & panel_tags
save_fig(fig_s3, "FigureS3_stress_robustness", width = 7.1, height = 3.2)
