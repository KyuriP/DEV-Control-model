# Simulation 6: does the same supportive experience matter more when it comes earlier?
# Support = temporary increase in the probability that engagement pays off,
# compared with a reference run with the same seed.
# Main conditions: half-sine pulse lasting 195 steps (13% of the run), peak +.12,
# starting at step 300 (early) or 900 (later). Effects are measured the same
# number of steps after each pulse ends.
# Robustness: pulse onset, size, duration, and follow-up lag.
# Output: Figure 7, Figure S4, results/sim6_*.csv

library(dplyr)
library(tidyr)
library(patchwork)
source("R/model.R")
source("R/metrics.R")
source("R/plot_style.R")

T <- default_pars$T
main_duration <- round(0.13 * T)     # 195 steps
main_boost <- 0.12
early_start <- round(0.20 * T)       # 300
later_start <- round(0.60 * T)       # 900
main_lag <- round(0.15 * T)          # 225 steps
halfwidth <- round(0.025 * T)        # +/- 38 steps
lags <- c(100, main_lag, 350)

ref <- lapply(seeds, function(s) simulate_dev_control("value_cost", seed = s))

support_effects <- function(start, duration, boost) {
  end <- start + duration - 1
  ok_lags <- lags[end + lags + halfwidth <= T]
  bind_rows(lapply(seq_along(seeds), function(i) {
    sim <- simulate_dev_control("value_cost", seed = seeds[i],
                                p_shift = sine_pulse(T, start, duration, boost))
    data.frame(
      seed = seeds[i], start = start, end = end, duration = duration, boost = boost,
      lag = ok_lags,
      effect = sapply(ok_lags, function(l) {
        window_mean_at(sim, end + l, halfwidth) - window_mean_at(ref[[i]], end + l, halfwidth)
      })
    )
  }))
}

# Conditions ----------------------------------------------------------------
grid <- bind_rows(
  # onset sweep, default pulse
  expand.grid(start = round(seq(0.05, 0.65, by = 0.05) * T),
              duration = main_duration, boost = main_boost),
  # size x duration at an early and a middle onset
  expand.grid(start = round(c(0.20, 0.50) * T),
              duration = round(main_duration * c(0.5, 1, 2)),
              boost = c(0.06, 0.12, 0.24))
) %>%
  distinct()

sim6 <- bind_rows(lapply(seq_len(nrow(grid)), function(g) {
  support_effects(grid$start[g], grid$duration[g], grid$boost[g])
}))

sim6_summary <- sim6 %>%
  group_by(start, end, duration, boost, lag) %>%
  summarize(effect_median = median(effect),
            effect_q25 = quantile(effect, 0.25),
            effect_q75 = quantile(effect, 0.75),
            .groups = "drop")

dir.create("results", showWarnings = FALSE)
write.csv(sim6, "results/sim6_support_by_replication.csv", row.names = FALSE)
write.csv(sim6_summary, "results/sim6_support_summary.csv", row.names = FALSE)

# Figure 7 ------------------------------------------------------------------
early_end <- early_start + main_duration - 1
later_end <- later_start + main_duration - 1
sup_cols <- c("Reference" = COL$dark_gray, "Early support" = COL$orange,
              "Later support" = COL$blue)

early1 <- simulate_dev_control("value_cost", seed = seeds[1],
                               p_shift = sine_pulse(T, early_start, main_duration, main_boost))
later1 <- simulate_dev_control("value_cost", seed = seeds[1],
                               p_shift = sine_pulse(T, later_start, main_duration, main_boost))

set.seed(seeds[1])
ids <- sort(sample(default_pars$N, 30))
runs1 <- list("Reference" = ref[[1]], "Early support" = early1, "Later support" = later1)

traj <- bind_rows(lapply(names(runs1), function(nm) {
  trajectories_df(runs1[[nm]], ids) %>% mutate(condition = nm)
}))
means <- bind_rows(lapply(names(runs1), function(nm) {
  data.frame(step = runs1[[nm]]$steps, mean_pi = mean_curve(runs1[[nm]]), condition = nm)
}))

p_a <- ggplot() +
  annotate("rect", xmin = early_start, xmax = early_end, ymin = -Inf, ymax = Inf,
           fill = COL$orange, alpha = 0.06) +
  annotate("rect", xmin = later_start, xmax = later_end, ymin = -Inf, ymax = Inf,
           fill = COL$blue, alpha = 0.06) +
  geom_line(data = traj, aes(step, pi, group = interaction(condition, agent), color = condition),
            linewidth = 0.14, alpha = 0.14) +
  geom_line(data = means, aes(step, mean_pi, color = condition), linewidth = 0.5) +
  scale_color_manual(values = sup_cols, breaks = names(sup_cols)) +
  step_axis +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Simulation step", y = "EF engagement propensity") +
  theme_pub(8.9)

# B: effect over time since the pulse ended, all replications.
aligned <- bind_rows(lapply(seq_along(seeds), function(i) {
  bind_rows(lapply(c(early_start, later_start), function(s) {
    sim <- simulate_dev_control("value_cost", seed = seeds[i],
                                p_shift = sine_pulse(T, s, main_duration, main_boost))
    data.frame(seed = seeds[i],
               condition = if (s == early_start) "Early support" else "Later support",
               since_end = sim$steps - (s + main_duration - 1),
               diff = mean_curve(sim) - mean_curve(ref[[i]]))
  }))
})) %>%
  filter(since_end >= 0, since_end <= T - later_end) %>%
  group_by(condition, since_end) %>%
  summarize(median = median(diff), q25 = quantile(diff, 0.25), q75 = quantile(diff, 0.75),
            .groups = "drop")

p_b <- ggplot(aligned, aes(since_end, median, color = condition, fill = condition)) +
  geom_hline(yintercept = 0, linewidth = 0.25, color = COL$gray) +
  geom_vline(xintercept = main_lag, linetype = "dashed", linewidth = 0.25, color = COL$gray) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.45) +
  scale_color_manual(values = sup_cols) +
  scale_fill_manual(values = sup_cols) +
  labs(x = "Steps since support ended", y = "Support minus reference") +
  theme_pub(8.9) +
  theme(legend.position = "none")

# C: effect at the same follow-up lag, for every onset.
onset <- filter(sim6_summary, duration == main_duration, boost == main_boost, lag == main_lag)

p_c <- ggplot(onset, aes(start, effect_median)) +
  geom_hline(yintercept = 0, linewidth = 0.25, color = COL$gray) +
  geom_pointrange(aes(ymin = effect_q25, ymax = effect_q75), color = COL$dark_gray, size = 0.2) +
  geom_line(color = COL$dark_gray, linewidth = 0.35) +
  geom_point(data = filter(onset, start %in% c(early_start, later_start)),
             aes(color = if_else(start == early_start, "Early support", "Later support")),
             size = 1.8) +
  scale_color_manual(values = sup_cols) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = "Support onset (simulation step)", y = "Effect 225 steps\nafter support ended") +
  theme_pub(8.9) +
  theme(legend.position = "none")

fig7 <- (p_a / (p_b | p_c)) + plot_layout(heights = c(1.3, 1)) +
  plot_annotation(tag_levels = "A") & panel_tags
save_fig(fig7, "Figure7_support_timing", width = 7.1, height = 6)

# Figure S4: size, duration, and follow-up lag ------------------------------
size_dur <- filter(sim6_summary, start %in% round(c(0.20, 0.50) * T), lag == main_lag) %>%
  mutate(onset = paste("Onset at step", start),
         duration = factor(paste(duration, "steps"),
                           levels = paste(sort(unique(duration)), "steps")))

p_s4a <- ggplot(size_dur, aes(boost, effect_median, color = duration)) +
  geom_hline(yintercept = 0, linewidth = 0.25, color = COL$gray) +
  geom_pointrange(aes(ymin = effect_q25, ymax = effect_q75),
                  position = position_dodge(width = 0.012), size = 0.2) +
  geom_line(position = position_dodge(width = 0.012), linewidth = 0.35) +
  facet_wrap(~ onset) +
  scale_x_continuous(breaks = c(0.06, 0.12, 0.24)) +
  scale_color_manual(values = c(COL$blue, COL$orange, COL$purple)) +
  labs(x = "Peak increase in reward probability", y = "Effect 225 steps\nafter support ended") +
  theme_pub(8.8) +
  theme(panel.spacing = grid::unit(1.5, "lines"))

lag_df <- filter(sim6_summary, start %in% c(early_start, later_start),
                 duration == main_duration, boost == main_boost) %>%
  mutate(condition = if_else(start == early_start, "Early support", "Later support"))

p_s4b <- ggplot(lag_df, aes(lag, effect_median, color = condition)) +
  geom_hline(yintercept = 0, linewidth = 0.25, color = COL$gray) +
  geom_pointrange(aes(ymin = effect_q25, ymax = effect_q75),
                  position = position_dodge(width = 12), size = 0.2) +
  geom_line(position = position_dodge(width = 12), linewidth = 0.35) +
  scale_color_manual(values = sup_cols) +
  labs(x = "Follow-up lag (steps after support ended)", y = "Support minus reference") +
  theme_pub(8.8)

fig_s4 <- (p_s4a / p_s4b) + plot_annotation(tag_levels = "A") & panel_tags
save_fig(fig_s4, "FigureS4_support_robustness", width = 7.1, height = 6)
