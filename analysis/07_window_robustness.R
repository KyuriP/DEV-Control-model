# Robustness: do the Simulation 1 conclusions depend on where the
# developmental periods are placed?
# Same runs as Simulation 1 (3 models x 20 seeds), summarized with five
# different sets of early / middle / late windows (fractions of the run).
# Output: Figure S2, results/robustness_windows_*.csv

library(dplyr)
library(tidyr)
source("R/model.R")
source("R/metrics.R")
source("R/plot_style.R")

window_sets <- list(
  "Default (20-30, 50-60, 85-95%)" = default_windows,
  "Shifted (20-30, 53-63, 87-97%)" = list(early = c(0.20, 0.30), middle = c(0.5333, 0.6333), late = c(0.8667, 0.9667)),
  "Equal thirds"                    = list(early = c(0, 1/3), middle = c(1/3, 2/3), late = c(2/3, 1)),
  "Wide (15-35, 45-65, 80-100%)"   = list(early = c(0.15, 0.35), middle = c(0.45, 0.65), late = c(0.80, 1)),
  "Narrow (22.5-27.5, 52.5-57.5, 87.5-92.5%)" = list(early = c(0.225, 0.275), middle = c(0.525, 0.575), late = c(0.875, 0.925))
)

models <- c("evc", "value", "value_cost")

win <- bind_rows(lapply(models, function(m) {
  bind_rows(lapply(seeds, function(s) {
    sim <- simulate_dev_control(m, seed = s)
    bind_rows(lapply(names(window_sets), function(w) {
      cbind(model_label = model_labels[[m]], seed = s, windows = w,
            dev_metrics(sim, window_sets[[w]]))
    }))
  }))
}))

win_summary <- win %>%
  group_by(model_label, windows) %>%
  summarize(across(c(delta_mean, sd_gain, rho_middle_late, rho_early_late),
                   ~ median(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(windows = factor(windows, levels = names(window_sets))) %>%
  arrange(model_label, windows)

dir.create("results", showWarnings = FALSE)
write.csv(win, "results/robustness_windows_by_replication.csv", row.names = FALSE)
write.csv(win_summary, "results/robustness_windows_summary.csv", row.names = FALSE)

# Figure S2 -----------------------------------------------------------------
long <- win %>%
  filter(model_label != model_labels[["evc"]]) %>%
  pivot_longer(c(delta_mean, sd_gain, rho_middle_late), names_to = "measure") %>%
  mutate(measure = recode(measure,
                          delta_mean = "Change in mean engagement",
                          sd_gain = "Change in between-agent SD",
                          rho_middle_late = "Middle-to-late rank persistence"),
         measure = factor(measure, levels = c("Change in mean engagement",
                                              "Change in between-agent SD",
                                              "Middle-to-late rank persistence")),
         windows = factor(windows, levels = rev(names(window_sets))))

fig_s2 <- ggplot(long, aes(value, windows, color = model_label)) +
  geom_vline(xintercept = 0, linewidth = 0.25, color = COL$light_gray) +
  geom_boxplot(outlier.size = 0.4, linewidth = 0.3, width = 0.6,
               position = position_dodge(width = 0.7)) +
  facet_wrap(~ measure, scales = "free_x") +
  scale_color_manual(values = model_cols[-1]) +
  labs(x = NULL, y = NULL) +
  theme_pub(8.5)

save_fig(fig_s2, "FigureS2_window_robustness", width = 7.4, height = 3.4)
