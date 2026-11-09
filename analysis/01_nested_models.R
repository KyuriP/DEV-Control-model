# Simulation 1: what does each mechanism add?
# M0 (control allocation only), M1 (+ value learning), M2 (+ practice feedback).
# All agents start identical and share one reward environment.
# Output: Figure 2, results/sim1_*.csv

library(dplyr)
library(tidyr)
source("R/model.R")
source("R/metrics.R")
source("R/plot_style.R")

models <- c("evc", "value", "value_cost")

# 20 replications per model -------------------------------------------------
sim1 <- expand.grid(model = models, seed = seeds, stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(dev_metrics(simulate_dev_control(model, seed = seed))) %>%
  ungroup() %>%
  mutate(model_label = unname(model_labels[model]))

sim1_summary <- sim1 %>%
  group_by(model_label) %>%
  summarize(across(early_mean:rho_early_late,
                   list(median = ~ median(.x, na.rm = TRUE),
                        q25 = ~ quantile(.x, 0.25, na.rm = TRUE),
                        q75 = ~ quantile(.x, 0.75, na.rm = TRUE))),
            .groups = "drop")

dir.create("results", showWarnings = FALSE)
write.csv(sim1, "results/sim1_metrics_by_replication.csv", row.names = FALSE)
write.csv(sim1_summary, "results/sim1_metrics_summary.csv", row.names = FALSE)

# Figure 2 (first replication) ----------------------------------------------
fig_runs <- lapply(models, function(m) simulate_dev_control(m, seed = seeds[1]))
names(fig_runs) <- models

set.seed(seeds[1])
ids <- sample(default_pars$N, 40)

indiv <- bind_rows(lapply(c("value", "value_cost"), function(m) {
  trajectories_df(fig_runs[[m]], ids) %>% mutate(model_label = model_labels[[m]])
}))

means <- bind_rows(lapply(models, function(m) {
  data.frame(step = fig_runs[[m]]$steps,
             mean_pi = mean_curve(fig_runs[[m]]),
             model_label = model_labels[[m]])
}))

fig2 <- ggplot() +
  geom_line(data = indiv,
            aes(step, pi, group = interaction(model_label, agent), color = model_label),
            linewidth = 0.2, alpha = 0.5) +
  geom_line(data = means, aes(step, mean_pi, color = model_label), linewidth = 0.55) +
  scale_color_manual(values = model_cols, breaks = names(model_cols)) +
  step_axis +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.025))) +
  labs(x = "Simulation step", y = "EF engagement propensity") +
  theme_pub()

save_fig(fig2, "Figure2_nested_models", width = 6.8, height = 3.6)
