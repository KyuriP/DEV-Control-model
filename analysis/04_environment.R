# Simulation 4: do small, persistent differences in the reward environment
# make individual differences more stable?
# Each agent gets a fixed reward probability p_i ~ N(p, SD). SD = 0 is the
# shared environment of Simulations 1 and 3 (same seeds, identical runs).
# Output: Figure 5, results/sim4_*.csv

library(dplyr)
library(tidyr)
library(patchwork)
source("R/model.R")
source("R/metrics.R")
source("R/plot_style.R")

sd_grid <- sort(unique(c(seq(0, 0.06, by = 0.003), 0.01, 0.02, 0.04)))

sim4 <- expand.grid(sd_p = sd_grid, seed = seeds) %>%
  rowwise() %>%
  mutate(dev_metrics(simulate_dev_control("value_cost", p_reward_sd = sd_p, seed = seed))) %>%
  ungroup()

sim4_summary <- sim4 %>%
  group_by(sd_p) %>%
  summarize(late_sd = median(late_sd),
            rho_median = median(rho_middle_late),
            rho_q25 = quantile(rho_middle_late, 0.25),
            rho_q75 = quantile(rho_middle_late, 0.75),
            .groups = "drop")

dir.create("results", showWarnings = FALSE)
write.csv(sim4, "results/sim4_environment_by_replication.csv", row.names = FALSE)
write.csv(sim4_summary, "results/sim4_environment_summary.csv", row.names = FALSE)

# Figure 5 ------------------------------------------------------------------
p_a <- ggplot(sim4_summary, aes(sd_p, rho_median)) +
  geom_ribbon(aes(ymin = rho_q25, ymax = rho_q75), fill = COL$blue, alpha = 0.10) +
  geom_line(linewidth = 0.45, color = COL$blue) +
  geom_point(size = 1.2, color = COL$blue) +
  labs(x = "Between-agent SD in reward probability",
       y = "Rank persistence") +
  theme_pub(8.8) +
  theme(plot.margin = margin(6, 6, 5, 12))

# Long run with SD = .02, agents grouped by their fixed reward probability.
T_long <- 4 * (default_pars$T - 1) + 1
env_run <- simulate_dev_control("value_cost", T = T_long, p_reward_sd = 0.02,
                                record_every = 10, seed = seeds[1])

group_labels <- c("Less rewarding environment", "Intermediate environment",
                  "More rewarding environment")
env_group <- cut(env_run$p_i, quantile(env_run$p_i, c(0, 1/3, 2/3, 1)),
                 labels = group_labels, include.lowest = TRUE)
env_cols <- setNames(c(COL$blue, COL$dark_gray, COL$orange), group_labels)

set.seed(seeds[1])
ids <- unlist(lapply(group_labels, function(g) sample(which(env_group == g), 15)))

traj <- trajectories_df(env_run, ids) %>% mutate(group = env_group[agent])
group_means <- bind_rows(lapply(group_labels, function(g) {
  data.frame(step = env_run$steps,
             mean_pi = colMeans(env_run$pi[env_group == g, , drop = FALSE]),
             group = g)
}))

p_b <- ggplot() +
  geom_line(data = traj, aes(step, pi, group = agent, color = group),
            linewidth = 0.2, alpha = 0.3) +
  geom_line(data = group_means, aes(step, mean_pi, color = group), linewidth = 0.5) +
  geom_vline(xintercept = default_pars$T, linetype = "dashed", linewidth = 0.28, color = COL$gray) +
  scale_color_manual(values = env_cols, breaks = group_labels) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Simulation step", y = "EF engagement propensity") +
  theme_pub(8.8) +
  theme(plot.margin = margin(6, 6, 5, 12))

fig5 <- (p_a / p_b) + plot_layout(heights = c(0.75, 1.25)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 9.5),
        plot.tag.position = c(-0.02, 1.02))

save_fig(fig5, "Figure5_persistent_environment", width = 6.8, height = 5.4)
