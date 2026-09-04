# publication_figures.R
# DEV-Control / Park & Munakata
#
# Purpose
# -------
# Recreate the main manuscript figures with one consistent publication theme.
# The script is intentionally separate from the analysis Quarto file so that
# figure styling can change without changing the underlying model analyses.
#
# Main outputs
# ------------
# Figure 1  Model schematic
# Figure 2  Nested models: developmental mean + differentiation
# Figure 3  Emergence and persistence from identical starts
# Figure 4  Persistent reward environments stabilize rank ordering
# Figure 5  Temporary contextual perturbation
# Figure 6  Early versus later support
# Figure 7  Parameter robustness
#
# Each figure is exported as PDF and 600-dpi PNG. SVG is also exported when
# the svglite package is available.
#
# IMPORTANT
# ---------
# Developmental windows are CONSISTENT throughout:
#   early  = 20-30%
#   middle = 50-60%
#   late   = 85-95%
#
# The default stress/context perturbation follows the current simulation code:
#   X = .20 from 55.0% to 58.5% of the developmental horizon.
# If the manuscript currently says 60-63.5%, update the manuscript or change
# STRESS_START_FRAC / STRESS_END_FRAC below before final submission.

# -----------------------------------------------------------------------------
# 0. Packages and user settings
# -----------------------------------------------------------------------------

required_pkgs <- c("ggplot2", "dplyr", "tidyr", "purrr", "scales", "patchwork")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Install the following packages before running this script: ",
    paste(missing_pkgs, collapse = ", ")
  )
}

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(scales)
library(patchwork)

# Directory containing optional precomputed sweep CSV files.
# Default: current working directory.
DATA_DIR <- Sys.getenv("EF_FIG_DATA_DIR", unset = ".")
OUT_DIR <- file.path(DATA_DIR, "publication_figures")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# If the consistent-window sweep CSV is unavailable, the script can recreate
# the sweep. Set to FALSE if you prefer the script to stop instead.
RECOMPUTE_SWEEP_IF_MISSING <- TRUE

# Current contextual perturbation used in the Quarto simulation.
STRESS_START_FRAC <- 0.55
STRESS_END_FRAC   <- 0.585
STRESS_COST       <- 0.20

# -----------------------------------------------------------------------------
# 1. Publication style
# -----------------------------------------------------------------------------

# Restrained palette: neutral structure + a few muted accents.
COL <- list(
  ink        = "#222222",
  dark_gray  = "#555555",
  gray       = "#8B8B8B",
  light_gray = "#D9D9D9",
  pale_gray  = "#F2F2F2",
  blue       = "#4C78A8",
  green      = "#4F8A6D",
  orange     = "#D97745",
  purple     = "#9A6EA6"
)

model_cols <- c(
  "M0: Control allocation only" = COL$dark_gray,
  "M1: + value learning"        = COL$blue,
  "M2: + practice feedback"     = COL$orange
)

snapshot_cols <- c(
  "Early"  = COL$dark_gray,
  "Middle" = COL$blue,
  "Late"   = COL$orange
)

environment_cols <- c(
  "Less rewarding environment"  = COL$blue,
  "Intermediate environment"    = COL$dark_gray,
  "More rewarding environment"  = COL$orange
)

context_cols <- c(
  "Reference"               = COL$dark_gray,
  "Contextual perturbation" = COL$orange
)

support_cols <- c(
  "Early support" = COL$orange,
  "Later support" = COL$blue
)

# Thin line hierarchy. Population means should never visually overpower
# the stochastic structure of the simulations.
LW_MEAN  <- 0.34
LW_GROUP <- 0.40
LW_INDIV <- 0.15
ALPHA_INDIV <- 0.08

theme_pub <- function(base_size = 9.2) {
  theme_classic(base_size = base_size, base_family = "sans") +
    theme(
      text = element_text(color = COL$ink),
      axis.text = element_text(color = COL$ink, size = rel(0.92)),
      axis.title = element_text(color = COL$ink, size = rel(0.98)),
      axis.line = element_line(linewidth = 0.28, color = COL$ink),
      axis.ticks = element_line(linewidth = 0.28, color = COL$ink),
      axis.ticks.length = grid::unit(2.0, "pt"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = rel(0.88)),
      legend.key.width = grid::unit(12, "pt"),
      legend.key.height = grid::unit(7, "pt"),
      strip.background = element_blank(),
      strip.text = element_text(face = "plain", hjust = 0),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      plot.margin = margin(5, 6, 5, 5)
    )
}

panel_tag_theme <- theme(
  plot.tag = element_text(face = "bold", size = 9.5),
  plot.tag.position = c(0.01, 0.99)
)

save_pub <- function(plot, filename, width, height) {
  pdf_file <- file.path(OUT_DIR, paste0(filename, ".pdf"))
  png_file <- file.path(OUT_DIR, paste0(filename, ".png"))

  pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf

  ggsave(
    pdf_file,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    device = pdf_device
  )

  ggsave(
    png_file,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 600,
    bg = "white"
  )

  if (requireNamespace("svglite", quietly = TRUE)) {
    ggsave(
      file.path(OUT_DIR, paste0(filename, ".svg")),
      plot = plot,
      width = width,
      height = height,
      units = "in",
      device = svglite::svglite
    )
  }

  invisible(plot)
}

# -----------------------------------------------------------------------------
# 2. Model and simulation helpers
# -----------------------------------------------------------------------------

clip <- function(x, lo = 0, hi = 1) pmin(pmax(x, lo), hi)

episode_at_fraction <- function(T, fraction) {
  round(fraction * (T - 1)) + 1L
}

default_pars <- list(
  N = 800,
  T = 1500,
  V0 = 0.577,
  C0 = 0.987,
  tau = 0.163,
  p_reward = 0.591,
  reward_mag = 1.150,
  alpha_V = 0.0244,
  alpha_C = 0.00263,
  C_min = 0.459,
  record_every = 5
)

model_labels <- c(
  evc        = "M0: Control allocation only",
  value      = "M1: + value learning",
  value_cost = "M2: + practice feedback"
)

simulate_population <- function(
  model = c("evc", "value", "value_cost"),
  N = default_pars$N,
  T = default_pars$T,
  V0 = default_pars$V0,
  C0 = default_pars$C0,
  tau = default_pars$tau,
  p_reward = default_pars$p_reward,
  p_reward_sd = 0,
  reward_mag = default_pars$reward_mag,
  alpha_V = default_pars$alpha_V,
  alpha_C = default_pars$alpha_C,
  C_min = default_pars$C_min,
  p_reward_shift = NULL,
  extra_cost = NULL,
  record_every = default_pars$record_every,
  seed = 1
) {

  model <- match.arg(model)

  if (is.null(p_reward_shift)) p_reward_shift <- rep(0, T)
  if (is.null(extra_cost)) extra_cost <- rep(0, T)

  stopifnot(length(p_reward_shift) == T, length(extra_cost) == T)
  stopifnot(N >= 2, T >= 2, tau > 0, C_min < C0)

  set.seed(seed)

  child_id <- seq_len(N)
  p_i <- clip(rnorm(N, mean = p_reward, sd = p_reward_sd))
  V <- rep(V0, N)
  C <- rep(C0, N)

  out <- vector("list", ceiling(T / record_every) + 2)
  k <- 1L

  for (tt in seq_len(T)) {
    p_t <- clip(p_i + p_reward_shift[tt])

    decision_value <- V - C - extra_cost[tt]
    pi_engage <- plogis(decision_value / tau)
    A <- as.integer(runif(N) < pi_engage)

    reward_success <- as.integer(runif(N) < p_t)
    reward_observed <- A * reward_success * reward_mag

    if (tt == 1 || tt %% record_every == 0 || tt == T) {
      out[[k]] <- tibble(
        child_id = child_id,
        step = tt,
        p_reward_child = p_i,
        p_reward_used = p_t,
        extra_cost = extra_cost[tt],
        V = V,
        C = C,
        pi = pi_engage,
        A = A,
        reward = reward_observed
      )
      k <- k + 1L
    }

    if (model %in% c("value", "value_cost")) {
      V <- V + alpha_V * A * (reward_observed - V)
    }

    if (model == "value_cost") {
      C <- C - alpha_C * A * (C - C_min)
    }

    V <- clip(V, 0, reward_mag)
    C <- pmax(C, C_min)
  }

  bind_rows(out[seq_len(k - 1L)]) %>%
    mutate(
      model = model,
      model_label = unname(model_labels[model])
    )
}

population_summary <- function(df) {
  df %>%
    group_by(model_label, step) %>%
    summarize(
      mean_pi = mean(pi),
      sd_pi = sd(pi),
      .groups = "drop"
    )
}

window_score <- function(df, lo, hi, label) {
  T_end <- max(df$step)
  lo_step <- episode_at_fraction(T_end, lo)
  hi_step <- episode_at_fraction(T_end, hi)

  df %>%
    filter(step >= lo_step, step <= hi_step) %>%
    group_by(child_id) %>%
    summarize(score = mean(pi), .groups = "drop") %>%
    mutate(window = label)
}

stability_table <- function(df) {
  scores <- bind_rows(
    window_score(df, 0.20, 0.30, "Early"),
    window_score(df, 0.50, 0.60, "Middle"),
    window_score(df, 0.85, 0.95, "Late")
  ) %>%
    pivot_wider(names_from = window, values_from = score)

  tibble(
    comparison = c("Early vs Middle", "Middle vs Late", "Early vs Late"),
    spearman_rho = suppressWarnings(c(
      cor(scores$Early, scores$Middle, method = "spearman"),
      cor(scores$Middle, scores$Late, method = "spearman"),
      cor(scores$Early, scores$Late, method = "spearman")
    ))
  )
}

development_metrics <- function(df) {
  T_end <- max(df$step)

  early_lo <- episode_at_fraction(T_end, 0.20)
  early_hi <- episode_at_fraction(T_end, 0.30)
  late_lo  <- episode_at_fraction(T_end, 0.85)
  late_hi  <- episode_at_fraction(T_end, 0.95)

  early <- df %>% filter(step >= early_lo, step <= early_hi)
  late  <- df %>% filter(step >= late_lo, step <= late_hi)

  early_sd <- early %>%
    group_by(step) %>%
    summarize(x = sd(pi), .groups = "drop") %>%
    summarize(x = mean(x, na.rm = TRUE)) %>%
    pull(x)

  late_sd <- late %>%
    group_by(step) %>%
    summarize(x = sd(pi), .groups = "drop") %>%
    summarize(x = mean(x, na.rm = TRUE)) %>%
    pull(x)

  stab <- stability_table(df)

  tibble(
    early_mean = mean(early$pi),
    late_mean = mean(late$pi),
    delta_mean = late_mean - early_mean,
    early_sd = early_sd,
    late_sd = late_sd,
    sd_gain = late_sd - early_sd,
    rho_middle_late = stab$spearman_rho[stab$comparison == "Middle vs Late"],
    rho_early_late = stab$spearman_rho[stab$comparison == "Early vs Late"]
  )
}

get_snapshot <- function(df, target, label) {
  target_step <- episode_at_fraction(max(df$step), target)

  df %>%
    group_by(child_id) %>%
    slice_min(order_by = abs(step - target_step), n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(snapshot = label)
}

rank_percentile <- function(x) {
  if (length(unique(x)) <= 1) return(rep(0.5, length(x)))
  dplyr::percent_rank(x)
}

sample_n_safe <- function(df, k = 5) {
  dplyr::slice_sample(df, n = min(k, nrow(df)))
}

# Fast consistent-window simulator used only if the sweep CSV is missing.
simulate_development_metrics_fast <- function(
  N, T, V0, C0, tau, p_reward,
  reward_mag = default_pars$reward_mag,
  alpha_V, alpha_C, C_min,
  record_every = 10,
  seed = 1
) {
  set.seed(seed)

  V <- rep(V0, N)
  C <- rep(C0, N)

  early_sum <- late_sum <- 0
  early_n <- late_n <- 0L
  early_sd_sum <- late_sd_sum <- 0
  early_sd_n <- late_sd_n <- 0L

  early_child_sum <- middle_child_sum <- late_child_sum <- numeric(N)
  early_child_n <- middle_child_n <- late_child_n <- 0L

  early_lo  <- episode_at_fraction(T, 0.20)
  early_hi  <- episode_at_fraction(T, 0.30)
  middle_lo <- episode_at_fraction(T, 0.50)
  middle_hi <- episode_at_fraction(T, 0.60)
  late_lo   <- episode_at_fraction(T, 0.85)
  late_hi   <- episode_at_fraction(T, 0.95)

  for (tt in seq_len(T)) {
    pi_engage <- plogis((V - C) / tau)
    record_this <- tt == 1 || tt %% record_every == 0 || tt == T

    if (record_this) {
      if (tt >= early_lo && tt <= early_hi) {
        early_sum <- early_sum + sum(pi_engage)
        early_n <- early_n + N
        early_sd_sum <- early_sd_sum + sd(pi_engage)
        early_sd_n <- early_sd_n + 1L
        early_child_sum <- early_child_sum + pi_engage
        early_child_n <- early_child_n + 1L
      }
      if (tt >= middle_lo && tt <= middle_hi) {
        middle_child_sum <- middle_child_sum + pi_engage
        middle_child_n <- middle_child_n + 1L
      }
      if (tt >= late_lo && tt <= late_hi) {
        late_sum <- late_sum + sum(pi_engage)
        late_n <- late_n + N
        late_sd_sum <- late_sd_sum + sd(pi_engage)
        late_sd_n <- late_sd_n + 1L
        late_child_sum <- late_child_sum + pi_engage
        late_child_n <- late_child_n + 1L
      }
    }

    A <- as.integer(runif(N) < pi_engage)
    reward_success <- as.integer(runif(N) < p_reward)
    reward_observed <- A * reward_success * reward_mag

    V <- V + alpha_V * A * (reward_observed - V)
    C <- C - alpha_C * A * (C - C_min)
    V <- clip(V, 0, reward_mag)
    C <- pmax(C, C_min)
  }

  early_scores <- early_child_sum / early_child_n
  middle_scores <- middle_child_sum / middle_child_n
  late_scores <- late_child_sum / late_child_n

  early_mean <- early_sum / early_n
  late_mean <- late_sum / late_n
  early_sd <- early_sd_sum / early_sd_n
  late_sd <- late_sd_sum / late_sd_n

  tibble(
    delta_mean = late_mean - early_mean,
    sd_gain = late_sd - early_sd,
    rho_middle_late = suppressWarnings(cor(middle_scores, late_scores, method = "spearman"))
  )
}

# -----------------------------------------------------------------------------
# 3. FIGURE 1: DEV-Control model schematic
# -----------------------------------------------------------------------------

# Keep the schematic deliberately sparse: variables are in boxes; arrows carry
# only the minimum process labels needed to understand the loop.
node_df <- tribble(
  ~node, ~x, ~y, ~label, ~fill,
  "V", 1.2, 3.45, "Learned expected payoff\nV(t)",        "slow",
  "C", 1.2, 2.05, "Effective effort cost\nC(t)",          "slow",
  "X", 1.2, 0.65, "Temporary context\nX(t)",              "external",
  "D", 3.45, 2.05, "Net value of\nengaging control",      "decision",
  "pi", 5.15, 2.05, "Engagement propensity\nπ(t)",        "choice",
  "A", 6.85, 2.05, "Actual engagement\nA(t)",             "choice",
  "P", 5.15, 0.55, "Reward environment\np(t)",            "external",
  "R", 6.85, 0.55, "Experienced outcome\nR(t)",           "outcome"
)

node_fills <- c(
  slow     = "#F3E8D7",
  external = COL$pale_gray,
  decision = "#E9F0E8",
  choice   = "#E8EEF5",
  outcome  = "#F0E7F0"
)

# Straight feed-forward structure
edge_df <- tribble(
  ~x, ~y, ~xend, ~yend,
  1.95, 3.35, 2.85, 2.35,   # V -> D
  1.95, 2.05, 2.80, 2.05,   # C -> D
  1.95, 0.80, 2.85, 1.78,   # X -> D
  4.05, 2.05, 4.55, 2.05,   # D -> pi
  5.75, 2.05, 6.25, 2.05,   # pi -> A
  5.80, 0.55, 6.25, 0.55,   # P -> R
  6.85, 1.55, 6.85, 1.05    # A -> R
)

fig1 <- ggplot() +
  geom_segment(
    data = edge_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 0.38,
    color = COL$dark_gray,
    arrow = grid::arrow(length = grid::unit(4.2, "pt"), type = "closed")
  ) +
  # Feedback from outcome to learned payoff
  geom_curve(
    aes(x = 6.45, y = 0.72, xend = 1.85, yend = 3.58),
    curvature = 0.36,
    linewidth = 0.38,
    color = COL$dark_gray,
    arrow = grid::arrow(length = grid::unit(4.2, "pt"), type = "closed")
  ) +
  # Feedback from engagement to effort cost
  geom_curve(
    aes(x = 6.45, y = 1.88, xend = 1.85, yend = 2.15),
    curvature = -0.22,
    linewidth = 0.38,
    color = COL$dark_gray,
    arrow = grid::arrow(length = grid::unit(4.2, "pt"), type = "closed")
  ) +
  geom_label(
    data = node_df,
    aes(x = x, y = y, label = label, fill = fill),
    size = 2.85,
    lineheight = 0.95,
    linewidth = 0.28,
    color = COL$ink,
    label.padding = grid::unit(0.18, "lines")
  ) +
  scale_fill_manual(values = node_fills, guide = "none") +
  annotate("text", x = 4.15, y = 3.72, label = "outcomes update value",
           size = 2.45, color = COL$dark_gray) +
  annotate("text", x = 4.20, y = 1.30, label = "practice lowers future cost",
           size = 2.45, color = COL$dark_gray) +
  coord_cartesian(xlim = c(0.35, 7.65), ylim = c(0.15, 3.95), clip = "off") +
  theme_void(base_size = 9.2) +
  theme(plot.margin = margin(6, 8, 6, 8))

save_pub(fig1, "Figure1_DEV_Control_schematic", width = 7.1, height = 3.7)


# -----------------------------------------------------------------------------
# 4. FIGURE 2: Nested models
# -----------------------------------------------------------------------------

core_models <- c("evc", "value", "value_cost")

df_models <- bind_rows(lapply(core_models, function(m) {
  simulate_population(model = m, p_reward_sd = 0, seed = 4100)
}))

summary_models <- population_summary(df_models)

model_metrics <- bind_rows(
  lapply(split(df_models, df_models$model_label), development_metrics),
  .id = "model_label"
)

# Light bands indicate the common early and late summary windows.
T_fig2 <- max(summary_models$step)
early_band <- c(episode_at_fraction(T_fig2, 0.20), episode_at_fraction(T_fig2, 0.30))
late_band  <- c(episode_at_fraction(T_fig2, 0.85), episode_at_fraction(T_fig2, 0.95))

window_bands <- function() {
  list(
    annotate("rect", xmin = early_band[1], xmax = early_band[2],
             ymin = -Inf, ymax = Inf, fill = COL$pale_gray, alpha = 0.55),
    annotate("rect", xmin = late_band[1], xmax = late_band[2],
             ymin = -Inf, ymax = Inf, fill = COL$pale_gray, alpha = 0.55)
  )
}

p2a <- ggplot(summary_models, aes(step, mean_pi, color = model_label)) +
  window_bands() +
  geom_line(linewidth = LW_MEAN) +
  scale_color_manual(values = model_cols) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.03))) +
  labs(x = NULL, y = "Mean engagement propensity") +
  theme_pub(base_size = 9) +
  theme(legend.position = "none")

p2b <- ggplot(summary_models, aes(step, sd_pi, color = model_label)) +
  window_bands() +
  geom_line(linewidth = LW_MEAN) +
  scale_color_manual(values = model_cols) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.03))) +
  labs(x = "Developmental episode", y = "Between-agent SD") +
  theme_pub(base_size = 9)

fig2 <- (p2a / p2b) +
  plot_annotation(tag_levels = "A") &
  panel_tag_theme

save_pub(fig2, "Figure2_nested_models", width = 6.8, height = 4.8)

write.csv(
  model_metrics,
  file.path(OUT_DIR, "Figure2_model_metrics.csv"),
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 5. FIGURE 3: Emergence + persistence from identical starts
# -----------------------------------------------------------------------------

df_identical <- simulate_population(
  model = "value_cost",
  N = 1000,
  p_reward_sd = 0,
  seed = 20260819
)

stability_identical <- stability_table(df_identical)

# A: randomly selected individual trajectories. No post-hoc high/low grouping is
# used here; the panel is meant only to show divergence from identical starts.
set.seed(20260820)
display_children <- sort(sample(unique(df_identical$child_id), 24))

trajectory_df <- df_identical %>%
  filter(child_id %in% display_children)

trajectory_mean <- df_identical %>%
  group_by(step) %>%
  summarize(mean_pi = mean(pi), .groups = "drop")

p3a <- ggplot() +
  geom_line(
    data = trajectory_df,
    aes(step, pi, group = child_id),
    color = COL$gray, linewidth = 0.18, alpha = 0.30
  ) +
  geom_line(
    data = trajectory_mean,
    aes(step, mean_pi),
    color = COL$ink, linewidth = LW_GROUP
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Developmental episode", y = "Engagement propensity") +
  theme_pub(base_size = 8.8) +
  theme(legend.position = "none")

# B: population distributions at the midpoints of the common windows.
snapshots <- bind_rows(
  get_snapshot(df_identical, 0.25, "Early"),
  get_snapshot(df_identical, 0.55, "Middle"),
  get_snapshot(df_identical, 0.90, "Late")
) %>%
  mutate(snapshot = factor(snapshot, levels = c("Early", "Middle", "Late")))

p3b <- ggplot(snapshots, aes(pi, color = snapshot)) +
  geom_density(linewidth = LW_GROUP, adjust = 1.1) +
  scale_color_manual(values = snapshot_cols) +
  labs(x = "Engagement propensity", y = "Density") +
  theme_pub(base_size = 8.8) +
  theme(legend.position = "bottom")

# C: rank persistence.
scores_identical <- bind_rows(
  window_score(df_identical, 0.50, 0.60, "Middle"),
  window_score(df_identical, 0.85, 0.95, "Late")
) %>%
  pivot_wider(names_from = window, values_from = score) %>%
  mutate(
    middle_rank = rank_percentile(Middle),
    late_rank = rank_percentile(Late)
  )

rho_ml <- stability_identical$spearman_rho[
  stability_identical$comparison == "Middle vs Late"
]

p3c <- ggplot(scores_identical, aes(middle_rank, late_rank)) +
  geom_point(alpha = 0.20, size = 0.65, color = COL$dark_gray) +
  geom_abline(linetype = "dashed", linewidth = 0.30, color = COL$gray) +
  annotate(
    "text", x = 0.04, y = 0.96,
    label = paste0("ρ = ", sprintf("%.2f", rho_ml)),
    hjust = 0, vjust = 1, size = 2.7
  ) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Middle-stage rank percentile", y = "Late-stage rank percentile") +
  theme_pub(base_size = 8.8)

# D: long-run fading of earlier rank ordering.
T_long <- 4 * (default_pars$T - 1) + 1

df_long <- simulate_population(
  model = "value_cost",
  N = 600,
  T = T_long,
  p_reward_sd = 0,
  record_every = 10,
  seed = 1400
)

reference_rank_scores <- df_long %>%
  filter(
    step >= round(0.80 * (default_pars$T - 1)) + 1,
    step <= round(0.95 * (default_pars$T - 1)) + 1
  ) %>%
  group_by(child_id) %>%
  summarize(reference_score = mean(pi), .groups = "drop")

memory_centers <- seq(0.90, 4.00, by = 0.20)
memory_half_width <- 0.06

rank_memory <- bind_rows(lapply(memory_centers, function(center_i) {
  center_step <- round(center_i * (default_pars$T - 1)) + 1
  half_width_steps <- round(memory_half_width * (default_pars$T - 1))

  later_scores <- df_long %>%
    filter(step >= center_step - half_width_steps, step <= center_step + half_width_steps) %>%
    group_by(child_id) %>%
    summarize(later_score = mean(pi), .groups = "drop")

  joined <- inner_join(reference_rank_scores, later_scores, by = "child_id")

  tibble(
    episode = center_step,
    rank_memory = cor(joined$reference_score, joined$later_score, method = "spearman")
  )
}))

p3d <- ggplot(rank_memory, aes(episode, rank_memory)) +
  geom_hline(yintercept = 0, linewidth = 0.28, color = COL$gray) +
  geom_vline(
    xintercept = default_pars$T,
    linetype = "dashed",
    linewidth = 0.28,
    color = COL$gray
  ) +
  geom_line(linewidth = LW_MEAN, color = COL$ink) +
  geom_point(size = 0.9, color = COL$ink) +
  coord_cartesian(ylim = c(-0.1, 1)) +
  labs(x = "Developmental episode", y = "Correlation with earlier rank") +
  theme_pub(base_size = 8.8)

fig3 <- ((p3a | p3b) / (p3c | p3d)) +
  plot_annotation(tag_levels = "A") &
  panel_tag_theme

save_pub(fig3, "Figure3_emergence_and_persistence", width = 7.1, height = 6.4)

# Supplementary long-run cross-sectional spread.
long_sd <- df_long %>%
  group_by(step) %>%
  summarize(sd_pi = sd(pi), .groups = "drop")

supp_long_sd <- ggplot(long_sd, aes(step, sd_pi)) +
  geom_line(linewidth = LW_MEAN, color = COL$ink) +
  geom_vline(
    xintercept = default_pars$T,
    linetype = "dashed",
    linewidth = 0.3,
    color = COL$gray
  ) +
  labs(x = "Developmental episode", y = "Between-agent SD") +
  theme_pub()

save_pub(supp_long_sd, "Supplement_long_horizon_between_agent_SD", width = 6.1, height = 3.4)


# -----------------------------------------------------------------------------
# 6. FIGURE 4: Persistent environmental differences
# -----------------------------------------------------------------------------

reward_sd_grid <- c(0, 0.01, 0.02, 0.04, 0.06)

reward_sd_sensitivity <- map_dfr(reward_sd_grid, function(sd_p) {
  df_sd <- simulate_population(
    model = "value_cost",
    N = 1000,
    p_reward_sd = sd_p,
    seed = 601
  )

  metrics_sd <- development_metrics(df_sd)

  tibble(
    imposed_sd_p = sd_p,
    late_sd_pi = metrics_sd$late_sd,
    rho_middle_late = metrics_sd$rho_middle_late
  )
})

p4a <- ggplot(reward_sd_sensitivity, aes(imposed_sd_p, rho_middle_late)) +
  geom_line(linewidth = LW_MEAN, color = COL$ink) +
  geom_point(size = 1.55, color = COL$ink) +
  scale_x_continuous(breaks = reward_sd_grid) +
  coord_cartesian(ylim = c(0.60, 0.90)) +
  labs(
    x = "Between-agent SD in reward probability",
    y = "Middle-to-late rank persistence"
  ) +
  theme_pub(base_size = 8.9)

T_long_env <- 4 * (default_pars$T - 1) + 1

df_environment_long <- simulate_population(
  model = "value_cost",
  N = 1000,
  T = T_long_env,
  p_reward_sd = 0.02,
  record_every = 10,
  seed = 601
)

environment_groups <- df_environment_long %>%
  distinct(child_id, p_reward_child) %>%
  mutate(
    environment_group = case_when(
      p_reward_child <= quantile(p_reward_child, 1/3) ~ "Less rewarding environment",
      p_reward_child >= quantile(p_reward_child, 2/3) ~ "More rewarding environment",
      TRUE ~ "Intermediate environment"
    )
  )

environment_summary <- df_environment_long %>%
  left_join(environment_groups, by = c("child_id", "p_reward_child")) %>%
  group_by(environment_group, step) %>%
  summarize(
    mean_pi = mean(pi),
    q20 = quantile(pi, 0.20),
    q80 = quantile(pi, 0.80),
    .groups = "drop"
  )

p4b <- ggplot(
  environment_summary,
  aes(step, mean_pi, color = environment_group, fill = environment_group)
) +
  geom_ribbon(aes(ymin = q20, ymax = q80), alpha = 0.07, color = NA) +
  geom_line(linewidth = LW_GROUP) +
  geom_vline(
    xintercept = default_pars$T,
    linetype = "dashed",
    linewidth = 0.28,
    color = COL$gray
  ) +
  scale_color_manual(values = environment_cols) +
  scale_fill_manual(values = environment_cols) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Developmental episode", y = "Engagement propensity") +
  theme_pub(base_size = 8.9)

fig4 <- (p4a / p4b) +
  plot_layout(heights = c(0.75, 1.25)) +
  plot_annotation(tag_levels = "A") &
  panel_tag_theme

save_pub(fig4, "Figure4_persistent_environment", width = 6.8, height = 5.4)

write.csv(
  reward_sd_sensitivity,
  file.path(OUT_DIR, "Figure4_environment_sensitivity.csv"),
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 7. FIGURE 5: Temporary contextual perturbation
# -----------------------------------------------------------------------------

T_context <- default_pars$T
context_vec <- rep(0, T_context)
context_start <- round(STRESS_START_FRAC * T_context)
context_end <- round(STRESS_END_FRAC * T_context)
context_vec[context_start:context_end] <- STRESS_COST

df_context_ref <- simulate_population(
  model = "value_cost",
  N = 1000,
  extra_cost = rep(0, T_context),
  seed = 810
) %>%
  mutate(condition = "Reference")

df_context <- simulate_population(
  model = "value_cost",
  N = 1000,
  extra_cost = context_vec,
  seed = 810
) %>%
  mutate(condition = "Contextual perturbation")

df_context_all <- bind_rows(df_context_ref, df_context)

context_summary <- df_context_all %>%
  group_by(condition, step) %>%
  summarize(
    mean_pi = mean(pi),
    .groups = "drop"
  )

# Paired condition differences use the same child IDs and common random seed.
context_effect <- df_context_all %>%
  select(child_id, step, condition, pi) %>%
  pivot_wider(names_from = condition, values_from = pi) %>%
  mutate(delta_pi = `Contextual perturbation` - Reference)

context_effect_summary <- context_effect %>%
  group_by(step) %>%
  summarize(
    mean_delta = mean(delta_pi),
    q20 = quantile(delta_pi, 0.20),
    q80 = quantile(delta_pi, 0.80),
    .groups = "drop"
  )

late_window_start_context <- episode_at_fraction(T_context, 0.85)
late_residual <- context_effect_summary %>%
  filter(step >= late_window_start_context) %>%
  summarize(mean_delta = mean(mean_delta)) %>%
  pull(mean_delta)

# Focus the raw-trajectory panel on the perturbation and recovery period instead
# of wasting most of the panel on unaffected developmental time.
context_view_start <- max(1, context_start - round(0.10 * T_context))
context_view_end   <- min(T_context, context_end + round(0.22 * T_context))

p5a <- context_summary %>%
  filter(step >= context_view_start, step <= context_view_end) %>%
  ggplot(aes(step, mean_pi, color = condition)) +
  annotate(
    "rect",
    xmin = context_start, xmax = context_end,
    ymin = -Inf, ymax = Inf,
    fill = COL$pale_gray, alpha = 0.70
  ) +
  geom_line(linewidth = LW_MEAN) +
  scale_color_manual(values = context_cols) +
  labs(x = NULL, y = "Mean engagement propensity") +
  theme_pub(base_size = 8.9) +
  theme(legend.position = "top")

# Pairwise effect panel: the ribbon is the middle 60% of individual paired
# effects, not uncertainty around the population mean.
p5b <- context_effect_summary %>%
  filter(step >= context_view_start) %>%
  ggplot(aes(step, mean_delta)) +
  annotate(
    "rect",
    xmin = context_start, xmax = context_end,
    ymin = -Inf, ymax = Inf,
    fill = COL$pale_gray, alpha = 0.70
  ) +
  geom_hline(yintercept = 0, linewidth = 0.28, color = COL$gray) +
  geom_ribbon(aes(ymin = q20, ymax = q80), fill = COL$orange, alpha = 0.10) +
  geom_line(linewidth = LW_MEAN, color = COL$orange) +
  annotate(
    "text",
    x = T_context * 0.98,
    y = late_residual,
    label = paste0("late mean = ", sprintf("%.3f", late_residual)),
    hjust = 1, vjust = -0.8, size = 2.5, color = COL$dark_gray
  ) +
  labs(x = "Developmental episode", y = "Perturbation − reference") +
  theme_pub(base_size = 8.9)

fig5 <- (p5a / p5b) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "A") &
  panel_tag_theme

save_pub(fig5, "Figure5_temporary_context", width = 6.7, height = 4.8)


# -----------------------------------------------------------------------------
# 8. FIGURE 6: Early versus later support
# -----------------------------------------------------------------------------

T_reward <- default_pars$T
reward_early <- rep(0, T_reward)
reward_late <- rep(0, T_reward)

duration <- round(0.13 * T_reward)
boost <- 0.12

early_start <- round(0.20 * T_reward)
late_start  <- round(0.60 * T_reward)
early_end <- early_start + duration - 1
late_end <- late_start + duration - 1

reward_early[early_start:early_end] <- boost
reward_late[late_start:late_end] <- boost

df_reward_ref <- simulate_population(
  model = "value_cost", N = 1000,
  p_reward_shift = rep(0, T_reward), seed = 910
) %>% mutate(condition = "Reference")

df_reward_early <- simulate_population(
  model = "value_cost", N = 1000,
  p_reward_shift = reward_early, seed = 910
) %>% mutate(condition = "Early support")

df_reward_late <- simulate_population(
  model = "value_cost", N = 1000,
  p_reward_shift = reward_late, seed = 910
) %>% mutate(condition = "Later support")

df_reward_timing <- bind_rows(df_reward_ref, df_reward_early, df_reward_late)

reward_effect <- df_reward_timing %>%
  select(child_id, step, condition, pi) %>%
  pivot_wider(names_from = condition, values_from = pi) %>%
  transmute(
    child_id,
    step,
    `Early support` = `Early support` - Reference,
    `Later support` = `Later support` - Reference
  ) %>%
  pivot_longer(
    cols = c(`Early support`, `Later support`),
    names_to = "condition",
    values_to = "delta_pi"
  )

# A: simple timeline of when the same support manipulation is delivered.
timeline_df <- tibble(
  condition = factor(c("Early support", "Later support"),
                     levels = c("Later support", "Early support")),
  start = c(early_start, late_start),
  end = c(early_end, late_end)
)

p6a <- ggplot(timeline_df) +
  geom_segment(
    aes(x = 1, xend = T_reward, y = condition, yend = condition),
    linewidth = 0.28, color = COL$light_gray
  ) +
  geom_segment(
    aes(x = start, xend = end, y = condition, yend = condition, color = condition),
    linewidth = 4.0, lineend = "butt"
  ) +
  scale_color_manual(values = support_cols) +
  scale_x_continuous(limits = c(1, T_reward), expand = expansion(mult = c(0, 0))) +
  labs(x = NULL, y = NULL) +
  theme_pub(base_size = 8.9) +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 8.2),
    legend.position = "none"
  )

# B: align both interventions by the end of support. This is the actual
# sensitive-period test used in the manuscript.
reward_effect_aligned <- reward_effect %>%
  mutate(
    relative_episode = if_else(
      condition == "Early support",
      step - early_end,
      step - late_end
    )
  ) %>%
  filter(relative_episode >= -duration, relative_episode <= round(0.36 * T_reward))

reward_effect_aligned_summary <- reward_effect_aligned %>%
  group_by(condition, relative_episode) %>%
  summarize(
    mean_delta = mean(delta_pi),
    q20 = quantile(delta_pi, 0.20),
    q80 = quantile(delta_pi, 0.80),
    .groups = "drop"
  )

p6b <- ggplot(
  reward_effect_aligned_summary,
  aes(relative_episode, mean_delta, color = condition, fill = condition)
) +
  annotate(
    "rect", xmin = -duration, xmax = 0, ymin = -Inf, ymax = Inf,
    fill = COL$pale_gray, alpha = 0.75
  ) +
  geom_vline(xintercept = 0, linewidth = 0.28, linetype = "dashed", color = COL$gray) +
  geom_hline(yintercept = 0, linewidth = 0.28, color = COL$gray) +
  geom_ribbon(aes(ymin = q20, ymax = q80), alpha = 0.08, color = NA) +
  geom_line(linewidth = LW_GROUP) +
  scale_color_manual(values = support_cols) +
  scale_fill_manual(values = support_cols) +
  labs(
    x = "Episodes relative to end of support",
    y = "Change from reference"
  ) +
  theme_pub(base_size = 8.9)

fig6 <- (p6a / p6b) +
  plot_layout(heights = c(0.55, 1.45)) +
  plot_annotation(tag_levels = "A") &
  panel_tag_theme

save_pub(fig6, "Figure6_support_timing", width = 6.7, height = 4.7)

# Export timing comparison used in the text/caption.
final_start <- episode_at_fraction(T_reward, 0.85)

final_effects <- df_reward_timing %>%
  filter(step >= final_start) %>%
  group_by(condition) %>%
  summarize(mean_pi = mean(pi), .groups = "drop") %>%
  pivot_wider(names_from = condition, values_from = mean_pi) %>%
  transmute(
    comparison = "Common final time",
    early_effect = `Early support` - Reference,
    later_effect = `Later support` - Reference
  )

followup_lag <- round(0.15 * T_reward)
window_halfwidth <- round(0.025 * T_reward)
early_eval_center <- early_end + followup_lag
late_eval_center  <- late_end + followup_lag

window_mean <- function(df, condition_name, center, halfwidth) {
  df %>%
    filter(
      condition == condition_name,
      step >= center - halfwidth,
      step <= center + halfwidth
    ) %>%
    summarize(value = mean(pi)) %>%
    pull(value)
}

equal_lag_effects <- tibble(
  comparison = "Same follow-up lag",
  early_effect =
    window_mean(df_reward_timing, "Early support", early_eval_center, window_halfwidth) -
    window_mean(df_reward_timing, "Reference", early_eval_center, window_halfwidth),
  later_effect =
    window_mean(df_reward_timing, "Later support", late_eval_center, window_halfwidth) -
    window_mean(df_reward_timing, "Reference", late_eval_center, window_halfwidth)
)

reward_timing_effects <- bind_rows(final_effects, equal_lag_effects)
write.csv(
  reward_timing_effects,
  file.path(OUT_DIR, "Figure6_support_timing_effects.csv"),
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 9. FIGURE 7: Parameter robustness
# -----------------------------------------------------------------------------

sweep_rep_file <- file.path(DATA_DIR, "sweep_results_consistent_windows.csv")
sweep_par_file <- file.path(DATA_DIR, "sweep_params_exact.csv")

run_consistent_sweep <- function() {
  message("Precomputed consistent-window sweep not found. Recomputing 250 x 5 sweep...")

  log_runif <- function(n, min, max) exp(runif(n, log(min), log(max)))
  set.seed(260819)

  n_parameter_sets <- 250
  n_mc_reps <- 5
  sweep_N <- 150
  sweep_T <- 900

  sweep_pars <- tibble(
    set_id = seq_len(n_parameter_sets),
    alpha_V = log_runif(n_parameter_sets, 0.006, 0.070),
    alpha_C = log_runif(n_parameter_sets, 0.0004, 0.0070),
    tau = runif(n_parameter_sets, 0.10, 0.30),
    p_reward = runif(n_parameter_sets, 0.45, 0.72),
    initial_delta = runif(n_parameter_sets, -0.60, -0.20),
    practice_range = runif(n_parameter_sets, 0.25, 0.75),
    V0 = default_pars$V0
  ) %>%
    mutate(
      C0 = V0 - initial_delta,
      C_min = C0 - practice_range
    )

  rep_results <- bind_rows(lapply(seq_len(n_parameter_sets), function(j) {
    pars_j <- sweep_pars[j, ]

    bind_rows(lapply(seq_len(n_mc_reps), function(mc) {
      metrics_j <- simulate_development_metrics_fast(
        N = sweep_N,
        T = sweep_T,
        V0 = pars_j$V0,
        C0 = pars_j$C0,
        C_min = pars_j$C_min,
        tau = pars_j$tau,
        p_reward = pars_j$p_reward,
        alpha_V = pars_j$alpha_V,
        alpha_C = pars_j$alpha_C,
        record_every = 10,
        seed = 100000 + j * 100 + mc
      )

      bind_cols(tibble(set_id = j, mc = mc), metrics_j)
    }))
  }))

  write.csv(rep_results, sweep_rep_file, row.names = FALSE)
  write.csv(sweep_pars, sweep_par_file, row.names = FALSE)
  rep_results
}

if (file.exists(sweep_rep_file)) {
  sweep_rep_results <- read.csv(sweep_rep_file)
} else if (RECOMPUTE_SWEEP_IF_MISSING) {
  sweep_rep_results <- run_consistent_sweep()
} else {
  stop(
    "Missing ", sweep_rep_file,
    ". Put the consistent-window sweep CSV next to this script or set ",
    "RECOMPUTE_SWEEP_IF_MISSING <- TRUE."
  )
}

sweep_results <- sweep_rep_results %>%
  group_by(set_id) %>%
  summarize(
    delta_mean = mean(delta_mean, na.rm = TRUE),
    delta_mean_mc_sd = sd(delta_mean, na.rm = TRUE),
    sd_gain = mean(sd_gain, na.rm = TRUE),
    sd_gain_mc_sd = sd(sd_gain, na.rm = TRUE),
    rho_middle_late = mean(rho_middle_late, na.rm = TRUE),
    rho_middle_late_mc_sd = sd(rho_middle_late, na.rm = TRUE),
    .groups = "drop"
  )

sweep_patterns <- sweep_results %>%
  mutate(
    pattern = case_when(
      delta_mean > 0 & sd_gain > 0 ~ "Improvement + differentiation",
      delta_mean > 0 & sd_gain <= 0 ~ "Improvement only",
      delta_mean <= 0 & sd_gain > 0 ~ "Differentiation only",
      TRUE ~ "Neither"
    )
  )

pattern_levels <- c(
  "Improvement + differentiation",
  "Improvement only",
  "Differentiation only",
  "Neither"
)

pattern_counts <- sweep_patterns %>%
  count(pattern) %>%
  complete(pattern = pattern_levels, fill = list(n = 0)) %>%
  mutate(pct = n / sum(n))

x_right <- max(sweep_patterns$delta_mean, na.rm = TRUE)
x_left <- min(sweep_patterns$delta_mean, na.rm = TRUE)
y_top <- max(sweep_patterns$sd_gain, na.rm = TRUE)
y_bottom <- min(sweep_patterns$sd_gain, na.rm = TRUE)

label_positions <- tibble(
  pattern = pattern_levels,
  x = c(0.70 * x_right, 0.70 * x_right, 0.70 * x_left, 0.70 * x_left),
  y = c(0.82 * y_top, 0.72 * y_bottom, 0.82 * y_top, 0.72 * y_bottom)
) %>%
  left_join(pattern_counts, by = "pattern") %>%
  mutate(label = paste0(pattern, "\n", percent(pct, accuracy = 1)))

# Publication panel: emphasize the dominant upper-right region but keep all
# parameter sets visible. Quadrant percentages are annotations, not categories
# mapped to a rainbow palette.
main_region <- sweep_patterns %>% filter(pattern == "Improvement + differentiation")
other_regions <- sweep_patterns %>% filter(pattern != "Improvement + differentiation")

fig7 <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.30, color = COL$gray) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.30, color = COL$gray) +
  geom_point(
    data = other_regions,
    aes(delta_mean, sd_gain),
    shape = 1, stroke = 0.30, size = 1.25, color = COL$gray, alpha = 0.60
  ) +
  geom_point(
    data = main_region,
    aes(delta_mean, sd_gain),
    size = 1.15, color = COL$ink, alpha = 0.38
  ) +
  annotate(
    "text", x = Inf, y = Inf,
    label = paste0("Improvement + differentiation\n",
                   percent(pattern_counts$pct[pattern_counts$pattern == "Improvement + differentiation"], accuracy = 1)),
    hjust = 1.06, vjust = 1.25, size = 2.7
  ) +
  annotate(
    "text", x = Inf, y = -Inf,
    label = paste0("Improvement only  ",
                   percent(pattern_counts$pct[pattern_counts$pattern == "Improvement only"], accuracy = 1)),
    hjust = 1.06, vjust = -0.75, size = 2.5, color = COL$dark_gray
  ) +
  annotate(
    "text", x = -Inf, y = Inf,
    label = paste0("Differentiation only  ",
                   percent(pattern_counts$pct[pattern_counts$pattern == "Differentiation only"], accuracy = 1)),
    hjust = -0.02, vjust = 1.25, size = 2.5, color = COL$dark_gray
  ) +
  annotate(
    "text", x = -Inf, y = -Inf,
    label = paste0("Neither  ",
                   percent(pattern_counts$pct[pattern_counts$pattern == "Neither"], accuracy = 1)),
    hjust = -0.02, vjust = -0.75, size = 2.5, color = COL$dark_gray
  ) +
  labs(
    x = "Change in mean engagement (late − early)",
    y = "Change in between-agent SD (late − early)"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.08))) +
  theme_pub(base_size = 9)

save_pub(fig7, "Figure7_parameter_robustness", width = 6.7, height = 4.6)

robustness_summary <- tibble(
  outcome = c(
    "Mean engagement increases",
    "Between-agent variation increases",
    "Both occur",
    "Median middle-to-late rank persistence"
  ),
  value = c(
    percent(mean(sweep_results$delta_mean > 0), accuracy = 1),
    percent(mean(sweep_results$sd_gain > 0), accuracy = 1),
    percent(mean(sweep_results$delta_mean > 0 & sweep_results$sd_gain > 0), accuracy = 1),
    sprintf("%.2f", median(sweep_results$rho_middle_late, na.rm = TRUE))
  )
)

write.csv(
  robustness_summary,
  file.path(OUT_DIR, "Figure7_robustness_summary.csv"),
  row.names = FALSE
)

mc_summary <- tibble(
  outcome = c(
    "Developmental mean change",
    "Heterogeneity change",
    "Middle-to-late rank persistence"
  ),
  median_across_parameter_sets = c(
    median(sweep_results$delta_mean, na.rm = TRUE),
    median(sweep_results$sd_gain, na.rm = TRUE),
    median(sweep_results$rho_middle_late, na.rm = TRUE)
  ),
  median_within_set_mc_sd = c(
    median(sweep_results$delta_mean_mc_sd, na.rm = TRUE),
    median(sweep_results$sd_gain_mc_sd, na.rm = TRUE),
    median(sweep_results$rho_middle_late_mc_sd, na.rm = TRUE)
  )
)

write.csv(
  mc_summary,
  file.path(OUT_DIR, "Supplement_monte_carlo_summary.csv"),
  row.names = FALSE
)



# -----------------------------------------------------------------------------
# 10. Console checks
# -----------------------------------------------------------------------------

cat("\nPublication figures written to:\n  ", normalizePath(OUT_DIR), "\n\n", sep = "")

cat("Figure 2 model metrics (consistent windows):\n")
print(model_metrics)

cat("\nFigure 3 rank stability:\n")
print(stability_identical)

cat("\nFigure 4 environmental sensitivity:\n")
print(reward_sd_sensitivity)

cat("\nFigure 5 late contextual difference:\n")
print(late_residual)

cat("\nFigure 6 support timing effects:\n")
print(reward_timing_effects)

cat("\nFigure 7 robustness summary:\n")
print(robustness_summary)

cat("\nMonte Carlo summary:\n")
print(mc_summary)

