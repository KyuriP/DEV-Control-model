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
  "Reference"        = COL$dark_gray,
  "Temporary stress" = COL$orange
)

support_cols <- c(
  "Early support" = COL$orange,
  "Later support" = COL$blue
)

# Thin line hierarchy. Population means should never visually overpower
# the stochastic structure of the simulations.
LW_MEAN  <- 0.20
LW_GROUP <- 0.32
LW_INDIV <- 0.12
ALPHA_INDIV <- 0.20

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
# NOTE ON FIGURE 1 -----------------------------------------------------------
# Do not use the R-drawn schematic for the manuscript. The publication Figure 1
# should be exported directly from the Mermaid source in Figure1_DEV_Control.mmd,
# which preserves the original QMD structure and can be saved as vector SVG/PDF.
# The R schematic below is retained only as a fallback preview.

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
# 4. CORE MODEL RUNS USED IN FIGURE 2
# -----------------------------------------------------------------------------

core_models <- c("evc", "value", "value_cost")

df_models <- bind_rows(
  lapply(core_models, function(m) {
    simulate_population(
      model = m,
      p_reward_sd = 0,
      seed = 4100
    )
  })
)

summary_models <- population_summary(df_models)

model_metrics <- bind_rows(
  lapply(split(df_models, df_models$model_label), development_metrics),
  .id = "model_label"
)

# -----------------------------------------------------------------------------
# Figure 2: What does each mechanism add?
# -----------------------------------------------------------------------------

set.seed(4100)

fig2_ids <- sample(
  unique(df_models$child_id),
  40
)

# Individual trajectories only for models in which differentiation can emerge.
fig2_individuals <- df_models %>%
  filter(
    child_id %in% fig2_ids,
    model_label %in% c(
      "M1: + value learning",
      "M2: + practice feedback"
    )
  )

fig2 <- ggplot() +
  
  # Individual developmental trajectories
  geom_line(
    data = fig2_individuals,
    aes(
      step,
      pi,
      group = interaction(model_label, child_id),
      color = model_label
    ),
    linewidth = 0.2,
    alpha = 0.5
  ) +
  
  # Population means
  geom_line(
    data = summary_models,
    aes(
      step,
      mean_pi,
      color = model_label
    ),
    linewidth = 0.55
  ) +
  
  scale_color_manual(
    values = model_cols,
    breaks = c(
      "M0: Control allocation only",
      "M1: + value learning",
      "M2: + practice feedback"
    )
  ) +
  
  scale_x_continuous(
    breaks = c(1, 500, 1000, 1500),
    labels = c("0", "500", "1000", "1500"),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.025))
  ) +
  
  labs(
    x = "Experience episode",
    y = "EF engagement propensity"
  ) +
  
  theme_pub(base_size = 9) +
  
  theme(
    legend.position = "bottom",
    legend.key.width = grid::unit(15, "pt")
  )

save_pub(
  fig2,
  "Figure2_nested_models",
  width = 6.8,
  height = 3.6
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
    color = COL$blue,
    linewidth = 0.3,
    alpha = 0.3
  ) +
  geom_line(
    data = trajectory_mean,
    aes(step, mean_pi),
    color = "dodgerblue4",
    linewidth = 0.50
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Experience episode", y = "Engagement propensity") +
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
  geom_point(
    alpha = 0.22,
    size = 0.70,
    color = COL$blue
  ) +
  geom_abline(
    linetype = "dashed",
    linewidth = 0.30,
    color = COL$light_gray
  ) +
  annotate(
    "text", x = 0.04, y = 0.96,
    label = paste0("ρ = ", sprintf("%.2f", rho_ml)),
    hjust = 0, vjust = 1, size = 2.7
  ) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Middle-stage rank percentile", y = "Late-stage rank percentile") +
  theme_pub(base_size = 8.8)

# D: long-run fading of earlier rank ordering.
# Replicate the long-horizon simulation so the plotted decline reflects the
# typical stochastic pattern rather than one particular realization.

T_long <- 4 * (default_pars$T - 1) + 1

n_rank_reps <- 20
rank_rep_N  <- 1000

# Start at 1.0x so the plotted curve is strictly subsequent to the reference
# window used to define the earlier ranking.
memory_centers <- seq(1.00, 4.00, by = 0.20)
memory_half_width <- 0.06

rank_memory_reps <- bind_rows(
  lapply(seq_len(n_rank_reps), function(rep_i) {

    df_long_rep <- simulate_population(
      model = "value_cost",
      N = rank_rep_N,
      T = T_long,
      p_reward_sd = 0,
      record_every = 10,
      seed = 1400 + rep_i
    )

    reference_rank_scores <- df_long_rep %>%
      filter(
        step >= round(0.80 * (default_pars$T - 1)) + 1,
        step <= round(0.95 * (default_pars$T - 1)) + 1
      ) %>%
      group_by(child_id) %>%
      summarize(reference_score = mean(pi), .groups = "drop")

    bind_rows(
      lapply(memory_centers, function(center_i) {

        center_step <- round(center_i * (default_pars$T - 1)) + 1
        half_width_steps <- round(memory_half_width * (default_pars$T - 1))

        later_scores <- df_long_rep %>%
          filter(
            step >= center_step - half_width_steps,
            step <= center_step + half_width_steps
          ) %>%
          group_by(child_id) %>%
          summarize(later_score = mean(pi), .groups = "drop")

        joined <- inner_join(
          reference_rank_scores,
          later_scores,
          by = "child_id"
        )

        tibble(
          replication = rep_i,
          episode = center_step,
          rank_memory = suppressWarnings(
            cor(
              joined$reference_score,
              joined$later_score,
              method = "spearman"
            )
          )
        )
      })
    )
  })
)

rank_memory_summary <- rank_memory_reps %>%
  group_by(episode) %>%
  summarize(
    median_rho = median(rank_memory, na.rm = TRUE),
    q25 = quantile(rank_memory, 0.25, na.rm = TRUE),
    q75 = quantile(rank_memory, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

p3d <- ggplot(rank_memory_summary, aes(episode, median_rho)) +
  geom_ribbon(
    aes(ymin = q25, ymax = q75),
    fill = COL$orange,
    alpha = 0.14,
    linewidth = 0
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.28,
    color = COL$light_gray
  ) +
  geom_vline(
    xintercept = default_pars$T,
    linetype = "dashed",
    linewidth = 0.28,
    color = COL$gray
  ) +
  geom_line(
    linewidth = 0.50,
    color = COL$orange
  ) +
  geom_point(
    size = 0.90,
    color = COL$orange
  ) +
  coord_cartesian(ylim = c(-0.10, 1)) +
  labs(
    x = "Experience episode",
    y = "Correlation with earlier rank"
  ) +
  theme_pub(base_size = 8.8)

fig3 <- ((p3a | p3b) / (p3c | p3d)) +
  plot_annotation(tag_levels = "A") &
  panel_tag_theme

save_pub(
  fig3,
  "Figure3_emergence_and_persistence",
  width = 7.1,
  height = 6.4
)

write.csv(
  rank_memory_reps,
  file.path(OUT_DIR, "Figure3_rank_memory_replications.csv"),
  row.names = FALSE
)

write.csv(
  rank_memory_summary,
  file.path(OUT_DIR, "Figure3_rank_memory_summary.csv"),
  row.names = FALSE
)

# Supplementary long-run cross-sectional spread.
# This is a representative long-run population used only to show how
# cross-sectional dispersion changes with time.
df_long_supp <- simulate_population(
  model = "value_cost",
  N = 1000,
  T = T_long,
  p_reward_sd = 0,
  record_every = 10,
  seed = 1400
)

long_sd <- df_long_supp %>%
  group_by(step) %>%
  summarize(sd_pi = sd(pi), .groups = "drop")

supp_long_sd <- ggplot(long_sd, aes(step, sd_pi)) +
  geom_line(linewidth = LW_MEAN, color = COL$ink) +
  geom_vline(
    xintercept = default_pars$T,
    linetype = "dashed",
    linewidth = 0.30,
    color = COL$gray
  ) +
  labs(
    x = "Experience episode",
    y = "Between-agent SD"
  ) +
  theme_pub()

save_pub(
  supp_long_sd,
  "Supplement_long_horizon_between_agent_SD",
  width = 6.1,
  height = 3.4
)


# -----------------------------------------------------------------------------
# 6. FIGURE 4: Persistent environmental differences
# -----------------------------------------------------------------------------

reward_sd_grid <- seq(0, 0.06, by = 0.003)
n_reward_sd_reps <- 5

# Replicate each environmental-heterogeneity setting so the sensitivity curve
# represents a systematic relation rather than one stochastic realization.
reward_sd_sensitivity_reps <- map_dfr(reward_sd_grid, function(sd_p) {
  map_dfr(seq_len(n_reward_sd_reps), function(rep_i) {

    df_sd <- simulate_population(
      model = "value_cost",
      N = 1000,
      p_reward_sd = sd_p,
      seed = 6000 + rep_i
    )

    metrics_sd <- development_metrics(df_sd)

    tibble(
      imposed_sd_p = sd_p,
      replication = rep_i,
      late_sd_pi = metrics_sd$late_sd,
      rho_middle_late = metrics_sd$rho_middle_late
    )
  })
})

reward_sd_sensitivity <- reward_sd_sensitivity_reps %>%
  group_by(imposed_sd_p) %>%
  summarize(
    late_sd_pi = median(late_sd_pi, na.rm = TRUE),
    rho_middle_late = median(rho_middle_late, na.rm = TRUE),
    rho_q25 = quantile(rho_middle_late, 0.25, na.rm = TRUE),
    rho_q75 = quantile(rho_middle_late, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

p4a <- ggplot(
  reward_sd_sensitivity,
  aes(imposed_sd_p, rho_middle_late)
) +
  geom_ribbon(
    aes(ymin = rho_q25, ymax = rho_q75),
    fill = COL$blue,
    alpha = 0.10,
    linewidth = 0
  ) +
  geom_line(
    linewidth = 0.45,
    color = COL$blue
  ) +
  geom_point(
    size = 1.20,
    color = COL$blue
  ) +
  coord_cartesian(
    ylim = c(0.60, 0.90)
  ) +
  labs(
    x = "Between-agent SD in reward probability",
    y = "Middle-to-late rank persistence"
  ) +
  theme_pub(base_size = 8.8)

T_long_env <- 4 * (default_pars$T - 1) + 1

df_environment_long <- simulate_population(
  model = "value_cost",
  N = 1000,
  T = T_long_env,
  p_reward_sd = 0.02,
  record_every = 10,
  seed = 601
)

# Define environmental groups from each agent's fixed reward probability.
environment_groups <- df_environment_long %>%
  distinct(
    child_id,
    p_reward_child
  ) %>%
  mutate(
    environment_group = case_when(
      p_reward_child <= quantile(p_reward_child, 1/3) ~
        "Less rewarding environment",
      
      p_reward_child >= quantile(p_reward_child, 2/3) ~
        "More rewarding environment",
      
      TRUE ~
        "Intermediate environment"
    )
  )

set.seed(20260820)

sampled_environment_children <- bind_rows(
  environment_groups %>%
    filter(environment_group == "Less rewarding environment") %>%
    sample_n_safe(15),
  
  environment_groups %>%
    filter(environment_group == "Intermediate environment") %>%
    sample_n_safe(15),
  
  environment_groups %>%
    filter(environment_group == "More rewarding environment") %>%
    sample_n_safe(15)
)

trajectory_environment <- df_environment_long %>%
  semi_join(
    sampled_environment_children,
    by = "child_id"
  ) %>%
  left_join(
    sampled_environment_children %>%
      select(child_id, environment_group),
    by = "child_id"
  )

environment_group_mean <- df_environment_long %>%
  left_join(
    environment_groups,
    by = c("child_id", "p_reward_child")
  ) %>%
  group_by(
    environment_group,
    step
  ) %>%
  summarize(
    mean_pi = mean(pi),
    .groups = "drop"
  )


p4b <- ggplot() +
  
  # Actual individual trajectories
  geom_line(
    data = trajectory_environment,
    aes(
      step,
      pi,
      group = child_id,
      color = environment_group
    ),
    linewidth = 0.2,
    alpha = 0.3
  ) +
  
  # Group averages
  geom_line(
    data = environment_group_mean,
    aes(
      step,
      mean_pi,
      color = environment_group
    ),
    linewidth = 0.50
  ) +
  
  geom_vline(
    xintercept = default_pars$T,
    linetype = "dashed",
    linewidth = 0.28,
    color = COL$gray
  ) +
  
  scale_color_manual(
    values = environment_cols
  ) +
  
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  
  labs(
    x = "Experience episode",
    y = "EF engagement propensity"
  ) +
  
  theme_pub(base_size = 8.8) +
  
  theme(
    legend.position = "bottom"
  )

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
context_start <- round(STRESS_START_FRAC * T_context)
context_end   <- round(STRESS_END_FRAC * T_context)
context_duration <- context_end - context_start + 1

# Smooth temporary stress pulse.
# STRESS_COST is the PEAK additional cost, not an average-equivalent dose.
make_stress_pulse <- function(T, start, duration, peak) {
  shape <- sin(seq(0, pi, length.out = duration))

  out <- rep(0, T)
  out[start:(start + duration - 1)] <- peak * shape
  out
}

context_vec <- make_stress_pulse(
  T = T_context,
  start = context_start,
  duration = context_duration,
  peak = STRESS_COST
)

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
  mutate(condition = "Temporary stress")

df_context_all <- bind_rows(df_context_ref, df_context)

context_summary <- df_context_all %>%
  group_by(condition, step) %>%
  summarize(mean_pi = mean(pi), .groups = "drop")

set.seed(810)
context_ids <- sort(sample(unique(df_context_all$child_id), 30))

context_spaghetti <- df_context_all %>%
  filter(child_id %in% context_ids)

late_window_start_context <- episode_at_fraction(T_context, 0.85)

late_residual <- df_context_all %>%
  filter(step >= late_window_start_context) %>%
  group_by(condition) %>%
  summarize(mean_pi = mean(pi), .groups = "drop") %>%
  pivot_wider(names_from = condition, values_from = mean_pi) %>%
  transmute(delta = `Temporary stress` - Reference) %>%
  pull(delta)

# Panel B is a literal magnification of the boxed region in Panel A.
zoom_start <- max(1, context_start - round(0.05 * T_context))
zoom_end   <- min(T_context, context_end + round(0.18 * T_context))

zoom_spaghetti <- context_spaghetti %>%
  filter(step >= zoom_start, step <= zoom_end)

zoom_summary <- context_summary %>%
  filter(step >= zoom_start, step <= zoom_end)

zoom_y_range <- range(
  c(zoom_spaghetti$pi, zoom_summary$mean_pi),
  na.rm = TRUE
)

zoom_y_pad <- max(diff(zoom_y_range) * 0.05, 0.01)
zoom_y_lim <- c(
  max(0, zoom_y_range[1] - zoom_y_pad),
  min(1, zoom_y_range[2] + zoom_y_pad)
)

# Panel A: full developmental trajectories with the stress interval marked.
stress_fill <- "#E76F51"
stress_line <- "#D96B2B"

p5a <- ggplot() +
  
  annotate(
    "rect",
    xmin = context_start,
    xmax = context_end,
    ymin = -Inf,
    ymax = Inf,
    fill = stress_fill,
    alpha = 0.08
  ) +
  
  geom_line(
    data = context_spaghetti,
    aes(
      step,
      pi,
      group = interaction(condition, child_id),
      color = condition
    ),
    linewidth = 0.14,
    alpha = 0.13
  ) +
  
  geom_line(
    data = context_summary,
    aes(
      step,
      mean_pi,
      color = condition
    ),
    linewidth = 0.52
  ) +
  
  annotate(
    "segment",
    x = context_start,
    xend = context_end,
    y = 0.91,
    yend = 0.91,
    linewidth = 1.0,
    color = stress_line
  ) +
  
  annotate(
    "text",
    x = (context_start + context_end) / 2,
    y = 0.955,
    label = "Temporary stress",
    color = stress_line,
    size = 2.7
  ) +
  
  scale_color_manual(
    values = c(
      "Reference" = COL$dark_gray,
      "Temporary stress" = COL$orange
    )
  ) +
  
  coord_cartesian(
    xlim = c(1, T_context),
    ylim = c(0, 1)
  ) +
  
  labs(
    x = "Experience episode",
    y = "EF engagement propensity"
  ) +
  
  theme_pub(base_size = 8.9) +
  
  theme(
    legend.position = "bottom"
  )


# ------------------------------------------------------------------
# Panel B: post-stress recovery zoom
# ------------------------------------------------------------------

context_post_start <- 1000
context_post_end   <- 1300

# Use fewer sampled children in the zoomed panel so the mean lines stay visible.
set.seed(812)

context_post_ids <- sort(
  sample(context_ids, 15)
)

context_post_summary <- context_summary %>%
  filter(
    step >= context_post_start,
    step <= context_post_end
  )

context_post_spaghetti <- context_spaghetti %>%
  filter(
    child_id %in% context_post_ids,
    step >= context_post_start,
    step <= context_post_end
  )

# Use fewer sampled children in the zoomed panel so the mean lines stay visible.
set.seed(812)

context_post_ids <- sort(
  sample(context_ids, 30)
)

context_post_summary <- context_summary %>%
  filter(
    step >= context_post_start,
    step <= context_post_end
  )

context_post_spaghetti <- context_spaghetti %>%
  filter(
    child_id %in% context_post_ids,
    step >= context_post_start,
    step <= context_post_end
  )

# Tight y-range based on the population means, with a modest amount of padding.
post_range <- range(
  context_post_summary$mean_pi,
  na.rm = TRUE
)

post_pad <- max(
  diff(post_range) * 0.20,
  0.01
)

p5b <- ggplot() +
  
  geom_line(
    data = context_post_spaghetti,
    aes(
      step,
      pi,
      group = interaction(condition, child_id),
      color = condition
    ),
    linewidth = 0.13,
    alpha = 0.3
  ) +
  
  geom_line(
    data = context_post_summary,
    aes(
      step,
      mean_pi,
      color = condition
    ),
    linewidth = 0.58
  ) +
  
  scale_color_manual(
    values = c(
      "Reference" = COL$dark_gray,
      "Temporary stress" = COL$orange
    )
  ) +
  
  coord_cartesian(
    xlim = c(1000, 1300),
    ylim = c(
      post_range[1] - post_pad,
      post_range[2] + post_pad
    )
  ) +
  
  labs(
    x = NULL,
    y = "EF engagement propensity"
  ) +
  
  theme_pub(base_size = 8.9) +
  
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  )


fig5 <- (p5a / p5b) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "A") &
  panel_tag_theme

save_pub(
  fig5,
  "Figure5_temporary_context",
  width = 6.7,
  height = 4.8
)

# Save panels separately for manual assembly.
save_pub(
  p5a,
  "Figure5A_temporary_stress_overview",
  width = 6.7,
  height = 3.3
)

save_pub(
  p5b,
  "Figure5B_post_stress_zoom",
  width = 3,
  height = 3
)

write.csv(
  tibble(
    stress_start = context_start,
    stress_end = context_end,
    peak_extra_cost = STRESS_COST,
    late_difference = late_residual
  ),
  file.path(OUT_DIR, "Figure5_stress_summary.csv"),
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 8. FIGURE 6: Early versus later support
# -----------------------------------------------------------------------------

T_reward <- default_pars$T
duration <- round(0.13 * T_reward)

early_start <- round(0.20 * T_reward)
late_start  <- round(0.60 * T_reward)

early_end <- early_start + duration - 1
late_end  <- late_start + duration - 1

# Smooth support pulse.
# 'boost' is the PEAK increase in reward probability.
boost <- 0.12

make_support_pulse <- function(T, start, duration, peak) {
  shape <- sin(seq(0, pi, length.out = duration))

  out <- rep(0, T)
  out[start:(start + duration - 1)] <- peak * shape
  out
}

reward_early <- make_support_pulse(
  T = T_reward,
  start = early_start,
  duration = duration,
  peak = boost
)

reward_late <- make_support_pulse(
  T = T_reward,
  start = late_start,
  duration = duration,
  peak = boost
)

df_reward_ref <- simulate_population(
  model = "value_cost",
  N = 1000,
  p_reward_shift = rep(0, T_reward),
  seed = 910
) %>%
  mutate(condition = "Reference")

df_reward_early <- simulate_population(
  model = "value_cost",
  N = 1000,
  p_reward_shift = reward_early,
  seed = 910
) %>%
  mutate(condition = "Early support")

df_reward_late <- simulate_population(
  model = "value_cost",
  N = 1000,
  p_reward_shift = reward_late,
  seed = 910
) %>%
  mutate(condition = "Later support")

df_reward_timing <- bind_rows(
  df_reward_ref,
  df_reward_early,
  df_reward_late
)

reward_summary <- df_reward_timing %>%
  group_by(condition, step) %>%
  summarize(mean_pi = mean(pi), .groups = "drop")

set.seed(910)
support_ids <- sort(
  sample(unique(df_reward_timing$child_id), 30)
)

support_spaghetti <- df_reward_timing %>%
  filter(child_id %in% support_ids)

# Panel A: full trajectories.
# Shading is drawn first so it remains behind the data.
p6a <- ggplot() +
  annotate(
    "rect",
    xmin = early_start, xmax = early_end,
    ymin = -Inf, ymax = Inf,
    fill = COL$orange, alpha = 0.025
  ) +
  annotate(
    "rect",
    xmin = late_start, xmax = late_end,
    ymin = -Inf, ymax = Inf,
    fill = COL$blue, alpha = 0.025
  ) +
  geom_line(
    data = support_spaghetti,
    aes(
      step, pi,
      group = interaction(condition, child_id),
      color = condition
    ),
    linewidth = 0.14,
    alpha = 0.14
  ) +
  geom_line(
    data = reward_summary,
    aes(step, mean_pi, color = condition),
    linewidth = 0.50
  ) +
  annotate(
    "segment",
    x = early_start, xend = early_end,
    y = 0.92, yend = 0.92,
    linewidth = 0.90,
    color = COL$orange
  ) +
  annotate(
    "segment",
    x = late_start, xend = late_end,
    y = 0.92, yend = 0.92,
    linewidth = 0.90,
    color = COL$blue
  ) +
  annotate(
    "text",
    x = (early_start + early_end) / 2,
    y = 0.955,
    label = "Early support",
    size = 2.35,
    color = COL$orange,
    vjust = 0
  ) +
  annotate(
    "text",
    x = (late_start + late_end) / 2,
    y = 0.955,
    label = "Later support",
    size = 2.35,
    color = COL$blue,
    vjust = 0
  ) +
  scale_color_manual(
    values = c(
      "Reference" = COL$dark_gray,
      support_cols
    )
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Experience episode",
    y = "EF engagement propensity"
  ) +
  theme_pub(base_size = 8.9) +
  theme(
    legend.position = "bottom"
  )

# Child-level support effects relative to the paired reference condition.
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
  ) %>%
  mutate(
    relative_episode = if_else(
      condition == "Early support",
      step - early_end,
      step - late_end
    )
  )

# Restrict both conditions to the follow-up horizon available for the later
# support condition so the two curves have the same x-range.
common_followup_max <- T_reward - late_end

reward_effect_aligned <- reward_effect %>%
  filter(
    relative_episode >= 0,
    relative_episode <= common_followup_max
  )

reward_effect_aligned_mean <- reward_effect_aligned %>%
  group_by(condition, relative_episode) %>%
  summarize(mean_delta = mean(delta_pi), .groups = "drop")

reward_effect_spaghetti <- reward_effect_aligned %>%
  filter(child_id %in% support_ids)

p6b <- ggplot() +
  geom_hline(
    yintercept = 0,
    linewidth = 0.25,
    color = COL$light_gray
  ) +
  geom_line(
    data = reward_effect_spaghetti,
    aes(
      relative_episode,
      delta_pi,
      group = interaction(condition, child_id),
      color = condition
    ),
    linewidth = 0.12,
    alpha = 0.10
  ) +
  geom_line(
    data = reward_effect_aligned_mean,
    aes(relative_episode, mean_delta, color = condition),
    linewidth = 0.50
  ) +
  scale_color_manual(values = support_cols) +
  labs(
    x = NULL,
    y = "Change from reference"
  ) +
  theme_pub(base_size = 8.9) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

fig6 <- (p6a / p6b) +
  plot_layout(heights = c(1.15, 1)) +
  plot_annotation(tag_levels = "A") &
  panel_tag_theme

save_pub(
  fig6,
  "Figure6_support_timing",
  width = 6.7,
  height = 5.0
)

# Save panels separately for manual assembly.
save_pub(
  p6a,
  "Figure6A_support_timing_overview",
  width = 6.7,
  height = 3.3
)

save_pub(
  p6b,
  "Figure6B_support_effect_zoom",
  width = 6.7,
  height = 2.6
)

# Timing comparison used in the text/caption.
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
    window_mean(
      df_reward_timing, "Early support",
      early_eval_center, window_halfwidth
    ) -
    window_mean(
      df_reward_timing, "Reference",
      early_eval_center, window_halfwidth
    ),
  later_effect =
    window_mean(
      df_reward_timing, "Later support",
      late_eval_center, window_halfwidth
    ) -
    window_mean(
      df_reward_timing, "Reference",
      late_eval_center, window_halfwidth
    )
)

reward_timing_effects <- bind_rows(
  final_effects,
  equal_lag_effects
)

write.csv(
  reward_timing_effects,
  file.path(OUT_DIR, "Figure6_support_timing_effects.csv"),
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 8.5. REPLICATED PARAMETER SWEEP USED IN FIGURE 7
# -----------------------------------------------------------------------------

log_runif <- function(n, min, max) {
  exp(runif(n, log(min), log(max)))
}

parallel_lapply <- function(X, FUN, workers = 1L) {
  if (.Platform$OS.type == "windows" || workers <= 1L) {
    lapply(X, FUN)
  } else {
    parallel::mclapply(X, FUN, mc.cores = workers)
  }
}

SWEEP_CACHE <- file.path(DATA_DIR, "parameter_sweep_consistent_windows.csv")

if (file.exists(SWEEP_CACHE)) {

  sweep_results <- read.csv(SWEEP_CACHE)

} else {

  if (!RECOMPUTE_SWEEP_IF_MISSING) {
    stop(
      "Parameter-sweep cache not found: ", SWEEP_CACHE,
      "\nSet RECOMPUTE_SWEEP_IF_MISSING <- TRUE to recreate it."
    )
  }

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

    # Initial net value D0 = V0 - C0 and available practice-related
    # cost reduction are varied independently.
    initial_delta = runif(n_parameter_sets, -0.60, -0.20),
    practice_range = runif(n_parameter_sets, 0.25, 0.75),

    V0 = default_pars$V0
  ) %>%
    mutate(
      C0 = V0 - initial_delta,
      C_min = C0 - practice_range
    )

  detected_cores <- parallel::detectCores(logical = FALSE)
  if (is.na(detected_cores)) detected_cores <- 1L

  n_workers <- max(
    1L,
    min(n_parameter_sets, detected_cores - 1L)
  )

  message(
    "Parameter sweep: ",
    n_parameter_sets, " parameter sets x ",
    n_mc_reps, " replications; ",
    n_workers, " workers."
  )

  sweep_list <- parallel_lapply(
    seq_len(n_parameter_sets),

    function(j) {
      pars_j <- sweep_pars[j, ]

      bind_rows(
        lapply(seq_len(n_mc_reps), function(mc) {

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

          bind_cols(
            pars_j,
            tibble(mc_rep = mc),
            metrics_j
          )
        })
      )
    },

    workers = n_workers
  )

  sweep_rep_results <- bind_rows(sweep_list)

  sweep_results <- sweep_rep_results %>%
    group_by(
      set_id,
      alpha_V,
      alpha_C,
      tau,
      p_reward,
      initial_delta,
      practice_range,
      V0,
      C0,
      C_min
    ) %>%
    summarize(
      delta_mean = mean(delta_mean, na.rm = TRUE),
      sd_gain = mean(sd_gain, na.rm = TRUE),
      rho_middle_late = mean(rho_middle_late, na.rm = TRUE),
      delta_mean_mc_sd = sd(delta_mean, na.rm = TRUE),
      sd_gain_mc_sd = sd(sd_gain, na.rm = TRUE),
      rho_middle_late_mc_sd = sd(rho_middle_late, na.rm = TRUE),
      .groups = "drop"
    )

  write.csv(
    sweep_results,
    SWEEP_CACHE,
    row.names = FALSE
  )
}

# Summary used in the Results/caption.
robustness_summary <- tibble(
  proportion_mean_increase = mean(sweep_results$delta_mean > 0, na.rm = TRUE),
  proportion_sd_increase = mean(sweep_results$sd_gain > 0, na.rm = TRUE),
  proportion_both = mean(
    sweep_results$delta_mean > 0 & sweep_results$sd_gain > 0,
    na.rm = TRUE
  ),
  median_middle_late_rho = median(
    sweep_results$rho_middle_late,
    na.rm = TRUE
  )
)

# -----------------------------------------------------------------------------
# 9. FIGURE 7: Parameter robustness
# -----------------------------------------------------------------------------

# Classify each parameter combination by the qualitative developmental pattern.
sweep_patterns <- sweep_results %>%
  mutate(
    pattern = case_when(
      delta_mean > 0 & sd_gain > 0 ~ "Improvement + differentiation",
      delta_mean > 0 & sd_gain <= 0 ~ "Improvement only",
      delta_mean <= 0 & sd_gain > 0 ~ "Differentiation only",
      TRUE ~ "Neither"
    ),
    pattern = factor(
      pattern,
      levels = c(
        "Improvement + differentiation",
        "Improvement only",
        "Differentiation only",
        "Neither"
      )
    )
  )

# Publication palette.
pattern_cols <- c(
  "Improvement + differentiation" = COL$orange,
  "Improvement only"              = COL$blue,
  "Differentiation only"          = COL$green,
  "Neither"                       = COL$light_gray
)

# Quadrant percentages.
pattern_summary <- sweep_patterns %>%
  count(pattern, .drop = FALSE) %>%
  mutate(
    proportion = n / sum(n),
    pct_label = scales::percent(proportion, accuracy = 1)
  )

# Pull percentages for annotation.
pct_both <- pattern_summary %>%
  filter(pattern == "Improvement + differentiation") %>%
  pull(pct_label)

pct_improve <- pattern_summary %>%
  filter(pattern == "Improvement only") %>%
  pull(pct_label)

pct_diff <- pattern_summary %>%
  filter(pattern == "Differentiation only") %>%
  pull(pct_label)

pct_neither <- pattern_summary %>%
  filter(pattern == "Neither") %>%
  pull(pct_label)

# Axis ranges.
x_rng <- range(sweep_patterns$delta_mean, na.rm = TRUE)
y_rng <- range(sweep_patterns$sd_gain, na.rm = TRUE)

x_span <- diff(x_rng)
y_span <- diff(y_rng)

# Keep a little breathing room without letting empty space dominate.
x_lim <- c(
  min(x_rng[1], -0.02),
  x_rng[2] + 0.04 * x_span
)

y_lim <- c(
  min(y_rng[1], -0.02),
  y_rng[2] + 0.05 * y_span
)

# Annotation positions.
x_right <- x_lim[2] - 0.03 * diff(x_lim)
x_left  <- x_lim[1] + 0.03 * diff(x_lim)

y_top    <- y_lim[2] - 0.04 * diff(y_lim)
y_bottom <- y_lim[1] + 0.06 * diff(y_lim)

p7 <- ggplot(
  sweep_patterns,
  aes(
    delta_mean,
    sd_gain,
    color = pattern
  )
) +
  
  # Zero lines define the four qualitative regions.
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.30,
    color = COL$gray
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.30,
    color = COL$gray
  ) +
  
  # All parameter combinations.
  geom_point(
    alpha = 0.62,
    size = 1.45,
    stroke = 0
  ) +
  
  scale_color_manual(
    values = pattern_cols,
    drop = FALSE
  ) +
  
  # Quadrant percentages only.
  annotate(
    "text",
    x = x_right,
    y = y_top,
    label = pct_both,
    hjust = 1,
    vjust = 1,
    size = 3.2,
    fontface = "bold",
    color = COL$orange
  ) +
  
  annotate(
    "text",
    x = x_right,
    y = y_bottom,
    label = pct_improve,
    hjust = 1,
    vjust = 0,
    size = 3.0,
    fontface = "bold",
    color = COL$blue
  ) +
  
  annotate(
    "text",
    x = x_left,
    y = y_top,
    label = pct_diff,
    hjust = 0,
    vjust = 1,
    size = 3.0,
    fontface = "bold",
    color = COL$green
  ) +
  
  annotate(
    "text",
    x = x_left,
    y = y_bottom,
    label = pct_neither,
    hjust = 0,
    vjust = 0,
    size = 2.8,
    color = COL$dark_gray
  ) +
  
  coord_cartesian(
    xlim = x_lim,
    ylim = y_lim,
    clip = "off"
  ) +
  
  labs(
    x = "Change in mean EF engagement (late - early)",
    y = "Change in between-agent SD (late - early)"
  ) +
  
  theme_pub(base_size = 9.0) +
  
  theme(
    legend.position = "bottom",
    legend.key.width = grid::unit(14, "pt"),
    legend.spacing.x = grid::unit(4, "pt"),
    plot.margin = margin(7, 8, 7, 7)
  )

save_pub(
  p7,
  "Figure7_parameter_robustness",
  width = 6.6,
  height = 4.8
)

# Export quadrant summary for text/caption.
write.csv(
  pattern_summary,
  file.path(
    OUT_DIR,
    "Figure7_parameter_pattern_summary.csv"
  ),
  row.names = FALSE
)



# -----------------------------------------------------------------------------
# 10. Console checks
# -----------------------------------------------------------------------------

cat(
  "\nPublication figures written to:\n  ",
  normalizePath(OUT_DIR),
  "\n\n",
  sep = ""
)

cat("Figure 2 model metrics (consistent windows):\n")
print(model_metrics)

cat("\nFigure 3 rank stability:\n")
print(stability_identical)

cat("\nFigure 3 replicated long-run rank memory:\n")
print(rank_memory_summary)

cat("\nFigure 4 environmental sensitivity:\n")
print(reward_sd_sensitivity)

cat("\nFigure 5 late stress difference:\n")
print(late_residual)

cat("\nFigure 6 support timing effects:\n")
print(reward_timing_effects)

cat("\nFigure 7 robustness summary:\n")
print(robustness_summary)

cat("\nFigure 7 quadrant summary:\n")
print(pattern_summary)
