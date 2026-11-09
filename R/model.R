# DEV-Control model
# Kyuri Park & Yuko Munakata

# Default parameters --------------------------------------------------------
# Provisional values, not estimates. Chosen to keep engagement away from the
# floor and ceiling so the dynamics are visible over 1,500 steps.
default_pars <- list(
  N = 1000,           # agents
  T = 1500,           # simulation steps (opportunities to engage EF)
  V0 = 0.577,         # initial expected payoff
  C0 = 0.987,         # initial effective effort cost
  tau = 0.163,        # decision temperature
  p_reward = 0.591,   # probability that engagement pays off
  reward_mag = 1.150, # payoff when it does
  alpha_V = 0.0244,   # value-learning rate
  alpha_C = 0.00263,  # practice (cost-reduction) rate
  C_min = 0.459       # lower bound on effort cost
)

# Simulations 1-5 use the same 20 seeds, so directly compared conditions are
# paired (e.g., stress vs. reference differ only in the stress pulse).
# Simulation 6 defines its own seeds.
seeds <- 1001:1020

model_labels <- c(
  evc        = "M0: Control allocation only",
  value      = "M1: + value learning",
  value_cost = "M2: + practice feedback"
)

clip <- function(x, lo = 0, hi = 1) pmin(pmax(x, lo), hi)

# Simulate one population ---------------------------------------------------
# model: "evc" (M0), "value" (M1), or "value_cost" (M2)
# ...: overrides for default_pars (e.g., N = 150, alpha_C = 0.004)
# p_reward_sd: between-agent SD in reward probability (0 = shared environment)
# p_shift: added to reward probability at each step (support manipulation)
# extra_cost: contextual cost X at each step (stress manipulation)
#
# Returns a list with the engagement propensity of every agent at the
# recorded steps (pi: agents x steps).
simulate_dev_control <- function(model = c("evc", "value", "value_cost"),
                                 ...,
                                 p_reward_sd = 0,
                                 p_shift = NULL,
                                 extra_cost = NULL,
                                 record_every = 5,
                                 seed = 1) {
  model <- match.arg(model)
  par <- modifyList(default_pars, list(...))
  N <- par$N
  T <- par$T

  if (is.null(p_shift)) p_shift <- rep(0, T)
  if (is.null(extra_cost)) extra_cost <- rep(0, T)
  stopifnot(length(p_shift) == T, length(extra_cost) == T)
  stopifnot(N >= 2, par$tau > 0, par$C_min < par$C0)

  set.seed(seed)
  p_i <- clip(rnorm(N, par$p_reward, p_reward_sd))
  V <- rep(par$V0, N)
  C <- rep(par$C0, N)

  steps <- sort(unique(c(1, seq(record_every, T, by = record_every), T)))
  pi_rec <- matrix(NA_real_, nrow = N, ncol = length(steps))
  k <- 1

  for (t in seq_len(T)) {
    p_t <- clip(p_i + p_shift[t])

    # engagement decision
    D <- V - C - extra_cost[t]
    pi_t <- plogis(D / par$tau)
    A <- as.integer(runif(N) < pi_t)

    # outcome (only experienced when EF is engaged)
    Y <- as.integer(runif(N) < p_t)
    R <- A * Y * par$reward_mag

    if (k <= length(steps) && t == steps[k]) {
      pi_rec[, k] <- pi_t
      k <- k + 1
    }

    # learning
    if (model != "evc") V <- V + par$alpha_V * A * (R - V)
    if (model == "value_cost") C <- C - par$alpha_C * A * (C - par$C_min)

    V <- clip(V, 0, par$reward_mag)
    C <- pmax(C, par$C_min)
  }

  list(model = model, T = T, N = N, steps = steps, pi = pi_rec, p_i = p_i)
}

# Temporary manipulations ---------------------------------------------------
# Smooth rise and fall (half sine) starting at `start`, lasting `duration` steps.
sine_pulse <- function(T, start, duration, peak) {
  stopifnot(start >= 1, start + duration - 1 <= T)
  out <- rep(0, T)
  out[start:(start + duration - 1)] <- peak * sin(seq(0, pi, length.out = duration))
  out
}
