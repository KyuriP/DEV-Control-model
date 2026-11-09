# Reproduce every result and figure in the paper.
# Run from the project root: Rscript run_all.R
# Takes about 10-15 minutes on a laptop.

scripts <- c(
  "analysis/01_nested_models.R",
  "analysis/02_parameter_sweep.R",
  "analysis/03_identical_starts.R",
  "analysis/04_environment.R",
  "analysis/05_temporary_stress.R",
  "analysis/06_support_timing.R",
  "analysis/07_window_robustness.R"
)

for (s in scripts) {
  message("Running ", s)
  t0 <- Sys.time()
  source(s, local = new.env())
  message("  done in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
}

writeLines(capture.output(sessionInfo()), "results/session_info.txt")
