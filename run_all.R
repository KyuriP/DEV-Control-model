# Reproduce every result and figure in the paper.
# Run from the project root: Rscript run_all.R
# Takes about 10 minutes on a laptop.

scripts <- c(
  "analysis/01_nested_models.R",
  "analysis/02_identical_starts.R",
  "analysis/03_environment.R",
  "analysis/04_temporary_stress.R",
  "analysis/05_support_timing.R",
  "analysis/06_parameter_sweep.R",
  "analysis/07_window_robustness.R"
)

for (s in scripts) {
  message("Running ", s)
  t0 <- Sys.time()
  source(s, local = new.env())
  message("  done in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
}

writeLines(capture.output(sessionInfo()), "results/session_info.txt")
