# DEV-Control: Developing Expected Value of Control

Code for Park & Munakata, *Individual Differences in Executive-Function Engagement Can Emerge Without Differences in Neurodevelopmental Capacity: A Developing Expected Value of Control Model* (in preparation for *Developmental Science*).

The model simulates repeated decisions about whether to engage executive function (EF). Engaging EF depends on its learned expected payoff (V), its current effort cost (C), and temporary context (X). Engagement then updates V through experienced outcomes and lowers C through practice. No simulation includes capacity differences. In the shared-environment simulations, agents also start with identical states, parameters, and environments.

## Running the code

Requires R (≥ 4.2) with `ggplot2`, `dplyr`, `tidyr`, `patchwork`, and `scales` (`svglite` is optional, for SVG output).

From the project root:

```r
Rscript run_all.R
```

This reproduces all simulation results, Figures 2 to 7, the supplementary figures, and the values in the tables (about 10 minutes on a laptop). Figure 1 has its own render script in `figure1/`. Each script in `analysis/` can also be run on its own from the project root.

## Structure

```
R/
  model.R          model, default parameters, seeds
  metrics.R        developmental periods, mean / SD change, rank persistence
  plot_style.R     shared figure theme
analysis/
  01_nested_models.R        Simulation 1 (M0, M1, M2)                 Figure 2
  02_identical_starts.R     Simulation 2 (emergence, persistence)      Figure 3, S2
  03_environment.R          Simulation 3 (environmental differences)   Figure 4
  04_temporary_stress.R     Simulation 4 (temporary stress)            Figure 5, S3
  05_support_timing.R       Simulation 5 (timing of support)           Figure 6, S4
  06_parameter_sweep.R      Simulation 6 (250 parameter sets)          Figure 7, Table S1
  07_window_robustness.R    choice of developmental periods            Figure S1
figure1/           model diagram (Mermaid source and render script)
results/           CSV output from the analysis scripts
figures/           PDF / PNG / SVG output from the analysis scripts
archive/           earlier model versions and notebooks (not needed to reproduce the paper)
```

## Notes

- Time in the model is a sequence of opportunities to engage EF, not literal days or years.
- Simulations 1 to 5 use the same 20 seeds (`seeds` in `R/model.R`). Conditions that are compared directly (e.g., stress vs. reference) use the same seed, so they differ only in the manipulation. Numbers in the paper are medians over the 20 replications. Simulation 6 runs each parameter combination five times with its own seeds.
- The first 1,500 steps of the long runs in Simulation 2, and the SD = 0 condition in Simulation 3, are identical to the Simulation 1 runs with the same seed.
- Parameter values are provisional and were chosen so the dynamics are visible within 1,500 steps. Simulation 6 and the robustness checks (Figures S1, S3, and S4) show how much the results depend on them.

## Figure 1

`figure1/render_figure1.sh` rebuilds the diagram from the Mermaid files. It needs `@mermaid-js/mermaid-cli` and `playwright` (see the comments in the script).

## License

The code is released under the MIT License (see `LICENSE`). To cite it, see `CITATION.cff`.
