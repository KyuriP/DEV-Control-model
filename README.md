# DEV-Control: Developing Expected Value of Control

Code for Park & Munakata, *Individual Differences in Executive-Function Engagement Can Emerge Without Differences in Neurodevelopmental Capacity: A Developing Expected Value of Control Model* (in preparation for *Developmental Science*).

The model simulates repeated decisions about whether to engage executive function (EF). Engaging EF depends on its learned expected payoff (V), its current effort cost (C), and temporary context (X). Engagement then updates V through experienced outcomes and lowers C through practice. All agents start identical and have no capacity differences.

## Running the code

Requires R (≥ 4.2) with `ggplot2`, `dplyr`, `tidyr`, `patchwork`, and `scales` (`svglite` is optional, for SVG output).

From the project root:

```r
Rscript run_all.R
```

This reproduces every number, table, and figure in the paper (about 10 minutes). Each script in `analysis/` can also be run on its own from the project root.

## Structure

```
R/
  model.R          model, default parameters, seeds
  metrics.R        developmental periods, mean / SD change, rank persistence
  plot_style.R     shared figure theme
analysis/
  01_nested_models.R        Simulation 1 (M0, M1, M2)              Figure 2
  02_parameter_sweep.R      Simulation 2 (250 parameter sets)       Figure 3
  03_identical_starts.R     Simulation 3 (emergence, persistence)   Figure 4, S1
  04_environment.R          Simulation 4 (environmental differences) Figure 5
  05_temporary_stress.R     Simulation 5 (temporary stress)         Figure 6, S3
  06_support_timing.R       Simulation 6 (timing of support)        Figure 7, S4
  07_window_robustness.R    choice of developmental periods         Figure S2
figure1/           model diagram (Mermaid source and render script)
results/           CSV output from the analysis scripts
figures/           PDF / PNG / SVG output from the analysis scripts
archive/           earlier model versions and notebooks (not needed to reproduce the paper)
```

## Notes

- Time in the model is a sequence of opportunities to engage EF, not literal days or years.
- All main simulations use the same 20 seeds (`seeds` in `R/model.R`). Conditions that are compared directly (e.g., stress vs. reference) use the same seed, so they differ only in the manipulation. Numbers in the paper are medians over the 20 replications.
- The first 1,500 steps of the long runs in Simulation 3, and the SD = 0 condition in Simulation 4, are identical to the Simulation 1 runs with the same seed.
- Parameter values are provisional and were chosen so the dynamics are visible within 1,500 steps. Simulation 2 and the robustness checks (Figures S2 to S4) show how much the results depend on them.

## Figure 1

`figure1/render_figure1.sh` rebuilds the diagram from the Mermaid files. It needs `@mermaid-js/mermaid-cli` and `playwright` (see the comments in the script).
