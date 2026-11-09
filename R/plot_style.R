# Shared figure style

library(ggplot2)

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

theme_pub <- function(base_size = 9) {
  theme_classic(base_size = base_size, base_family = "sans") +
    theme(
      text = element_text(color = COL$ink),
      axis.text = element_text(color = COL$ink, size = rel(0.92)),
      axis.line = element_line(linewidth = 0.28, color = COL$ink),
      axis.ticks = element_line(linewidth = 0.28, color = COL$ink),
      axis.ticks.length = grid::unit(2, "pt"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = rel(0.88)),
      legend.key.width = grid::unit(14, "pt"),
      legend.key.height = grid::unit(7, "pt"),
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0),
      plot.margin = margin(5, 6, 5, 5)
    )
}

panel_tags <- theme(
  plot.tag = element_text(face = "bold", size = 9.5),
  plot.tag.position = c(0.01, 0.99)
)

# Save as PDF, 600-dpi PNG, and SVG (if svglite is installed).
save_fig <- function(plot, name, width, height, dir = "figures") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  pdf_dev <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
  ggsave(file.path(dir, paste0(name, ".pdf")), plot,
         width = width, height = height, units = "in", device = pdf_dev)
  ggsave(file.path(dir, paste0(name, ".png")), plot,
         width = width, height = height, units = "in", dpi = 600, bg = "white")
  if (requireNamespace("svglite", quietly = TRUE)) {
    ggsave(file.path(dir, paste0(name, ".svg")), plot,
           width = width, height = height, units = "in", device = svglite::svglite)
  }
  invisible(plot)
}

# Long data frame of individual trajectories for plotting.
trajectories_df <- function(sim, ids) {
  data.frame(
    agent = rep(ids, times = length(sim$steps)),
    step = rep(sim$steps, each = length(ids)),
    pi = as.vector(sim$pi[ids, , drop = FALSE])
  )
}

step_axis <- scale_x_continuous(breaks = c(1, 500, 1000, 1500),
                                labels = c("0", "500", "1000", "1500"))
