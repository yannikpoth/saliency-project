#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

infile <- if (length(args) >= 1) {
  args[[1]]
} else {
  "analysis/outputs/tables/model02_alpha_shift_20260129_102952/diagnostics/parameter_diagnostics.csv"
}

outdir <- if (length(args) >= 2) {
  args[[2]]
} else {
  "analysis/outputs/figs"
}

if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

df <- read.csv(infile, stringsAsFactors = FALSE)

df_subj <- df[grepl("^alpha_shift_subj_raw\\[", df$Parameter), c("Parameter", "Mean")]
df_subj$subj <- as.integer(sub("^alpha_shift_subj_raw\\[([0-9]+)\\].*$", "\\1", df_subj$Parameter))
df_subj <- df_subj[order(df_subj$subj), ]

q <- unname(stats::quantile(df_subj$Mean, probs = c(0.25, 0.75), na.rm = TRUE, type = 7))
iqr <- q[[2]] - q[[1]]
lower_fence <- q[[1]] - 1.5 * iqr
upper_fence <- q[[2]] + 1.5 * iqr
df_subj$outlier <- (df_subj$Mean < lower_fence) | (df_subj$Mean > upper_fence)

outfile <- file.path(
  outdir,
  paste0(
    "boxplot_alpha_shift_subj_raw_mean_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    ".png"
  )
)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  df_in <- df_subj[!df_subj$outlier, , drop = FALSE]
  df_out <- df_subj[df_subj$outlier, , drop = FALSE]

  p <- ggplot(df_subj, aes(x = "", y = Mean)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(
      data = df_in,
      width = 0.12,
      height = 0,
      alpha = 0.6,
      size = 1.8,
      color = "black"
    ) +
    geom_point(
      data = df_out,
      color = "red",
      size = 2.2
    ) +
    geom_text(
      data = df_out,
      aes(label = subj),
      color = "red",
      nudge_x = 0.12,
      size = 3,
      check_overlap = TRUE
    ) +
    labs(
      x = NULL,
      y = "alpha_shift_subj_raw (posterior mean)",
      title = "Model 02: Subject-level alpha_shift (raw) posterior means"
    ) +
    theme_minimal(base_size = 12)

  ggsave(outfile, p, width = 6, height = 4.5, dpi = 300)
} else {
  png(outfile, width = 1800, height = 1350, res = 300)
  boxplot(
    df_subj$Mean,
    ylab = "alpha_shift_subj_raw (posterior mean)",
    main = "Model 02: Subject-level alpha_shift (raw) posterior means"
  )
  stripchart(
    df_subj$Mean[!df_subj$outlier],
    vertical = TRUE,
    method = "jitter",
    add = TRUE,
    pch = 16,
    col = rgb(0, 0, 0, 0.5)
  )
  points(rep(1, sum(df_subj$outlier)), df_subj$Mean[df_subj$outlier], pch = 16, col = "red")
  text(
    rep(1.07, sum(df_subj$outlier)),
    df_subj$Mean[df_subj$outlier],
    labels = df_subj$subj[df_subj$outlier],
    col = "red",
    cex = 0.8,
    pos = 4
  )
  dev.off()
}

cat("Wrote:", outfile, "\n")
