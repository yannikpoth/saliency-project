# Figure out whether we're in repo root or analysis/
.prefix <- if (dir.exists(file.path("analysis", "R"))) "analysis/" else ""
source("analysis/R/io.R")
source("analysis/R/preprocess.R")
source("analysis/R/rl_models.R")
source("analysis/R/behavior_metrics.R")
source("analysis/R/viz.R")

io_init()
df_raw  <- io_read_raw("data/raw")
df_proc <- preprocess(df_raw)

# (placeholder) Fit models later
# fit <- rl_fit_hierarchical(df_proc, "analysis/models/rl_hierarchical_shift.stan")

metrics <- compute_behavior_metrics(df_proc)
viz_all(NULL, metrics)
io_write_results(NULL, metrics)

message("Analysis pipeline (stub) finished.")
