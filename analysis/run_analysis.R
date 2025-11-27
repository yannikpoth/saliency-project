# ============================================
# Main Analysis Pipeline
# ============================================

# ========== Configuration ==========
RUN_EDA <- FALSE       # Run exploratory data analysis and generate report
RUN_MODELS <- TRUE  # Run reinforcement learning model fitting
# ===================================

# ========== Initialization ==========

# Load required packages
library(tidyverse)    # data manipulation and ggplot2
library(rstan)        # bayesian modeling
library(loo)          # model comparison
library(here)         # path management (optional but recommended)

# Configure rstan for optimal performance
options(mc.cores = parallel::detectCores())
# Note: auto_write disabled - we handle caching via rl_load_or_fit()
# rstan_options(auto_write = TRUE)

# Source Sub-scripts
source("analysis/R/io.R")
source("analysis/R/preprocess.R")
source("analysis/R/rl_models.R")
source("analysis/R/behavior_metrics.R")
source("analysis/R/viz.R")
source("analysis/R/diagnostics.R")

# Initialize Input/Output
io_init()

# Smart caching: use processed data if it exists, otherwise preprocess and cache
proc_task_file <- "data/processed/all_task_data.csv"
proc_quest_file <- "data/processed/all_questionnaire_data.csv"

if (file.exists(proc_task_file) && file.exists(proc_quest_file)) {
  message("Loading preprocessed data from cache...")
  data_proc <- io_read_processed("data/processed")
} else {
  message("Preprocessing raw data...")
  data_raw <- io_read_raw("data/raw")
  data_proc <- preprocess(data_raw)
  io_write_processed(data_proc, "data/processed")
}


# ========== Exploratory Data Analysis ==========
if (RUN_EDA) {
  message("\n=== Running Exploratory Data Analysis ===")

  # TODO: Compute behavioral metrics
  # choice_metrics <- compute_choice_metrics(data_proc$task)
  # rt_metrics <- compute_rt_metrics(data_proc$task)
  # accuracy_metrics <- compute_accuracy_metrics(data_proc$task)
  # wsls_metrics <- compute_wsls_metrics(data_proc$task)
  # prp_metrics <- compute_prp_metrics(data_proc$task)
  # quest_metrics <- compute_questionnaire_metrics(data_proc)

  # TODO: Generate and save visualizations
  # plot_choice_analysis(choice_metrics, "analysis/outputs/figs")
  # plot_rt_analysis(rt_metrics, "analysis/outputs/figs")
  # plot_accuracy_analysis(accuracy_metrics, "analysis/outputs/figs")
  # plot_wsls_analysis(wsls_metrics, "analysis/outputs/figs")
  # plot_prp_analysis(prp_metrics, "analysis/outputs/figs")
  # plot_questionnaire_analysis(quest_metrics, "analysis/outputs/figs")

  # Generate participant inspection plots (data inspection)
  # This creates plots in analysis/outputs/figs/inspection/participant_wise
  viz_inspection_participant_trials(data_proc$task, "analysis/outputs/figs")
  viz_inspection_participant_choice_reward(data_proc$task, "analysis/outputs/figs")

  # TODO: Generate EDA report
  # if (!dir.exists("analysis/outputs/reports")) {
  #   dir.create("analysis/outputs/reports", recursive = TRUE)
  # }
  # rmarkdown::render(
  #   "analysis/reports/eda_report.Rmd",
  #   output_dir = "analysis/outputs/reports",
  #   quiet = FALSE
  # )

  message("EDA complete. (Currently placeholder - functions not yet implemented)")
}

# ========== Model Fitting  ==========
if (RUN_MODELS) {
  message("\n=== Running Reinforcement Learning Models ===")

  # Prepare data for Stan models
  stan_data <- prepare_stan_data(data_proc$task)

  # Discover available models
  all_models <- rl_discover_models(model_dir = "analysis/models")

  message(sprintf("Found %d Stan models: %s",
                  length(all_models),
                  paste(all_models, collapse = ", ")))

  # Interactive model selection (prompts user in console)
  # Examples: "1" = first model only, "1,2,3" = first three models, "7" = all models
  model_names <- rl_select_models_interactive(all_models)

  # Timestamp for this analysis run (used for output directories and fit files)
  TIMESTAMP <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # Fit selected models (with smart caching - will load existing fits if available)
  # Set force_refit = TRUE to refit selected models from scratch
  fits <- rl_fit_all(
    model_names = model_names,
    stan_data = stan_data,
    fit_dir = "analysis/outputs/fits",
    force_refit = TRUE,
    timestamp = TIMESTAMP,
    chains = 2,
    iter = 12000,
    warmup = 10000,
    verbose = TRUE
  )

  # Run comprehensive diagnostics for all models
  message("\n=== Running Comprehensive Diagnostics ===")
  diagnostic_summaries <- list()

  for (model_name in names(fits)) {
    # Run full diagnostics suite (plots + tables)
    # Outputs are saved to analysis/outputs/[figs|tables]/[model]_[timestamp]/diagnostics/
    results <- diagnostics_run_all(
      fit = fits[[model_name]],
      model_name = model_name,
      timestamp = TIMESTAMP
    )
    diagnostic_summaries[[model_name]] <- results$summary
  }

  # Combine diagnostic summaries into one table for comparison
  all_diagnostics <- dplyr::bind_rows(diagnostic_summaries)
  if (!dir.exists("analysis/outputs/tables")) dir.create("analysis/outputs/tables", recursive = TRUE)
  readr::write_csv(all_diagnostics, file.path("analysis/outputs/tables", paste0("all_models_diagnostics_", TIMESTAMP, ".csv")))

  # Compare models using LOO
  comparison <- rl_compare_models(
    fit_list = fits,
    save_dir = "analysis/outputs/tables",
    save_file = TRUE
  )

  # Extract parameters from winning model
  winning_fit <- fits[[comparison$winning_model]]
  params_summary <- rl_extract_params(winning_fit)

  # Save parameter estimates
  if (!dir.exists("analysis/outputs/tables")) {
    dir.create("analysis/outputs/tables", recursive = TRUE)
  }
  readr::write_csv(
    params_summary,
    file.path("analysis/outputs/tables", "winning_model_params.csv")
  )
  message(sprintf("Saved parameter estimates to: analysis/outputs/tables/winning_model_params.csv"))

  message("\n=== Model fitting complete ===")
}

message("\n=== Analysis pipeline complete ===")
