# ============================================
# Main Analysis Pipeline
# ============================================

# ========== Configuration ==========
RUN_EDA <- FALSE       # Run exploratory data analysis and generate report
RUN_MODELS <- TRUE  # Run reinforcement learning model fitting
# ===================================

# ========== Modeling Policy ==========
# Professor preference (Jan): always use a 2-chain approach.
STAN_CHAINS <- 2
STAN_ITER <- 10000
STAN_WARMUP <- 8000
# =====================================

# ========== Initialization ==========

# Load required packages
library(tidyverse)    # data manipulation and ggplot2
library(rstan)        # bayesian modeling
library(loo)          # model comparison
library(here)         # path management (optional but recommended)

# Configure rstan for optimal performance
options(mc.cores = parallel::detectCores())
# Cache compiled Stan models (separate from caching fitted objects)
rstan_options(auto_write = TRUE)

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

  # 1. Compute Behavioral Metrics
  message("Computing data cleaning statistics...")
  cleaning_stats_subj <- compute_cleaning_stats_subject(data_proc$task)
  cleaning_stats_agg <- compute_cleaning_stats_aggregate(cleaning_stats_subj)

  # Stimulus preference (choice proportions)
  message("Computing stimulus preference (choice proportions)...")
  stim_pref_subj <- compute_stimulus_preference_subject(data_proc$task)
  stim_pref_agg <- compute_stimulus_preference_aggregate(data_proc$task)
  stim_pref_test <- test_stimulus_preference(stim_pref_subj, mu = 0.5)

  # TODO: Compute further behavioral metrics
  # choice_metrics <- compute_choice_metrics(data_proc$task)
  # rt_metrics <- compute_rt_metrics(data_proc$task)
  # accuracy_metrics <- compute_accuracy_metrics(data_proc$task)
  # wsls_metrics <- compute_wsls_metrics(data_proc$task)
  # prp_metrics <- compute_prp_metrics(data_proc$task)
  # quest_metrics <- compute_questionnaire_metrics(data_proc)



  # 2. Generate and Save Visualizations
  # Generate participant inspection plots (data inspection)
  # This creates plots in analysis/outputs/figs/inspection/participant_wise
  viz_inspection_participant_trials(data_proc$task, "analysis/outputs/figs")
  viz_inspection_participant_choice_reward(data_proc$task, "analysis/outputs/figs")

  # 3. Generate EDA Report
  message("Generating EDA Report...")
  if (!dir.exists("analysis/outputs/reports")) {
    dir.create("analysis/outputs/reports", recursive = TRUE)
  }

  rmarkdown::render(
    input = "analysis/reports/eda_report.Rmd",
    output_file = paste0("eda_report_", format(Sys.time(), "%Y%m%d"), ".html"),
    output_dir = "analysis/outputs/reports",
    params = list(
      cleaning_stats_agg = cleaning_stats_agg,
      cleaning_stats_subj = cleaning_stats_subj,
      stim_pref_agg = stim_pref_agg,
      stim_pref_subj = stim_pref_subj,
      stim_pref_test = stim_pref_test
    ),
    quiet = FALSE
  )
  message(sprintf("Report saved to: analysis/outputs/reports/eda_report_%s.html", format(Sys.time(), "%Y%m%d")))

  message("EDA complete.")
}

# Filter out Participant 16 (Non-convergence/Sticky behavior)
# Scientific Note: Participant 16 excluded due to non-convergence and high lose-stay probability (32% lose-shift),
# indicating "sticky" choice behavior inconsistent with RL assumptions.
message("Excluding Participant 16 (Non-convergence/Sticky behavior) for modeling...")
data_proc$task <- data_proc$task %>% dplyr::filter(participant_id != "16")
if (!is.null(data_proc$questionnaire)) {
  data_proc$questionnaire <- data_proc$questionnaire %>% dplyr::filter(participant_id != "16")
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

  # Fit + diagnostics + LOO + posterior extraction (optionally parallel across models)
  pipeline <- rl_run_models_pipeline(
    model_names = model_names,
    stan_data = stan_data,
    timestamp = TIMESTAMP,
    fit_dir = "analysis/outputs/fits",
    force_refit = FALSE,
    chains = STAN_CHAINS,
    iter = STAN_ITER,
    warmup = STAN_WARMUP,
    reserve_cores = 1,
    verbose = TRUE
  )

  # Container for LOO objects (lightweight), diagnostics, and posterior samples
  loo_objects <- lapply(pipeline$results, function(x) x$loo)
  diagnostic_summaries <- lapply(pipeline$results, function(x) x$diagnostics_summary)
  posterior_samples_list <- lapply(pipeline$results, function(x) x$posterior_data)

  # Combine diagnostic summaries into one table for comparison
  all_diagnostics <- dplyr::bind_rows(diagnostic_summaries)
  if (!dir.exists("analysis/outputs/tables")) dir.create("analysis/outputs/tables", recursive = TRUE)
  readr::write_csv(all_diagnostics, file.path("analysis/outputs/tables", paste0("all_models_diagnostics_", TIMESTAMP, ".csv")))

  # Generate posterior density grid for all models
  # Pass the list of extracted dataframes instead of fit objects
  viz_posterior_densities_grid(posterior_samples_list, "analysis/outputs/figs")

  # Compare models using LOO (pass the list of pre-computed LOO objects)
  comparison <- rl_compare_models(
    fit_list = loo_objects,
    save_dir = "analysis/outputs/tables",
    save_file = TRUE
  )

  # Extract parameters from winning model
  message(sprintf("Loading winning model (%s) for parameter extraction...", comparison$winning_model))
  winning_fit <- rl_load_or_fit(
      comparison$winning_model,
      stan_data = stan_data,
      fit_dir = "analysis/outputs/fits",
      force_refit = FALSE,
      chains = STAN_CHAINS,
      iter = STAN_ITER,
      warmup = STAN_WARMUP,
      verbose = TRUE
  )

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
