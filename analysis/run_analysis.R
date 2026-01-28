# ============================================
# Main Analysis Pipeline
# ============================================

# ========== Configuration ==========
RUN_EDA <- TRUE       # Run exploratory data analysis and generate report
RUN_MODELS <- FALSE  # Run reinforcement learning model fitting
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
# NOTE (macOS): fork-based parallelism can crash when ObjC-backed libs are loaded.
# For robustness, default to single-core sampling on macOS. If you need speed,
# consider switching to cmdstanr (separate processes) instead of forking.
sysname <- tryCatch(Sys.info()[["sysname"]], error = function(e) NA_character_)
if (!is.na(sysname) && sysname == "Darwin") {
  options(mc.cores = 1)
} else {
  options(mc.cores = parallel::detectCores())
}
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

  # 1. Compute Behavioral Metrics
  message("Computing data cleaning statistics...")
  cleaning_stats_subj <- compute_cleaning_stats_subject(data_proc$task)
  cleaning_stats_agg <- compute_cleaning_stats_aggregate(cleaning_stats_subj)

  # Stimulus preference (choice proportions)
  message("Computing stimulus preference (choice proportions)...")
  stim_pref_subj <- compute_stimulus_preference_subject(data_proc$task)
  stim_pref_agg <- compute_stimulus_preference_aggregate(data_proc$task)
  stim_pref_test <- test_stimulus_preference(stim_pref_subj, mu = 0.5)

  # Reward, feedback, and learning checks
  message("Computing reward/feedback summaries...")
  reward_rate_by_choice <- compute_reward_rate_by_choice(data_proc$task)
  reward_rate_over_bins <- compute_reward_rate_over_trial_bins(data_proc$task, n_bins = 10)
  feedback_condition_overall <- compute_feedback_condition_proportions(data_proc$task, valid_only = TRUE)
  feedback_condition_by_subject <- compute_feedback_condition_proportions_by_subject(data_proc$task, valid_only = TRUE)

  # Reaction time (RT) metrics
  message("Computing reaction time (RT) summaries...")
  rt_summary_subj <- compute_rt_subject_summary(data_proc$task, fast_rt_threshold = 0.2)
  rt_summary_agg <- compute_rt_aggregate_summary(data_proc$task, fast_rt_threshold = 0.2)
  rt_over_bins <- compute_rt_over_trial_bins(data_proc$task, n_bins = 10, fast_rt_threshold = 0.2)
  rt_by_condition <- compute_rt_by_condition(data_proc$task)

  # Accuracy metrics
  message("Computing accuracy summaries...")
  accuracy_subj <- compute_accuracy_subject(data_proc$task)
  accuracy_agg <- compute_accuracy_aggregate(accuracy_subj)
  accuracy_test <- test_accuracy_vs_chance(accuracy_subj, mu = 0.5)
  accuracy_over_bins <- compute_accuracy_over_trial_bins(data_proc$task, n_bins = 20)

  # Choice strategies
  message("Computing choice strategy metrics (WSLS, PRP)...")
  wsls_by_outcome_subj <- compute_wsls_by_outcome_subject(data_proc$task)
  prp_by_outcome_subj <- compute_prp_median_by_outcome_subject(data_proc$task)
  wsls_test <- test_wsls_salient_vs_nonsalient(wsls_by_outcome_subj)
  prp_test <- test_prp_salient_vs_nonsalient(prp_by_outcome_subj)
  reward_rate_subj <- compute_reward_rate_subject(data_proc$task)
  win_stay_overall_subj <- compute_win_stay_overall_subject(wsls_by_outcome_subj)



  # 2. Generate and Save Visualizations
  # Generate participant inspection plots (data inspection)
  # This creates plots in analysis/outputs/figs/inspection/participant_wise
  viz_inspection_participant_trials(data_proc$task, "analysis/outputs/figs")

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
      stim_pref_test = stim_pref_test,
      reward_rate_by_choice = reward_rate_by_choice,
      reward_rate_over_bins = reward_rate_over_bins,
      feedback_condition_overall = feedback_condition_overall,
      feedback_condition_by_subject = feedback_condition_by_subject,
      rt_summary_agg = rt_summary_agg,
      rt_summary_subj = rt_summary_subj,
      rt_over_bins = rt_over_bins,
      rt_by_condition_summary = rt_by_condition$summary,
      rt_by_condition_data = rt_by_condition$data,
      accuracy_agg = accuracy_agg,
      accuracy_subj = accuracy_subj,
      accuracy_test = accuracy_test,
      accuracy_over_bins = accuracy_over_bins,
      wsls_by_outcome_subj = wsls_by_outcome_subj,
      prp_by_outcome_subj = prp_by_outcome_subj,
      wsls_test = wsls_test,
      prp_test = prp_test,
      reward_rate_subj = reward_rate_subj,
      win_stay_overall_subj = win_stay_overall_subj
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

  # Container for LOO objects (lightweight), diagnostics, and posterior samples
  loo_objects <- list()
  diagnostic_summaries <- list()
  posterior_samples_list <- list()

  # Process models sequentially to save memory
  message(sprintf("\n=== Processing %d models sequentially ===", length(model_names)))

  for (model_name in model_names) {
    message(sprintf("\n--- Processing model: %s ---", model_name))

    # 1. Fit or Load Model
    fit <- rl_load_or_fit(
      model_name,
      stan_data = stan_data,
      fit_dir = "analysis/outputs/fits",
      force_refit = FALSE,
      timestamp = TIMESTAMP,
      chains = STAN_CHAINS,
      iter = STAN_ITER,
      warmup = STAN_WARMUP,
      verbose = TRUE
    )

    # 2. Run Diagnostics
    # Outputs are saved to analysis/outputs/[figs|tables]/[model]_[timestamp]/diagnostics/
    results <- diagnostics_run_all(
      fit = fit,
      model_name = model_name,
      timestamp = TIMESTAMP
    )
    diagnostic_summaries[[model_name]] <- results$summary

    # 3. Compute LOO (for model comparison later)
    # We extract log_lik and compute loo here, then discard fit
    message("Computing LOO...")

    # Check if log_lik exists (handling vector/array parameters which appear as log_lik[1], etc.)
    has_log_lik <- any(grepl("^log_lik", names(fit)))

    if (has_log_lik) {
      tryCatch({
        log_lik <- loo::extract_log_lik(fit, parameter_name = "log_lik")
        loo_objects[[model_name]] <- loo::loo(log_lik)
      }, error = function(e) {
        warning(sprintf("Failed to compute LOO for %s: %s", model_name, e$message))
      })
    } else {
        warning(sprintf("Model %s does not contain log_lik parameter. Skipping LOO.", model_name))
    }

    # 4. Extract posterior samples for density grid plot
    # Uses helper from viz.R to get lightweight data frame
    posterior_samples_list[[model_name]] <- viz_extract_posterior_data(fit, model_name)

    # 5. Clean up
    rm(fit)
    if (exists("log_lik")) rm(log_lik)
    gc() # Force garbage collection
  }

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
