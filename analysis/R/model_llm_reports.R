model_reports_flatten_comparison <- function(comparison) {
 #####
 # Convert a loo comparison object into a tibble for reporting
 #
 # Accepts either the full object returned by rl_compare_models() or the raw
 # loo::loo_compare matrix and converts it into a printable tibble.
 #
 # Parameters
 # ----
 # comparison : list | matrix | NULL
 # Object returned by rl_compare_models(), the raw loo comparison matrix,
 # or NULL
 #
 # Returns
 # ----
 # tibble
 # Tidy comparison table with one row per model. Returns an empty tibble when
 # no comparison is available.
 #####
 if (is.null(comparison)) {
   return(tibble::tibble())
 }

 comparison_matrix <- if (is.list(comparison) && !is.null(comparison$comparison)) {
   comparison$comparison
 } else {
   comparison
 }

 if (is.null(comparison_matrix) || length(comparison_matrix) == 0) {
   return(tibble::tibble())
 }

 comparison_tbl <- as.data.frame(comparison_matrix)
 comparison_tbl <- tibble::rownames_to_column(comparison_tbl, "model")
 tibble::as_tibble(comparison_tbl)
}

model_reports_extract_metadata <- function(winning_model_name,
                                           timestamp,
                                           fit,
                                           fit_path = NULL,
                                           stan_data = NULL) {
 #####
 # Build a compact metadata table for the winning-model reports
 #
 # Summarizes the model identity, run timestamp, fit cache path, Stan sampling
 # settings, and the modeled sample size where stan_data is available.
 #
 # Parameters
 # ----
 # winning_model_name : character
 # Name of the winning model
 # timestamp : character
 # Timestamp used for model outputs
 # fit : stanfit
 # Winning fitted Stan model
 # fit_path : character or NULL
 # Path to the cached winning fit file
 # stan_data : list or NULL
 # Stan data list from prepare_stan_data()
 #
 # Returns
 # ----
 # tibble
 # Two-column metadata table with fields and values for report display
 #####
 stopifnot("winning_model_name must be character" = is.character(winning_model_name) && length(winning_model_name) == 1)
 stopifnot("timestamp must be character" = is.character(timestamp) && length(timestamp) == 1)

 metadata <- tibble::tibble(
   field = c(
     "winning_model",
     "timestamp",
     "fit_path",
     "stan_chains",
     "stan_iter",
     "stan_warmup"
   ),
   value = c(
     winning_model_name,
     timestamp,
     if (is.null(fit_path)) NA_character_ else fit_path,
     as.character(fit@sim$chains),
     as.character(fit@sim$iter),
     as.character(fit@sim$warmup)
   )
 )

 if (!is.null(stan_data)) {
   metadata <- dplyr::bind_rows(
     metadata,
     tibble::tibble(
       field = c("n_subjects_modeled", "max_trials_modeled", "n_valid_trials_modeled"),
       value = c(
         as.character(stan_data$nSubs),
         as.character(stan_data$maxTrials),
         as.character(sum(stan_data$subTrials, na.rm = TRUE))
       )
     )
   )
 }

 metadata
}

model_reports_build_artifact_paths <- function(winning_model_name,
                                               timestamp,
                                               fit_path = NULL) {
 #####
 # Build a table of relevant artifact paths for winning-model reports
 #
 # Collects the most important tables and figures produced by the model run so
 # the Markdown reports can point to the supporting artifacts on disk.
 #
 # Parameters
 # ----
 # winning_model_name : character
 # Name of the winning model
 # timestamp : character
 # Timestamp used for model outputs
 # fit_path : character or NULL
 # Path to the cached winning fit file
 #
 # Returns
 # ----
 # tibble
 # Table with artifact labels, file paths, and existence flags
 #####
 stopifnot("winning_model_name must be character" = is.character(winning_model_name) && length(winning_model_name) == 1)
 stopifnot("timestamp must be character" = is.character(timestamp) && length(timestamp) == 1)

 io_get_model_output_dirs_fn <- get("io_get_model_output_dirs", mode = "function")
 dirs <- io_get_model_output_dirs_fn(winning_model_name, timestamp)

 artifacts <- tibble::tibble(
   artifact = c(
     "winning_fit_rds",
     "model_comparison_csv",
     "winning_model_params_csv",
     "all_models_diagnostics_csv",
     "posterior_density_grid_png",
     "diagnostics_summary_csv",
     "parameter_diagnostics_csv",
     "sampler_settings_csv",
     "sampler_parameter_summary_csv",
     "problematic_rhat_csv",
     "problematic_ess_csv",
     "trace_raw_png",
     "trace_transformed_png",
     "density_natural_png",
     "pairs_natural_png",
     "rhat_hist_png",
     "neff_hist_png",
     "nuts_energy_png",
     "problematic_trace_plots_png",
     "ppc_choice_prob_png",
     "ppc_choice_distribution_png",
     "ppc_accuracy_png",
     "ppc_stay_png",
     "ppc_learning_curve_png"
   ),
   path = c(
     if (is.null(fit_path)) NA_character_ else fit_path,
     file.path("analysis/outputs/tables", "model_comparison.csv"),
     file.path("analysis/outputs/tables", "winning_model_params.csv"),
     file.path("analysis/outputs/tables", paste0("all_models_diagnostics_", timestamp, ".csv")),
     file.path("analysis/outputs/figs", "all_models_posterior_density_grid.png"),
     file.path(dirs$tables_diag, "diagnostics_summary.csv"),
     file.path(dirs$tables_diag, "parameter_diagnostics.csv"),
     file.path(dirs$tables_diag, "sampler_settings.csv"),
     file.path(dirs$tables_diag, "sampler_parameter_summary.csv"),
     file.path(dirs$tables_diag, "problematic_rhat.csv"),
     file.path(dirs$tables_diag, "problematic_ess.csv"),
     file.path(dirs$figs_diag, "trace_raw.png"),
     file.path(dirs$figs_diag, "trace_transformed.png"),
     file.path(dirs$figs_diag, "density_natural.png"),
     file.path(dirs$figs_diag, "pairs_natural.png"),
     file.path(dirs$figs_diag, "rhat_hist.png"),
     file.path(dirs$figs_diag, "neff_hist.png"),
     file.path(dirs$figs_diag, "nuts_energy.png"),
     file.path(dirs$figs_diag, "problematic_trace_plots.png"),
     file.path(dirs$figs_diag, "ppc_choice_prob.png"),
     file.path(dirs$figs_ppc, "ppc_choice_distribution.png"),
     file.path(dirs$figs_ppc, "ppc_accuracy.png"),
     file.path(dirs$figs_ppc, "ppc_stay.png"),
     file.path(dirs$figs_ppc, "ppc_learning_curve.png")
   )
 )

 artifacts$exists <- file.exists(artifacts$path)
 artifacts
}

model_reports_extract_ppc_prob_array <- function(fit,
                                                 parameter_name = "pp_choice_stim2_prob") {
 #####
 # Extract posterior predictive choice probabilities from a Stan fit
 #
 # Pulls the generated-quantities array used for PPC summaries. The expected
 # structure is iterations x subjects x trials.
 #
 # Parameters
 # ----
 # fit : stanfit
 # Winning fitted Stan model
 # parameter_name : character
 # Generated-quantities parameter to extract (default: "pp_choice_stim2_prob")
 #
 # Returns
 # ----
 # array or NULL
 # Posterior predictive array if available; otherwise NULL
 #####
 stopifnot("parameter_name must be character" = is.character(parameter_name) && length(parameter_name) == 1)

 if (!(parameter_name %in% names(fit))) {
   return(NULL)
 }

 rstan::extract(fit, pars = parameter_name)[[parameter_name]]
}

model_reports_mean_valid <- function(x) {
 #####
 # Compute a mean after excluding Stan sentinel and invalid values
 #
 # Parameters
 # ----
 # x : numeric vector
 # Numeric values that may contain sentinel values such as -9
 #
 # Returns
 # ----
 # numeric
 # Mean of values in [0, 1], or NA if no valid values remain
 #####
 valid <- x[!is.na(x) & x >= 0 & x <= 1]
 if (length(valid) == 0) {
   return(NA_real_)
 }

 mean(valid)
}

model_reports_summarize_ppc_choice_prob <- function(fit,
                                                    stan_data,
                                                    parameter_name = "pp_choice_stim2_prob") {
 #####
 # Summarize posterior predictive choice probabilities for reporting
 #
 # Compares posterior predictive probabilities of choosing stimulus 2 against
 # the observed modeled choices overall, by subject, and by feedback condition.
 # Stan padding and sentinel values are excluded automatically.
 #
 # Parameters
 # ----
 # fit : stanfit
 # Winning fitted Stan model
 # stan_data : list
 # Stan data list from prepare_stan_data()
 # parameter_name : character
 # Generated-quantities parameter to extract (default: "pp_choice_stim2_prob")
 #
 # Returns
 # ----
 # list
 # Named list with overview, by_subject, by_condition, and notes entries
 #####
 stopifnot("stan_data must be a list" = is.list(stan_data))

 pp_array <- model_reports_extract_ppc_prob_array(fit, parameter_name = parameter_name)
 if (is.null(pp_array)) {
   return(list(
     overview = tibble::tibble(),
     by_subject = tibble::tibble(),
     by_condition = tibble::tibble(),
     notes = sprintf("Generated quantity `%s` was not found in the winning model.", parameter_name)
   ))
 }

 pp_valid <- pp_array[!is.na(pp_array) & pp_array >= 0 & pp_array <= 1]
 observed_choice <- stan_data$choice
 observed_valid_mask <- observed_choice %in% c(1L, 2L)
 observed_choice_stim2 <- ifelse(observed_valid_mask, as.numeric(observed_choice == 2L), NA_real_)
 predicted_mean_by_cell <- apply(pp_array, c(2, 3), model_reports_mean_valid)

 overall_pred_mean <- mean(predicted_mean_by_cell[observed_valid_mask], na.rm = TRUE)
 overall_obs_mean <- mean(observed_choice_stim2[observed_valid_mask], na.rm = TRUE)

 overview <- tibble::tibble(
   parameter = parameter_name,
   n_draws = dim(pp_array)[1],
   n_subjects = dim(pp_array)[2],
   max_trials = dim(pp_array)[3],
   n_valid_draw_entries = length(pp_valid),
   n_valid_modeled_cells = sum(observed_valid_mask, na.rm = TRUE),
   mean_pred_choice_stim2_prob = overall_pred_mean,
   sd_pred_choice_stim2_prob = stats::sd(pp_valid),
   q025_pred_choice_stim2_prob = stats::quantile(pp_valid, probs = 0.025, na.rm = TRUE, names = FALSE),
   median_pred_choice_stim2_prob = stats::median(pp_valid, na.rm = TRUE),
   q975_pred_choice_stim2_prob = stats::quantile(pp_valid, probs = 0.975, na.rm = TRUE, names = FALSE),
   observed_choice_stim2_rate = overall_obs_mean,
   abs_diff_observed_vs_predicted = abs(overall_obs_mean - overall_pred_mean)
 )

 observed_by_subject <- apply(observed_choice_stim2, 1, mean, na.rm = TRUE)
 predicted_by_subject <- apply(predicted_mean_by_cell, 1, mean, na.rm = TRUE)

 by_subject <- tibble::tibble(
   subject_index = seq_len(stan_data$nSubs),
   n_trials = as.integer(stan_data$subTrials),
   observed_choice_stim2_rate = observed_by_subject,
   predicted_choice_stim2_prob_mean = predicted_by_subject,
   abs_diff_observed_vs_predicted = abs(observed_by_subject - predicted_by_subject)
 )

 salient_feedback <- stan_data$salient_feedback
 by_condition <- dplyr::bind_rows(lapply(c(0L, 1L), function(condition_value) {
   condition_mask <- salient_feedback == condition_value & observed_valid_mask
   observed_rate <- mean(observed_choice_stim2[condition_mask], na.rm = TRUE)
   predicted_rate <- mean(predicted_mean_by_cell[condition_mask], na.rm = TRUE)
   tibble::tibble(
     condition_value = condition_value,
     condition_label = ifelse(condition_value == 1L, "salient", "non_salient"),
     n_modeled_cells = sum(condition_mask, na.rm = TRUE),
     observed_choice_stim2_rate = observed_rate,
     predicted_choice_stim2_prob_mean = predicted_rate,
     abs_diff_observed_vs_predicted = abs(observed_rate - predicted_rate)
   )
 }))

 list(
   overview = overview,
   by_subject = by_subject,
   by_condition = by_condition,
   notes = c(
     "Posterior predictive summaries exclude Stan padding and sentinel values outside [0, 1].",
     "Observed choices are taken from the modeled Stan data, where stimulus 2 is coded as choice value 2."
   )
 )
}

model_reports_filter_posterior_summary <- function(posterior_summary) {
 #####
 # Keep only the key transformed group-level parameters for the main report
 #
 # Parameters
 # ----
 # posterior_summary : data.frame
 # Full posterior summary table returned by rl_extract_params()
 #
 # Returns
 # ----
 # tibble
 # Filtered summary table containing alpha_mu, alpha_shift_mu, and beta_mu
 #####
 if (is.null(posterior_summary) || nrow(posterior_summary) == 0) {
   return(tibble::tibble())
 }

 keep_mask <- posterior_summary$parameter %in% c("alpha_mu", "alpha_shift_mu", "beta_mu")
 tibble::as_tibble(posterior_summary[keep_mask, , drop = FALSE])
}

model_reports_build_transformed_sd_diagnostics <- function(fit) {
 #####
 # Derive transformed group-level SD summaries for the main report
 #
 # Computes posterior draws of the across-participant SD for alpha,
 # alpha_shift, and beta on their interpretable scales, then summarizes those
 # draws with median, mean, SD, and R-hat.
 #
 # Parameters
 # ----
 # fit : stanfit
 # Winning fitted Stan model
 #
 # Returns
 # ----
 # tibble
 # Summary rows for alpha_sd, alpha_shift_sd, and beta_sd on transformed
 # scales. Returns an empty tibble when required parameters are unavailable.
 #####
 param_map <- c(
   alpha = "alpha_sd",
   alpha_shift = "alpha_shift_sd",
   beta = "beta_sd"
 )
 available_params <- intersect(names(param_map), names(fit))

 if (length(available_params) == 0) {
   return(tibble::tibble())
 }

 derived_rows <- lapply(available_params, function(source_param) {
   draw_array <- rstan::extract(
     fit,
     pars = source_param,
     permuted = FALSE,
     inc_warmup = FALSE
   )

   if (length(dim(draw_array)) != 3) {
     return(NULL)
   }

   sd_matrix <- apply(draw_array, c(1, 2), stats::sd)
   monitor_array <- array(
     sd_matrix,
     dim = c(nrow(sd_matrix), ncol(sd_matrix), 1),
     dimnames = list(NULL, NULL, param_map[[source_param]])
   )
   monitor_summary <- as.data.frame(
     rstan::monitor(monitor_array, warmup = 0, print = FALSE)
   )

   tibble::tibble(
     Parameter = param_map[[source_param]],
     Median = monitor_summary$`50%`,
     Mean = monitor_summary$mean,
     SD = monitor_summary$sd,
     Rhat = monitor_summary$Rhat
   )
 })

 derived_rows <- derived_rows[!vapply(derived_rows, is.null, logical(1))]
 if (length(derived_rows) == 0) {
   return(tibble::tibble())
 }

 dplyr::bind_rows(derived_rows)
}

model_reports_build_parameter_diagnostics <- function(fit) {
 #####
 # Build a concise parameter-level diagnostics table for the main report
 #
 # Retains transformed group-level means, derived transformed group-level SDs,
 # and transformed participant-level alpha, alpha_shift, and beta parameters.
 # The output is intentionally compact and focused on reportable summaries.
 #
 # Parameters
 # ----
 # fit : stanfit
 # Winning fitted Stan model
 #
 # Returns
 # ----
 # tibble
 # Table with Parameter, Median, Mean, SD, and Rhat columns
 #####
 keep_params <- intersect(
   c("alpha_mu", "alpha_shift_mu", "beta_mu"),
   names(fit)
 )

 for (param_prefix in c("alpha", "alpha_shift", "beta")) {
   keep_params <- c(
     keep_params,
     grep(
       paste0("^", param_prefix, "\\[[0-9]+\\]$"),
       names(fit),
       value = TRUE
     )
   )
 }

 keep_params <- unique(keep_params)

 base_rows <- if (length(keep_params) == 0) {
   tibble::tibble()
 } else {
   summary_df <- as.data.frame(
     rstan::summary(fit, pars = keep_params, probs = 0.5)$summary
   )
    summary_tbl <- tibble::as_tibble(summary_df, rownames = "Parameter")
    tibble::tibble(
      Parameter = summary_tbl$Parameter,
      Median = summary_tbl$`50%`,
      Mean = summary_tbl$mean,
      SD = summary_tbl$sd,
      Rhat = summary_tbl$Rhat
    )
 }

 dplyr::bind_rows(
    base_rows[base_rows$Parameter %in% c("alpha_mu", "alpha_shift_mu", "beta_mu"), , drop = FALSE],
    model_reports_build_transformed_sd_diagnostics(fit = fit),
    base_rows[grepl("^alpha\\[[0-9]+\\]$", base_rows$Parameter), , drop = FALSE],
    base_rows[grepl("^alpha_shift\\[[0-9]+\\]$", base_rows$Parameter), , drop = FALSE],
    base_rows[grepl("^beta\\[[0-9]+\\]$", base_rows$Parameter), , drop = FALSE]
 )
}

model_reports_build_upper_interval <- function(fit,
                                               parameter_name = "alpha_shift_mu",
                                               cred_mass = 0.95) {
 #####
 # Build a one-sided upper posterior interval table for a parameter
 #
 # For directed hypotheses, this helper reports the upper credible interval:
 # the lower bound is the (1 - cred_mass) quantile and the upper bound is the
 # observed upper end of the posterior draws.
 #
 # Parameters
 # ----
 # fit : stanfit
 # Winning fitted Stan model
 # parameter_name : character
 # Parameter to summarize (default: "alpha_shift_mu")
 # cred_mass : numeric
 # Posterior mass retained in the upper interval (default: 0.95)
 #
 # Returns
 # ----
 # tibble
 # One-row table with the one-sided upper interval. Returns an empty tibble if
 # the parameter is unavailable.
 #####
 stopifnot("parameter_name must be character" = is.character(parameter_name) && length(parameter_name) == 1)
 stopifnot("cred_mass must be in (0, 1)" = is.numeric(cred_mass) && length(cred_mass) == 1 && cred_mass > 0 && cred_mass < 1)

 if (!(parameter_name %in% names(fit))) {
   return(tibble::tibble())
 }

 draws <- rstan::extract(fit, pars = parameter_name)[[parameter_name]]
 lower_prob <- 1 - cred_mass

 tibble::tibble(
   parameter = parameter_name,
   interval = sprintf("upper_%s%%", format(cred_mass * 100, trim = TRUE, scientific = FALSE)),
   lower = stats::quantile(draws, probs = lower_prob, na.rm = TRUE, names = FALSE),
    upper = max(draws, na.rm = TRUE)
 )
}

model_reports_build_main_params <- function(winning_fit,
                                            winning_model_name,
                                            comparison,
                                            all_diagnostics = NULL,
                                            timestamp,
                                            fit_path = NULL,
                                            stan_data = NULL) {
 #####
 # Assemble the params list for the winning-model main LLM report
 #
 # Parameters
 # ----
 # winning_fit : stanfit
 # Winning fitted Stan model
 # winning_model_name : character
 # Name of the winning model
 # comparison : list | matrix
 # Comparison object from rl_compare_models() or raw loo comparison output
 # all_diagnostics : data.frame | NULL
 # Combined diagnostics table across all fitted models
 # timestamp : character
 # Timestamp used for the current model run
 # fit_path : character or NULL
 # Path to the cached winning fit file
 # stan_data : list or NULL
 # Stan data list from prepare_stan_data()
 #
 # Returns
 # ----
 # list
 # Named params list consumed by the main-model LLM report Rmd
 #####
 stopifnot("winning_model_name must be character" = is.character(winning_model_name) && length(winning_model_name) == 1)
 stopifnot("timestamp must be character" = is.character(timestamp) && length(timestamp) == 1)

 comparison_tbl <- model_reports_flatten_comparison(comparison)
 rl_extract_params_fn <- get("rl_extract_params", mode = "function")

  posterior_summary <- model_reports_filter_posterior_summary(
    rl_extract_params_fn(winning_fit)
  )
  parameter_diagnostics <- model_reports_build_parameter_diagnostics(
    fit = winning_fit
  )

 list(
   metadata = model_reports_extract_metadata(
     winning_model_name = winning_model_name,
     timestamp = timestamp,
     fit = winning_fit,
     fit_path = fit_path,
     stan_data = stan_data
   ),
   model_comparison = comparison_tbl,
   parameter_summary = posterior_summary,
   parameter_diagnostics = parameter_diagnostics,
    alpha_shift_hdi = model_reports_build_upper_interval(
      fit = winning_fit,
      parameter_name = "alpha_shift_mu",
      cred_mass = 0.95
    ),
   notes = c(
     "This report describes the modeled sample used for Stan fitting, not the broader EDA sample.",
     "Participant 16 is excluded before model fitting in the analysis pipeline."
   )
 )
}

# ============================================================================
# Discrete-draw PPCs (aligned with compacted Stan data)
# ============================================================================

model_reports_build_ppc_context <- function(task_data, stan_data) {
  #####
  # Build per-cell PPC context aligned with compacted Stan data
  #
  # Stan data compacts each subject's valid trials into columns 1..subTrials[s].
  # The compact column index is NOT the original trial number, so discrete PPCs
  # that depend on trial-level information (optimal choice, trial block) need a
  # mapping back to the original trial. This helper recomputes the compaction
  # against task_data to produce matrices shaped like stan_data$choice.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data (post-preprocess, post Participant 16 exclusion)
  #     containing columns: participant_id, trial, choice, reward, condition,
  #     reward_prob_1, reward_prob_2.
  # stan_data : list
  #     Stan data list from prepare_stan_data(); used for shape, nSubs,
  #     maxTrials, subTrials, and to verify alignment.
  #
  # Returns
  # ----
  # list
  #     Named list with:
  #     - subject_ids : character vector aligned with stan_data rows
  #     - original_trial : integer matrix (nSubs x maxTrials), -9 for padding
  #     - optimal_choice : integer matrix (nSubs x maxTrials), 1/2, -9 for
  #       padding or ties in reward probabilities
  #     - valid_mask : logical matrix (nSubs x maxTrials) of valid cells
  #####
  stopifnot("task_data must be a data.frame" = is.data.frame(task_data))
  stopifnot("stan_data must be a list" = is.list(stan_data))

  required_cols <- c("participant_id", "trial", "choice", "reward", "condition",
                     "reward_prob_1", "reward_prob_2")
  missing_cols <- setdiff(required_cols, names(task_data))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "model_reports_build_ppc_context(): task_data is missing columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  subject_ids <- sort(unique(as.character(task_data$participant_id)))
  n_subs <- length(subject_ids)
  max_trials <- as.integer(stan_data$maxTrials)

  if (n_subs != stan_data$nSubs) {
    stop(sprintf(
      "model_reports_build_ppc_context(): subject count mismatch (task_data=%d, stan_data=%d). Did you forget to filter Participant 16 before prepare_stan_data()?",
      n_subs, stan_data$nSubs
    ))
  }

  original_trial <- matrix(-9L, nrow = n_subs, ncol = max_trials)
  optimal_choice <- matrix(-9L, nrow = n_subs, ncol = max_trials)

  task_pid <- as.character(task_data$participant_id)

  for (i in seq_along(subject_ids)) {
    row_idx <- which(task_pid == subject_ids[i])
    subj_data <- task_data[row_idx, , drop = FALSE]
    subj_data <- subj_data[order(subj_data$trial), , drop = FALSE]

    is_valid <- (!is.na(subj_data$choice) & subj_data$choice %in% c(0, 1)) &
      (!is.na(subj_data$reward) & subj_data$reward %in% c(0, 1)) &
      (!is.na(subj_data$condition) & subj_data$condition %in% c(0, 1))

    valid_rows <- which(is_valid)
    n_valid <- length(valid_rows)

    if (n_valid != stan_data$subTrials[i]) {
      stop(sprintf(
        "model_reports_build_ppc_context(): subTrials mismatch for subject %s (task_data=%d, stan_data=%d).",
        subject_ids[i], n_valid, stan_data$subTrials[i]
      ))
    }

    if (n_valid > 0) {
      cols <- seq_len(n_valid)
      original_trial[i, cols] <- as.integer(subj_data$trial[valid_rows])

      rp1 <- subj_data$reward_prob_1[valid_rows]
      rp2 <- subj_data$reward_prob_2[valid_rows]
      opt <- ifelse(rp1 > rp2, 1L,
                    ifelse(rp2 > rp1, 2L, -9L))
      opt[is.na(opt)] <- -9L
      optimal_choice[i, cols] <- as.integer(opt)
    }
  }

  valid_mask <- stan_data$choice %in% c(1L, 2L)
  dim(valid_mask) <- dim(stan_data$choice)

  list(
    subject_ids = subject_ids,
    original_trial = original_trial,
    optimal_choice = optimal_choice,
    valid_mask = valid_mask
  )
}


model_reports_extract_predicted_choices <- function(fit,
                                                    num_ppc_sims = 100,
                                                    parameter_name = "predicted_choices") {
  #####
  # Extract and subsample the posterior predictive discrete-choice array
  #
  # Pulls `predicted_choices` (shape [iter, nSubs, maxTrials]) from a Stan fit
  # and returns the first `num_ppc_sims` iterations. Matches the subsampling
  # convention used by the legacy PPC script.
  #
  # Parameters
  # ----
  # fit : stanfit
  #     Winning fitted Stan model
  # num_ppc_sims : integer
  #     Number of posterior draws to retain (default: 100)
  # parameter_name : character
  #     Generated-quantities parameter to extract (default: "predicted_choices")
  #
  # Returns
  # ----
  # array or NULL
  #     3-D array [num_ppc_sims, nSubs, maxTrials], or NULL if the parameter is
  #     not in the fit.
  #####
  stopifnot("num_ppc_sims must be positive integer" = is.numeric(num_ppc_sims) && length(num_ppc_sims) == 1 && num_ppc_sims >= 1)
  stopifnot("parameter_name must be character" = is.character(parameter_name) && length(parameter_name) == 1)

  if (!(parameter_name %in% names(fit))) {
    return(NULL)
  }

  draws <- rstan::extract(fit, pars = parameter_name)[[parameter_name]]
  if (is.null(draws)) return(NULL)

  n_iter <- dim(draws)[1]
  take <- min(as.integer(num_ppc_sims), as.integer(n_iter))
  draws[seq_len(take), , , drop = FALSE]
}


model_reports_ppc_choice_distribution <- function(yrep_draws,
                                                  stan_data,
                                                  ppc_context) {
  #####
  # PPC: distribution of choice counts (stim 1 vs stim 2) over valid cells
  #
  # Computes observed choice counts on valid cells, replicates the same mask
  # across all posterior draws, and summarises counts across simulations.
  # Discrepancy is the chi-squared statistic of observed vs mean replicated.
  #
  # Parameters
  # ----
  # yrep_draws : array
  #     3-D array [num_ppc_sims, nSubs, maxTrials] from
  #     model_reports_extract_predicted_choices().
  # stan_data : list
  #     Stan data list from prepare_stan_data()
  # ppc_context : list
  #     PPC context from model_reports_build_ppc_context()
  #
  # Returns
  # ----
  # list
  #     Named list with:
  #     - observed : tibble(choice, count, proportion)
  #     - replicated_summary : tibble with mean/sd/q025/q500/q975 per choice
  #     - chi_squared : tibble(statistic)
  #####
  stopifnot("yrep_draws must be a 3-D array" = is.array(yrep_draws) && length(dim(yrep_draws)) == 3)

  mask <- ppc_context$valid_mask
  y_obs <- as.integer(stan_data$choice[mask])

  observed_df <- tibble::tibble(choice = c(1L, 2L))
  observed_df$count <- c(sum(y_obs == 1L), sum(y_obs == 2L))
  observed_df$proportion <- observed_df$count / sum(observed_df$count)

  n_sims <- dim(yrep_draws)[1]
  replicated_counts <- matrix(NA_real_, nrow = n_sims, ncol = 2,
                              dimnames = list(NULL, c("1", "2")))
  for (i in seq_len(n_sims)) {
    sim_i <- yrep_draws[i, , ]
    sim_valid <- as.integer(sim_i[mask])
    replicated_counts[i, 1] <- sum(sim_valid == 1L)
    replicated_counts[i, 2] <- sum(sim_valid == 2L)
  }

  replicated_summary <- tibble::tibble(
    choice = c(1L, 2L),
    mean = c(mean(replicated_counts[, 1]), mean(replicated_counts[, 2])),
    sd = c(stats::sd(replicated_counts[, 1]), stats::sd(replicated_counts[, 2])),
    q025 = c(stats::quantile(replicated_counts[, 1], 0.025, names = FALSE),
             stats::quantile(replicated_counts[, 2], 0.025, names = FALSE)),
    median = c(stats::median(replicated_counts[, 1]), stats::median(replicated_counts[, 2])),
    q975 = c(stats::quantile(replicated_counts[, 1], 0.975, names = FALSE),
             stats::quantile(replicated_counts[, 2], 0.975, names = FALSE))
  )

  mean_rep <- colMeans(replicated_counts)
  mean_rep_safe <- ifelse(mean_rep == 0, 1e-9, mean_rep)
  chi_sq <- sum((observed_df$count - mean_rep_safe)^2 / mean_rep_safe)

  list(
    observed = observed_df,
    replicated_summary = replicated_summary,
    chi_squared = tibble::tibble(statistic = chi_sq)
  )
}


model_reports_ppc_accuracy <- function(yrep_draws,
                                       stan_data,
                                       ppc_context) {
  #####
  # PPC: overall subject-level accuracy (proportion of optimal choices)
  #
  # For each subject, compares the observed proportion of optimal choices (on
  # valid cells with an unambiguous optimum) against the average predicted
  # accuracy across simulations. Uses sweep() for the row-wise comparison to
  # avoid R column-recycling pitfalls exposed in the legacy script.
  #
  # Parameters
  # ----
  # yrep_draws : array
  #     3-D array [num_ppc_sims, nSubs, maxTrials]
  # stan_data : list
  #     Stan data list from prepare_stan_data()
  # ppc_context : list
  #     PPC context from model_reports_build_ppc_context()
  #
  # Returns
  # ----
  # list
  #     Named list with:
  #     - per_subject : tibble(subject_index, n_valid, observed_accuracy,
  #       predicted_accuracy_mean, abs_diff)
  #     - group_summary : tibble(type, mean_accuracy, sd_accuracy,
  #       median_accuracy) with rows for Observed and Predicted
  #     - discrepancy : tibble(metric, value) with mean absolute difference
  #####
  stopifnot("yrep_draws must be a 3-D array" = is.array(yrep_draws) && length(dim(yrep_draws)) == 3)

  n_subs <- stan_data$nSubs
  n_sims <- dim(yrep_draws)[1]

  y_obs <- stan_data$choice
  opt <- ppc_context$optimal_choice

  obs_accuracy <- numeric(n_subs)
  pred_accuracy_mean <- numeric(n_subs)
  n_valid_per_sub <- integer(n_subs)

  for (s in seq_len(n_subs)) {
    subj_mask <- ppc_context$valid_mask[s, ] & (opt[s, ] %in% c(1L, 2L))
    n_valid <- sum(subj_mask)
    n_valid_per_sub[s] <- n_valid

    if (n_valid == 0) {
      obs_accuracy[s] <- NA_real_
      pred_accuracy_mean[s] <- NA_real_
      next
    }

    opt_s <- as.integer(opt[s, subj_mask])
    obs_accuracy[s] <- mean(as.integer(y_obs[s, subj_mask]) == opt_s)

    sim_s <- yrep_draws[, s, subj_mask, drop = FALSE]
    sim_matrix <- matrix(as.integer(sim_s), nrow = n_sims, ncol = n_valid)
    correct_matrix <- sweep(sim_matrix, 2, opt_s, `==`)
    pred_accuracy_mean[s] <- mean(rowMeans(correct_matrix), na.rm = TRUE)
  }

  per_subject <- tibble::tibble(
    subject_index = seq_len(n_subs),
    n_valid = n_valid_per_sub,
    observed_accuracy = obs_accuracy,
    predicted_accuracy_mean = pred_accuracy_mean,
    abs_diff = abs(obs_accuracy - pred_accuracy_mean)
  )

  group_summary <- tibble::tibble(
    type = c("Observed", "Predicted"),
    mean_accuracy = c(mean(obs_accuracy, na.rm = TRUE),
                      mean(pred_accuracy_mean, na.rm = TRUE)),
    sd_accuracy = c(stats::sd(obs_accuracy, na.rm = TRUE),
                    stats::sd(pred_accuracy_mean, na.rm = TRUE)),
    median_accuracy = c(stats::median(obs_accuracy, na.rm = TRUE),
                        stats::median(pred_accuracy_mean, na.rm = TRUE))
  )

  discrepancy <- tibble::tibble(
    metric = "mean_abs_diff_subject_accuracy",
    value = mean(abs(obs_accuracy - pred_accuracy_mean), na.rm = TRUE)
  )

  list(
    per_subject = per_subject,
    group_summary = group_summary,
    discrepancy = discrepancy
  )
}


model_reports_stay_probs_from_choices <- function(choice_matrix,
                                                  stan_data,
                                                  ppc_context) {
  #####
  # Internal helper: compute mean-of-subject-means stay probabilities by
  # previous-outcome category (Loss, Non-Salient Win, Salient Win)
  #
  # Operates on a compacted choice matrix (nSubs x maxTrials, 1/2/-9) and uses
  # the observed reward + salient_feedback from stan_data for the lagged
  # outcome classification. Lag is computed on the compacted sequence (i.e.,
  # "previous valid trial"), consistent with the legacy script which applied
  # the same logic to trial-filtered data.
  #
  # Parameters
  # ----
  # choice_matrix : integer matrix
  #     (nSubs x maxTrials) with choices coded 1/2 (and -9 padding).
  # stan_data : list
  #     Stan data list from prepare_stan_data().
  # ppc_context : list
  #     PPC context from model_reports_build_ppc_context().
  #
  # Returns
  # ----
  # numeric vector of length 3 (named "Loss", "Non-Salient Win", "Salient Win")
  #     Group-level mean stay probability per outcome condition. NA if no
  #     subject contributes trials in that condition.
  #####
  outcome_levels <- c("Loss", "Non-Salient Win", "Salient Win")
  n_subs <- stan_data$nSubs

  subject_probs <- matrix(NA_real_, nrow = n_subs, ncol = length(outcome_levels),
                          dimnames = list(NULL, outcome_levels))

  for (s in seq_len(n_subs)) {
    n_valid <- stan_data$subTrials[s]
    if (n_valid < 2) next

    cols <- seq_len(n_valid)
    ch <- as.integer(choice_matrix[s, cols])
    rw <- as.integer(stan_data$reward[s, cols])
    sf <- as.integer(stan_data$salient_feedback[s, cols])

    if (any(!(ch %in% c(1L, 2L)))) next

    prev_ch <- c(NA_integer_, ch[-n_valid])
    prev_rw <- c(NA_integer_, rw[-n_valid])
    prev_sf <- c(NA_integer_, sf[-n_valid])

    stayed <- (ch == prev_ch)

    outcome_type <- rep(NA_character_, n_valid)
    outcome_type[!is.na(prev_rw) & prev_rw == 0L] <- "Loss"
    outcome_type[!is.na(prev_rw) & prev_rw == 1L &
                   !is.na(prev_sf) & prev_sf == 1L] <- "Salient Win"
    outcome_type[!is.na(prev_rw) & prev_rw == 1L &
                   !is.na(prev_sf) & prev_sf == 0L] <- "Non-Salient Win"

    for (lvl in outcome_levels) {
      idx <- which(outcome_type == lvl & !is.na(stayed))
      if (length(idx) > 0) {
        subject_probs[s, lvl] <- mean(stayed[idx], na.rm = TRUE)
      }
    }
  }

  colMeans(subject_probs, na.rm = TRUE)
}


model_reports_ppc_conditional_stay <- function(yrep_draws,
                                               stan_data,
                                               ppc_context) {
  #####
  # PPC: conditional stay probabilities by previous-outcome category
  #
  # For each outcome condition (Loss, Non-Salient Win, Salient Win), computes
  # observed vs replicated mean-of-subject-means stay probabilities and the
  # mean absolute error between observed and mean replicated.
  #
  # Parameters
  # ----
  # yrep_draws : array
  #     3-D array [num_ppc_sims, nSubs, maxTrials]
  # stan_data : list
  #     Stan data list from prepare_stan_data()
  # ppc_context : list
  #     PPC context from model_reports_build_ppc_context()
  #
  # Returns
  # ----
  # list
  #     Named list with:
  #     - observed : tibble(outcome_type, observed_prob_stay)
  #     - replicated_summary : tibble(outcome_type, mean, sd, q025, median, q975)
  #     - discrepancy : tibble(metric, value) with mean absolute error
  #####
  stopifnot("yrep_draws must be a 3-D array" = is.array(yrep_draws) && length(dim(yrep_draws)) == 3)

  outcome_levels <- c("Loss", "Non-Salient Win", "Salient Win")
  n_sims <- dim(yrep_draws)[1]

  obs_probs <- model_reports_stay_probs_from_choices(
    choice_matrix = stan_data$choice,
    stan_data = stan_data,
    ppc_context = ppc_context
  )

  rep_matrix <- matrix(NA_real_, nrow = n_sims, ncol = length(outcome_levels),
                       dimnames = list(NULL, outcome_levels))

  for (i in seq_len(n_sims)) {
    sim_choice <- yrep_draws[i, , ]
    mode(sim_choice) <- "integer"
    sim_choice[!ppc_context$valid_mask] <- -9L
    rep_matrix[i, ] <- model_reports_stay_probs_from_choices(
      choice_matrix = sim_choice,
      stan_data = stan_data,
      ppc_context = ppc_context
    )
  }

  observed <- tibble::tibble(
    outcome_type = factor(outcome_levels, levels = outcome_levels),
    observed_prob_stay = as.numeric(obs_probs[outcome_levels])
  )

  replicated_summary <- tibble::tibble(
    outcome_type = factor(outcome_levels, levels = outcome_levels),
    mean = apply(rep_matrix, 2, mean, na.rm = TRUE),
    sd = apply(rep_matrix, 2, stats::sd, na.rm = TRUE),
    q025 = apply(rep_matrix, 2, stats::quantile, probs = 0.025, na.rm = TRUE, names = FALSE),
    median = apply(rep_matrix, 2, stats::median, na.rm = TRUE),
    q975 = apply(rep_matrix, 2, stats::quantile, probs = 0.975, na.rm = TRUE, names = FALSE)
  )

  mean_rep <- replicated_summary$mean
  mae <- mean(abs(observed$observed_prob_stay - mean_rep), na.rm = TRUE)

  list(
    observed = observed,
    replicated_summary = replicated_summary,
    discrepancy = tibble::tibble(metric = "mean_absolute_error", value = mae)
  )
}


model_reports_learning_curve_from_choices <- function(choice_matrix,
                                                      stan_data,
                                                      ppc_context,
                                                      block_size,
                                                      validity_override = NULL) {
  #####
  # Internal helper: compute per-block proportion-optimal, averaged over subjects
  #
  # Groups cells by block_idx = ceiling(original_trial / block_size), computes
  # per-subject proportion-optimal per block, then takes the group-level mean.
  #
  # Parameters
  # ----
  # choice_matrix : integer matrix
  #     (nSubs x maxTrials), choices coded 1/2, padding -9.
  # stan_data : list
  #     Stan data list from prepare_stan_data()
  # ppc_context : list
  #     PPC context from model_reports_build_ppc_context()
  # block_size : integer
  #     Trials per block (default: 20)
  # validity_override : logical matrix or NULL
  #     Optional validity mask to use instead of ppc_context$valid_mask &
  #     optimal-choice-defined mask. Used to ensure simulated draws are scored
  #     on exactly the same cells as the observed data.
  #
  # Returns
  # ----
  # numeric vector
  #     Group-level mean proportion-optimal per block, ordered by block index.
  #####
  opt <- ppc_context$optimal_choice
  trial_idx <- ppc_context$original_trial

  base_valid <- ppc_context$valid_mask & (opt %in% c(1L, 2L))
  if (!is.null(validity_override)) {
    base_valid <- base_valid & validity_override
  }

  block_idx <- matrix(NA_integer_, nrow = nrow(trial_idx), ncol = ncol(trial_idx))
  block_idx[base_valid] <- as.integer(ceiling(trial_idx[base_valid] / block_size))

  n_blocks <- as.integer(ceiling(max(trial_idx, na.rm = TRUE) / block_size))
  n_subs <- stan_data$nSubs

  per_subject_block <- matrix(NA_real_, nrow = n_subs, ncol = n_blocks)

  for (s in seq_len(n_subs)) {
    sb <- block_idx[s, ]
    ch <- as.integer(choice_matrix[s, ])
    op <- as.integer(opt[s, ])
    for (b in seq_len(n_blocks)) {
      cells <- which(!is.na(sb) & sb == b)
      if (length(cells) > 0) {
        per_subject_block[s, b] <- mean(ch[cells] == op[cells], na.rm = TRUE)
      }
    }
  }

  colMeans(per_subject_block, na.rm = TRUE)
}


model_reports_ppc_learning_curve <- function(yrep_draws,
                                             stan_data,
                                             ppc_context,
                                             block_size = 20) {
  #####
  # PPC: learning curve (proportion of optimal choices across trial blocks)
  #
  # Uses the original trial number (not compact column index) to bin cells into
  # blocks of fixed size, then computes per-subject proportion-optimal per
  # block, averaged across subjects. Simulated draws are scored on exactly the
  # same cells as the observed data.
  #
  # Parameters
  # ----
  # yrep_draws : array
  #     3-D array [num_ppc_sims, nSubs, maxTrials]
  # stan_data : list
  #     Stan data list from prepare_stan_data()
  # ppc_context : list
  #     PPC context from model_reports_build_ppc_context()
  # block_size : integer
  #     Trials per block (default: 20)
  #
  # Returns
  # ----
  # list
  #     Named list with:
  #     - per_block : tibble(block, block_start_trial, block_end_trial,
  #       observed, replicated_mean, replicated_sd, q025, median, q975)
  #     - discrepancy : tibble(metric, value) with mean absolute error
  #     - block_size : integer
  #####
  stopifnot("yrep_draws must be a 3-D array" = is.array(yrep_draws) && length(dim(yrep_draws)) == 3)
  stopifnot("block_size must be positive integer" = is.numeric(block_size) && length(block_size) == 1 && block_size >= 1)

  n_sims <- dim(yrep_draws)[1]

  observed_lc <- model_reports_learning_curve_from_choices(
    choice_matrix = stan_data$choice,
    stan_data = stan_data,
    ppc_context = ppc_context,
    block_size = block_size
  )

  rep_matrix <- matrix(NA_real_, nrow = n_sims, ncol = length(observed_lc))
  for (i in seq_len(n_sims)) {
    sim_choice <- yrep_draws[i, , ]
    mode(sim_choice) <- "integer"
    sim_choice[!ppc_context$valid_mask] <- -9L
    rep_matrix[i, ] <- model_reports_learning_curve_from_choices(
      choice_matrix = sim_choice,
      stan_data = stan_data,
      ppc_context = ppc_context,
      block_size = block_size
    )
  }

  n_blocks <- length(observed_lc)
  block_starts <- seq.int(from = 1L, by = block_size, length.out = n_blocks)
  block_ends <- block_starts + block_size - 1L

  per_block <- tibble::tibble(
    block = seq_len(n_blocks),
    block_start_trial = as.integer(block_starts),
    block_end_trial = as.integer(block_ends),
    observed = observed_lc,
    replicated_mean = apply(rep_matrix, 2, mean, na.rm = TRUE),
    replicated_sd = apply(rep_matrix, 2, stats::sd, na.rm = TRUE),
    q025 = apply(rep_matrix, 2, stats::quantile, probs = 0.025, na.rm = TRUE, names = FALSE),
    median = apply(rep_matrix, 2, stats::median, na.rm = TRUE),
    q975 = apply(rep_matrix, 2, stats::quantile, probs = 0.975, na.rm = TRUE, names = FALSE)
  )

  mae <- mean(abs(per_block$observed - per_block$replicated_mean), na.rm = TRUE)

  list(
    per_block = per_block,
    discrepancy = tibble::tibble(metric = "mean_absolute_error", value = mae),
    block_size = as.integer(block_size)
  )
}


model_reports_build_ppc_discrepancy_summary <- function(ppc_choice_distribution,
                                                        ppc_accuracy,
                                                        ppc_stay,
                                                        ppc_learning_curve) {
  #####
  # Compact discrepancy summary across the four discrete PPCs
  #
  # Parameters
  # ----
  # ppc_choice_distribution : list
  #     Output of model_reports_ppc_choice_distribution()
  # ppc_accuracy : list
  #     Output of model_reports_ppc_accuracy()
  # ppc_stay : list
  #     Output of model_reports_ppc_conditional_stay()
  # ppc_learning_curve : list
  #     Output of model_reports_ppc_learning_curve()
  #
  # Returns
  # ----
  # tibble
  #     One row per PPC with columns: ppc, metric, value
  #####
  tibble::tibble(
    ppc = c("choice_distribution", "overall_accuracy",
            "conditional_stay", "learning_curve"),
    metric = c("chi_squared", ppc_accuracy$discrepancy$metric,
               ppc_stay$discrepancy$metric, ppc_learning_curve$discrepancy$metric),
    value = c(ppc_choice_distribution$chi_squared$statistic,
              ppc_accuracy$discrepancy$value,
              ppc_stay$discrepancy$value,
              ppc_learning_curve$discrepancy$value)
  )
}


model_reports_build_ppc_params <- function(winning_fit,
                                           winning_model_name,
                                           timestamp,
                                           fit_path = NULL,
                                           stan_data,
                                           task_data,
                                           num_ppc_sims = 100,
                                           block_size = 20) {
  #####
  # Assemble the params list for the winning-model PPC LLM report
  #
  # Combines the existing aggregate choice-probability PPC with four
  # discrete-draw PPCs (choice distribution, overall accuracy, conditional stay
  # probabilities, learning curve) derived from `predicted_choices`.
  #
  # Parameters
  # ----
  # winning_fit : stanfit
  #     Winning fitted Stan model
  # winning_model_name : character
  #     Name of the winning model
  # timestamp : character
  #     Timestamp used for the current model run
  # fit_path : character or NULL
  #     Path to the cached winning fit file
  # stan_data : list
  #     Stan data list from prepare_stan_data()
  # task_data : tibble
  #     Cleaned task data (post-preprocess, post Participant 16 exclusion)
  # num_ppc_sims : integer
  #     Number of posterior draws to use for discrete PPCs (default: 100)
  # block_size : integer
  #     Trials per learning-curve block (default: 20)
  #
  # Returns
  # ----
  # list
  #     Named params list consumed by the PPC LLM report Rmd
  #####
  stopifnot("winning_model_name must be character" = is.character(winning_model_name) && length(winning_model_name) == 1)
  stopifnot("timestamp must be character" = is.character(timestamp) && length(timestamp) == 1)
  stopifnot("stan_data must be a list" = is.list(stan_data))
  stopifnot("task_data must be a data.frame" = is.data.frame(task_data))

  prob_summary <- model_reports_summarize_ppc_choice_prob(
    fit = winning_fit,
    stan_data = stan_data
  )

  ppc_context <- model_reports_build_ppc_context(
    task_data = task_data,
    stan_data = stan_data
  )
  yrep_draws <- model_reports_extract_predicted_choices(
    fit = winning_fit,
    num_ppc_sims = num_ppc_sims
  )

  if (is.null(yrep_draws)) {
    notes <- c(
      prob_summary$notes,
      "Generated quantity `predicted_choices` was not found in the winning model; discrete-draw PPCs were skipped."
    )

    artifact_paths <- model_reports_build_artifact_paths(
      winning_model_name = winning_model_name,
      timestamp = timestamp,
      fit_path = fit_path
    )
    artifact_paths <- artifact_paths[
      grepl("^winning_fit_rds$|^ppc_|^diagnostics_summary_csv$", artifact_paths$artifact),
      ,
      drop = FALSE
    ]

    return(list(
      metadata = model_reports_extract_metadata(
        winning_model_name = winning_model_name,
        timestamp = timestamp,
        fit = winning_fit,
        fit_path = fit_path,
        stan_data = stan_data
      ),
      ppc_discrepancy_summary = tibble::tibble(),
      ppc_choice_distribution = list(observed = tibble::tibble(),
                                     replicated_summary = tibble::tibble(),
                                     chi_squared = tibble::tibble()),
      ppc_accuracy = list(per_subject = tibble::tibble(),
                          group_summary = tibble::tibble(),
                          discrepancy = tibble::tibble()),
      ppc_stay = list(observed = tibble::tibble(),
                      replicated_summary = tibble::tibble(),
                      discrepancy = tibble::tibble()),
      ppc_learning_curve = list(per_block = tibble::tibble(),
                                discrepancy = tibble::tibble(),
                                block_size = as.integer(block_size)),
      ppc_prob_overview = prob_summary$overview,
      ppc_prob_by_condition = prob_summary$by_condition,
      artifact_paths = artifact_paths,
      settings = tibble::tibble(
        num_ppc_sims = NA_integer_,
        block_size = as.integer(block_size)
      ),
      notes = notes
    ))
  }

  ppc_cd <- model_reports_ppc_choice_distribution(
    yrep_draws = yrep_draws,
    stan_data = stan_data,
    ppc_context = ppc_context
  )
  ppc_acc <- model_reports_ppc_accuracy(
    yrep_draws = yrep_draws,
    stan_data = stan_data,
    ppc_context = ppc_context
  )
  ppc_stay <- model_reports_ppc_conditional_stay(
    yrep_draws = yrep_draws,
    stan_data = stan_data,
    ppc_context = ppc_context
  )
  ppc_lc <- model_reports_ppc_learning_curve(
    yrep_draws = yrep_draws,
    stan_data = stan_data,
    ppc_context = ppc_context,
    block_size = block_size
  )
  discrepancy_summary <- model_reports_build_ppc_discrepancy_summary(
    ppc_choice_distribution = ppc_cd,
    ppc_accuracy = ppc_acc,
    ppc_stay = ppc_stay,
    ppc_learning_curve = ppc_lc
  )

  artifact_paths <- model_reports_build_artifact_paths(
    winning_model_name = winning_model_name,
    timestamp = timestamp,
    fit_path = fit_path
  )
  artifact_paths <- artifact_paths[
    grepl("^winning_fit_rds$|^ppc_|^diagnostics_summary_csv$", artifact_paths$artifact),
    ,
    drop = FALSE
  ]

  notes <- c(
    prob_summary$notes,
    sprintf("Discrete-draw PPCs use the first %d posterior draws of `predicted_choices`.", dim(yrep_draws)[1]),
    sprintf("Learning-curve PPC uses %d-trial blocks (indexed by original trial number).", as.integer(block_size)),
    "Stan data compacts valid trials per subject; `original_trial` in the PPC context restores the mapping to the task trial number."
  )

  list(
    metadata = model_reports_extract_metadata(
      winning_model_name = winning_model_name,
      timestamp = timestamp,
      fit = winning_fit,
      fit_path = fit_path,
      stan_data = stan_data
    ),
    ppc_discrepancy_summary = discrepancy_summary,
    ppc_choice_distribution = ppc_cd,
    ppc_accuracy = ppc_acc,
    ppc_stay = ppc_stay,
    ppc_learning_curve = ppc_lc,
    ppc_prob_overview = prob_summary$overview,
    ppc_prob_by_condition = prob_summary$by_condition,
    artifact_paths = artifact_paths,
    settings = tibble::tibble(
      num_ppc_sims = as.integer(dim(yrep_draws)[1]),
      block_size = as.integer(block_size)
    ),
    notes = notes
  )
}
