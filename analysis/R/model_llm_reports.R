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
     "ppc_choice_prob_png"
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
     file.path(dirs$figs_diag, "ppc_choice_prob.png")
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

model_reports_build_ppc_params <- function(winning_fit,
                                           winning_model_name,
                                           timestamp,
                                           fit_path = NULL,
                                           stan_data) {
 #####
 # Assemble the params list for the winning-model PPC LLM report
 #
 # Parameters
 # ----
 # winning_fit : stanfit
 # Winning fitted Stan model
 # winning_model_name : character
 # Name of the winning model
 # timestamp : character
 # Timestamp used for the current model run
 # fit_path : character or NULL
 # Path to the cached winning fit file
 # stan_data : list
 # Stan data list from prepare_stan_data()
 #
 # Returns
 # ----
 # list
 # Named params list consumed by the PPC LLM report Rmd
 #####
 stopifnot("winning_model_name must be character" = is.character(winning_model_name) && length(winning_model_name) == 1)
 stopifnot("timestamp must be character" = is.character(timestamp) && length(timestamp) == 1)
 stopifnot("stan_data must be a list" = is.list(stan_data))

 ppc_summary <- model_reports_summarize_ppc_choice_prob(
   fit = winning_fit,
   stan_data = stan_data
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

 list(
   metadata = model_reports_extract_metadata(
     winning_model_name = winning_model_name,
     timestamp = timestamp,
     fit = winning_fit,
     fit_path = fit_path,
     stan_data = stan_data
   ),
   ppc_overview = ppc_summary$overview,
   ppc_by_subject = ppc_summary$by_subject,
   ppc_by_condition = ppc_summary$by_condition,
   artifact_paths = artifact_paths,
   notes = ppc_summary$notes
 )
}
