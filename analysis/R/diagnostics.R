# ============================================
# Model Diagnostics
# ============================================
# Functions for MCMC diagnostics, convergence checking, and visual inspection
# of fitted Stan models.

diagnostics_run_all <- function(fit, model_name, timestamp = NULL) {
  #####
  # Run comprehensive diagnostics for a fitted model
  #
  # Generates a full suite of diagnostic plots and tables, organizing them
  # into a structured directory tree.
  #
  # Parameters
  # ----
  # fit : stanfit
  #     Fitted Stan model object
  # model_name : character
  #     Name of the model (e.g., "model01_baseline")
  # timestamp : character or NULL
  #     Timestamp for directory naming. If NULL, generates one.
  #
  # Returns
  # ----
  # list
  #     List containing paths to generated outputs and summary statistics
  #####

  message(sprintf("\n=== Running diagnostics for: %s ===", model_name))

  # 1. Setup directories
  io_get_model_output_dirs_fn <- get("io_get_model_output_dirs", mode = "function")
  dirs <- io_get_model_output_dirs_fn(model_name, timestamp)

  # 2. Generate Summary Tables
  # General model diagnostics (Rhat, ESS, Divergences)
  diag_summary <- diagnostics_compute_summary(fit, model_name)
  readr::write_csv(diag_summary, file.path(dirs$tables_diag, "diagnostics_summary.csv"))

  # Parameter-specific diagnostics (Rhat/ESS per parameter)
  param_diag <- diagnostics_compute_parameter_stats(fit)
  readr::write_csv(param_diag, file.path(dirs$tables_diag, "parameter_diagnostics.csv"))

  # Problem-focused tables
  high_rhat <- diagnostics_filter_problematic_parameters(
    param_diag = param_diag,
    metric_col = "Rhat",
    threshold = 1.01,
    direction = "high"
  )
  if (nrow(high_rhat) > 0) {
    readr::write_csv(high_rhat, file.path(dirs$tables_diag, "problematic_rhat.csv"))
  }

  low_ess <- diagnostics_filter_problematic_parameters(
    param_diag = param_diag,
    metric_col = "ESS",
    threshold = 400,
    direction = "low"
  )
  if (nrow(low_ess) > 0) {
    readr::write_csv(low_ess, file.path(dirs$tables_diag, "problematic_ess.csv"))
  }

  sampler_settings <- diagnostics_extract_sampler_settings(fit)
  readr::write_csv(sampler_settings, file.path(dirs$tables_diag, "sampler_settings.csv"))

  sampler_param_summary <- diagnostics_summarize_sampler_params(fit)
  readr::write_csv(
    sampler_param_summary,
    file.path(dirs$tables_diag, "sampler_parameter_summary.csv")
  )

  # 3. Generate Plots
  diagnostics_plot_mcmc(fit, dirs$figs_diag)

  # Generate focused plots for problematic parameters if any
  diagnostics_plot_problematic(fit, param_diag, dirs$figs_diag)

  # 4. Print brief summary to console
  message(sprintf("  Max Rhat: %.4f | Min ESS: %.0f | Min BFMI: %.3f",
                  diag_summary$Max_Rhat, diag_summary$Min_ESS, diag_summary$Min_BFMI))
  message(sprintf("  Divergences: %d", diag_summary$N_Divergent))
  message(sprintf("  Outputs saved to: %s", dirs$figs_diag))

  return(list(
    summary = diag_summary,
    dirs = dirs
  ))
}

diagnostics_compute_bfmi <- function(fit = NULL, sampler_params = NULL) {
  #####
  # Compute per-chain BFMI values from Stan sampler output
  #
  # BFMI is a chain-level energy diagnostic, so the returned values are used
  # both in the model summary and repeated in the parameter-level table for
  # convenience when reviewing one CSV per fit.
  #
  # Parameters
  # ----
  # fit : stanfit or NULL
  #     Fitted Stan model object. Used only when sampler_params is NULL.
  # sampler_params : list or NULL
  #     Output from rstan::get_sampler_params(). If NULL, extracted from fit.
  #
  # Returns
  # ----
  # numeric
  #     Named numeric vector with one BFMI value per chain
  #####

  if (is.null(sampler_params)) {
    if (is.null(fit)) {
      stop("Either fit or sampler_params must be provided.")
    }
    sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  }

  bfmi_vals <- vapply(seq_along(sampler_params), function(chain_idx) {
    energy <- sampler_params[[chain_idx]][, "energy__"]
    energy <- energy[!is.na(energy)]

    if (length(energy) < 2) {
      return(NA_real_)
    }

    energy_var <- stats::var(energy)
    if (is.na(energy_var) || energy_var <= 0) {
      return(NA_real_)
    }

    diff_energy <- diff(energy)
    mean(diff_energy^2) / energy_var
  }, numeric(1))

  names(bfmi_vals) <- paste0("Chain_", seq_along(bfmi_vals))
  bfmi_vals
}

diagnostics_compute_summary <- function(fit, model_name = "Model") {
  #####
  # Compute concise model-level diagnostics
  #
  # Parameters
  # ----
  # fit : stanfit
  #     Fitted Stan model
  # model_name : character
  #     Name for the summary table
  #
  # Returns
  # ----
  # data.frame
  #     Single-row data frame with key metrics
  #####

  summ <- rstan::summary(fit)$summary
  sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)

  # Convergence
  rhat_vals <- summ[, "Rhat"]
  max_rhat <- max(rhat_vals, na.rm = TRUE)
  worst_param_rhat <- names(which.max(rhat_vals))
  n_high_rhat <- sum(rhat_vals > 1.01, na.rm = TRUE)

  # Efficiency
  neff_vals <- summ[, "n_eff"]
  min_neff <- min(neff_vals, na.rm = TRUE)
  worst_param_neff <- names(which.min(neff_vals))
  # n_low_neff calculation removed as it was unused, causing linter warning

  # Sampling problems
  n_divergent <- sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))

  # Max treedepth
  max_td_control <- fit@stan_args[[1]]$control$max_treedepth
  if (is.null(max_td_control)) max_td_control <- 10
  n_max_treedepth <- sum(sapply(sampler_params, function(x) sum(x[, "treedepth__"] >= max_td_control)))

  # BFMI (Energy)
  bfmi_vals <- diagnostics_compute_bfmi(sampler_params = sampler_params)
  min_bfmi <- if (all(is.na(bfmi_vals))) NA_real_ else min(bfmi_vals, na.rm = TRUE)

  data.frame(
    Model = model_name,
    Max_Rhat = round(max_rhat, 4),
    Worst_Param_Rhat = worst_param_rhat,
    N_High_Rhat = n_high_rhat,
    Min_ESS = round(min_neff, 0),
    Worst_Param_ESS = worst_param_neff,
    N_Divergent = n_divergent,
    N_Max_Treedepth = n_max_treedepth,
    Min_BFMI = round(min_bfmi, 3),
    stringsAsFactors = FALSE
  )
}

diagnostics_compute_parameter_stats <- function(fit, ess_threshold = 400, bfmi_threshold = 0.3) {
  #####
  # Compute detailed parameter-level diagnostics
  #
  # Parameters
  # ----
  # fit : stanfit
  #     Fitted Stan model
  # ess_threshold : numeric
  #     ESS threshold used to flag parameters as low efficiency (default: 400)
  # bfmi_threshold : numeric
  #     BFMI threshold used to flag the fit-level energy diagnostic (default: 0.3)
  #
  # Returns
  # ----
  # data.frame
  #     Table with Mean, SD, Rhat, ESS, and fit-level BFMI columns
  #####

  summ <- rstan::summary(fit)$summary
  sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  bfmi_vals <- diagnostics_compute_bfmi(sampler_params = sampler_params)
  min_bfmi <- if (all(is.na(bfmi_vals))) NA_real_ else min(bfmi_vals, na.rm = TRUE)
  mean_bfmi <- if (all(is.na(bfmi_vals))) NA_real_ else mean(bfmi_vals, na.rm = TRUE)

  # Convert to data frame
  param_df <- as.data.frame(summ)
  param_df$Parameter <- rownames(summ)

  # Select relevant columns
  result <- param_df[, c("Parameter", "mean", "sd", "Rhat", "n_eff")]
  colnames(result) <- c("Parameter", "Mean", "SD", "Rhat", "ESS")

  # Add status flags
  result$Rhat_Status <- ifelse(result$Rhat < 1.01, "OK", "High")
  result$ESS_Status <- ifelse(result$ESS > ess_threshold, "OK", "Low")

  # BFMI is chain-level, so attach the same fit-level values to each row.
  result$Min_BFMI <- round(min_bfmi, 3)
  result$Mean_BFMI <- round(mean_bfmi, 3)
  result$BFMI_Status <- ifelse(result$Min_BFMI >= bfmi_threshold, "OK", "Low")

  for (chain_name in names(bfmi_vals)) {
    result[[paste0("BFMI_", chain_name)]] <- round(bfmi_vals[[chain_name]], 3)
  }

  # Move Parameter to first column
  bfmi_cols <- grep("^BFMI_Chain_", names(result), value = TRUE)
  result <- result[, c(
    "Parameter", "Mean", "SD", "Rhat", "Rhat_Status",
    "ESS", "ESS_Status", "Min_BFMI", "Mean_BFMI", "BFMI_Status", bfmi_cols
  )]

  # Filter out pp_choice and predicted_ parameters
  exclude_patterns <- c("^pp_choice", "^predicted_")
  keep_mask <- !grepl(paste(exclude_patterns, collapse = "|"), result$Parameter)
  result <- result[keep_mask, ]

  # Filter out internal calculations if desired, but keeping all is safer for diagnostics
  # Just ensure row names are clean
  rownames(result) <- NULL

  result
}

diagnostics_get_problematic_exclusion_patterns <- function() {
  #####
  # Return parameter-name patterns excluded from problem-focused tables
  #
  # Keeps the focused CSVs on interpretable parameters and omits large
  # generated quantities or transformed subject-level vectors that are not
  # useful for manual diagnosis.
  #
  # Parameters
  # ----
  # None
  #
  # Returns
  # ----
  # character
  #     Vector of regular-expression patterns to exclude
  #####

  c(
    "^pp_", "^predicted", "^log_lik", "^y_pred",
    "_gq", "_transformed",
    "_subj\\[",
    "^(alpha|beta|kappa|alpha_shift|kappa_shift)\\["
  )
}

diagnostics_filter_problematic_parameters <- function(param_diag,
                                                      metric_col,
                                                      threshold,
                                                      direction = c("high", "low")) {
  #####
  # Filter and sort problematic parameters for focused diagnostics tables
  #
  # Applies the same nuisance-parameter exclusions used by the diagnostic
  # plotting functions, then retains only rows above or below the supplied
  # threshold depending on the requested direction.
  #
  # Parameters
  # ----
  # param_diag : data.frame
  #     Parameter diagnostics table from diagnostics_compute_parameter_stats()
  # metric_col : character
  #     Column name used to determine problematic rows
  # threshold : numeric
  #     Threshold applied to metric_col
  # direction : character
  #     "high" keeps values greater than threshold; "low" keeps values lower
  #
  # Returns
  # ----
  # data.frame
  #     Filtered and sorted subset of param_diag
  #####

  direction <- match.arg(direction)
  exclude_patterns <- diagnostics_get_problematic_exclusion_patterns()

  keep_mask <- !grepl(paste(exclude_patterns, collapse = "|"), param_diag$Parameter)
  filtered <- param_diag[keep_mask, , drop = FALSE]

  metric_vals <- filtered[[metric_col]]
  idx <- if (direction == "high") {
    which(metric_vals > threshold)
  } else {
    which(metric_vals < threshold)
  }

  problematic <- filtered[idx, , drop = FALSE]
  if (nrow(problematic) == 0) {
    rownames(problematic) <- NULL
    return(problematic)
  }

  problematic <- problematic[order(
    problematic[[metric_col]],
    decreasing = identical(direction, "high")
  ), , drop = FALSE]
  rownames(problematic) <- NULL
  problematic
}

diagnostics_flatten_named_list <- function(x, prefix = NULL) {
  #####
  # Flatten nested list settings into a named character vector
  #
  # This helper converts nested stan_args entries such as control lists into
  # simple key-value pairs that can be written cleanly to CSV.
  #
  # Parameters
  # ----
  # x : list
  #     Named list to flatten recursively
  # prefix : character or NULL
  #     Optional prefix for nested names
  #
  # Returns
  # ----
  # character
  #     Named character vector of flattened key-value pairs
  #####

  flattened <- character(0)

  for (nm in names(x)) {
    value <- x[[nm]]
    full_name <- if (is.null(prefix)) nm else paste(prefix, nm, sep = ".")

    if (is.list(value)) {
      flattened <- c(flattened, diagnostics_flatten_named_list(value, prefix = full_name))
    } else if (length(value) == 0) {
      flattened[full_name] <- NA_character_
    } else {
      flattened[full_name] <- paste(as.character(value), collapse = ", ")
    }
  }

  flattened
}

diagnostics_extract_sampler_settings <- function(fit) {
  #####
  # Extract sampler settings from a stanfit object into tidy rows
  #
  # Shared settings are collapsed into one row, while chain-specific settings
  # such as chain_id are retained as per-chain rows.
  #
  # Parameters
  # ----
  # fit : stanfit
  #     Fitted Stan model object
  #
  # Returns
  # ----
  # data.frame
  #     Tidy table with Scope, Chain, Setting, and Value columns
  #####

  stan_args <- fit@stan_args

  settings_by_chain <- lapply(seq_along(stan_args), function(chain_idx) {
    chain_settings <- stan_args[[chain_idx]]
    chain_settings$init <- NULL
    flattened <- diagnostics_flatten_named_list(chain_settings)

    data.frame(
      Scope = "chain",
      Chain = chain_idx,
      Setting = names(flattened),
      Value = unname(flattened),
      stringsAsFactors = FALSE
    )
  })

  settings_df <- dplyr::bind_rows(settings_by_chain)

  shared_rows <- list()
  chain_rows <- list()
  unique_settings <- unique(settings_df$Setting)

  for (setting_name in unique_settings) {
    subset_df <- settings_df[settings_df$Setting == setting_name, , drop = FALSE]
    unique_values <- unique(subset_df$Value)

    if (length(unique_values) == 1) {
      shared_rows[[length(shared_rows) + 1L]] <- data.frame(
        Scope = "shared",
        Chain = NA_integer_,
        Setting = setting_name,
        Value = unique_values[[1]],
        stringsAsFactors = FALSE
      )
    } else {
      chain_rows[[length(chain_rows) + 1L]] <- subset_df
    }
  }

  fit_level_rows <- data.frame(
    Scope = "fit",
    Chain = NA_integer_,
    Setting = c("fit.chains", "fit.iter", "fit.warmup"),
    Value = as.character(c(fit@sim$chains, fit@sim$iter, fit@sim$warmup)),
    stringsAsFactors = FALSE
  )

  result <- dplyr::bind_rows(
    fit_level_rows,
    dplyr::bind_rows(shared_rows),
    dplyr::bind_rows(chain_rows)
  )

  rownames(result) <- NULL
  result
}

diagnostics_summarize_sampler_params <- function(fit) {
  #####
  # Summarize Stan sampler parameters across chains and overall
  #
  # Produces descriptive summaries for sampler diagnostics such as accept_stat,
  # stepsize, treedepth, leapfrog counts, divergences, and energy.
  #
  # Parameters
  # ----
  # fit : stanfit
  #     Fitted Stan model object
  #
  # Returns
  # ----
  # data.frame
  #     Summary table with chain-wise and overall statistics per sampler column
  #####

  sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)

  summarize_matrix <- function(param_matrix, scope, chain = NA_integer_) {
    param_names <- colnames(param_matrix)

    dplyr::bind_rows(lapply(param_names, function(param_name) {
      values <- param_matrix[, param_name]

      data.frame(
        Scope = scope,
        Chain = chain,
        Sampler_Parameter = param_name,
        Mean = mean(values, na.rm = TRUE),
        SD = stats::sd(values, na.rm = TRUE),
        Median = stats::median(values, na.rm = TRUE),
        Min = min(values, na.rm = TRUE),
        Max = max(values, na.rm = TRUE),
        P05 = stats::quantile(values, probs = 0.05, na.rm = TRUE, names = FALSE),
        P95 = stats::quantile(values, probs = 0.95, na.rm = TRUE, names = FALSE),
        stringsAsFactors = FALSE
      )
    }))
  }

  chain_summaries <- lapply(seq_along(sampler_params), function(chain_idx) {
    summarize_matrix(sampler_params[[chain_idx]], scope = "chain", chain = chain_idx)
  })

  overall_matrix <- do.call(rbind, sampler_params)
  overall_summary <- summarize_matrix(overall_matrix, scope = "overall", chain = NA_integer_)

  result <- dplyr::bind_rows(overall_summary, dplyr::bind_rows(chain_summaries))
  rownames(result) <- NULL
  result
}

diagnostics_plot_mcmc <- function(fit, output_dir) {
  #####
  # Generate and save diagnostic plots
  #
  # Creates trace plots, density overlays, pairs plots, and autocorrelation plots.
  # Distinguishes between raw (for convergence) and transformed (for interpretation)
  # parameters automatically based on availability.
  #
  # Parameters
  # ----
  # fit : stanfit
  #     Fitted Stan model
  # output_dir : character
  #     Directory to save plots
  #
  # Returns
  # ----
  # NULL
  #     Saves files to disk
  #####

  requireNamespace("bayesplot")
  requireNamespace("ggplot2")

  # 1. Identify Parameters
  all_pars <- names(fit)

  # Filter out generated quantities that are large matrices (e.g., pp_choice, log_lik)
  # to avoid massive plots
  exclude_patterns <- c("pp_choice", "predicted_choices", "log_lik", "y_pred", "\\[")
  scalar_pars <- all_pars[!grepl(paste(exclude_patterns, collapse = "|"), all_pars)]

  # Identify groups
  pars_raw <- grep("_raw$", scalar_pars, value = TRUE)
  pars_trans <- grep("_mu$", scalar_pars, value = TRUE) # usually means/SDs end in _mu or just no _raw
  # If no _mu suffix, look for parameters that are NOT _raw and NOT subject-level vectors
  if (length(pars_trans) == 0) {
    pars_trans <- setdiff(scalar_pars, pars_raw)
  }

  # Subject level samples (take first 3 subjects for checking)
  pars_subj <- grep("\\[1\\]|\\[2\\]|\\[3\\]", all_pars, value = TRUE)
  pars_subj <- pars_subj[!grepl("pp_|log_lik|predicted", pars_subj)]

  # 2. Trace Plots
  if (length(pars_raw) > 0) {
    # Extract only necessary parameters to avoid issues with NAs in other parts of the fit
    trace_array_raw <- as.array(fit, pars = pars_raw)
    p_trace_raw <- bayesplot::mcmc_trace(trace_array_raw) +
      ggplot2::ggtitle("Trace Plots: Raw Parameters (Convergence)")
    ggplot2::ggsave(file.path(output_dir, "trace_raw.png"), p_trace_raw, width = 12, height = 8)
  }

  if (length(pars_trans) > 0) {
    trace_array_trans <- as.array(fit, pars = pars_trans)
    p_trace_trans <- bayesplot::mcmc_trace(trace_array_trans) +
      ggplot2::ggtitle("Trace Plots: Transformed Parameters")
    ggplot2::ggsave(file.path(output_dir, "trace_transformed.png"), p_trace_trans, width = 10, height = 6)
  }

  # 3. Density Overlays (Posterior Checks)
  if (length(pars_trans) > 0) {
    dens_array_trans <- as.array(fit, pars = pars_trans)
    p_dens <- bayesplot::mcmc_dens_overlay(dens_array_trans) +
      ggplot2::ggtitle("Posterior Densities (Natural Scale)")
    ggplot2::ggsave(file.path(output_dir, "density_natural.png"), p_dens, width = 10, height = 6)
  }

  # 4. Pairs Plots (Correlation)
  # Only do this if we have a manageable number of parameters (< 10)
  if (length(pars_trans) > 0 && length(pars_trans) <= 10) {
    pairs_array_trans <- as.array(fit, pars = pars_trans)
    p_pairs <- bayesplot::mcmc_pairs(pairs_array_trans,
                                     off_diag_fun = "hex")
    ggplot2::ggsave(file.path(output_dir, "pairs_natural.png"), p_pairs, width = 12, height = 12)
  }

  # 5. R-hat and ESS Histograms
  # Use rstan summary to safely extract metrics and filter out NAs
  fit_summary <- rstan::summary(fit)$summary

  # R-hat
  rhats <- fit_summary[, "Rhat"]
  rhats <- rhats[!is.na(rhats)]
  # Use explicit binwidth to silence warnings and improve visualization
  p_rhat <- bayesplot::mcmc_rhat_hist(rhats, binwidth = 0.005) +
    ggplot2::ggtitle("R-hat Distribution")
  ggplot2::ggsave(file.path(output_dir, "rhat_hist.png"), p_rhat, width = 6, height = 4)

  # ESS Ratio
  # Calculate ratio manually from summary to be safe
  total_samples <- (fit@sim$iter - fit@sim$warmup) * fit@sim$chains
  neff_ratios <- fit_summary[, "n_eff"] / total_samples
  neff_ratios <- neff_ratios[!is.na(neff_ratios)]

  # Use explicit binwidth (0.05 = 20 bins for 0-1 range)
  p_neff <- bayesplot::mcmc_neff_hist(neff_ratios, binwidth = 0.05) +
    ggplot2::ggtitle("Effective Sample Size Ratio")
  ggplot2::ggsave(file.path(output_dir, "neff_hist.png"), p_neff, width = 6, height = 4)

  # 6. NUTS Energy
  # nuts_params is exported by bayesplot, not rstan
  np <- bayesplot::nuts_params(fit)
  # Explicit bins to silence warning
  p_energy <- bayesplot::mcmc_nuts_energy(np, bins = 30) + ggplot2::ggtitle("NUTS Energy")
  ggplot2::ggsave(file.path(output_dir, "nuts_energy.png"), p_energy, width = 8, height = 6)

  # 7. Posterior Predictive Checks (if available)
  # Look for pp_choice_stim2_prob or similar
  if (any(grepl("pp_choice", all_pars))) {
    # Extract just one example PPC if available
    # pp_name unused but keeping commented logic for clarity if future expansion needs it
    # pp_name <- grep("pp_choice", all_pars, value = TRUE)[1]

    tryCatch({
      pp_vals <- rstan::extract(fit, pars = "pp_choice_stim2_prob")$pp_choice_stim2_prob
      # PPC SUMMARY CONVENTION (sentinel + padding)
      # - Stan models encode missing/padded trials as -9.
      # - PPC summaries must exclude these sentinel entries *and* any out-of-range
      #   values. (After explicit initialization in Stan, padded trials are -9.)
      pp_valid <- pp_vals[!is.na(pp_vals) & pp_vals != -9.0 & pp_vals >= 0 & pp_vals <= 1]

      if (length(pp_valid) > 0) {
        ppc_df <- data.frame(prob = pp_valid)
        prob <- NULL
        p_ppc <- ggplot2::ggplot(ppc_df, ggplot2::aes(x = prob)) +
          ggplot2::geom_histogram(bins = 50, fill = "skyblue", color = "black") +
          ggplot2::labs(title = "PPC: Predicted Choice Probabilities (Stim 2)",
                        x = "Probability", y = "Count") +
          ggplot2::theme_minimal()
        ggplot2::ggsave(file.path(output_dir, "ppc_choice_prob.png"), p_ppc, width = 8, height = 6)
      }
    }, error = function(e) {
      message("Skipping PPC plot due to structure mismatch: ", e$message)
    })
  }
}

diagnostics_plot_problematic <- function(fit, param_diag, output_dir) {
  #####
  # Plot trace plots for parameters with high R-hat (> 1.01)
  #
  # Parameters
  # ----
  # fit : stanfit
  #     Fitted Stan model
  # param_diag : data.frame
  #     Parameter diagnostics table from diagnostics_compute_parameter_stats
  # output_dir : character
  #     Directory to save plots
  #
  # Returns
  # ----
  # NULL
  #####

  requireNamespace("bayesplot")
  requireNamespace("ggplot2")

  # 1. Filter for high R-hat
  # Use which() to safely handle NAs
  prob_params <- param_diag[which(param_diag$Rhat > 1.01), ]

  if (nrow(prob_params) == 0) return(NULL)

  # 2. Exclude generated quantities and other nuisance parameters
  # Exclude:
  # - Posterior predictive checks (pp_, predicted, y_pred)
  # - Log likelihood (log_lik)
  # - Generated quantities/transformed suffixes (_gq, _transformed)
  # - Transformed subject vectors ending in _subj[...] (keep _subj_raw[...])
  # - Transformed base vectors like alpha[...] (keep alpha_mu, alpha_raw, etc.)
  exclude_patterns <- diagnostics_get_problematic_exclusion_patterns()
  keep_mask <- !grepl(paste(exclude_patterns, collapse = "|"), prob_params$Parameter)
  prob_params <- prob_params[keep_mask, ]

  if (nrow(prob_params) == 0) return(NULL)

  # 3. Sort and Limit
  prob_params <- prob_params[order(prob_params$Rhat, decreasing = TRUE), ]
  top_n <- min(nrow(prob_params), 15)
  # Ensure parameters are characters
  params_to_plot <- as.character(prob_params$Parameter[1:top_n])

  message(sprintf("  Generating trace plots for top %d problematic parameters...", length(params_to_plot)))

  tryCatch({
    # 4. Create Plot
    # Extract array first to check for validity/NAs
    post_array <- as.array(fit, pars = params_to_plot)

    # Check for NAs in array and warn/replace if needed
    if (any(is.na(post_array))) {
       message("  Warning: NAs found in posterior samples for problematic parameters. Trace plot might fail or show gaps.")
       # If we really wanted to fix, we could filter valid iterations, but that breaks structure.
       # Proceeding, hoping bayesplot handles it or users fix the model.
    }

    # Calculate height: ~2 inches per row, min 4
    n_rows <- ceiling(length(params_to_plot) / 3)
    plot_height <- max(4, n_rows * 2.5)

    # Use mcmc_trace with array
    p_trace <- bayesplot::mcmc_trace(post_array,
                                     facet_args = list(ncol = 3)) +
      ggplot2::ggtitle("Trace Plots: Highest R-hat Parameters") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_color_manual(values = c("red", "blue", "green4", "yellow"))

    ggplot2::ggsave(file.path(output_dir, "problematic_trace_plots.png"),
                    p_trace, width = 12, height = plot_height)

  }, error = function(e) {
    message("  Warning: Could not create problematic trace plots: ", e$message)
  })
}
