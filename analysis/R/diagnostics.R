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
  dirs <- io_get_model_output_dirs(model_name, timestamp)

  # 2. Generate Summary Tables
  # General model diagnostics (Rhat, ESS, Divergences)
  diag_summary <- diagnostics_compute_summary(fit, model_name)
  readr::write_csv(diag_summary, file.path(dirs$tables_diag, "diagnostics_summary.csv"))

  # Parameter-specific diagnostics (Rhat/ESS per parameter)
  param_diag <- diagnostics_compute_parameter_stats(fit)
  readr::write_csv(param_diag, file.path(dirs$tables_diag, "parameter_diagnostics.csv"))

  # Save problematic parameters specifically
  # Use which() to explicitly select indices where the condition is TRUE,
  # automatically dropping NAs or NaNs to prevent empty/NA rows.
  high_rhat <- param_diag[which(param_diag$Rhat > 1.01), ]

  if (nrow(high_rhat) > 0) {
    # Exclude generated quantities and other nuisance parameters from the CSV as well
    # (Same logic as for the plots)
    exclude_patterns <- c(
      "^pp_", "^predicted", "^log_lik", "^y_pred",
      "_gq", "_transformed",
      "_subj\\[",
      "^(alpha|beta|kappa|alpha_shift|kappa_shift)\\["
    )
    keep_mask <- !grepl(paste(exclude_patterns, collapse = "|"), high_rhat$Parameter)
    high_rhat <- high_rhat[keep_mask, ]

    if (nrow(high_rhat) > 0) {
      # Sort by Rhat descending (worst first)
      high_rhat <- high_rhat[order(high_rhat$Rhat, decreasing = TRUE), ]
      readr::write_csv(high_rhat, file.path(dirs$tables_diag, "problematic_rhat.csv"))
    }
  }

  # 3. Generate Plots
  diagnostics_plot_mcmc(fit, dirs$figs_diag)

  # Generate focused plots for problematic parameters if any
  diagnostics_plot_problematic(fit, param_diag, dirs$figs_diag)

  # 4. Print brief summary to console
  message(sprintf("  Max Rhat: %.4f | Min ESS: %.0f",
                  diag_summary$Max_Rhat, diag_summary$Min_ESS))
  message(sprintf("  Divergences: %d", diag_summary$N_Divergent))
  message(sprintf("  Outputs saved to: %s", dirs$figs_diag))

  return(list(
    summary = diag_summary,
    dirs = dirs
  ))
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
  bfmi_vals <- sapply(sampler_params, function(x) {
    energy <- x[, "energy__"]
    diff_energy <- diff(energy)
    mean(diff_energy^2) / var(energy)
  })
  min_bfmi <- min(bfmi_vals)

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

diagnostics_compute_parameter_stats <- function(fit) {
  #####
  # Compute detailed parameter-level diagnostics
  #
  # Parameters
  # ----
  # fit : stanfit
  #     Fitted Stan model
  #
  # Returns
  # ----
  # data.frame
  #     Table with Mean, SD, Rhat, ESS for key parameters
  #####

  summ <- rstan::summary(fit)$summary

  # Convert to data frame
  param_df <- as.data.frame(summ)
  param_df$Parameter <- rownames(summ)

  # Select relevant columns
  result <- param_df[, c("Parameter", "mean", "sd", "Rhat", "n_eff")]
  colnames(result) <- c("Parameter", "Mean", "SD", "Rhat", "ESS")

  # Add status flags
  result$Rhat_Status <- ifelse(result$Rhat < 1.01, "OK", "High")
  result$ESS_Status <- ifelse(result$ESS > 400, "OK", "Low")

  # Move Parameter to first column
  result <- result[, c("Parameter", "Mean", "SD", "Rhat", "Rhat_Status", "ESS", "ESS_Status")]

  # Filter out pp_choice and predicted_ parameters
  exclude_patterns <- c("^pp_choice", "^predicted_")
  keep_mask <- !grepl(paste(exclude_patterns, collapse = "|"), result$Parameter)
  result <- result[keep_mask, ]

  # Filter out internal calculations if desired, but keeping all is safer for diagnostics
  # Just ensure row names are clean
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
      # Remove NAs and Sentinel values (-9)
      pp_valid <- pp_vals[pp_vals >= 0 & pp_vals <= 1]

      if (length(pp_valid) > 0) {
        # Use .data$prob to avoid 'no visible binding' note
        p_ppc <- ggplot2::ggplot(data.frame(prob = pp_valid), ggplot2::aes(x = .data$prob)) +
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
  exclude_patterns <- c(
    "^pp_", "^predicted", "^log_lik", "^y_pred",
    "_gq", "_transformed",
    "_subj\\[",
    "^(alpha|beta|kappa|alpha_shift|kappa_shift)\\["
  )
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
