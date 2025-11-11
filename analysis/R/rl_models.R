# ============================================
# Reinforcement Learning Model Fitting
# ============================================
# Functions for fitting, diagnosing, and comparing hierarchical RL models

# ========== Model Discovery & Selection ==========

rl_discover_models <- function(model_dir = "analysis/models") {
  #####
  # Discover all available Stan models in the models directory
  #
  # Scans the specified directory for .stan files and returns their names
  # (without the .stan extension) in sorted order.
  #
  # Parameters
  # ----
  # model_dir : character
  #     Path to directory containing Stan model files (default: "analysis/models")
  #
  # Returns
  # ----
  # character vector
  #     Sorted vector of model names (without .stan extension)
  #####

  stan_files <- list.files(
    path = model_dir,
    pattern = "\\.stan$",
    full.names = FALSE,
    recursive = FALSE
  )

  if (length(stan_files) == 0) {
    stop(sprintf("No .stan files found in directory: %s", model_dir))
  }

  # Extract model names (remove .stan extension) and sort
  model_names <- tools::file_path_sans_ext(stan_files)
  sort(model_names)
}


rl_select_models_interactive <- function(model_names) {
  #####
  # Interactively select which models to fit
  #
  # Displays a numbered menu of available models and prompts the user to
  # select which models to fit. Supports single selection (e.g., "1"),
  # multiple selection (e.g., "1,2,3"), or all models (last option).
  #
  # Parameters
  # ----
  # model_names : character vector
  #     Vector of available model names from rl_discover_models()
  #
  # Returns
  # ----
  # character vector
  #     Selected model names to fit
  #####

  # Check if running interactively
  if (!interactive()) {
    message("Non-interactive session detected. Fitting all models.")
    return(model_names)
  }

  # Display menu
  cat("\n")
  cat("========================================\n")
  cat("  Select Models to Fit\n")
  cat("========================================\n\n")

  for (i in seq_along(model_names)) {
    cat(sprintf("  %d: %s\n", i, model_names[i]))
  }

  cat(sprintf("  %d: ALL MODELS\n", length(model_names) + 1))
  cat("\n")
  cat("Enter your selection:\n")
  cat("  - Single model: e.g., '1'\n")
  cat("  - Multiple models: e.g., '1,2,3'\n")
  cat(sprintf("  - All models: '%d'\n", length(model_names) + 1))
  cat("\nYour choice: ")

  # Get user input
  user_input <- readline()

  # Parse input
  user_input <- trimws(user_input)

  # Check for "ALL" option
  all_option <- as.character(length(model_names) + 1)
  if (user_input == all_option) {
    message(sprintf("\n✓ Selected: ALL MODELS (%d models)", length(model_names)))
    return(model_names)
  }

  # Parse comma-separated indices
  indices <- tryCatch({
    as.integer(unlist(strsplit(user_input, ",")))
  }, error = function(e) {
    stop("Invalid input. Please enter numbers separated by commas (e.g., '1,2,3')")
  })

  # Validate indices
  if (any(is.na(indices))) {
    stop("Invalid input. Please enter valid numbers.")
  }

  if (any(indices < 1 | indices > length(model_names))) {
    stop(sprintf("Invalid model number. Please choose between 1 and %d",
                 length(model_names)))
  }

  # Extract selected models
  selected_models <- model_names[indices]

  message(sprintf("\n✓ Selected: %s (%d model%s)",
                  paste(selected_models, collapse = ", "),
                  length(selected_models),
                  ifelse(length(selected_models) > 1, "s", "")))

  selected_models
}


# ========== Core Fitting Functions ==========

rl_get_init_function <- function() {
  #####
  # Create flexible initialization function for Stan sampling
  #
  # Returns a function that generates sensible starting values for RL model
  # parameters. Works with all model variants (basic, shift, perseveration)
  # by providing all possible parameters; Stan ignores unused ones.
  #
  # Parameters
  # ----
  # None
  #
  # Returns
  # ----
  # function
  #     Initialization function that returns a list of starting values
  #####

  function() {
    list(
      # Group-level means (start near center of transformed range)
      alpha_mu_raw = runif(1, -1, 1),
      beta_mu_raw = runif(1, -1, 1),
      alpha_shift_mu_raw = rnorm(1, 0, 0.2),
      kappa_mu_raw = rnorm(1, 0, 0.2),

      # Group-level SDs (start with small-to-moderate positive values)
      alpha_sd_raw = runif(1, 0.5, 1.5),
      alpha_shift_sd_raw = runif(1, 0.5, 1.5),
      beta_sd_raw = runif(1, 0.5, 1.5),
      kappa_sd_raw = runif(1, 0.5, 1.5)
    )
  }
}


rl_fit_single <- function(model_name,
                          stan_data,
                          chains = 4,
                          iter = 10000,
                          warmup = 6000,
                          adapt_delta = 0.999,
                          max_treedepth = 14,
                          seed = 123,
                          verbose = TRUE) {
  #####
  # Fit a single hierarchical RL model using Stan
  #
  # Parameters
  # ----
  # model_name : character
  #     Name of Stan model file (without .stan extension) in analysis/models/
  # stan_data : list
  #     Stan data list from prepare_stan_data()
  # chains : integer
  #     Number of MCMC chains (default: 4)
  # iter : integer
  #     Total iterations per chain (default: 10000)
  # warmup : integer
  #     Warmup iterations per chain (default: 8000)
  # adapt_delta : numeric
  #     Target acceptance rate (default: 0.999)
  # max_treedepth : integer
  #     Maximum tree depth for sampler (default: 14)
  # seed : integer
  #     Random seed for reproducibility (default: 123)
  # verbose : logical
  #     Print progress messages (default: TRUE)
  #
  # Returns
  # ----
  # stanfit
  #     Fitted Stan model object
  #####

  model_file <- file.path("analysis/models", paste0(model_name, ".stan"))

  if (!file.exists(model_file)) {
    stop("Model file not found: ", model_file)
  }

  if (verbose) {
    message(sprintf("Fitting model: %s", model_name))
    message(sprintf("  Chains: %d | Iterations: %d | Warmup: %d",
                    chains, iter, warmup))
  }

  fit <- rstan::stan(
    file = model_file,
    data = stan_data,
    chains = chains,
    iter = iter,
    warmup = warmup,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    seed = seed,
    init = rl_get_init_function()
  )

  if (verbose) {
    message(sprintf("✓ Model %s fitted successfully", model_name))
  }

  fit
}


rl_load_or_fit <- function(model_name,
                           stan_data,
                           fit_dir = "analysis/outputs/fits",
                           force_refit = FALSE,
                           ...) {
  #####
  # Load existing model fit or fit a new one (smart caching)
  #
  # Checks if a previously fitted model exists. If yes, loads it; otherwise
  # fits the model and saves it for future use.
  #
  # Parameters
  # ----
  # model_name : character
  #     Name of Stan model
  # stan_data : list
  #     Stan data list from prepare_stan_data()
  # fit_dir : character
  #     Directory to save/load fit objects (default: "analysis/outputs/fits")
  # force_refit : logical
  #     If TRUE, refit even if cached fit exists (default: FALSE)
  # ... : additional arguments
  #     Passed to rl_fit_single()
  #
  # Returns
  # ----
  # stanfit
  #     Fitted Stan model object
  #####

  # Ensure fit directory exists
  if (!dir.exists(fit_dir)) {
    dir.create(fit_dir, recursive = TRUE)
  }

  fit_path <- file.path(fit_dir, paste0(model_name, "_fit.rds"))

  # Try to load existing fit
  if (!force_refit && file.exists(fit_path)) {
    message(sprintf("Loading cached fit: %s", model_name))
    fit <- readRDS(fit_path)
    return(fit)
  }

  # Fit new model
  fit <- rl_fit_single(model_name, stan_data, ...)

  # Save for future use
  saveRDS(fit, fit_path)
  message(sprintf("Saved fit to: %s", fit_path))

  fit
}


rl_fit_all <- function(model_names,
                       stan_data,
                       fit_dir = "analysis/outputs/fits",
                       force_refit = FALSE,
                       ...) {
  #####
  # Fit multiple RL models with smart caching
  #
  # Fits a list of models, using cached fits when available unless force_refit
  # is TRUE. Returns a named list of fitted models.
  #
  # Parameters
  # ----
  # model_names : character vector
  #     Names of Stan models to fit
  # stan_data : list
  #     Stan data list from prepare_stan_data()
  # fit_dir : character
  #     Directory to save/load fit objects (default: "analysis/outputs/fits")
  # force_refit : logical
  #     If TRUE, refit all models even if cached (default: FALSE)
  # ... : additional arguments
  #     Passed to rl_fit_single() (e.g., chains, iter, warmup)
  #
  # Returns
  # ----
  # list
  #     Named list of stanfit objects
  #####

  message(sprintf("\n=== Fitting %d models ===", length(model_names)))

  fits <- list()
  for (model_name in model_names) {
    fits[[model_name]] <- rl_load_or_fit(
      model_name,
      stan_data,
      fit_dir = fit_dir,
      force_refit = force_refit,
      ...
    )
  }

  message("\n=== All models fitted ===\n")
  fits
}


# ========== Diagnostics ==========

rl_check_convergence <- function(fit, model_name = NULL) {
  #####
  # Check MCMC convergence diagnostics for a fitted model
  #
  # Examines Rhat, effective sample size, and divergent transitions.
  # Prints warnings for any convergence issues.
  #
  # Parameters
  # ----
  # fit : stanfit
  #     Fitted Stan model object
  # model_name : character or NULL
  #     Model name for output messages (default: NULL)
  #
  # Returns
  # ----
  # list
  #     Named list with:
  #     - converged: logical, TRUE if all checks pass
  #     - max_rhat: maximum Rhat across parameters
  #     - min_ess_bulk: minimum bulk ESS across parameters
  #     - min_ess_tail: minimum tail ESS across parameters
  #     - n_divergent: number of divergent transitions
  #     - n_max_treedepth: number of iterations hitting max treedepth
  #####

  prefix <- if (!is.null(model_name)) sprintf("[%s] ", model_name) else ""

  # Extract summary
  summary_df <- as.data.frame(rstan::summary(fit)$summary)

  # Rhat
  max_rhat <- max(summary_df$Rhat, na.rm = TRUE)
  rhat_ok <- max_rhat < 1.1

  # Effective sample sizes
  min_ess_bulk <- min(summary_df$n_eff, na.rm = TRUE)
  min_ess_tail <- min(summary_df$n_eff, na.rm = TRUE)  # Simplified; full version would use tail ESS
  ess_ok <- min_ess_bulk > 100

  # Divergences and treedepth
  sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  n_divergent <- sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))
  n_max_treedepth <- sum(sapply(sampler_params, function(x) sum(x[, "treedepth__"] >= 10)))

  # Print diagnostics
  message(sprintf("%sConvergence Diagnostics:", prefix))
  message(sprintf("  Max Rhat: %.4f %s", max_rhat, ifelse(rhat_ok, "✓", "⚠ WARNING")))
  message(sprintf("  Min ESS:  %.0f %s", min_ess_bulk, ifelse(ess_ok, "✓", "⚠ WARNING")))
  message(sprintf("  Divergent transitions: %d %s", n_divergent,
                  ifelse(n_divergent == 0, "✓", "⚠ WARNING")))
  message(sprintf("  Max treedepth hits: %d %s", n_max_treedepth,
                  ifelse(n_max_treedepth == 0, "✓", "⚠")))

  converged <- rhat_ok && ess_ok && n_divergent == 0

  list(
    converged = converged,
    max_rhat = max_rhat,
    min_ess_bulk = min_ess_bulk,
    min_ess_tail = min_ess_tail,
    n_divergent = n_divergent,
    n_max_treedepth = n_max_treedepth
  )
}


# ========== Model Comparison ==========

rl_compare_models <- function(fit_list,
                              save_dir = "analysis/outputs/tables",
                              save_file = TRUE) {
  #####
  # Compare multiple RL models using LOO cross-validation
  #
  # Computes LOO for each model and performs pairwise comparisons.
  # Optionally saves results to CSV.
  #
  # Parameters
  # ----
  # fit_list : named list
  #     List of stanfit objects from rl_fit_all()
  # save_dir : character
  #     Directory to save comparison table (default: "analysis/outputs/tables")
  # save_file : logical
  #     Save comparison results to CSV file (default: TRUE)
  #
  # Returns
  # ----
  # list
  #     Named list with:
  #     - comparison: loo_compare object (matrix)
  #     - loo_objects: named list of individual loo objects
  #     - winning_model: name of best model
  #####

  message("\n=== Computing LOO for model comparison ===")

  loo_objects <- list()
  for (model_name in names(fit_list)) {
    message(sprintf("Computing LOO for: %s", model_name))
    log_lik <- loo::extract_log_lik(fit_list[[model_name]], parameter_name = "log_lik")
    loo_objects[[model_name]] <- loo::loo(log_lik)
  }

  # Perform comparison
  message("\n=== Model Comparison (LOOIC) ===")
  comparison <- loo::loo_compare(loo_objects)
  print(comparison)

  winning_model <- rownames(comparison)[1]
  message(sprintf("\n✓ Winning model: %s\n", winning_model))

  # Save to file
  if (save_file) {
    if (!dir.exists(save_dir)) {
      dir.create(save_dir, recursive = TRUE)
    }

    comparison_df <- as.data.frame(comparison) %>%
      tibble::rownames_to_column("model")

    save_path <- file.path(save_dir, "model_comparison.csv")
    readr::write_csv(comparison_df, save_path)
    message(sprintf("Saved comparison table to: %s", save_path))
  }

  list(
    comparison = comparison,
    loo_objects = loo_objects,
    winning_model = winning_model
  )
}


# ========== Parameter Extraction ==========

rl_extract_params <- function(fit,
                              params = c("alpha_mu", "beta_mu", "alpha_shift_mu", "kappa_mu"),
                              probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  #####
  # Extract and summarize key parameters from fitted model
  #
  # Extracts posterior summaries (mean, SD, quantiles) for specified
  # group-level parameters.
  #
  # Parameters
  # ----
  # fit : stanfit
  #     Fitted Stan model object
  # params : character vector
  #     Names of parameters to extract (default: main group-level means)
  # probs : numeric vector
  #     Quantiles to compute (default: c(0.025, 0.25, 0.5, 0.75, 0.975))
  #
  # Returns
  # ----
  # tibble
  #     Data frame with columns: parameter, mean, sd, and quantile columns
  #####

  # Get all available parameters
  all_params <- names(fit)
  params <- intersect(params, all_params)  # Only keep params that exist in this model

  if (length(params) == 0) {
    warning("None of the requested parameters found in model")
    return(tibble::tibble())
  }

  # Extract summaries
  summary_df <- as.data.frame(rstan::summary(fit, pars = params, probs = probs)$summary)

  result <- summary_df %>%
    tibble::rownames_to_column("parameter") %>%
    tibble::as_tibble() %>%
    dplyr::select(parameter, mean, sd, dplyr::everything())

  result
}


# ========== Posterior Predictive Checks ==========

rl_posterior_predict <- function(fit, stan_data, n_samples = 100) {
  #####
  # Generate posterior predictive samples for model checking
  #
  # Extracts generated quantities from Stan fit (y_pred, if available)
  # or provides structure for custom PPC implementation.
  #
  # Parameters
  # ----
  # fit : stanfit
  #     Fitted Stan model object
  # stan_data : list
  #     Original Stan data list
  # n_samples : integer
  #     Number of posterior samples to extract (default: 100)
  #
  # Returns
  # ----
  # array or NULL
  #     Posterior predictive samples (structure depends on generated quantities)
  #     Returns NULL if y_pred not found in model
  #####

  # Check if model has posterior predictions
  param_names <- names(fit)

  if ("y_pred" %in% param_names) {
    # Extract predicted choices
    y_pred <- rstan::extract(fit, pars = "y_pred")$y_pred

    # Subsample if needed
    n_iter <- dim(y_pred)[1]
    if (n_iter > n_samples) {
      idx <- sample(n_iter, n_samples)
      y_pred <- y_pred[idx, , ]
    }

    return(y_pred)
  } else {
    warning("Model does not contain 'y_pred' in generated quantities")
    return(NULL)
  }
}
