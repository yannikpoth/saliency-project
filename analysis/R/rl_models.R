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

# ========== System Resources & Parallel Planning ==========

rl_detect_system_resources <- function(reserve_cores = 1) {
  #####
  # Detect system resources relevant for Stan parallelization
  #
  # Keeps this intentionally simple and dependency-free:
  # - detects logical and (if available) physical CPU cores
  # - derives a conservative "available cores" budget by reserving cores for OS/IO
  #
  # Parameters
  # ----
  # reserve_cores : integer
  #     Number of CPU cores to reserve for OS/background tasks (default: 1)
  #
  # Returns
  # ----
  # list
  #     Named list with:
  #     - os_type : character ("unix" or "windows")
  #     - sysname : character (e.g., "Darwin", "Linux", "Windows")
  #     - cores_logical : integer
  #     - cores_physical : integer or NA
  #     - cores_available : integer (>= 1)
  #####
  reserve_cores <- as.integer(reserve_cores)
  if (is.na(reserve_cores) || reserve_cores < 0) reserve_cores <- 1

  os_type <- .Platform$OS.type
  sysname <- tryCatch(Sys.info()[["sysname"]], error = function(e) NA_character_)
  if (is.na(sysname) || !nzchar(sysname)) sysname <- NA_character_

  cores_logical <- parallel::detectCores(logical = TRUE)
  if (is.na(cores_logical) || cores_logical < 1) cores_logical <- 1L

  cores_physical <- tryCatch(
    parallel::detectCores(logical = FALSE),
    error = function(e) NA_integer_
  )

  if (is.na(cores_physical) || cores_physical < 1) cores_physical <- NA_integer_

  cores_available <- max(1L, as.integer(cores_logical - reserve_cores))

  list(
    os_type = os_type,
    sysname = sysname,
    cores_logical = as.integer(cores_logical),
    cores_physical = ifelse(is.na(cores_physical), NA_integer_, as.integer(cores_physical)),
    cores_available = as.integer(cores_available)
  )
}


rl_plan_model_parallelism <- function(n_models,
                                      chains_per_model = 2,
                                      reserve_cores = 1) {
  #####
  # Plan parallel model fitting given CPU resources and chains-per-model policy
  #
  # Goal: use available CPU cores efficiently without nested parallel oversubscription.
  # We treat "chains_per_model" as the number of cores needed per concurrently fitted model
  # (because rstan parallelizes chains using option(mc.cores)).
  #
  # On Unix, we can *in principle* run multiple models in parallel via fork.
  #
  # NOTE (macOS): Fork-based parallelism via parallel::mclapply() is not reliable on
  # macOS when Objective-C / GUI / plotting-related libraries are initialized (common
  # with packages like ggplot2/bayesplot). This can crash workers with errors like:
  # "objc[...] may have been in progress in another thread when fork() was called".
  # To keep the pipeline robust, we disable model-level fork-parallelism on macOS.
  # On Windows, we fall back to sequential execution (keep-it-simple).
  #
  # Parameters
  # ----
  # n_models : integer
  #     Number of models to process
  # chains_per_model : integer
  #     Chains per model (also used as cores per model) (default: 2)
  # reserve_cores : integer
  #     Reserved cores for OS/background tasks (default: 1)
  #
  # Returns
  # ----
  # list
  #     Named list with:
  #     - resources: output of rl_detect_system_resources()
  #     - workers: integer, number of models to run concurrently
  #     - use_parallel: logical
  #     - reason: character, short explanation
  #####
  n_models <- as.integer(n_models)
  if (is.na(n_models) || n_models < 1) n_models <- 1L

  chains_per_model <- as.integer(chains_per_model)
  if (is.na(chains_per_model) || chains_per_model < 1) chains_per_model <- 1L

  resources <- rl_detect_system_resources(reserve_cores = reserve_cores)

  if (resources$os_type == "windows") {
    return(list(
      resources = resources,
      workers = 1L,
      use_parallel = FALSE,
      reason = "Windows detected; using sequential execution (keep-it-simple)."
    ))
  }

  # macOS: disable fork-parallel model fitting (see NOTE above).
  if (!is.na(resources$sysname) && resources$sysname == "Darwin") {
    return(list(
      resources = resources,
      workers = 1L,
      use_parallel = FALSE,
      reason = "macOS (Darwin) detected; disabling fork-parallel model fitting for stability."
    ))
  }

  # Each concurrently fitted model is assumed to consume chains_per_model cores.
  workers <- floor(resources$cores_available / chains_per_model)
  workers <- max(1L, min(as.integer(workers), n_models))

  use_parallel <- workers > 1L
  reason <- if (use_parallel) {
    sprintf(
      "Using %d parallel workers (≈ %d cores total via %d chains/model).",
      workers, workers * chains_per_model, chains_per_model
    )
  } else {
    "Using sequential execution (insufficient CPU headroom for parallel workers)."
  }

  list(
    resources = resources,
    workers = as.integer(workers),
    use_parallel = use_parallel,
    reason = reason
  )
}


rl_process_single_model <- function(model_name,
                                    stan_data,
                                    timestamp,
                                    fit_dir,
                                    force_refit,
                                    chains,
                                    iter,
                                    warmup,
                                    verbose = TRUE) {
  #####
  # Fit/load one model and compute all downstream artifacts for comparison
  #
  # This wraps the per-model part of `analysis/run_analysis.R` into a single
  # function so we can (optionally) parallelize across models.
  #
  # Side effects:
  # - saves fit object via rl_load_or_fit()
  # - runs diagnostics_run_all() (saves figs/tables to disk)
  #
  # Parameters
  # ----
  # model_name : character
  #     Stan model name (without .stan extension)
  # stan_data : list
  #     Stan data list from prepare_stan_data()
  # timestamp : character
  #     Timestamp for output directories / cached fit file names
  # fit_dir : character
  #     Directory for cached fits
  # force_refit : logical
  #     If TRUE, refit even if cached fit exists
  # chains : integer
  #     Chains per model
  # iter : integer
  #     Iterations per chain
  # warmup : integer
  #     Warmup iterations per chain
  # verbose : logical
  #     Print progress messages
  #
  # Returns
  # ----
  # list
  #     Named list with:
  #     - model_name : character
  #     - loo : psis_loo or NULL
  #     - diagnostics_summary : tibble/data.frame or NULL
  #     - posterior_data : tibble/data.frame or NULL
  #####
  if (verbose) message(sprintf("\n--- Processing model: %s ---", model_name))

  # Ensure rstan uses one core per chain (parallel chains within this worker).
  options(mc.cores = as.integer(chains))

  fit <- rl_load_or_fit(
    model_name,
    stan_data = stan_data,
    fit_dir = fit_dir,
    force_refit = force_refit,
    timestamp = timestamp,
    chains = chains,
    iter = iter,
    warmup = warmup,
    verbose = verbose
  )

  # Diagnostics (writes outputs to disk)
  diag_summary <- NULL
  if (exists("diagnostics_run_all", mode = "function")) {
    results <- diagnostics_run_all(fit = fit, model_name = model_name, timestamp = timestamp)
    diag_summary <- results$summary
  } else {
    warning("diagnostics_run_all() not found. Did you source analysis/R/diagnostics.R?")
  }

  # LOO (keep lightweight)
  loo_obj <- NULL
  has_log_lik <- any(grepl("^log_lik", names(fit)))
  if (has_log_lik) {
    loo_obj <- tryCatch({
      log_lik <- loo::extract_log_lik(fit, parameter_name = "log_lik")
      loo::loo(log_lik)
    }, error = function(e) {
      warning(sprintf("Failed to compute LOO for %s: %s", model_name, e$message))
      NULL
    })
  } else {
    warning(sprintf("Model %s does not contain log_lik parameter. Skipping LOO.", model_name))
  }

  # Posterior samples (for density grid)
  posterior_df <- NULL
  if (exists("viz_extract_posterior_data", mode = "function")) {
    posterior_df <- viz_extract_posterior_data(fit, model_name)
  } else {
    warning("viz_extract_posterior_data() not found. Did you source analysis/R/viz.R?")
  }

  rm(fit)
  gc()

  list(
    model_name = model_name,
    loo = loo_obj,
    diagnostics_summary = diag_summary,
    posterior_data = posterior_df
  )
}


rl_run_models_pipeline <- function(model_names,
                                  stan_data,
                                  timestamp,
                                  fit_dir = "analysis/outputs/fits",
                                  force_refit = FALSE,
                                  chains = 2,
                                  iter = 10000,
                                  warmup = 8000,
                                  reserve_cores = 1,
                                  verbose = TRUE) {
  #####
  # Run model fitting + diagnostics + LOO + posterior extraction (optionally parallel)
  #
  # Uses a simple, conservative parallelization strategy:
  # - parallelize across models
  # - within each model, allow rstan to parallelize chains via option(mc.cores)
  # - cap the number of concurrent models so total cores ≈ workers * chains
  #
  # Parameters
  # ----
  # model_names : character vector
  #     Models to process
  # stan_data : list
  #     Stan data list from prepare_stan_data()
  # timestamp : character
  #     Timestamp for output directories / cached fit file names
  # fit_dir : character
  #     Directory for cached fits (default: "analysis/outputs/fits")
  # force_refit : logical
  #     If TRUE, refit even if cached fits exist (default: FALSE)
  # chains : integer
  #     Chains per model (default: 2)
  # iter : integer
  #     Iterations per chain (default: 10000)
  # warmup : integer
  #     Warmup iterations per chain (default: 8000)
  # reserve_cores : integer
  #     Reserved cores for OS/background tasks (default: 1)
  # verbose : logical
  #     Print progress messages (default: TRUE)
  #
  # Returns
  # ----
  # list
  #     Named list with:
  #     - plan : list, parallel plan from rl_plan_model_parallelism()
  #     - results : list, per-model result objects
  #####
  if (length(model_names) == 0) {
    warning("No models provided to rl_run_models_pipeline().")
    return(list(plan = NULL, results = list()))
  }

  plan <- rl_plan_model_parallelism(
    n_models = length(model_names),
    chains_per_model = chains,
    reserve_cores = reserve_cores
  )

  if (verbose) {
    message("\n=== Stan resource plan ===")
    message(sprintf("  OS type: %s", plan$resources$os_type))
    message(sprintf("  Logical cores: %d", plan$resources$cores_logical))
    if (!is.na(plan$resources$cores_physical)) {
      message(sprintf("  Physical cores: %d", plan$resources$cores_physical))
    }
    message(sprintf("  Reserved cores: %d", as.integer(reserve_cores)))
    message(sprintf("  Available cores: %d", plan$resources$cores_available))
    message(sprintf("  Chains/model: %d", as.integer(chains)))
    message(sprintf("  Parallel workers: %d", plan$workers))
    message(sprintf("  Decision: %s", plan$reason))
  }

  worker_fun <- function(m) {
    rl_process_single_model(
      model_name = m,
      stan_data = stan_data,
      timestamp = timestamp,
      fit_dir = fit_dir,
      force_refit = force_refit,
      chains = chains,
      iter = iter,
      warmup = warmup,
      verbose = verbose
    )
  }

  if (plan$use_parallel) {
    results <- parallel::mclapply(
      model_names,
      worker_fun,
      mc.cores = plan$workers
    )
  } else {
    results <- lapply(model_names, worker_fun)
  }

  names(results) <- model_names

  list(
    plan = plan,
    results = results
  )
}


# ========== Core Fitting Functions ==========

rl_get_init_function <- function(stan_data = NULL) {
  #####
  # Create flexible initialization function for Stan sampling
  #
  # Returns a function that generates sensible starting values for RL model
  # parameters. Works with all model variants (basic, shift, perseveration)
  # by providing all possible parameters; Stan ignores unused ones.
  #
  # Uses hard-coded sensible values based on typical RL parameter ranges:
  # - alpha (learning rate): ~0.65-0.70 in transformed space
  # - beta (inverse temperature): ~3-4 in [0, 10] space
  # - shift and kappa parameters: near 0 (neutral starting point)
  #
  # Parameters
  # ----
  # stan_data : list or NULL
  #     Stan data list containing nSubs (default: NULL). If provided, generates
  #     subject-level initial values; otherwise only group-level values.
  #
  # Returns
  # ----
  # function
  #     Initialization function that returns a list of starting values
  #####

  function() {
    init_list <- list(
      # Group-level means
      # alpha_mu_raw = 0.5 gives Phi(0.5) ≈ 0.69 for transformed alpha
      alpha_mu_raw = 0.5,

      # beta_mu_raw = -0.2 gives Phi(-0.2) * 10 ≈ 4.2 for transformed beta
      beta_mu_raw = -0.2,

      # Shift and kappa start near 0
      alpha_shift_mu_raw = 0.0,
      kappa_mu_raw = 0.0,
      kappa_shift_mu_raw = 0.0,

      # Group-level SDs (start with moderate positive values)
      alpha_sd_raw = 0.8,
      alpha_shift_sd_raw = 0.6,
      beta_sd_raw = 0.8,
      kappa_sd_raw = 0.6,
      kappa_shift_sd_raw = 0.6
    )

    # Add subject-level initial values if stan_data is provided
    if (!is.null(stan_data) && "nSubs" %in% names(stan_data)) {
      nSubs <- stan_data$nSubs

      # Initialize subject-level parameters near group means with small jitter
      # to avoid identical starting points across subjects
      init_list$alpha_subj_raw <- rnorm(nSubs, mean = 0.5, sd = 0.1)
      init_list$beta_subj_raw <- rnorm(nSubs, mean = -0.2, sd = 0.1)
      init_list$alpha_shift_subj_raw <- rnorm(nSubs, mean = 0.0, sd = 0.05)
      init_list$kappa_subj_raw <- rnorm(nSubs, mean = 0.0, sd = 0.05)
      init_list$kappa_shift_subj_raw <- rnorm(nSubs, mean = 0.0, sd = 0.05)
    }

    return(init_list)
  }
}


rl_fit_single <- function(model_name,
                          stan_data,
                          chains = 2,
                          iter = 12000,
                          warmup = 10000,
                          adapt_delta = 0.8,
                          max_treedepth = 10,
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
 #     Number of MCMC chains (default: 2)
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
    init = rl_get_init_function(stan_data)
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
                           timestamp = NULL,
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
  # timestamp : character
  #     Optional timestamp string to append to the filename (default: NULL)
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

  # Look for existing fits (with or without timestamp)
  # Matches model_name_fit.rds or model_name_fit_TIMESTAMP.rds
  pattern <- paste0("^", model_name, "_fit.*\\.rds$")
  existing_files <- list.files(fit_dir, pattern = pattern, full.names = TRUE)

  # Try to load existing fit
  if (!force_refit && length(existing_files) > 0) {
    # Find the most recently modified file
    info <- file.info(existing_files)
    latest_file <- rownames(info)[which.max(info$mtime)]

    message(sprintf("Loading cached fit: %s", basename(latest_file)))
    fit <- readRDS(latest_file)
    return(fit)
  }

  # Fit new model
  fit <- rl_fit_single(model_name, stan_data, ...)

  # Determine save path
  if (!is.null(timestamp)) {
    fit_path <- file.path(fit_dir, paste0(model_name, "_fit_", timestamp, ".rds"))
  } else {
    fit_path <- file.path(fit_dir, paste0(model_name, "_fit.rds"))
  }

  # Save for future use
  saveRDS(fit, fit_path)
  message(sprintf("Saved fit to: %s", fit_path))

  fit
}


rl_fit_all <- function(model_names,
                       stan_data,
                       fit_dir = "analysis/outputs/fits",
                       force_refit = FALSE,
                       timestamp = NULL,
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
  # timestamp : character
  #     Optional timestamp string to append to filenames (default: NULL)
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
      timestamp = timestamp,
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
  # Can accept either a list of fitted model objects or a list of pre-computed
  # LOO objects (for memory efficiency).
  #
  # Parameters
  # ----
  # fit_list : named list
  #     List of stanfit objects from rl_fit_all(), OR list of psis_loo objects
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

  message("\n=== Computing/Aggregating LOO for model comparison ===")

  if (length(fit_list) == 0) {
    warning("No models provided for comparison (fit_list is empty).")
    return(NULL)
  }

  loo_objects <- list()

  # Check if input is already a list of LOO objects
  first_item <- fit_list[[1]]
  is_loo_list <- inherits(first_item, "psis_loo")

  if (is_loo_list) {
    message("Input detected as list of pre-computed LOO objects.")
    loo_objects <- fit_list
  } else {
    message("Input detected as list of Stan fit objects. Computing LOO now...")
    for (model_name in names(fit_list)) {
      message(sprintf("Computing LOO for: %s", model_name))
      log_lik <- loo::extract_log_lik(fit_list[[model_name]], parameter_name = "log_lik")
      loo_objects[[model_name]] <- loo::loo(log_lik)
    }
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
                              params = c(
                                "alpha_mu", "alpha_salient_mu", "alpha_shift_mu",
                                "beta_mu",
                                "kappa_mu", "kappa_salient_mu", "kappa_shift_mu"
                              ),
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
  #     Names of parameters to extract (default: key group-level parameters,
  #     including salient-condition totals and salience shifts where available)
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
