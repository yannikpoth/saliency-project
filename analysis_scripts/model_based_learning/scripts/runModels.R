#      Run models -- Version 2
# Last edit:    2025/05/07 
# Authors:      Poth, Yannik (YP)
#               Geysen, Steven (SG)
# Notes:        - Fits YP's and SG's basic RL models (rl_cp_basic_YP.stan, rl_cp_basic_SG.stan).
#               - Assumes script is run from analysis_scripts/model_based_learning/
#               - Saves output to analysis_scripts/model_based_learning/results/
#               - Uses LOOIC for model comparison and stores fit objects in a list.
#               - Increased adapt_delta for potentially better sampling.
# To do:
# Comments:
# Sources:      https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
# --------------

# --- SCRIPT START ---
print(paste("Current working directory:", getwd()))


#### Imports ####


# Clear working memory (optional, uncomment if desired)
# rm(list = ls())


# Libraries
library(loo)      # For LOOIC calculation
library(rstan)    # For running Stan
library(ggplot2)  # For plotting
library(bayesplot)# For plotting
library(shinystan)# For model diagnostics
library(dplyr)    # For data manipulation (used in PPC)
library(tidyr)    # For data reshaping (used in PPC)

# Source the data preparation script (adjust path relative to this project location)
# Assumes this script is in 'scripts/' and StanList.R is in '../rls/data/'
print("Sourcing StanList.R from rls/data/StanList.R")
source("rls/data/StanList.R") 


## Stan settings
rstan_options(auto_write = TRUE)            # Avoid recompiling unchanged models
options(mc.cores = parallel::detectCores()) # Use all available CPU cores



#### Variables ####
###################


# Stan execution parameters
nChains <- 4      # Number of Markov chains
nIters <- 4000    # Total iterations per chain (incl. warmup)
# nWarmup <- 1000 # Specify warmup separately if needed, otherwise defaults to nIters/2

# Prepare the data using the sourced function
# Set saveStan = TRUE if you want StanList.RData to be saved in its processed dir
# Set debugsize > 0 to run on a subset of subjects
print("Preparing StanList using StanCleaned()...")
StanList <- StanCleaned(debugsize = 0, saveStan = TRUE) # saveStan can be TRUE or FALSE
print("StanList preparation finished. Structure of StanList:")
print(str(StanList))
if(!is.null(StanList) && !is.null(StanList$nSubs)) {
    print(paste("Number of subjects (StanList$nSubs):", StanList$nSubs))
} else if (!is.null(StanList) && !is.null(StanList$nSubs)) {
    print(paste("Number of subjects (StanList$nSubs from StanList.R):", StanList$nSubs))
    # If StanList$nSubs isn't there but StanList$nSubs is, let's ensure S exists for later code
    if(is.null(StanList$nSubs)) StanList$nSubs <- StanList$nSubs
} else {
    print("StanList$nSubs or StanList$nSubs is NULL or StanList itself is NULL.")
}


# Define the model(s) to run
modelist <- c("rl_cp_shift_normal", "rl_cp_shift_uniform")
# modelist <- c("rl_cp_basic_final", "rl_cp_shift")

# Initialize dataframe to store model comparison results
model_df <- data.frame(
  model = character(),
  elpd_loo = numeric(),
  elpd_loo_se = numeric(),
  p_loo = numeric(),
  p_loo_se = numeric(),
  looic = numeric(),
  looic_se = numeric(),
  max_rhat = numeric(),      # Maximum R-hat value
  min_n_eff = numeric(),     # Minimum Effective Sample Size (n_eff)
  elpd_diff = numeric(),     # Placeholder, will be filled after comparing all models
  se_diff = numeric(),       # Placeholder
  stringsAsFactors = FALSE   # Prevent factors by default
)
# Initialize lists to store fit objects and loo objects
model_fits_list <- list()
loo_objects_list <- list()


# Define output directories (relative to the script execution location)
# Comment out if you don't want to save the fit objects and model comparison results
results_basedir <- "results" # Base directory for results
fit_dir <- file.path(results_basedir, "fitobjects")
comp_dir <- file.path(results_basedir, "model_comparison")

# Create directories if they don't exist
dir.create(results_basedir, showWarnings = FALSE)
dir.create(fit_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(comp_dir, showWarnings = FALSE, recursive = TRUE)


#### Run models ####
####################


for (modeli in modelist) {
  print(paste("Now running:", modeli))

  # Construct model file path (relative to script execution location)
  # Assumes models are in 'rls/models/'
  modelfile_name <- file.path("rls", "models", paste0(modeli, ".stan"))

  # Check if model file exists
  if (!file.exists(modelfile_name)) {
    warning(paste("Model file not found:", modelfile_name, "Skipping this model."))
    next # Skip to the next model in the list
  }

  # Fit the Stan model
  model_fit <- stan(
    file = modelfile_name,
    data = StanList,
    chains = nChains,
    iter = nIters,
    # warmup = nWarmup, # Uncomment if nWarmup is defined
    control = list(adapt_delta = 0.95), # Increased adapt_delta
    seed = 123 # Add a seed for reproducibility
  )

  # --- Extract Diagnostic Metrics ---
  # These are extracted regardless of whether LOOIC calculation succeeds
  fit_summary_stats <- summary(model_fit)$summary
  max_rhat_val <- if (nrow(fit_summary_stats) > 0 && "Rhat" %in% colnames(fit_summary_stats)) {
    suppressWarnings(max(fit_summary_stats[, "Rhat"], na.rm = TRUE))
  } else {
    NA_real_
  }
  min_n_eff_val <- if (nrow(fit_summary_stats) > 0 && "n_eff" %in% colnames(fit_summary_stats)) {
    suppressWarnings(min(fit_summary_stats[, "n_eff"], na.rm = TRUE))
  } else {
    NA_real_
  }
  # Handle cases where max/min return Inf/-Inf if all values were NA or no valid data
  if (is.infinite(max_rhat_val)) max_rhat_val <- NA_real_
  if (is.infinite(min_n_eff_val)) min_n_eff_val <- NA_real_
  
  # --- Store and Save Fit Object ---
  # Store fit object in the list
  model_fits_list[[modeli]] <- model_fit
  
  # Assign to global environment (optional, for interactive debugging)
  assign(
    paste(modeli, "fitobject", sep="_"), model_fit,
    envir=.GlobalEnv
  )
  
  # Save RDS
  fit_filename <- paste0(modeli, "_", format(Sys.time(), "%Y%m%d-%H%M"), ".rds")
  fit_outpath <- file.path(fit_dir, fit_filename)
  saveRDS(model_fit, fit_outpath)
  print(paste("Fit object saved to:", fit_outpath))


  # --- LOOIC Calculation ---
  # Requires log_lik matrix in generated quantities of Stan model
  print("Calculating LOOIC...")
  log_like <- tryCatch({
     loo::extract_log_lik(model_fit, parameter_name = "log_lik")
  }, error = function(e) {
     warning(paste("Could not extract log_lik for model:", modeli, ". LOOIC calculation skipped. Error:", e$message))
     NULL
  })

  if (!is.null(log_like)) {
    model_loo_obj <- loo::loo(log_like)
    loo_objects_list[[modeli]] <- model_loo_obj # Store loo object for later comparison

    model_loo_estimates <- model_loo_obj$estimates

    # Store per-model LOOIC results
    model_df <- rbind(
      model_df,
      data.frame(
        model = modeli,
        elpd_loo = model_loo_estimates["elpd_loo", "Estimate"],
        elpd_loo_se = model_loo_estimates["elpd_loo", "SE"],
        p_loo = model_loo_estimates["p_loo", "Estimate"],
        p_loo_se = model_loo_estimates["p_loo", "SE"],
        looic = model_loo_estimates["looic", "Estimate"],
        looic_se = model_loo_estimates["looic", "SE"],
        max_rhat = max_rhat_val,
        min_n_eff = min_n_eff_val,
        elpd_diff = NA, # Placeholder, will be filled after comparing all models
        se_diff = NA    # Placeholder
      )
    )
    print("LOOIC calculated.")
  } else {
       # Add row with NAs if log_lik extraction failed
       model_df <- rbind(
         model_df,
         data.frame(
            model = modeli,
            elpd_loo = NA, elpd_loo_se = NA,
            p_loo = NA, p_loo_se = NA,
            looic = NA, looic_se = NA,
            max_rhat = max_rhat_val,
            min_n_eff = min_n_eff_val,
            elpd_diff = NA, se_diff = NA
         )
       )
  }
}

# --- Consolidate Model Comparison Results ---
if (nrow(model_df) > 0) {
  if (length(loo_objects_list) > 1) {
    print("Comparing models using loo_compare...")
    comparison <- loo::loo_compare(loo_objects_list)
    print(comparison) # Print to console

    comparison_df <- as.data.frame(comparison)
    
    # Update model_df with elpd_diff and se_diff
    # The row names of comparison_df are the model names
    for (m_name in rownames(comparison_df)) {
      if (m_name %in% model_df$model) {
        model_df[model_df$model == m_name, "elpd_diff"] <- comparison_df[m_name, "elpd_diff"]
        model_df[model_df$model == m_name, "se_diff"] <- comparison_df[m_name, "se_diff"]
      }
    }
  } else if (length(loo_objects_list) == 1 && nrow(model_df) == 1) {
    # If only one model, elpd_diff can be set to 0
    model_df[1, "elpd_diff"] <- 0
    model_df[1, "se_diff"] <- 0
  }

  # --- Save Model Comparison Results Table ---
  comp_filename <- paste0("modelcomparison_", format(Sys.time(), "%Y%m%d-%H%M"), ".tsv")
  comp_outpath <- file.path(comp_dir, comp_filename)

  write.table(
    model_df,
    file = comp_outpath,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  print(paste("Model comparison results saved to:", comp_outpath))
} else {
  print("No models were successfully run or LOOIC calculated. No comparison file saved.")
}

print("Model fitting finished.")

#### Posterior Predictive Checks - Learning Curves (Proportion Correct by Block) ####
####################################################################################
print("Starting Posterior Predictive Checks - Learning Curves (Proportion Correct by Block)...")

# --- 1. Setup --- 
T_trials <- 200 # Number of main trials
block_size <- 10
num_blocks <- T_trials / block_size

plots_dir <- file.path(results_basedir, "plots") # Ensure plots_dir is defined
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

# Load true reward probabilities
# Path assumes working directory is 'analysis_scripts/model_based_learning/' as per user file structure
path_to_rw_csv <- "../../bandit_task/task_data/random_walk/csv/main_random_walk.csv"
true_probs_data <- tryCatch({
  read.csv(path_to_rw_csv)
}, error = function(e) {
  warning(paste("Could not load true reward probabilities from:", path_to_rw_csv, ". Error:", e$message))
  NULL
})

optimal_choice_per_trial <- rep(NA, T_trials)
if (!is.null(true_probs_data) && nrow(true_probs_data) >= T_trials) {
  true_reward_prob_stim1 <- true_probs_data$mu_1[1:T_trials]
  true_reward_prob_stim2 <- true_probs_data$mu_2[1:T_trials]
  # Determine optimal choice (1 for stim1, 2 for stim2)
  # Handling ties: if probs are equal, designate stim2 (choice 2) as optimal (can be changed if needed)
  optimal_choice_per_trial <- ifelse(true_reward_prob_stim1 > true_reward_prob_stim2, 1, 2)
  optimal_choice_per_trial[true_reward_prob_stim1 == true_reward_prob_stim2] <- 2 # Consistent tie-breaking
} else {
  warning("True reward probabilities data not loaded or insufficient rows. Optimal choices cannot be determined.")
  # optimal_choice_per_trial remains all NAs, downstream calculations will reflect this
}

# --- 2. Process y_obs (Actual Participant Choices) ---
y_obs_prop_correct_blocks <- rep(NA, num_blocks)
if (!is.null(StanList) && !is.null(StanList$choice) && !is.null(StanList$nSubs) && StanList$nSubs > 0 && any(!is.na(optimal_choice_per_trial))) {
  actual_choices_raw <- StanList$choice # Matrix [nSubs, T_trials], choices are 1 or 2, -9 for missing
  n_subs_for_ppc <- StanList$nSubs

  prop_correct_per_subject_block <- matrix(NA_real_, nrow = n_subs_for_ppc, ncol = num_blocks)

  for (subi in 1:n_subs_for_ppc) {
    for (block_idx in 1:num_blocks) {
      start_trial_idx <- (block_idx - 1) * block_size + 1
      end_trial_idx <- block_idx * block_size
      
      subject_choices_block <- actual_choices_raw[subi, start_trial_idx:end_trial_idx]
      optimal_choices_block <- optimal_choice_per_trial[start_trial_idx:end_trial_idx]
      
      # Filter out missing trials (-9 for choices, or NA for optimal if probs failed to load)
      valid_trials_mask <- subject_choices_block != -9 & !is.na(optimal_choices_block)
      
      if (sum(valid_trials_mask) > 0) {
        correct_choices_in_block <- sum(subject_choices_block[valid_trials_mask] == optimal_choices_block[valid_trials_mask])
        prop_correct_per_subject_block[subi, block_idx] <- correct_choices_in_block / sum(valid_trials_mask)
      } else {
        prop_correct_per_subject_block[subi, block_idx] <- NA # No valid trials in this block for this subject
      }
    }
  }
  y_obs_prop_correct_blocks <- colMeans(prop_correct_per_subject_block, na.rm = TRUE)
} else {
  warning("Cannot calculate observed proportion correct: StanList not populated, or optimal choices not determined.")
}

print("Summary of y_obs_prop_correct_blocks:")
print(summary(y_obs_prop_correct_blocks))

# --- 3. Process y_rep (Model-Predicted Choices) & Plot for each model ---
for (model_name in modelist) {
  print(paste("Processing PPC for model:", model_name))
  model_fit_obj <- model_fits_list[[model_name]]

  if (is.null(model_fit_obj)) {
    warning(paste("Fit object for model", model_name, "not found. Skipping PPC plot."))
    next
  }
  
  if (any(is.na(optimal_choice_per_trial))){
      warning(paste("Optimal choices not determined. Skipping PPC plot for model:", model_name))
      next
  }

  # Extract predicted choices (Stan model output: 1 or 2 for choices, -9 or -999 for missing/skipped)
  predicted_choices_all_iter <- tryCatch({
    rstan::extract(model_fit_obj, pars = "predicted_choices")[["predicted_choices"]]
  }, error = function(e) {
    warning(paste("Could not extract 'predicted_choices' for model:", model_name, ". Error:", e$message))
    NULL
  })

  if (is.null(predicted_choices_all_iter)) {
    warning(paste("Skipping PPC for", model_name, "due to missing predicted_choices."))
    next
  }
  
  # Check dimensions: [iterations, nSubs, T_trials]
  if (length(dim(predicted_choices_all_iter)) != 3 || 
      dim(predicted_choices_all_iter)[2] != StanList$nSubs || 
      dim(predicted_choices_all_iter)[3] != T_trials) {
    warning(paste("Dimensions of 'predicted_choices' for", model_name, "are unexpected. Dim:", 
                  paste(dim(predicted_choices_all_iter), collapse="x"), 
                  ". Expected: iter x", StanList$nSubs, "x", T_trials, ". Skipping PPC plot."))
    next
  }

  num_iterations <- dim(predicted_choices_all_iter)[1]
  y_rep_prop_correct_blocks_matrix <- matrix(NA_real_, nrow = num_iterations, ncol = num_blocks)

  for (iter in 1:num_iterations) {
    current_iter_subject_preds <- predicted_choices_all_iter[iter, , ] # Matrix [nSubs, T_trials]
    prop_correct_per_subject_block_iter <- matrix(NA_real_, nrow = StanList$nSubs, ncol = num_blocks)
    
    for (subi in 1:StanList$nSubs) {
      for (block_idx in 1:num_blocks) {
        start_trial_idx <- (block_idx - 1) * block_size + 1
        end_trial_idx <- block_idx * block_size
        
        subject_preds_block <- current_iter_subject_preds[subi, start_trial_idx:end_trial_idx]
        optimal_choices_block <- optimal_choice_per_trial[start_trial_idx:end_trial_idx]
        
        # Filter out missing/skipped predictions (e.g., -9, -999) & NA optimal choices
        # Stan models use -9 or -999 for non-computed predictions in generated quantities
        valid_preds_mask <- subject_preds_block >= 1 & !is.na(optimal_choices_block) 
        
        if (sum(valid_preds_mask) > 0) {
          correct_preds_in_block <- sum(subject_preds_block[valid_preds_mask] == optimal_choices_block[valid_preds_mask])
          prop_correct_per_subject_block_iter[subi, block_idx] <- correct_preds_in_block / sum(valid_preds_mask)
        } else {
          prop_correct_per_subject_block_iter[subi, block_idx] <- NA
        }
      }
    }
    y_rep_prop_correct_blocks_matrix[iter, ] <- colMeans(prop_correct_per_subject_block_iter, na.rm = TRUE)
  }
  
  print(paste("Summary of y_rep_prop_correct_blocks_matrix for model:", model_name))
  # Print summary of a few columns of the y_rep matrix to check values
  if(ncol(y_rep_prop_correct_blocks_matrix) > 0) print(summary(y_rep_prop_correct_blocks_matrix[, 1:min(5, num_blocks)]))

  # --- 4. Plot using bayesplot::ppc_ribbon --- 
  if (any(!is.na(y_obs_prop_correct_blocks)) && any(!is.na(y_rep_prop_correct_blocks_matrix))) {
    ppc_plot <- bayesplot::ppc_ribbon(
      y = y_obs_prop_correct_blocks, 
      yrep = y_rep_prop_correct_blocks_matrix, 
      x = 1:num_blocks,
      prob = 0.5, # Median line for y_rep
      prob_outer = 0.9 # 90% credible interval ribbon
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = c(0,0.01)) +
    ggplot2::scale_x_continuous(breaks = seq(0, num_blocks, by = max(1, floor(num_blocks/10))), expand = c(0.01,0.01)) +
    ggplot2::labs(
      title = paste("PPC: Learning Curve (Proportion Correct) - Model:", model_name),
      subtitle = paste(num_blocks, "blocks of", block_size, "trials each. Ribbon = 90% CI from model predictions."),
      x = "Trial Block",
      y = "Proportion of Optimal Choices"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "none") # ppc_ribbon y vs y_rep is clear

    print(ppc_plot)
    plot_filename <- file.path(plots_dir, paste0("ppc_prop_correct_blocks_", model_name, "_", format(Sys.time(), "%Y%m%d-%H%M"), ".png"))
    ggplot2::ggsave(plot_filename, plot = ppc_plot, width = 8, height = 6, dpi = 300, bg = "white")
    print(paste("PPC plot saved to:", plot_filename))
  } else {
    warning(paste("Skipping PPC plot for model:", model_name, "due to all NA values in y_obs or y_rep."))
  }
}

print("Posterior Predictive Checks - Learning Curves (Proportion Correct by Block) section finished.")





# ------------------------------------------------------------------------- End