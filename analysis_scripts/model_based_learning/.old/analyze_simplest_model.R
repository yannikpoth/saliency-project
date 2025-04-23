# R Script for Analyzing Saliency Task Data with Simplest Viable RL Model
# Focuses on fitting the model and basic diagnostics/summaries

# --- 0. Load Libraries ---
# Ensure these packages are installed: install.packages(c("tidyverse", "rstan", "bayesplot", "here", "fs", "loo"))
library(tidyverse)
library(rstan)      # For running Stan models
library(bayesplot)  # For plotting Stan results
library(here)       # For easy path management
library(fs)         # For file system operations
library(loo)        # For LOO-CV (optional, requires log_lik)

# Set Stan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()) # Use multiple cores

# Set seed for reproducibility
set.seed(456) # Use a different seed than the other script

# --- 1. Load and Prepare Data ---

# Define data directory path using 'here' package
data_dir <- here::here("bandit_task", "collected_data")

# List task data files
task_files <- fs::dir_ls(data_dir, regexp = "_task_data\\.csv$")

# Function to read and preprocess a single task file (same as in analyze_saliency_data.R)
read_task_data <- function(file_path) {
  subject_id <- sub("_task_data\\.csv$", "", basename(file_path))
  read_csv(file_path, col_types = cols(
    mode = col_character(),
    trial = col_integer(),
    choice = col_double(),
    reaction_time = col_double(),
    reward = col_double(),
    condition = col_integer()
  )) %>%
    mutate(
      subject_id = as.character(subject_id),
      choice = if_else(is.na(choice) | !choice %in% c(0, 1), NA_integer_, as.integer(choice)),
      reward = as.integer(reward)
      ) %>%
    select(subject_id, mode, trial, choice, reaction_time, reward, condition)
}

# Read and combine all task data
all_task_data <- map_dfr(task_files, read_task_data)

# --- Data Cleaning and Preparation ---

# Filter for main trials and valid choices
analysis_data <- all_task_data %>%
  filter(mode == 'main') %>%
  filter(!is.na(choice)) %>%
  mutate(subj_idx = as.integer(factor(subject_id)))

# Create salient feedback indicator (1 if salient feedback condition, 0 otherwise)
# The Stan model 'simplest_viable_RL_model.stan' uses the 'salient' variable
# to check if the trial was in the salient *condition* (salient[n]==1),
# and *then* checks if it was a win (outcome[n]==1) to apply the shift.
analysis_data <- analysis_data %>%
  mutate(salient = if_else(condition == 1, 1, 0)) # 1 if salient condition, 0 if non-salient

# Check data structure
head(analysis_data)
summary(analysis_data)
n_subjects_found <- length(unique(analysis_data$subj_idx))
message(paste("Found data for", n_subjects_found, "subjects."))

# --- Prepare Data for Stan Model ---

# Get dimensions
N <- nrow(analysis_data)
N_subj <- max(analysis_data$subj_idx)

# Create Stan data list - names must match simplest_viable_RL_model.stan
stan_data <- list(
  N = N,
  N_subj = N_subj,
  subj_id = analysis_data$subj_idx,
  choice = analysis_data$choice,   # Already 0 or 1 integer
  outcome = analysis_data$reward,  # Already 0 or 1 integer
  salient = analysis_data$salient # 1 if salient feedback condition, 0 otherwise
)

# Verify Stan data list components
str(stan_data)
stopifnot(
  N > 0,
  N_subj > 0,
  length(stan_data$subj_id) == N,
  length(stan_data$choice) == N,
  length(stan_data$outcome) == N,
  length(stan_data$salient) == N,
  all(!is.na(stan_data$choice)),
  all(stan_data$choice %in% c(0, 1)),
  all(!is.na(stan_data$outcome)),
  all(stan_data$outcome %in% c(0, 1)),
  all(!is.na(stan_data$salient)),
  all(stan_data$salient %in% c(0, 1)),
  max(stan_data$subj_id) == N_subj
)

message("Stan data list prepared successfully.")

# --- 2. Fit Stan Model ---

message("Compiling and fitting the simplest viable Stan model...")

# Define Stan model file path
stan_model_file <- here::here("analysis_scripts", "model_based_learning", "simplest_viable_RL_model.stan")

# Check if file exists
if (!fs::file_exists(stan_model_file)) {
  stop("Stan model file not found at: ", stan_model_file)
}

# Fit the model
# Using slightly fewer iterations as it's a simpler model, but can be increased if needed
fit_simple <- stan(
  file = stan_model_file,
  data = stan_data,
  iter = 1500,       # Total iterations per chain
  warmup = 750,      # Burn-in iterations
  chains = 4,        # Number of chains
  cores = getOption("mc.cores", 1),
  seed = 456,
  control = list(adapt_delta = 0.95) # Increase adapt_delta slightly
)

message("Simplest Stan model fitting complete.")

# --- 3. Model Diagnostics ---

# Summary statistics (Rhat, n_eff)
summary_simple <- summary(fit_simple)$summary
filtered_summary <- summary_simple[c("mu_alpha", "mu_alpha_shift", "mu_beta"), ]
print(filtered_summary, digits = 3)

# Check Rhat values (should be < 1.05 or 1.1)
rhat_values_simple <- summary_simple[, "Rhat"]
message("Max Rhat: ", round(max(rhat_values_simple, na.rm = TRUE), 3))
if (any(rhat_values_simple > 1.1, na.rm = TRUE)) {
  warning("High Rhat values detected! Check convergence.")
  print(rhat_values_simple[rhat_values_simple > 1.1])
}

# Check effective sample size (n_eff) (should be adequate, e.g., > 400 or 1000)
neff_values_simple <- summary_simple[, "n_eff"]
min_neff <- min(neff_values_simple, na.rm = TRUE)
message("Min n_eff: ", round(min_neff))
if (any(neff_values_simple < 400, na.rm = TRUE)) {
  warning(paste("Low n_eff values detected (min =", round(min_neff), "). Model may need more iterations or reparameterization."))
}

# Check for divergences
divergences_simple <- get_num_divergent(fit_simple)
message("Number of divergent transitions: ", divergences_simple)
if (divergences_simple > 0) {
  warning("Divergent transitions detected! Check model parameters, priors, or increase adapt_delta.")
}