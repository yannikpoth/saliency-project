# R Script for Analyzing Saliency Task Data with Basic Hierarchical RL Model

# --- 0. Load Libraries ---
# Ensure these packages are installed: 
#install.packages(c("tidyverse", "rstan", "bayesplot", "here", "fs"))
library(tidyverse)
library(rstan)      # For running Stan models
library(bayesplot)  # For plotting Stan results
library(here)       # For easy path management
library(fs)         # For file system operations
# library(loo)        # LOO-CV not applicable without log_lik in basic model

# Set Stan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()) # Use multiple cores

# Set seed for reproducibility
set.seed(123) # Use a different seed

# --- 1. Load and Prepare Data ---

# Define data directory path using 'here' package
data_dir <- here::here("bandit_task", "collected_data")

# List task data files
task_files <- fs::dir_ls(data_dir, regexp = "_task_data\\.csv$")

# Function to read and preprocess a single task file
read_task_data <- function(file_path) {
  subject_id <- sub("_task_data\\.csv$", "", basename(file_path))
  read_csv(file_path, col_types = cols(
    mode = col_character(),
    trial = col_integer(),
    choice = col_double(), # Read as double first to handle potential NAs properly
    reaction_time = col_double(),
    reward = col_double(), # Read as double first
    condition = col_integer()
  )) %>%
    mutate(
      subject_id = as.character(subject_id),
      # Ensure choice is 0 or 1 before converting to integer, handle NAs
      choice = if_else(is.na(choice) | !choice %in% c(0, 1), NA_integer_, as.integer(choice)),
      # Ensure reward is 0 or 1 before converting to integer
      reward = if_else(is.na(reward) | !reward %in% c(0, 1), NA_real_, as.double(reward)) # Keep as double for Stan input
    ) %>%
    select(subject_id, mode, trial, choice, reaction_time, reward, condition)
}

# Read and combine all task data
all_task_data <- map_dfr(task_files, read_task_data)

# --- Data Cleaning and Preparation ---

# Filter for main trials and valid choices/rewards
analysis_data <- all_task_data %>%
  filter(mode == 'main') %>%
  filter(!is.na(choice) & !is.na(reward)) %>% # Filter out trials with missing choice OR reward
  # Ensure subject IDs are consecutive integers starting from 1
  mutate(subj_idx = as.integer(factor(subject_id))) %>%
  arrange(subj_idx, trial) # IMPORTANT: Ensure data is sorted by subject and trial for correct indexing

# Check final subject count
n_subjects <- max(analysis_data$subj_idx)
nTotalTrials <- nrow(analysis_data)
message(paste("Prepared data for", n_subjects, "subjects with a total of", nTotalTrials, "valid trials."))

if (n_subjects == 0 || nTotalTrials == 0) {
    stop("No subjects or trials remaining after filtering NAs. Cannot proceed.")
}

# --- Prepare Data for Stan Model ---

# Calculate trial lengths and start indices for each subject
subj_summary <- analysis_data %>%
  group_by(subj_idx) %>%
  summarise(n_trials_subj = n(), .groups = 'drop') %>%
  mutate(
    trial_end_idx = cumsum(n_trials_subj),
    trial_start_idx = trial_end_idx - n_trials_subj + 1
  )

# Create Stan data list - names must match the updated basic_rl_model.stan
stan_data <- list(
  nSubjects = n_subjects,
  nTotalTrials = nTotalTrials,
  subj_map = analysis_data$subj_idx,        # Map from total trial index to subject index
  trial_lengths = subj_summary$n_trials_subj, # Number of trials for each subject
  trial_starts = subj_summary$trial_start_idx, # Start index in the long vectors for each subject
  choice = analysis_data$choice + 1,    # Long vector of choices (1 or 2)
  reward = analysis_data$reward       # Long vector of rewards (0.0 or 1.0)
)

# Verify Stan data list components
str(stan_data)
stopifnot(
  stan_data$nSubjects > 0,
  stan_data$nTotalTrials > 0,
  length(stan_data$subj_map) == stan_data$nTotalTrials,
  length(stan_data$trial_lengths) == stan_data$nSubjects,
  length(stan_data$trial_starts) == stan_data$nSubjects,
  length(stan_data$choice) == stan_data$nTotalTrials,
  length(stan_data$reward) == stan_data$nTotalTrials,
  sum(stan_data$trial_lengths) == stan_data$nTotalTrials,
  all(stan_data$choice %in% c(1, 2)),
  all(stan_data$reward %in% c(0.0, 1.0)),
  !anyNA(stan_data$choice),
  !anyNA(stan_data$reward),
  !anyNA(stan_data$subj_map),
  # Check that start/end indices align correctly
  stan_data$trial_starts[1] == 1,
  all(stan_data$trial_starts[-1] == (cumsum(stan_data$trial_lengths[-stan_data$nSubjects]) + 1)),
  all(stan_data$subj_map[stan_data$trial_starts] == 1:stan_data$nSubjects) # Check subj_map aligns with starts
)

message("Stan data list prepared successfully.")

# --- 2. Fit Stan Model ---

message("Compiling and fitting the basic hierarchical Stan model...")

# Define Stan model file path
stan_model_file <- here::here("analysis_scripts", "model_based_learning", "basic_rl_model.stan")

# Check if file exists
if (!fs::file_exists(stan_model_file)) {
  stop("Stan model file not found at: ", stan_model_file)
}

# Fit the model (using the same settings for now)
fit_basic <- stan(
  file = stan_model_file,
  data = stan_data,
  iter = 3000, # Increased iterations
  warmup = 1500, # Increased warmup
  chains = 4,
  cores = getOption("mc.cores", 1),
  seed = 123,
  control = list(adapt_delta = 0.95) # Increased adapt_delta
)

message("Basic Stan model fitting complete.")

# --- 3. Model Diagnostics ---

# Summary statistics (Rhat, n_eff) for key parameters
summary_basic <- summary(fit_basic)$summary
# Parameters of interest: group means alpha_mu, beta_mu, alpha_mu_raw, beta_mu_raw, alpha_sd_raw, beta_sd_raw
filtered_summary <- summary_basic[c("alpha_mu", "beta_mu", "alpha_mu_raw", "beta_mu_raw", "alpha_sd_raw", "beta_sd_raw"), ]
print(filtered_summary, digits = 3)

# Check Rhat values (should be < 1.05 or 1.1)
rhat_values_basic <- summary_basic[, "Rhat"]
message("Max Rhat: ", round(max(rhat_values_basic, na.rm = TRUE), 3))
if (any(rhat_values_basic > 1.1, na.rm = TRUE)) {
  warning("High Rhat values detected! Check convergence.")
  print(rhat_values_basic[rhat_values_basic > 1.1])
}

# Check effective sample size (n_eff) (should be adequate, e.g., > 400 or 1000)
neff_values_basic <- summary_basic[, "n_eff"]
min_neff <- min(neff_values_basic, na.rm = TRUE)
message("Min n_eff: ", round(min_neff))
if (any(neff_values_basic < 400, na.rm = TRUE)) {
  warning(paste("Low n_eff values detected (min =", round(min_neff), "). Model may need more iterations or reparameterization."))
}

# Check for divergences
divergences_basic <- get_num_divergent(fit_basic)
message("Number of divergent transitions: ", divergences_basic)
if (divergences_basic > 0) {
  warning("Divergent transitions detected! Check model parameters, priors, or increase adapt_delta.")
}

# --- 4. Extract and Visualize Results ---

# Extract posterior samples for parameters of interest
posterior_basic <- as.data.frame(fit_basic, pars = c("alpha_mu", "beta_mu"))

# Plot posterior distributions for group-level parameters
mcmc_hist(posterior_basic, pars = c("alpha_mu", "beta_mu")) +
  ggtitle("Posterior Distributions of Group-Level Parameters (Basic Model)")

# Plot trace plots for group-level parameters to check mixing
mcmc_trace(fit_basic, pars = c("alpha_mu", "beta_mu", "alpha_mu_raw", "beta_mu_raw", "alpha_sd_raw", "beta_sd_raw")) +
  ggtitle("Trace Plots for Group-Level Parameters (Basic Model)")

# Update subject parameter extraction to handle potential filtering differences
alpha_subj <- summary(fit_basic, pars = "alpha")$summary[, "mean"]
beta_subj <- summary(fit_basic, pars = "beta")$summary[, "mean"]

# Create a tibble with subject estimates
subject_params <- tibble(
    subj_idx_fit = 1:n_subjects, # Subject index used in the fitted model
    alpha_mean = alpha_subj,
    beta_mean = beta_subj
)

# Map back to original subject IDs (no need for exists("subjects_to_keep"))
original_subject_ids <- analysis_data %>% distinct(subj_idx, subject_id)
subject_params <- subject_params %>%
    left_join(original_subject_ids, by = c("subj_idx_fit" = "subj_idx")) %>%
    select(subject_id, alpha_mean, beta_mean) # Use original ID

print("Subject-level parameter posterior means:")
print(head(subject_params))

# --- 5. Posterior Predictive Checks (using generated quantities) ---

# Extract predicted choices (y_pred) - NOTE: The structure of y_pred in the Stan model
# will need to change to a long vector as well.
y_pred_samples_long <- extract(fit_basic, pars = "y_pred")$y_pred
# y_pred_samples_long should be an array [n_samples, nTotalTrials]

# Example check: Compare average choice probability per subject
# Need to aggregate predictions and observations by subject

# Calculate mean observed choice (1 or 2) for each subject
mean_obs_choice_subj <- analysis_data %>%
  group_by(subj_idx) %>%
  summarise(mean_obs_choice = mean(choice + 1)) %>% # Use choice + 1 (1 or 2)
  pull(mean_obs_choice)

# Calculate mean predicted choice (1 or 2) for each subject across posterior samples
# This requires mapping the long y_pred vector back to subjects
mean_pred_choice_subj <- numeric(n_subjects)
if (length(dim(y_pred_samples_long)) == 2 && dim(y_pred_samples_long)[2] == nTotalTrials) { # Check dimensions match
    for(s in 1:n_subjects) {
        subj_indices <- which(stan_data$subj_map == s)
        # Average across samples (dim 1) and trials for this subject (subset of dim 2)
        mean_pred_choice_subj[s] <- mean(y_pred_samples_long[, subj_indices])
    }
} else {
    warning("Dimensions of y_pred samples (", paste(dim(y_pred_samples_long), collapse="x"), ") do not seem to match [n_samples, nTotalTrials]. Skipping PPC plot.")
    mean_pred_choice_subj <- rep(NA, n_subjects) # Assign NAs if dimensions mismatch
}


# Plot observed vs predicted mean choice per subject
plot_data_ppc <- tibble(
    observed = mean_obs_choice_subj,
    predicted = mean_pred_choice_subj,
    subj_idx_fit = 1:n_subjects
)

# Join with original subject IDs if available
if (exists("subject_params") && "subject_id" %in% names(subject_params)) {
    plot_data_ppc <- plot_data_ppc %>%
        left_join(subject_params %>% select(subj_idx_fit, subject_id), by = "subj_idx_fit")
}


if(!anyNA(plot_data_ppc$predicted)) { # Only plot if predictions were calculated
    print(
        ggplot(plot_data_ppc, aes(x = observed, y = predicted)) +
          geom_point(alpha = 0.7) +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
          labs(
            title = "Posterior Predictive Check: Mean Choice Probability (Long Format)",
            x = "Observed Mean Choice (1 or 2) per Subject",
            y = "Predicted Mean Choice (1 or 2) per Subject (Posterior Mean)"
          ) +
          theme_minimal()
    )
} else {
     message("Skipping PPC plot due to issues with y_pred dimensions or calculation.")
}



message("Analysis script for basic model finished.")
# End of script 