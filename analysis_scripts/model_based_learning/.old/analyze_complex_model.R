# R Script for Analyzing Saliency Task Data
# Uses hierarchical Bayesian RL model (Stan) and frequentist tests

# --- 0. Load Libraries ---
# Ensure these packages are installed: install.packages(c("tidyverse", "rstan", "bayesplot", "effsize", "rstatix", "car", "lme4", "here", "fs", "loo"))
library(tidyverse)
library(rstan)      # For running Stan models
library(bayesplot)  # For plotting Stan results
library(effsize)    # For Cohen's d
library(rstatix)    # For convenient stats tests (e.g., pipeable t-test, anova)
library(car)        # For Levene's test
library(lme4)       # Potentially useful for mixed models, though primary focus is Stan
library(here)       # For easy path management
library(fs)         # For file system operations
library(loo)        # For LOO-CV model comparison

# Set Stan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()) # Use multiple cores

# Set seed for reproducibility
set.seed(123)

# --- 1. Load and Prepare Data ---

# Define data directory path using 'here' package
# Assumes the script is run from the project root or 'analysis_scripts/model_based_learning/'
# Adjust if your project structure differs or if running interactively
data_dir <- here::here("bandit_task", "collected_data") # Original line using here
# Use a relative path assuming the script is run from its location or the project root
# This navigates up two levels from analysis_scripts/model_based_learning/ to the project root
# data_dir <- file.path(here::here(), "bandit_task", "collected_data") # Attempt 1
# If here::here() still fails, try a direct relative path from the script's location:
# data_dir <- "../../bandit_task/collected_data"

# List task data files
task_files <- fs::dir_ls(data_dir, regexp = "_task_data\\.csv$")

# Function to read and preprocess a single task file
read_task_data <- function(file_path) {
  subject_id <- sub("_task_data\\.csv$", "", basename(file_path))
  read_csv(file_path, col_types = cols(
    mode = col_character(),
    trial = col_integer(),
    choice = col_double(), # Read as double initially to handle potential NAs / non-integer entries
    reaction_time = col_double(),
    reward = col_double(), # Read as double for consistency
    condition = col_integer(),
    participant_id = col_character() # Assuming participant_id is in the file
    # Add other columns if needed
  )) %>%
    mutate(
      subject_id = as.character(subject_id), # Ensure consistent type
      # Ensure choice is integer 0 or 1, handle potential NAs/other values if necessary
      choice = if_else(is.na(choice) | !choice %in% c(0, 1), NA_integer_, as.integer(choice)),
      reward = as.integer(reward) # Ensure reward is integer 0 or 1
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
  # Create a consecutive subject index for Stan (1 to J)
  mutate(subj_idx = as.integer(factor(subject_id)))

# Create salient feedback indicator (1 if salient win, 0 otherwise)
# condition: 0 = non-salient, 1 = salient, 2 = missed (already filtered out)
# reward: 0 = no reward, 1 = reward
analysis_data <- analysis_data %>%
  mutate(salient_win = (condition == 1 & reward == 1)) %>%
  mutate(salient_feedback = if_else(salient_win, 1, 0))

# Check data structure
head(analysis_data)
summary(analysis_data)
length(unique(analysis_data$subj_idx)) # Number of subjects

# --- Prepare Data for Stan Model ---

# Get dimensions
N <- nrow(analysis_data)
J <- max(analysis_data$subj_idx)
# Tsubj is not needed for the vectorised Stan model

# Create Stan data list
stan_data <- list(
  N = N,
  N_subj = J, # Renamed from J
  subj_id = analysis_data$subj_idx,
  choice = analysis_data$choice,       # Should be 0 or 1 integer
  outcome = analysis_data$reward,       # Renamed from reward
  salient = analysis_data$salient_feedback # Renamed from salient_feedback
  # Removed T and Tsubj
)

# Verify Stan data list components
str(stan_data)
stopifnot(
  all(!is.na(stan_data$choice)),
  all(stan_data$choice %in% c(0, 1)),
  all(!is.na(stan_data$outcome)), # Check outcome
  all(stan_data$outcome %in% c(0, 1)), # Check outcome
  all(!is.na(stan_data$salient)), # Check salient
  all(stan_data$salient %in% c(0, 1)), # Check salient
  length(stan_data$subj_id) == N,
  length(stan_data$choice) == N,
  length(stan_data$outcome) == N, # Check outcome length
  length(stan_data$salient) == N, # Check salient length
  # Removed check for Tsubj length
  max(stan_data$subj_id) == J
)


# --- 2. Fit Stan Model ---

message("Compiling and fitting Stan model...")

# Compile the Stan model
stan_model_file <- here::here("analysis_scripts", "model_based_learning", "saliency_rl_model.stan")
# Pre-compile if not already done
# compiled_model <- stan_model(stan_model_file)

# Fit the model
# Adjust iterations/warmup/chains as needed for convergence
# Consider using cmdstanr for potentially faster sampling
fit <- stan(
  file = stan_model_file,
  data = stan_data,
  iter = 2000,       # Total iterations per chain
  warmup = 1000,     # Burn-in iterations
  chains = 4,        # Number of chains
  cores = getOption("mc.cores", 1),
  seed = 123,
  control = list(adapt_delta = 0.95) # Increase adapt_delta if divergences occur
)

message("Stan model fitting complete.")

# --- 3. Model Diagnostics ---

# Summary statistics (Rhat, n_eff)
print(summary(fit)$summary, digits = 3)

# Check Rhat values (should be < 1.05 or 1.1)
rhat_values <- summary(fit)$summary[, "Rhat"]
message("Max Rhat: ", max(rhat_values, na.rm = TRUE))
if (any(rhat_values > 1.1, na.rm = TRUE)) {
  warning("High Rhat values detected! Check convergence.")
  print(rhat_values[rhat_values > 1.1])
}

# Check effective sample size (n_eff) (should be adequate, e.g., > 400 or 1000)
neff_values <- summary(fit)$summary[, "n_eff"]
message("Min n_eff: ", min(neff_values, na.rm = TRUE))
if (any(neff_values < 400, na.rm = TRUE)) {
  warning("Low n_eff values detected! Model may need more iterations.")
}

# Check for divergences
divergences <- get_num_divergent(fit)
message("Number of divergent transitions: ", divergences)
if (divergences > 0) {
  warning("Divergent transitions detected! Check model parameters or increase adapt_delta.")
}

# Visual diagnostics
# Trace plots for key group-level parameters
color_scheme_set("viridis")
mcmc_trace(fit, pars = c("mu_pr[1]", "mu_pr[2]", "mu_pr[3]", "sigma[1]", "sigma[2]", "sigma[3]"))

# Posterior distributions for key group-level parameters
mcmc_hist(fit, pars = c("mu_pr[1]", "mu_pr[2]", "mu_pr[3]"))
mcmc_areas(fit, pars = c("mu_pr[1]", "mu_pr[2]", "mu_pr[3]"), prob = 0.95)

# Posterior predictive checks (using generated quantities 'y_pred')
# Compare observed data (y) distribution to distributions of simulated data (y_rep)
y <- stan_data$choice # Observed choices (already 0/1)
y_rep <- as.matrix(fit, pars = "y_pred") # Extract posterior predictive simulations

# Example PPC: Density overlay
ppc_dens_overlay(y, y_rep[1:50, ]) # Use first 50 simulations for clarity

# Example PPC: Bar plot for discrete outcomes (0/1)
ppc_bars(y, y_rep)

# LOO-CV Calculation (requires log_lik in generated quantities)
log_lik_samples <- extract_log_lik(fit, parameter_name = "log_lik", merge_chains = FALSE)
# Consider using recommendations for calculating r_eff if needed
# r_eff_log_lik <- relative_eff(exp(log_lik_samples), chain_id = rep(1:4, each = 1000)) # Example if 4 chains, 1000 post-warmup iter
loo_result <- loo(log_lik_samples)
print(loo_result)
plot(loo_result) # Check for high Pareto k values

# --- 4. Extract and Summarize Results (Hypothesis 1.1: RL Model) ---

# Extract posterior samples
posterior <- as.data.frame(fit)

# Summarize group-level parameters and other key generated quantities
# Note: mu_pr[1] = mu_latent_alpha, mu_pr[2] = mu_latent_alpha_shift, mu_pr[3] = mu_log_beta
summary_fit <- summary(fit, pars = c("mu_pr[1]", "mu_pr[2]", "mu_pr[3]",
                                   "sigma[1]", "sigma[2]", "sigma[3]",
                                   "mu_alpha_base", "mu_alpha_salient_win", "mu_beta"), # Using generated quantities
                      probs = c(0.025, 0.5, 0.975))$summary

print("Summary of key group-level parameters:")
print(summary_fit, digits = 3)

# Hypothesis 1.1: Effect of Salient Feedback on Learning Rate (latent_alpha_shift)
# Examine the posterior distribution of mu_pr[2] (mean of latent_alpha_shift)
# If the 95% CI excludes 0, there is evidence for an effect.
mu_latent_alpha_shift_summary <- summary_fit["mu_pr[2]", ]
message(paste0(
  "Group-level latent_alpha_shift (mu_pr[2]): Mean = ", round(mu_latent_alpha_shift_summary["mean"], 3),
  ", 95% CI = [", round(mu_latent_alpha_shift_summary["2.5%"], 3),
  ", ", round(mu_latent_alpha_shift_summary["97.5%"], 3), "]"
))

# Optional: Calculate posterior probability of mu_pr[2] > 0 (or < 0)
# Need to use backticks because mu_pr[2] is not a standard R name
prob_latent_alpha_shift_positive <- mean(posterior$`mu_pr[2]` > 0)
message(paste0("P(mu_pr[2] > 0 | data) = ", round(prob_latent_alpha_shift_positive, 3)))

# Optional: Visualize subject-level parameters (using interpretable parameters)
# alpha_base, alpha_salient_win, beta
mcmc_intervals(fit, pars = vars(starts_with("alpha_base[")))
mcmc_intervals(fit, pars = vars(starts_with("alpha_salient_win[")))
mcmc_intervals(fit, pars = vars(starts_with("beta[")))


# --- 5. Analyze Win-Stay Probability (Hypothesis 1.2) ---

# Calculate win-stay behavior for each subject
# Need choice[t+1] based on outcome[t] and salient[t]
win_stay_prep <- analysis_data %>% 
  group_by(subject_id) %>% 
  mutate(
    next_choice = lead(choice), 
    next_trial = lead(trial)
  ) %>% 
  ungroup() %>% 
  # Keep only trials where the next trial is the immediately subsequent one
  filter(!is.na(next_choice) & next_trial == (trial + 1)) %>% 
  # Identify if the *current* trial was a win and if it was salient
  # This info determines the condition for the *following* choice (stay/shift)
  mutate(is_win = (outcome == 1),
         is_salient = (salient == 1)) %>%
  # Calculate 'stay' behavior (current choice == previous choice implies previous stay)
  # To get win-stay, we filter for wins on trial t, then check if choice[t+1] == choice[t]
  # Easier: filter for wins (is_win==TRUE), then calculate stay = (next_choice == choice)
  filter(is_win == TRUE) %>% 
  mutate(stay = (next_choice == choice)) 

# Summarize win-stay probability per subject and condition (salient vs non-salient WIN)
win_stay_summary <- win_stay_prep %>% 
  group_by(subject_id, is_salient) %>% 
  summarise(win_stay_prob = mean(stay, na.rm = TRUE), .groups = 'drop') %>% 
  # Pivot wider for paired test
  pivot_wider(names_from = is_salient, 
              values_from = win_stay_prob, 
              names_prefix = "ws_", 
              values_fill = list(win_stay_prob = NA)) %>% 
  # Rename columns for clarity (ws_TRUE -> ws_salient, ws_FALSE -> ws_nonsalient)
  rename(ws_nonsalient = `ws_FALSE`, ws_salient = `ws_TRUE`)

head(win_stay_summary)

# Filter out subjects missing one condition for paired tests
paired_data_ws <- win_stay_summary %>% 
  filter(!is.na(ws_nonsalient) & !is.na(ws_salient))

message(paste("Performing paired win-stay analysis on", nrow(paired_data_ws), "subjects with complete data."))

if (nrow(paired_data_ws) > 1) { # Need at least 2 subjects for paired test
  # Check normality of the DIFFERENCE for paired t-test
  diff_check_ws <- shapiro.test(paired_data_ws$ws_salient - paired_data_ws$ws_nonsalient)
  print(diff_check_ws)

  if (diff_check_ws$p.value > 0.05) {
    message("Normality of win-stay differences assumed (Shapiro-Wilk p > 0.05). Performing paired t-test.")
    # Paired t-test
    t_test_result_ws <- t.test(paired_data_ws$ws_salient, paired_data_ws$ws_nonsalient, paired = TRUE)
    print(t_test_result_ws)

    # Calculate Cohen's d for paired samples
    cohen_d_result_ws <- cohen.d(paired_data_ws$ws_salient, paired_data_ws$ws_nonsalient, paired = TRUE)
    print(cohen_d_result_ws)
  } else {
    message("Normality of win-stay differences rejected (Shapiro-Wilk p <= 0.05). Performing Wilcoxon signed-rank test.")
    # Wilcoxon signed-rank test
    wilcox_test_result_ws <- wilcox.test(paired_data_ws$ws_salient, paired_data_ws$ws_nonsalient, paired = TRUE, conf.int = TRUE)
    print(wilcox_test_result_ws)

    # Calculate effect size for Wilcoxon (e.g., rank biserial correlation using rstatix)
    effect_size_result_ws <- paired_data_ws %>% 
      # Need to pivot longer first for rstatix function
      pivot_longer(cols = c(ws_nonsalient, ws_salient), names_to = "condition", values_to = "prob") %>% 
      wilcox_effsize(prob ~ condition, paired = TRUE)
    print(effect_size_result_ws)
  }
} else {
  warning("Cannot perform paired win-stay test: Not enough subjects with data for both conditions.")
}


# --- 6. Analyze Post-Reinforcement Pauses (PRPs) (Hypotheses 2.1 & 2.2) ---

# PRP is the reaction time on the trial *after* a specific outcome.
# Need RT[t+1] based on outcome[t] and salient[t].
prp_prep <- analysis_data %>% 
  group_by(subject_id) %>% 
  mutate(
    next_rt = lead(reaction_time),
    next_trial = lead(trial)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(next_rt) & next_trial == (trial + 1)) %>% # Ensure consecutive trial
  # Define outcome type based on the *current* trial (t)
  mutate(outcome_type = case_when(
    outcome == 0 ~ "Loss",
    outcome == 1 & salient == 1 ~ "Salient Win",
    outcome == 1 & salient == 0 ~ "Non-Salient Win",
    TRUE ~ NA_character_ # Should not happen if data is clean
  )) %>% 
  filter(!is.na(outcome_type)) %>%
  # Use next_rt as the PRP for the outcome defined above
  rename(prp = next_rt)


# Calculate median PRP per subject and condition
prp_summary <- prp_prep %>% 
  group_by(subject_id, outcome_type) %>% 
  summarise(median_prp = median(prp, na.rm = TRUE), .groups = 'drop')

head(prp_summary)

# Ensure outcome_type is a factor with desired order
prp_summary$outcome_type <- factor(prp_summary$outcome_type, 
                                   levels = c("Loss", "Non-Salient Win", "Salient Win"))

# --- ANOVA / Friedman Test for PRPs ---

# Check assumptions for repeated measures ANOVA
# Filter for subjects who have data in all 3 conditions
prp_complete <- prp_summary %>% 
  group_by(subject_id) %>% 
  filter(n() == 3) %>% 
  ungroup()

message(paste("Performing PRP analysis on", length(unique(prp_complete$subject_id)), "subjects with complete data across 3 conditions."))

if (nrow(prp_complete) >= 3) { # Need at least 3 subjects * 3 conditions
  
  # Check normality using Shapiro-Wilk test within each group for complete cases
  normality_check_prp <- prp_complete %>% 
    group_by(outcome_type) %>% 
    shapiro_test(median_prp)
  print(normality_check_prp)

  # Check homogeneity of variances using Levene's test
  # Note: Levene's test assumes independence, less ideal for repeated measures.
  variance_check_prp <- leveneTest(median_prp ~ outcome_type, data = prp_complete)
  print(variance_check_prp)

  # Check sphericity using Mauchly's test via rstatix::anova_test
  sphericity_test_object <- anova_test(data = prp_complete, dv = median_prp, wid = subject_id, within = outcome_type)
  print(mauchly_test(sphericity_test_object))
  
  # --- Perform ANOVA or Alternatives based on assumptions ---
  all_normal <- all(normality_check_prp$p > 0.05)
  # leveneTest returns a data frame, access p-value with variance_check_prp$`Pr(>F)`[1]
  variances_equal <- variance_check_prp$`Pr(>F)`[1] > 0.05 

  if (all_normal && variances_equal) {
    message("Normality and variance assumptions appear met (with caution on variance test). Proceeding with ANOVA.")
    # Use the anova_test result which already calculated sphericity
    anova_result_prp <- get_anova_table(sphericity_test_object)
    print(anova_result_prp)

    # Post-hoc tests if ANOVA is significant
    if (anova_result_prp$p < 0.05) {
      message("Significant ANOVA result. Performing pairwise t-tests (Bonferroni corrected).")
      pwc_prp <- prp_complete %>% 
        pairwise_t_test(
          median_prp ~ outcome_type,
          paired = TRUE,
          p.adjust.method = "bonferroni"
        )
      print(pwc_prp)
    }
  } else {
    message("Normality or homogeneity of variance assumption violated. Performing non-parametric Friedman test.")
    # Friedman test uses the complete data directly
    friedman_result_prp <- friedman.test(median_prp ~ outcome_type | subject_id, data = prp_complete)
    print(friedman_result_prp)

    # Post-hoc tests if Friedman is significant (e.g., Wilcoxon signed-rank tests with correction)
    if(friedman_result_prp$p.value <= 0.05) {
      message("Significant Friedman test result. Performing pairwise Wilcoxon tests (Bonferroni corrected).")
      pwc_friedman_prp <- prp_complete %>% 
        pairwise_wilcox_test(
          median_prp ~ outcome_type,
          paired = TRUE, 
          p.adjust.method = "bonferroni"
        )
      print(pwc_friedman_prp)
    }
  }
} else {
  warning("PRP analysis requires complete data across 3 conditions for at least 3 subjects.")
}


# --- 7. Generate Plots ---

# Example 1: Posterior distributions of group-level means (using interpretable names)
plot_data_group <- posterior %>% 
  select(`mu_pr[1]`, `mu_pr[2]`, `mu_pr[3]`) %>% 
  rename(mu_latent_alpha = `mu_pr[1]`,
         mu_latent_alpha_shift = `mu_pr[2]`,
         mu_log_beta = `mu_pr[3]`)

mcmc_hist(plot_data_group)
mcmc_areas(plot_data_group, prob = 0.95)

# Example 2: Win-stay probability plot
# Use paired_data_ws which contains only subjects with both conditions
win_stay_plot_data <- paired_data_ws %>% 
  select(subject_id, ws_nonsalient, ws_salient) %>% 
  pivot_longer(cols = c(ws_nonsalient, ws_salient), names_to = "condition", values_to = "win_stay_prob") %>% 
  mutate(condition = factor(condition, levels = c("ws_nonsalient", "ws_salient"), labels = c("Non-Salient Win", "Salient Win")))

ggplot(win_stay_plot_data, aes(x = condition, y = win_stay_prob, fill = condition)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.7) +
  geom_line(aes(group = subject_id), color = "grey", alpha = 0.3) + # Add lines for paired data
  geom_point(aes(color = condition), alpha = 0.5, position = position_jitter(width = 0.1)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  labs(title = "Win-Stay Probability by Feedback Condition",
       x = "Feedback Condition on Preceding Winning Trial (t)",
       y = "Probability of Staying (Choosing Same Arm on t+1)") +
  theme_minimal() +
  theme(legend.position = "none")

# Example 3: PRP comparison plot
# Use prp_complete which contains only subjects with all 3 conditions
prp_plot_data <- prp_complete 

ggplot(prp_plot_data, aes(x = outcome_type, y = median_prp, fill = outcome_type)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.7) +
  geom_line(aes(group = subject_id), color = "grey", alpha = 0.3) + # Add lines for within-subjects data
  geom_point(aes(color = outcome_type), alpha = 0.5, position = position_jitter(width = 0.1)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  labs(title = "Post-Reinforcement Pause by Outcome Type",
       x = "Outcome Type on Preceding Trial (t)",
       y = "Median Post-Outcome Reaction Time on Trial t+1 (s)") +
  theme_minimal() +
  theme(legend.position = "none")

message("Analysis script finished.")

# --- End of Script --- 