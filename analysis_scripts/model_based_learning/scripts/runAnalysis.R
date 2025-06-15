#      Master Analysis Script -- Version 1
# Last edit:    2024/07/25
# Author:       Yannik Poth (YP)
#
# Notes:        - This script performs the full analysis pipeline for the saliency project.
#               - It assumes the working directory is 'analysis_scripts/model_based_learning/'.
#               - It sources 'scripts/prepare_data.R' to load and process data.
#               - It conducts:
#                   1. Model-agnostic behavioral analyses (Win-Stay, GLMMs).
#                   2. Hierarchical Bayesian RL model fitting and comparison.
#                   3. Analysis and visualization of the winning model.
#                   4. Posterior predictive checks.
#                   5. Exploratory analysis of parameter-trait correlations.
#
# To do:        -
#
# Comments:     - Ensure Stan models are located in 'rls/models/'.
# --------------------------------------------------------------------------------------------------

# --- 0. SETUP AND INITIALIZATION ---
# -----------------------------------

# Print working directory for verification
print(paste("Current working directory:", getwd()))

# Clear working memory (optional, uncomment if desired)
# rm(list = ls())

# Load required libraries
print("Loading required libraries...")
pacman::p_load(
  # Core
  tidyverse, rstan, loo, bayesplot,
  # Modeling & Stats
  lme4, effsize,
  # Plotting & Tables
  patchwork, ggpubr, gt,
  # Utilities
  here, janitor
)

# Stan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Source the data preparation script
print("Sourcing data preparation script...")
source("rls/data/prepData.R")

# Define output directories
results_basedir <- "results"
fit_dir <- file.path(results_basedir, "fit_objects")
comp_dir <- file.path(results_basedir, "model_comparison")
plots_dir <- file.path(results_basedir, "plots")
tables_dir <- file.path(results_basedir, "tables")

# Create directories if they don't exist
dir.create(fit_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(comp_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)


# --- 1. DATA LOADING AND PREPARATION ---
# ---------------------------------------

print("Preparing Stan list for models...")
# Set debugsize > 0 for a quick test on a subset of subjects
StanList <- StanCleaned(debugsize = 0, saveStan = FALSE) # Set saveStan to TRUE if you want to save the .RData file

print("Loading and tidying processed data for R analyses...")
# Load the merged data files created by the prep script
task_data_raw <- read_tsv(file.path("data/processed", "all_task_data.tsv"), show_col_types = FALSE)
quest_data_raw <- read_tsv(file.path("data/processed", "all_questionnaire_data.tsv"), show_col_types = FALSE)

# Tidy task data
task_data <- task_data_raw %>%
    filter(mode == "main") %>%
    mutate(
      trial = as.integer(trial),
      choice = as.numeric(choice),
      reaction_time = as.numeric(reaction_time),
      reward = as.numeric(reward),
      condition = as.integer(condition),
      reward_prob_1 = as.numeric(reward_prob_1),
      reward_prob_2 = as.numeric(reward_prob_2),
      choice_fac = factor(choice, levels = c(0, 1), labels = c("Stimulus 1", "Stimulus 2")),
      choice_fac = forcats::fct_explicit_na(choice_fac, na_level = "Missed"),
      reward_fac = factor(reward, levels = c(0, 1), labels = c("Loss", "Win")),
      condition_fac = factor(condition, levels = c(0, 1, 2), labels = c("Non-Salient", "Salient", "Missed"))
    )

# Tidy questionnaire data
quest_data <- quest_data_raw %>%
    janitor::clean_names() %>%
    mutate(
        across(
            c(bis_total, ss_total, sst, sse, ssd, ssb, ss_percent),
            as.numeric
        )
    )

print(paste("Data prepared for", StanList$nSubs, "subjects."))


# --- 2. MODEL-AGNOSTIC BEHAVIORAL ANALYSIS ---
# ---------------------------------------------
print("--- Starting Section 2: Model-Agnostic Analyses ---")

# 2.1. Win-Stay Probability Analysis (Hypothesis 1.2)
print("Analyzing win-stay probability...")

# Calculate win-stay probabilities
wsls_data <- task_data %>%
  filter(choice_fac != "Missed" & !is.na(reward_fac)) %>%
  arrange(subject_id, trial) %>%
  group_by(subject_id) %>%
  mutate(
    prev_choice = lag(choice_fac, 1),
    prev_reward = lag(reward_fac, 1),
    prev_condition = lag(condition_fac, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(prev_choice) & !is.na(prev_reward) & prev_reward == "Win") %>%
  mutate(
    stayed = (choice_fac == prev_choice),
    outcome_type = ifelse(prev_condition == "Salient", "Salient Win", "Non-Salient Win")
  )

win_stay_summary <- wsls_data %>%
  group_by(subject_id, outcome_type) %>%
  summarise(prob_stay = mean(stayed, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = outcome_type, values_from = prob_stay)

# Perform statistical test
shapiro_test <- shapiro.test(win_stay_summary$`Salient Win` - win_stay_summary$`Non-Salient Win`)
if (shapiro_test$p.value < 0.05) {
  test_result <- wilcox.test(win_stay_summary$`Salient Win`, win_stay_summary$`Non-Salient Win`, paired = TRUE)
  test_method <- "Wilcoxon signed-rank test"
} else {
  test_result <- t.test(win_stay_summary$`Salient Win`, win_stay_summary$`Non-Salient Win`, paired = TRUE)
  test_method <- "Paired t-test"
}
cohens_d <- cohen.d(win_stay_summary$`Salient Win`, win_stay_summary$`Non-Salient Win`, paired = TRUE)

# Create and save results table
win_stay_results_table <- tibble(
  Method = test_method,
  Statistic = test_result$statistic,
  p_value = test_result$p.value,
  CI_low = test_result$conf.int[1],
  CI_high = test_result$conf.int[2],
  `Cohen's d` = cohens_d$estimate
) %>%
  gt(rowname_col = "Method") %>%
  tab_header(title = "Win-Stay Probability Comparison") %>%
  fmt_number(columns = everything(), decimals = 3)

gtsave(win_stay_results_table, filename = file.path(tables_dir, "win_stay_test_results.png"))
print(win_stay_results_table)

# Create and save plot
win_stay_long <- win_stay_summary %>%
  pivot_longer(cols = c("Non-Salient Win", "Salient Win"), names_to = "condition", values_to = "prob_stay") %>%
  mutate(condition = factor(condition, levels = c("Non-Salient Win", "Salient Win")))

win_stay_plot <- ggplot(win_stay_long, aes(x = condition, y = prob_stay, fill = condition)) +
    ggdist::stat_halfeye(adjust = .5, width = .6, .width = 0, justification = -.3, point_color = NA) +
    geom_boxplot(width = .15, outlier.shape = NA) +
    geom_line(aes(group = subject_id), color = "grey60", alpha = 0.4) +
    labs(title = "Win-Stay Probability by Feedback Condition",
         subtitle = paste0(test_method, " (p = ", scales::pvalue(test_result$p.value, accuracy=0.001), ")"),
         x = "Outcome on Previous Trial", y = "Probability of Staying") +
    scale_fill_manual(values = c("Non-Salient Win" = "grey70", "Salient Win" = "gold")) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() + 
    theme(legend.position = "none")

ggsave(file.path(plots_dir, "win_stay_probability.png"), plot = win_stay_plot, width = 8, height = 7, bg="white")
print(win_stay_plot)

# 2.2. Generalized Linear Mixed-Effects Models (GLMMs)
print("Fitting GLMM for choice optimality...")

glmm_data <- task_data %>%
  arrange(subject_id, trial) %>%
  group_by(subject_id) %>%
  mutate(
    # Lagged variables
    prev_outcome = case_when(
      lag(reward_fac) == "Loss" ~ "Loss",
      lag(reward_fac) == "Win" & lag(condition_fac) == "Non-Salient" ~ "Non-Salient Win",
      lag(reward_fac) == "Win" & lag(condition_fac) == "Salient" ~ "Salient Win",
      TRUE ~ NA_character_
    ),
    # Dependent variables
    optimal_choice = ifelse(reward_prob_1 > reward_prob_2, 0, 1) == choice
  ) %>%
  ungroup() %>%
  filter(!is.na(prev_outcome)) %>%
  # Fix: Scale the trial variable for numerical stability
  mutate(trial_scaled = as.numeric(scale(trial)))


# Model 1: Choice Optimality
glmm_optimality <- glmer(optimal_choice ~ prev_outcome + trial_scaled + (1 | subject_id),
                         data = glmm_data, family = binomial,
                         control = glmerControl(optimizer = "bobyqa"))

# Create and save results tables
broom.mixed::tidy(glmm_optimality, conf.int = TRUE, exponentiate = TRUE) %>%
    gt() %>%
    tab_header("GLMM Results: Predicting Choice Optimality") %>%
    gtsave(file.path(tables_dir, "glmm_optimality_results.png"))


# --- 3. HIERARCHICAL BAYESIAN MODELING ---
# -----------------------------------------
print("--- Starting Section 3: Hierarchical Bayesian Modeling ---")

# Stan execution parameters
nChains <- 4
nIters <- 4000
nWarmup <- 2000

# Define models to run based on the method section
# Assuming stan files are named accordingly, e.g., M1_Baseline.stan
modelist <- c("M1_Baseline", "M2_Salience_Shift", "M3_Salience_Shift_Perseveration")
model_files <- paste0("rls/models/", modelist, ".stan")
names(model_files) <- modelist

# Check if model files exist
model_files_exist <- sapply(model_files, file.exists)
if(!all(model_files_exist)) {
    stop("Missing Stan model files: ", paste(model_files[!model_files_exist], collapse=", "))
}

# Initialize lists
model_fits_list <- list()
loo_objects_list <- list()

for (model_name in modelist) {
  print(paste("Now running:", model_name))
  modelfile_name <- model_files[model_name]

  model_fit <- stan(
    file = modelfile_name,
    data = StanList,
    chains = nChains, iter = nIters, warmup = nWarmup,
    control = list(adapt_delta = 0.98),
    seed = 123
  )

  # Store fit object
  model_fits_list[[model_name]] <- model_fit
  saveRDS(model_fit, file.path(fit_dir, paste0(model_name, "_fit.rds")))

  # Calculate and store LOO
  log_like <- extract_log_lik(model_fit, parameter_name = "log_lik")
  loo_obj <- loo(log_like)
  loo_objects_list[[model_name]] <- loo_obj
}

# Model comparison
print("Comparing models...")
model_comparison <- loo_compare(loo_objects_list)
print(model_comparison)

# Save comparison table
as.data.frame(model_comparison) %>%
    rownames_to_column("model") %>%
    gt() %>%
    tab_header("Model Comparison Results (LOOIC)") %>%
    gtsave(file.path(comp_dir, "model_comparison_looic.png"))

winning_model_name <- rownames(model_comparison)[1]
print(paste("Winning model:", winning_model_name))


# --- 4. WINNING MODEL ANALYSIS ---
# ---------------------------------
print("--- Starting Section 4: Winning Model Analysis ---")

winning_fit <- model_fits_list[[winning_model_name]]

# 4.1. Parameter Summary Table
# Define parameters based on the winning model
base_params <- c("mu_alpha", "mu_beta")
if (grepl("Salience_Shift", winning_model_name)) {
    base_params <- c(base_params, "mu_alpha_shift")
}
if (grepl("Perseveration", winning_model_name)) {
    base_params <- c(base_params, "mu_kappa")
}

param_summary <- summary(winning_fit, pars = base_params, probs = c(0.025, 0.975))$summary

as.data.frame(param_summary) %>%
    rownames_to_column("Parameter") %>%
    gt() %>%
    tab_header(paste("Posterior Summary: Group-Level Parameters for", winning_model_name)) %>%
    fmt_number(columns = where(is.numeric), decimals = 3) %>%
    gtsave(file.path(tables_dir, "winning_model_group_params.png"))

# 4.2. Parameter Plots
group_params_plot <- plot(winning_fit, pars = base_params) +
  labs(title = "Group-Level Parameter Posterior Distributions")
ggsave(file.path(plots_dir, "winning_model_group_params.png"), plot = group_params_plot, width = 8, height = 6, bg="white")

# Individual parameters raincloud plot
indiv_param_names <- sub("mu_", "", base_params) # "mu_alpha" -> "alpha"
indiv_params <- rstan::extract(winning_fit, pars = indiv_param_names)
indiv_params_df <- as.data.frame(indiv_params) %>%
    pivot_longer(everything(), names_to = "parameter", values_to = "value")

indiv_params_plot <- ggplot(indiv_params_df, aes(x = parameter, y = value, fill = parameter)) +
  ggdist::stat_halfeye(adjust = .5, width = .6, .width = 0, justification = -.3, point_color = NA) +
  geom_boxplot(width = .15, outlier.shape = NA) +
  geom_point(aes(color=parameter), alpha=0.1, position=position_jitter(width=0.05,seed=1)) +
  labs(title = "Individual-Level Parameter Estimates", y = "Parameter Value", x = "") +
  theme_minimal() + theme(legend.position="none")
ggsave(file.path(plots_dir, "winning_model_indiv_params.png"), plot = indiv_params_plot, width = 10, height = 6, bg="white")


# --- 5. POSTERIOR PREDICTIVE CHECKS ---
# --------------------------------------
print("--- Starting Section 5: Posterior Predictive Checks ---")

y <- StanList$choice
yrep_choices <- rstan::extract(winning_fit, pars = "predicted_choices")[["predicted_choices"]] # Dims: [iter, nSubs, T_trials]

# PPC for overall choice distribution
ppc_choices_plot <- ppc_bars(
    y = as.vector(y[y != -9]), 
    yrep = as.vector(yrep_choices[1:50, , ][yrep_choices[1:50, , ] != -9]), 
    freq = FALSE
) + labs(title="PPC: Distribution of Choices")
ggsave(file.path(plots_dir, "ppc_choices.png"), plot = ppc_choices_plot, width = 8, height = 6, bg="white")

# PPC for Win-Stay Probability
print("Performing PPC for win-stay probability...")

# Function to calculate win-stay props for a single simulation
calculate_win_stay_for_sim <- function(choices_mat, stan_list) {
  sim_df <- tibble(
    subject_id = rep(1:stan_list$nSubs, each = stan_list$maxTrials),
    trial = rep(1:stan_list$maxTrials, times = stan_list$nSubs),
    choice = as.vector(choices_mat),
    reward = as.vector(stan_list$reward),
    salience = as.vector(stan_list$salient_feedback)
  ) %>%
    filter(choice != -9) %>%
    group_by(subject_id) %>%
    arrange(trial) %>%
    mutate(
      prev_choice = lag(choice),
      prev_reward = lag(reward),
      prev_salience = lag(salience)
    ) %>%
    ungroup() %>%
    filter(!is.na(prev_reward) & prev_reward == 1) %>%
    mutate(
      stayed = (choice == prev_choice),
      outcome_type = ifelse(prev_salience == 1, "Salient Win", "Non-Salient Win")
    )
  
  sim_summary <- sim_df %>%
    group_by(outcome_type) %>%
    summarise(mean_prob = mean(stayed, na.rm = TRUE), .groups = "drop")
  
  # Ensure both conditions are present
  tidyr::crossing(outcome_type = c("Non-Salient Win", "Salient Win")) %>%
      left_join(sim_summary, by="outcome_type") %>%
      pull(mean_prob)
}

# Calculate for a subset of simulations to save time
num_ppc_sims <- 100
yrep_win_stay <- t(sapply(1:num_ppc_sims, function(i) {
  calculate_win_stay_for_sim(choices_mat = yrep_choices[i, , ], stan_list = StanList)
}))
colnames(yrep_win_stay) <- c("Non-Salient Win", "Salient Win")

# Observed data
y_obs_win_stay <- win_stay_long %>%
    group_by(condition) %>%
    summarise(mean_prob = mean(prob_stay, na.rm=TRUE))

# Plot
ppc_win_stay_plot <- ppc_stat_grouped(
  y = y_obs_win_stay$mean_prob,
  yrep = yrep_win_stay,
  group = y_obs_win_stay$condition
) + labs(title = "PPC: Mean Win-Stay Probability", x="Mean Probability") +
  theme(legend.position="bottom")

ggsave(file.path(plots_dir, "ppc_win_stay.png"), plot = ppc_win_stay_plot, width = 8, height = 6, bg="white")

# PPC for Learning Curve (Proportion of Optimal Choices)
print("Performing PPC for learning curve...")

# --- 1. Determine optimal choice for each trial ---
optimal_choices_per_trial <- task_data %>%
  filter(trial <= StanList$maxTrials) %>% # Ensure we only use trials that are in the model
  distinct(trial, reward_prob_1, reward_prob_2) %>%
  arrange(trial) %>%
  mutate(optimal_choice = ifelse(reward_prob_1 > reward_prob_2, 1, 2)) %>% # 1 for stim1, 2 for stim2
  pull(optimal_choice)

# --- 2. Calculate observed learning curve ---
T_trials <- StanList$maxTrials
block_size <- 20
num_blocks <- T_trials / block_size

# Get actual choices (1 or 2, -9 for missing)
y_actual_choices <- StanList$choice 

prop_correct_per_subject_block <- matrix(NA_real_, nrow = StanList$nSubs, ncol = num_blocks)

for (subi in 1:StanList$nSubs) {
  for (block_idx in 1:num_blocks) {
    start_trial <- (block_idx - 1) * block_size + 1
    end_trial <- block_idx * block_size
    
    subject_choices_block <- y_actual_choices[subi, start_trial:end_trial]
    optimal_choices_block <- optimal_choices_per_trial[start_trial:end_trial]
    
    valid_trials_mask <- subject_choices_block != -9
    if (sum(valid_trials_mask) > 0) {
      correct_choices <- sum(subject_choices_block[valid_trials_mask] == optimal_choices_block[valid_trials_mask])
      prop_correct_per_subject_block[subi, block_idx] <- correct_choices / sum(valid_trials_mask)
    }
  }
}
y_obs_learning_curve <- colMeans(prop_correct_per_subject_block, na.rm = TRUE)

# --- 3. Calculate simulated learning curves ---
yrep_learning_curve_matrix <- matrix(NA_real_, nrow = num_ppc_sims, ncol = num_blocks)

for (iter in 1:num_ppc_sims) {
  current_iter_preds <- yrep_choices[iter, , ]
  prop_correct_per_subject_block_iter <- matrix(NA_real_, nrow = StanList$nSubs, ncol = num_blocks)
  
  for (subi in 1:StanList$nSubs) {
    for (block_idx in 1:num_blocks) {
      start_trial <- (block_idx - 1) * block_size + 1
      end_trial <- block_idx * block_size
      
      subject_preds_block <- current_iter_preds[subi, start_trial:end_trial]
      optimal_choices_block <- optimal_choices_per_trial[start_trial:end_trial]
      
      valid_preds_mask <- subject_preds_block != -9
      if (sum(valid_preds_mask) > 0) {
        correct_preds <- sum(subject_preds_block[valid_preds_mask] == optimal_choices_block[valid_preds_mask])
        prop_correct_per_subject_block_iter[subi, block_idx] <- correct_preds / sum(valid_preds_mask)
      }
    }
  }
  yrep_learning_curve_matrix[iter, ] <- colMeans(prop_correct_per_subject_block_iter, na.rm = TRUE)
}

# --- 4. Plot learning curve PPC ---
ppc_learning_curve_plot <- bayesplot::ppc_ribbon(
  y = y_obs_learning_curve,
  yrep = yrep_learning_curve_matrix,
  x = 1:num_blocks,
  prob = 0.5, # Median line
  prob_outer = 0.9 # 90% CI
) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title = paste("PPC: Learning Curve - Model:", winning_model_name),
    subtitle = "Proportion of optimal choices across trial blocks.",
    x = "Trial Block",
    y = "Proportion Optimal"
  ) +
  theme_minimal()

ggsave(file.path(plots_dir, "ppc_learning_curve.png"), plot = ppc_learning_curve_plot, width = 8, height = 6, bg="white")


# --- 6. EXPLORATORY TRAIT-BEHAVIOR ANALYSIS ---
# ----------------------------------------------
print("--- Starting Section 6: Exploratory Analysis ---")

# Define parameters to extract based on winning model
indiv_param_names_corr <- c("alpha", "beta")
if (grepl("Salience_Shift", winning_model_name)) {
    indiv_param_names_corr <- c(indiv_param_names_corr, "alpha_shift")
}
if (grepl("Perseveration", winning_model_name)) {
    indiv_param_names_corr <- c(indiv_param_names_corr, "kappa")
}

# Extract individual parameter means
indiv_param_means <- as.data.frame(summary(winning_fit, pars = indiv_param_names_corr)$summary) %>%
    rownames_to_column("param_id") %>%
    mutate(
        subject_idx = as.numeric(str_extract(param_id, "(?<=\\[)[0-9]+(?=\\])")),
        parameter = str_extract(param_id, "^[a-z_]+")
    ) %>%
    select(subject_idx, parameter, mean) %>%
    pivot_wider(names_from = parameter, values_from = mean)

# Get subject IDs in the correct order
ordered_subs <- tibble(
    subject_idx = 1:StanList$nSubs,
    subject_id = unique(task_data$subject_id)[1:StanList$nSubs]
)

# Merge with parameter estimates
indiv_param_means <- left_join(indiv_param_means, ordered_subs, by = "subject_idx")

# Merge with questionnaire data
corr_data <- inner_join(indiv_param_means, quest_data, by = "subject_id")

# Create and save correlation plot
corr_plot_params <- c(indiv_param_names_corr, "bis_total", "ss_total")

corr_plot <- corr_data %>%
  select(all_of(corr_plot_params)) %>%
  GGally::ggpairs(title = "Correlations between Parameters and Traits")
ggsave(file.path(plots_dir, "param_trait_correlations.png"), plot = corr_plot, width = 12, height = 12, bg="white")

print("--- Analysis Complete ---") 