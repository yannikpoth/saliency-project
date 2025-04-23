# Model-Based Learning Analysis

This directory contains the scripts necessary for analyzing the reinforcement learning (RL) aspects of the saliency study, focusing on how salient feedback modulates learning rates and choice behavior.

## Overview

The primary goal of this analysis is to quantify learning dynamics using a hierarchical Bayesian RL model and to test specific hypotheses regarding the influence of salient reward cues on behavior (win-stay probability and post-reinforcement pauses). The analysis uses R for data preparation, model execution, and statistical testing, and Stan for defining and fitting the Bayesian model.

## Files

1.  **`saliency_rl_model.stan`**: Defines the hierarchical Bayesian Q-learning model used to estimate learning parameters.
2.  **`analyze_saliency_data.R`**: The main R script that performs the following steps:
    *   Loads and preprocesses the behavioral data from the bandit task.
    *   Prepares the data for the Stan model.
    *   Runs the Stan model using `rstan`.
    *   Performs diagnostic checks on the model fit (Rhat, n_eff, divergences, PPCs, LOO).
    *   Extracts and summarizes the results from the Bayesian model (Hypothesis 1.1).
    *   Calculates and statistically analyzes win-stay probabilities (Hypothesis 1.2).
    *   Calculates and statistically analyzes post-reinforcement pauses (PRPs) (Hypotheses 2.1 & 2.2).
    *   Generates visualizations.

## `saliency_rl_model.stan` - The Bayesian RL Model

This file specifies the computational model assumed to underlie participant choices.

*   **Purpose**: To estimate subject-specific learning parameters while accounting for the potential modulatory effect of salient feedback *on winning trials*.
*   **Model Type**: A standard Q-learning model is implemented. It assumes agents learn the expected value (Q-value) of each choice option (initialized at 0.5) based on reward prediction errors. Choices are made probabilistically based on these Q-values via a softmax decision rule (implemented using `bernoulli_logit`).
*   **Key Features**:
    *   **Hierarchical Structure**: The model estimates parameters for each participant (`latent_alpha`, `latent_alpha_shift`, `beta`). These subject-level parameters are drawn from group-level distributions defined by means (`mu_pr`) and standard deviations (`sigma`), along with a correlation structure (`L_Omega`).
        *   *Rationale*: This approach allows for individual differences while leveraging data across participants for more stable estimates (partial pooling).
    *   **Parameters (Group-Level)**:
        *   `mu_pr[1]`: Group mean for base latent learning rate (`latent_alpha`).
        *   `mu_pr[2]`: Group mean for latent learning rate shift (`latent_alpha_shift`).
        *   `mu_pr[3]`: Group mean for log inverse temperature (`log_beta`).
        *   `sigma`: Vector of group standard deviations for the three parameters.
        *   `L_Omega`: Cholesky factor of the correlation matrix for the three parameters.
    *   **Parameters (Subject-Level)**:
        *   `latent_alpha`: Subject's base latent learning rate (unbounded).
        *   `latent_alpha_shift`: Subject's latent learning rate shift due to *salient wins* (unbounded).
        *   `beta`: Subject's inverse temperature (constrained positive via `exp(log_beta)`). Controls choice stochasticity.
    *   **Learning Rate Calculation**:
        *   For each trial, a `current_latent_lr` is determined: `latent_alpha[s]`.
        *   If the trial was a *win* (`outcome[n] == 1`) and had *salient feedback* (`salient[n] == 1`), then `latent_alpha_shift[s]` is added: `current_latent_lr += latent_alpha_shift[s]`.
        *   The `effective_alpha` (bounded 0-1) used in the Q-update rule is obtained by transforming `current_latent_lr` using the inverse probit function (`Phi_approx`).
        *   *Rationale*: The shift is applied only under specific conditions (salient wins), and the transformation ensures the learning rate stays within valid bounds.
    *   **Priors**:
        *   Group means (`mu_pr`): `normal(0, 1)` (slightly regularizing). `mu_pr[3]` (for `log_beta`) has a prior centered near `log(1)`.
        *   Group SDs (`sigma`): `normal(0, 1)` (implicitly truncated at 0).
        *   Correlation structure (`L_Omega`): `lkj_corr_cholesky(2)` (pushes slightly away from perfect correlations).
        *   Subject standardized deviations (`z`): `std_normal()`.
        *   *Rationale*: Uses weakly informative priors to regularize gently without dominating the data.
    *   **Parameterization**: A non-centered parameterization is used for subject-level effects (`z`).
        *   *Rationale*: Often improves sampling efficiency in Stan for hierarchical models.
    *   **Generated Quantities**: Calculates interpretable subject-level parameters (`alpha_base`, `alpha_salient_win` on the 0-1 scale), interpretable group-level parameters (`mu_alpha_base`, `mu_alpha_salient_win`, `mu_beta`), trial-level log-likelihood (`log_lik`) for model comparison (LOOIC), and posterior predictive choices (`y_pred`) for model checking.
        *   *Rationale*: Provides parameters on the scale of interest and enables model diagnostics and comparison.

## `analyze_saliency_data.R` - The Analysis Workflow

This script drives the analysis, combining data processing, model fitting, and statistical testing.

*   **Purpose**: To execute the full analysis pipeline described in the research plan, from raw data to interpretable results and figures.
*   **Steps**:
    1.  **Load Libraries**: Loads necessary packages: `tidyverse`, `rstan`, `bayesplot`, `effsize`, `rstatix`, `car`, `lme4`, `here`, `fs`, `loo`.
    2.  **Load Data**: Uses `here::here` and `fs::dir_ls` to find and load all `_task_data.csv` files from `bandit_task/collected_data/`.
    3.  **Data Preprocessing**:
        *   Filters for `mode == 'main'` and non-missing `choice`.
        *   Creates numeric `subj_idx` (1 to N_subj).
        *   Creates `salient_feedback` indicator (1 if `condition == 1` AND `reward == 1`, else 0). This maps to the `salient` variable in the Stan model.
    4.  **Stan Data Preparation**: Collates required data (`N`, `N_subj`, `subj_id`, `choice`, `outcome` [renamed from `reward`], `salient` [renamed from `salient_feedback`]) into the `stan_data` list. Includes `stopifnot` checks for data integrity.
    5.  **Stan Model Fitting**: Calls `rstan::stan` to run the MCMC sampler for `saliency_rl_model.stan`. Specifies iterations, warmup, chains, cores, seed, and `adapt_delta`.
    6.  **Model Diagnostics**: Performs essential checks:
        *   **Convergence**: Checks `Rhat` (< 1.1) and `n_eff` (> 400 or 1000).
        *   **Sampling Issues**: Checks for `divergences`.
        *   **Visual Checks**: Uses `bayesplot` (`mcmc_trace`, `mcmc_hist`, `mcmc_areas`) for parameter diagnostics.
        *   **Posterior Predictive Checks (PPCs)**: Uses `bayesplot::ppc_dens_overlay` and `ppc_bars` comparing observed choices (`y`) to simulated choices (`y_pred`).
        *   **Model Comparison**: Calculates LOOIC using `loo::loo()` based on `log_lik` from generated quantities and checks Pareto k values.
        *   *Rationale*: Ensures the reliability of MCMC samples and assesses model fit.
    7.  **Summarize RL Model Results (Hypothesis 1.1)**: Extracts posterior samples. Calculates summary statistics (mean, 95% CI) for group-level parameters, focusing on `mu_pr[2]` (mean latent alpha shift). Hypothesis 1.1 is assessed by checking if the 95% CI for `mu_pr[2]` excludes zero. Also reports interpretable group parameters (`mu_alpha_base`, `mu_alpha_salient_win`, `mu_beta`). Calculates posterior probability `P(mu_pr[2] > 0 | data)`.
    8.  **Win-Stay Analysis (Hypothesis 1.2)**:
        *   Calculates win-stay probability per subject after **salient wins** vs. **non-salient wins**.
        *   Checks normality of the difference using Shapiro-Wilk test.
        *   Performs paired t-test (if normal) or Wilcoxon signed-rank test (if not normal).
        *   Reports p-value, effect size (Cohen's d for t-test), and CIs.
        *   *Rationale*: Directly tests if repeating a choice after a win is different depending on feedback salience.
    9.  **PRP Analysis (Hypotheses 2.1 & 2.2)**:
        *   Calculates the **median** Post-Reinforcement Pause (PRP, reaction time on trial *t+1*) per subject following three outcome types on trial *t*: Loss, Non-Salient Win, Salient Win.
        *   Performs a repeated-measures ANOVA (`rstatix::anova_test`) comparing median PRPs across the three outcome types.
        *   Checks assumptions: normality (Shapiro-Wilk on residuals) and sphericity (Mauchly's test). Applies Greenhouse-Geisser correction if sphericity is violated. Reports generalized eta-squared (ges).
        *   If ANOVA is significant, performs pairwise post-hoc tests (paired t-tests with Bonferroni correction).
        *   Includes notes on non-parametric alternatives (Kruskal-Wallis, Dunn's test).
        *   *Rationale*: Investigates how different reinforcement outcomes influence subsequent response latency using appropriate repeated-measures techniques.
    10. **Visualization**: Uses `ggplot2` for win-stay and PRP plots, and `bayesplot` for model diagnostics and parameter visualization.
    11. **Individual Differences (Optional)**: Placeholder structure for loading questionnaire data and correlating with subject-level Stan parameters.

## How to Run

1.  **Prerequisites**: Ensure R, the RStan toolchain, and the R packages listed in `analyze_saliency_data.R` (`tidyverse`, `rstan`, `bayesplot`, etc.) are installed.
2.  **Data Location**: Ensure raw task data CSVs are in `bandit_task/collected_data/` relative to the project root.
3.  **Execution**: Open the R project. Run `analyze_saliency_data.R`. It uses `here` for path management and should run correctly from the project root or the script's directory.
4.  **Output**: Prints summaries, diagnostics, test results, and saves plots (if saving implemented). The Stan fit object (`fit`) is available in the R environment.

## Rationale Summary

This analysis employs a hierarchical Bayesian RL model to estimate latent learning processes, specifically how salient feedback *after wins* influences the learning rate (Hypothesis 1.1 via `latent_alpha_shift`). This provides a mechanistic account alongside uncertainty quantification. Standard frequentist tests (paired t-test/Wilcoxon, rm ANOVA) are then used to directly test specific behavioral predictions regarding win-stay probability (H1.2) and post-reinforcement pauses (H2.1 & H2.2) as outlined in the analysis plan. This combination leverages the strengths of both modeling and targeted statistical testing. 