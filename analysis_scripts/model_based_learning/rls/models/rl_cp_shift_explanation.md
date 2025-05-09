# Explanation of the `rl_cp_shift.stan` Model

**Last Updated**: 2025/05/09

This document provides a conceptual overview of the hierarchical Bayesian reinforcement learning model implemented in `rl_cp_shift.stan`. The model is designed to investigate how salient audiovisual feedback modulates learning rates in a restless two-armed bandit task. It estimates parameters such as base learning rate (`alpha`), a shift in learning rate due to salient cues (`alpha_shift`), and decision consistency (`beta`).

## Model Structure Overview

The Stan model is divided into several main blocks:

1.  **`data`**: Defines the input data required by the model.
2.  **`parameters`**: Declares the parameters the model will estimate. These are "raw" parameters, often on an unconstrained scale.
3.  **`transformed parameters`**: Derives new parameters from the "raw" parameters, often by transforming them to a constrained or more interpretable scale for use within the model's likelihood.
4.  **`model`**: Specifies the prior distributions for the parameters and the likelihood function, which defines how the observed data are generated given the parameters (i.e., the core Rescorla-Wagner learning and softmax choice rule).
5.  **`generated quantities`**: Calculates additional quantities of interest from the posterior samples of the parameters. This includes interpretable versions of parameters, log-likelihoods for model comparison, and data for posterior predictive checks.

---

### 1. Data Input (`data` block)

This block declares the data that Stan expects from the R script. It includes:
*   Counts: Number of subjects (`nSubs`), maximum trials across all subjects (`maxTrials`).
*   Per-subject information: Actual number of valid trials for each subject (`subTrials`).
*   Trial-level data (as matrices, with subjects as rows and trials as columns):
    *   `reward`: Whether a reward was received (0 or 1).
    *   `choice`: Which option was chosen (1 or 2).
    *   `salient_feedback`: Whether the feedback for the trial was salient (1) or non-salient (0).
*   Missing trials or padded data within these matrices are typically denoted by -9.

---

### 2. Parameter Definition (`parameters` block)

This block defines the parameters the model will estimate. These are "raw" parameters, meaning they are often on an unbounded scale suitable for Stan's samplers. The model uses a hierarchical structure (Centered Parameterization - CP).

*   **Group-Level Raw Parameters (Means)**:
    *   `alpha_mu_raw`: The group-level mean for the base learning rate (before transformation).
    *   `alpha_shift_mu_raw`: The group-level mean for the raw shift in learning rate due to salient feedback.
    *   `beta_mu_raw`: The group-level mean for the inverse temperature (decision consistency parameter, before transformation).
*   **Group-Level Raw Parameters (Standard Deviations)**:
    *   `alpha_sd_raw`, `alpha_shift_sd_raw`, `beta_sd_raw`: These are the standard deviations for the distributions of subject-level parameters around their respective group means. They are constrained to be positive.
*   **Subject-Level Raw Parameters**:
    *   `alpha_subj_raw`: A vector containing each subject's specific base learning rate (raw value).
    *   `alpha_shift_subj_raw`: A vector containing each subject's specific raw shift in learning rate.
    *   `beta_subj_raw`: A vector containing each subject's specific inverse temperature (raw value).

---

### 3. Initial Parameter Transformation (`transformed parameters` block)

This block performs initial transformations on some raw parameters, primarily to bring them to a scale that is directly usable in the model or more interpretable.

*   **Subject-Level Transformed Beta (`beta_subj_transformed`)**:
    1.  Each subject's `beta_subj_raw` is passed through the `Phi()` function (the standard normal cumulative distribution function, `inv_Phi()` in Stan). This maps the unbounded raw beta value to the range (0, 1).
    2.  The result is then multiplied by 10.0, scaling the subject's beta to the range (0, 10). A higher beta indicates more deterministic (less random) choices.
*   **Note on Alpha and Alpha Shift**: The raw `alpha_subj_raw` and `alpha_shift_subj_raw` parameters are not transformed in this block. Their combination and transformation occur directly within the `model` block on a trial-by-trial basis.

---

### 4. Model Specification (`model` block)

This is where the statistical model is fully defined, including priors for the parameters and the likelihood of the data.

*   **Priors**:
    *   **Group-Level Means (`_mu_raw`)**: These are given weakly informative `normal(0, 1)` priors.
    *   **Group-Level Standard Deviations (`_sd_raw`)**: These are given weakly informative `normal(0, 1) T[0, ]` priors (half-normal distributions, constrained to be positive).
    *   **Subject-Level Raw Parameters (`_subj_raw`)**: These are modeled hierarchically. Each subject's raw parameter (e.g., `alpha_subj_raw`) is drawn from a normal distribution centered at the corresponding group-level mean (e.g., `alpha_mu_raw`) with the corresponding group-level standard deviation (e.g., `alpha_sd_raw`). This is the "Centered Parameterization" approach.

*   **Likelihood Calculation (Core of the Reinforcement Learning Model)**:
    The model iterates through each subject and then through each of that subject's valid trials (ignoring trials with missing data).

    *   **For each subject**:
        *   The subject's transformed `beta_subj_transformed` is used as `current_beta_subj`.
        *   Q-values (expected values for each of the two options) are initialized, typically to 0.5 (representing no initial preference).

    *   **For each trial**:
        1.  **Determine Effective Learning Rate**:
            *   An `effective_raw_alpha` is calculated:
                *   If `salient_feedback` for the trial is 1 (salient): `effective_raw_alpha = alpha_subj_raw[subi] + alpha_shift_subj_raw[subi]`.
                *   If `salient_feedback` is 0 (non-salient): `effective_raw_alpha = alpha_subj_raw[subi]`.
            *   This `effective_raw_alpha` is then transformed using the `Phi()` function to get `trial_alpha_transformed`. This maps the (potentially shifted) raw alpha to a learning rate on the (0, 1) scale for the current trial.

        2.  **Policy (Softmax Choice Rule)**:
            *   The probability of the subject making their `choice[subi, triali]` is determined using a `categorical_logit` distribution.
            *   The inputs to this (the logits) are the current Q-values (`qval`) for the two options, each multiplied by the subject's `current_beta_subj`. A higher beta makes the choice rule more sensitive to differences in Q-values (more deterministic), while a lower beta leads to more random choices.

        3.  **Learning (Rescorla-Wagner Update)**:
            *   A Prediction Error (`pe`) is calculated: `pe = current_reward_val - qval[chosen_option_index]`. This is the difference between the actual reward received and the expected reward (Q-value) for the chosen option.
            *   The Q-value of the chosen option is updated: `qval[chosen_option_index] = qval[chosen_option_index] + trial_alpha_transformed * pe`. The Q-value is adjusted based on the prediction error, scaled by the `trial_alpha_transformed` (the learning rate for that trial).

---

### 5. Post-Hoc Calculations (`generated quantities` block)

After the model has been fitted (posterior distributions for parameters are obtained), this block is used to compute various quantities of interest based on these posterior samples.

*   **Transformed Group-Level Interpretable Parameters**:
    *   `alpha_mu`: The group-level mean base learning rate, transformed to the (0, 1) scale: `Phi(alpha_mu_raw)`.
    *   `beta_mu`: The group-level mean inverse temperature, transformed to the (0, 10) scale: `Phi(beta_mu_raw) * 10.0`.
    *   `alpha_shift_mu_raw_gq`: This is simply the raw group-level mean shift (`alpha_shift_mu_raw`), kept for reference.
    *   **`alpha_learning_rate_salient_mu`**: The group mean learning rate (on the 0-1 scale) when salient feedback is present. Calculated as `Phi(alpha_mu_raw + alpha_shift_mu_raw_gq)`.
    *   **`interpretable_alpha_shift_mu`**: The *actual change* in the group mean learning rate (on the 0-1 scale) due to salient feedback. Calculated as `alpha_learning_rate_salient_mu - alpha_mu`. This is a key output for understanding the average effect of salience.

*   **Transformed/Raw Subject-Level Interpretable Parameters**:
    *   `alpha[subi]`: Each subject's base learning rate, transformed to the (0, 1) scale: `Phi(alpha_subj_raw[subi])`. This is the learning rate for non-salient trials.
    *   `alpha_shift_subj_raw_gq[subi]`: Each subject's raw shift parameter (`alpha_shift_subj_raw[subi]`), kept for reference.
    *   `beta[subi]`: Each subject's transformed inverse temperature (0-10 scale), same as `beta_subj_transformed[subi]`.
    *   **`alpha_learning_rate_salient_subj[subi]`**: Each subject's learning rate (on the 0-1 scale) when salient feedback is present. Calculated as `Phi(alpha_subj_raw[subi] + alpha_shift_subj_raw_gq[subi])`.
    *   **`interpretable_alpha_shift_subj[subi]`**: Each subject's *actual change* in their learning rate (on the 0-1 scale) due to salient feedback. Calculated as `alpha_learning_rate_salient_subj[subi] - alpha[subi]`. These values are crucial for correlating with individual difference measures (e.g., sensation seeking).

*   **Log-Likelihood (`log_lik`)**:
    *   The log-likelihood of the observed data given the parameters is calculated for each subject. This is useful for model comparison metrics like LOOIC or WAIC.

*   **Posterior Predictive Checks**:
    *   `predicted_choices`: Simulates choices for each trial and subject based on the fitted model parameters.
    *   `pp_choice_stim2_prob`: Stores the model-predicted probability of choosing stimulus 2 for each trial and subject.
    *   These are used to assess how well the model can replicate patterns observed in the actual data.

---

This step-by-step process—from raw data and unconstrained parameters to model fitting and finally to interpretable quantities—allows for a flexible and robust estimation of how salient cues influence learning behavior. The hierarchical nature accounts for both group trends and individual differences. The interpretable shift parameters generated are particularly important for testing the study's hypotheses regarding individual differences in the impact of salient cues. 