/*      Reinforcement learning model: Model 1 - Baseline
Last edit:  2025/11/07
Authors:    Poth, Yannik (YP)
            Geysen, Steven (SG)
Notes:      - Factorial design model 1/6: Baseline model
            - Parameters: alpha, beta only
            - Updated priors per professor's specifications:
                - alpha_mu_raw, beta_mu_raw: uniform(-3, 3)
                - All SDs: uniform(0.0001, 10)
            - Transforms alpha and beta via Phi
            - Hierarchical structure with Centered Parameterization (CP)
To do:      - Test and validate
Comments:   - Baseline model for factorial comparison
Sources:    Internal project files, Stan documentation, Professor's feedback
*/

data {
  // Number of subjects
  int<lower=1> nSubs;
  // Maximum number of trials for padding (total trials in task data)
  int<lower=1> maxTrials;
  // Number of actual valid trials for each subject
  int<lower=1> subTrials[nSubs];
  // Matrix of rewards (0 or 1, -9 for missing/padding)
  int<lower=-9, upper=1> reward[nSubs, maxTrials];
  // Matrix of choices (1 for option 1, 2 for option 2, -9 for missing/padding)
  int<lower=-9,upper=2> choice[nSubs, maxTrials];
}

parameters {
  // --- Group-level Raw Parameters (on an unbounded scale) ---
  real alpha_mu_raw;  // Mean for learning rate (raw)
  real beta_mu_raw;   // Mean for inverse temperature (raw)

  real<lower=0> alpha_sd_raw; // SD for learning rate (raw)
  real<lower=0> beta_sd_raw;  // SD for inverse temperature (raw)

  // --- Subject-level Raw Parameters (deviations for CP) ---
  vector[nSubs] alpha_subj_raw; // Subject-specific learning rates (raw)
  vector[nSubs] beta_subj_raw;  // Subject-specific inverse temperatures (raw)
}

transformed parameters {
  // --- Transformed Subject-level Parameters (for use in the model) ---
  vector<lower=0, upper=10>[nSubs] beta_subj_transformed; // Scaled beta

  for (subi in 1:nSubs) {
    beta_subj_transformed[subi]  = Phi(beta_subj_raw[subi]) * 10.0; // Scale beta to [0, 10]
  }
}

model {
  // --- Priors ---
  // Group-level Means (on the raw, unbounded scale)
  alpha_mu_raw ~ uniform(-3, 3);   // Updated uniform prior
  beta_mu_raw  ~ uniform(-3, 3);   // Updated uniform prior

  // Group-level Standard Deviations (on the raw, unbounded scale, constrained positive)
  alpha_sd_raw ~ uniform(0.0001, 10); // Updated uniform prior
  beta_sd_raw  ~ uniform(0.0001, 10); // Updated uniform prior

  // Subject-level Raw Parameters (Centered Parameterization)
  alpha_subj_raw ~ normal(alpha_mu_raw, alpha_sd_raw);
  beta_subj_raw  ~ normal(beta_mu_raw, beta_sd_raw);

  // --- Likelihood Calculation ---
  for (subi in 1:nSubs) {
    // Beta is taken from transformed parameters
    real current_beta_subj  = beta_subj_transformed[subi];

    // Initialize Q-values for this subject
    vector[2] qval = rep_vector(0.5, 2); // Q-values for two options
    real pe; // Prediction error

    for (triali in 1:subTrials[subi]) {
      // Skip trials with missing data (coded as -9 or other non-valid values)
      if ((choice[subi, triali] >= 1) && (reward[subi, triali] >= 0)) {
        int current_choice_idx = choice[subi, triali]; // 1 or 2
        int current_reward_val = reward[subi, triali]; // 0 or 1

        // Calculate trial-specific alpha here
        real trial_alpha_transformed = Phi(alpha_subj_raw[subi]);

        // --- Policy (Softmax Choice Rule) ---
        // Log-likelihood contribution for the current trial's observed choice
        choice[subi, triali] ~ categorical_logit(current_beta_subj * qval);

        // --- Learning (Rescorla-Wagner Update) ---
        // Calculate prediction error
        pe = current_reward_val - qval[current_choice_idx];
        // Update the Q-value for the chosen option
        qval[current_choice_idx] = qval[current_choice_idx] + trial_alpha_transformed * pe;
      }
    } // End trial loop
  } // End subject loop
}

generated quantities {
  // --- Transformed Group-level Parameters (for interpretation) ---
  real<lower=0, upper=1> alpha_mu = Phi(alpha_mu_raw);
  real<lower=0, upper=10> beta_mu  = Phi(beta_mu_raw) * 10.0; // Scaled to [0, 10]

  // --- Transformed Subject-level Parameters (for interpretation) ---
  vector<lower=0, upper=1>[nSubs] alpha;
  vector<lower=0, upper=10>[nSubs] beta; // Scaled to [0, 10]
  for (subi in 1:nSubs) {
    alpha[subi] = Phi(alpha_subj_raw[subi]); // Transform alpha_subj_raw here
    beta[subi]  = beta_subj_transformed[subi];  // Use pre-computed beta
  }

  // --- Log-Likelihood Calculation (for LOOIC, WAIC) ---
  real log_lik[nSubs]; // Log-likelihood for each subject

  // --- Posterior Predictive Check Variables ---
  int predicted_choices[nSubs, maxTrials];      // Simulated choices
  real pp_choice_stim2_prob[nSubs, maxTrials]; // Simulated prob of choosing stimulus 2

  for (subi in 1:nSubs) {
    // Initialize Q-values and log-likelihood for this subject
    vector[2] qval_gq = rep_vector(0.5, 2);
    real pe_gq;
    log_lik[subi] = 0;

    // Use this subject's transformed parameters (same as in model block)
    real current_beta_s_gq  = beta[subi];  // From above GQ calculation

    for (triali in 1:subTrials[subi]) {
      // Initialize with sentinel values
      predicted_choices[subi, triali] = -9;
      pp_choice_stim2_prob[subi, triali] = -9.0;

      // Process only valid observed trials for GQ
      if ((choice[subi, triali] >= 1) && (reward[subi, triali] >= 0)) {
        int observed_choice_idx = choice[subi, triali];
        int observed_reward_val = reward[subi, triali];

        // --- Calculate Choice Probabilities and Log Likelihood ---
        vector[2] gq_raw_log_probs = current_beta_s_gq * qval_gq;
        vector[2] gq_choice_probs  = softmax(gq_raw_log_probs);

        pp_choice_stim2_prob[subi, triali] = gq_choice_probs[2]; // Prob of choosing stimulus 2 (index 2)

        log_lik[subi] += categorical_logit_lpmf(observed_choice_idx | gq_raw_log_probs);

        // --- Generate Predicted Choice (based on Q-values before this trial's update) ---
        predicted_choices[subi, triali] = categorical_logit_rng(gq_raw_log_probs);

        // --- Update Q-values (based on *observed* choice and reward for this trial) ---
        // This ensures Q-values evolve based on actual experience for next trial's prediction/log-lik
        pe_gq = observed_reward_val - qval_gq[observed_choice_idx];
        qval_gq[observed_choice_idx] = qval_gq[observed_choice_idx] + alpha[subi] * pe_gq;
      }
    } // End trial loop (GQ)
  } // End subject loop (GQ)
}
