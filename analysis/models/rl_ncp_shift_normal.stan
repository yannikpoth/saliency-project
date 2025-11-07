/*      Reinforcement learning model: Alpha Shift - NORMAL PRIORS / NCP VERSION
Last edit:  2024/07/25
Authors:    Poth, Yannik (YP); Gemini
Notes:      - Simpler version of rl_ncp_shift_perserv_normal.stan.
            - This version REMOVES the perseveration parameter (kappa) to test if the
              more complex model was overparameterized.
            - Implements a Non-Centered Parameterization (NCP).
            - Uses weakly informative normal priors.
To do:      - Compare this model's convergence and LOOIC against the perseveration version.
Comments:   - This model tests the core hypothesis about alpha_shift without the confounding
              influence of a perseveration parameter. It is a critical step in model comparison.
Sources:    Internal project files, Stan documentation, Stan community recommendations
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
  // Matrix indicating salient feedback (1 if salient, 0 if not, -9 for missing/padding)
  int<lower=-9,upper=1> salient_feedback[nSubs, maxTrials];
}

parameters {
  // --- Group-level Raw Parameters (on an unbounded scale) ---
  real alpha_mu_raw;          // Mean for base learning rate (raw)
  real alpha_shift_mu_raw;    // Mean for learning rate shift (raw)
  real beta_mu_raw;           // Mean for inverse temperature (raw)

  real<lower=0> alpha_sd_raw;       // SD for base learning rate (raw)
  real<lower=0> alpha_shift_sd_raw; // SD for learning rate shift (raw)
  real<lower=0> beta_sd_raw;        // SD for inverse temperature (raw)

  // --- Subject-level Standardized Offsets (for NCP) ---
  vector[nSubs] alpha_subj_z;         // Subject-specific base learning rate offsets (z-scored)
  vector[nSubs] alpha_shift_subj_z;   // Subject-specific learning rate shift offsets (z-scored)
  vector[nSubs] beta_subj_z;          // Subject-specific inverse temperature offsets (z-scored)
}

transformed parameters {
  // --- Transformed Subject-level Parameters (derived via NCP for use in the model) ---
  vector[nSubs] alpha_subj_raw;
  vector[nSubs] alpha_shift_subj_raw;
  vector[nSubs] beta_subj_raw;
  vector<lower=0, upper=10>[nSubs] beta_subj_transformed; // Scaled beta

  // NCP: Scale standardized offsets by group-level parameters
  alpha_subj_raw = alpha_mu_raw + alpha_subj_z * alpha_sd_raw;
  alpha_shift_subj_raw = alpha_shift_mu_raw + alpha_shift_subj_z * alpha_shift_sd_raw;
  beta_subj_raw = beta_mu_raw + beta_subj_z * beta_sd_raw;

  for (subi in 1:nSubs) {
    beta_subj_transformed[subi]  = Phi(beta_subj_raw[subi]) * 10.0; // Scale beta to [0, 10]
  }
}

model {
  // --- Priors ---
  // Group-level Means (on the raw, unbounded scale) - Weakly Informative
  alpha_mu_raw ~ normal(0, 1.5);
  alpha_shift_mu_raw ~ normal(0, 1);
  beta_mu_raw  ~ normal(0, 1.5);

  // Group-level Standard Deviations (on the raw, unbounded scale, constrained positive) - Weakly Informative
  alpha_sd_raw ~ normal(0, 1);
  alpha_shift_sd_raw ~ normal(0, 1);
  beta_sd_raw  ~ normal(0, 1);

  // Subject-level Standardized Offsets (Non-Centered Parameterization)
  alpha_subj_z ~ std_normal();
  alpha_shift_subj_z ~ std_normal();
  beta_subj_z ~ std_normal();

  // --- Likelihood Calculation ---
  for (subi in 1:nSubs) {
    real current_beta_subj  = beta_subj_transformed[subi]; // Use pre-transformed beta

    // Initialize Q-values for this subject
    vector[2] qval = rep_vector(0.5, 2); // Q-values for two options
    real pe; // Prediction error

    for (triali in 1:subTrials[subi]) {
      // Skip trials with missing data
      if ((choice[subi, triali] >= 1) && (reward[subi, triali] >= 0) && (salient_feedback[subi, triali] >= 0)) {
        int current_choice_idx = choice[subi, triali]; // 1 or 2
        int current_reward_val = reward[subi, triali]; // 0 or 1
        int current_salient_feedback = salient_feedback[subi, triali]; // 0 or 1

        vector[2] choice_logits;

        // Calculate effective raw alpha for the trial
        real effective_raw_alpha;
        if (current_salient_feedback == 1) {
          effective_raw_alpha = alpha_subj_raw[subi] + alpha_shift_subj_raw[subi];
        } else {
          effective_raw_alpha = alpha_subj_raw[subi];
        }
        // Transform effective alpha
        real trial_alpha_transformed = Phi(effective_raw_alpha);

        // --- Policy (Softmax Choice Rule) ---
        choice_logits = current_beta_subj * qval; // Base logits from Q-values

        choice[subi, triali] ~ categorical_logit(choice_logits);

        // --- Learning (Rescorla-Wagner Update) ---
        pe = current_reward_val - qval[current_choice_idx];
        qval[current_choice_idx] = qval[current_choice_idx] + trial_alpha_transformed * pe;
      }
    } // End trial loop
  } // End subject loop
}

generated quantities {
  // --- Transformed Group-level Parameters (for interpretation) ---
  real<lower=0, upper=1> alpha_mu = Phi(alpha_mu_raw);       // Transformed base learning rate mean
  real alpha_shift_mu_raw_gq = alpha_shift_mu_raw;           // Raw alpha shift mean (group level)
  real<lower=0, upper=10> beta_mu  = Phi(beta_mu_raw) * 10.0; // Scaled beta mean

  // --- Transformed/Raw Subject-level Parameters (for interpretation) ---
  vector<lower=0, upper=1>[nSubs] alpha;          // Transformed base learning rate per subject (non-salient feedback)
  vector[nSubs] alpha_shift_subj_raw_gq;          // Raw alpha shift per subject
  vector<lower=0, upper=10>[nSubs] beta;          // Scaled beta per subject

  // --- New: Interpretable Alpha Shift Parameters ---
  // Subject-level
  vector<lower=0, upper=1>[nSubs] alpha_learning_rate_salient_subj; // Subject's learning rate (0-1 scale) with salient feedback
  vector[nSubs] interpretable_alpha_shift_subj;       // Subject's interpretable shift (difference on 0-1 scale due to salience)

  // Group-level (derived from mean raw parameters)
  real<lower=0, upper=1> alpha_learning_rate_salient_mu;    // Group mean learning rate (0-1 scale) with salient feedback
  real interpretable_alpha_shift_mu;              // Group mean interpretable shift (difference on 0-1 scale due to salience)

  for (subi in 1:nSubs) {
    alpha[subi] = Phi(alpha_subj_raw[subi]);
    alpha_shift_subj_raw_gq[subi] = alpha_shift_subj_raw[subi]; // Storing the raw shift for clarity
    beta[subi]  = beta_subj_transformed[subi];

    // Calculate interpretable shift components for each subject
    alpha_learning_rate_salient_subj[subi] = Phi(alpha_subj_raw[subi] + alpha_shift_subj_raw_gq[subi]);
    interpretable_alpha_shift_subj[subi] = alpha_learning_rate_salient_subj[subi] - alpha[subi];
  }

  // Calculate interpretable shift components at the group level using mean raw parameters
  alpha_learning_rate_salient_mu = Phi(alpha_mu_raw + alpha_shift_mu_raw_gq);
  interpretable_alpha_shift_mu = alpha_learning_rate_salient_mu - alpha_mu;

  // --- Log-Likelihood Calculation (for LOOIC, WAIC) ---
  real log_lik[nSubs];

  // --- Posterior Predictive Check Variables ---
  int predicted_choices[nSubs, maxTrials];
  real pp_choice_stim2_prob[nSubs, maxTrials];

  for (subi in 1:nSubs) {
    vector[2] qval_gq = rep_vector(0.5, 2);
    real pe_gq;
    log_lik[subi] = 0;
    int prev_choice_idx_gq = -9; // Not used in this model, but keep for consistency

    real current_beta_s_gq  = beta[subi]; // Uses subject's transformed beta from above

    for (triali in 1:subTrials[subi]) {
      predicted_choices[subi, triali] = -9;
      pp_choice_stim2_prob[subi, triali] = -9.0;

      if ((choice[subi, triali] >= 1) && (reward[subi, triali] >= 0) && (salient_feedback[subi, triali] >= 0)) {
        int observed_choice_idx = choice[subi, triali];
        int observed_reward_val = reward[subi, triali];
        int gq_salient_feedback = salient_feedback[subi, triali];

        vector[2] gq_choice_logits;

        // Calculate effective raw alpha for GQ (same logic as model block)
        real gq_effective_raw_alpha;
        if (gq_salient_feedback == 1) {
          gq_effective_raw_alpha = alpha_subj_raw[subi] + alpha_shift_subj_raw_gq[subi];
        } else {
          gq_effective_raw_alpha = alpha_subj_raw[subi];
        }
        real gq_trial_alpha_transformed = Phi(gq_effective_raw_alpha);

        gq_choice_logits = current_beta_s_gq * qval_gq;

        vector[2] gq_choice_probs  = softmax(gq_choice_logits);

        pp_choice_stim2_prob[subi, triali] = gq_choice_probs[2];
        log_lik[subi] += categorical_logit_lpmf(observed_choice_idx | gq_choice_logits);
        predicted_choices[subi, triali] = categorical_logit_rng(gq_choice_logits);

        pe_gq = observed_reward_val - qval_gq[observed_choice_idx];
        qval_gq[observed_choice_idx] = qval_gq[observed_choice_idx] + gq_trial_alpha_transformed * pe_gq;

        prev_choice_idx_gq = observed_choice_idx; // Storing for potential future use, no effect here
      }
    }
  }
}
