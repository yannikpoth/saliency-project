/*      Reinforcement learning model: Alpha Shift + Perseveration - UNIFORM PRIORS VERSION
Last edit:  2025/05/23
Authors:    Poth, Yannik (YP)
            Geysen, Steven (SG)
Notes:      - Based on rl_cp_shift_uniform.stan.
            - Adds a hierarchical perseveration/choice repetition parameter (kappa).
            - Implements prior changes based on professor Jan Peter's feedback (2025/04/23 & 2025/05/09):
                - Group-level SDs (alpha_sd_raw, alpha_shift_sd_raw, beta_sd_raw, kappa_sd_raw): uniform(0.001, 3)
                - Group-level alpha_mu_raw: uniform(-4, 4)
                - Group-level beta_mu_raw: uniform(-4, 4)
                - Group-level alpha_shift_mu_raw: remains normal(0, 1)
                - Group-level kappa_mu_raw: normal(0, 1) (to allow for perseveration and switching)
            - Includes alpha_shift parameter modulated by salient feedback.
            - Alpha (base + shift) is transformed via Phi within the trial loop.
            - Hierarchical structure with Centered Parameterization (CP).
            - Interpretable alpha shift values are calculated in the generated quantities block.
To do:      - Test and validate.
Comments:   - Model to test hypothesis about salient feedback modulating learning rate,
              while accounting for choice repetition tendencies.
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
  // Matrix indicating salient feedback (1 if salient, 0 if not, -9 for missing/padding)
  int<lower=-9,upper=1> salient_feedback[nSubs, maxTrials];
}

parameters {
  // --- Group-level Raw Parameters (on an unbounded scale) ---
  real alpha_mu_raw;          // Mean for base learning rate (raw)
  real alpha_shift_mu_raw;    // Mean for learning rate shift (raw)
  real beta_mu_raw;           // Mean for inverse temperature (raw)
  real kappa_mu_raw;          // Mean for perseveration strength (raw)

  real<lower=0> alpha_sd_raw;       // SD for base learning rate (raw)
  real<lower=0> alpha_shift_sd_raw; // SD for learning rate shift (raw)
  real<lower=0> beta_sd_raw;        // SD for inverse temperature (raw)
  real<lower=0> kappa_sd_raw;       // SD for perseveration strength (raw)

  // --- Subject-level Raw Parameters ---
  vector[nSubs] alpha_subj_raw;         // Subject-specific base learning rates (raw)
  vector[nSubs] alpha_shift_subj_raw;   // Subject-specific learning rate shifts (raw)
  vector[nSubs] beta_subj_raw;          // Subject-specific inverse temperatures (raw)
  vector[nSubs] kappa_subj_raw;         // Subject-specific perseveration strengths (raw)
}

transformed parameters {
  // --- Transformed Subject-level Parameters (for use in the model) ---
  // Beta is transformed here. Alpha, Alpha Shift, and Kappa are used raw and then Phi-transformed (alpha) or used directly (kappa) in model block.
  vector<lower=0, upper=10>[nSubs] beta_subj_transformed; // Scaled beta

  for (subi in 1:nSubs) {
    beta_subj_transformed[subi]  = Phi(beta_subj_raw[subi]) * 10.0; // Scale beta to [0, 10]
  }
}

model {
  // --- Priors ---
  // Group-level Means (on the raw, unbounded scale)
  alpha_mu_raw ~ uniform(-4, 4);       // Uniform prior for base alpha mean (raw)
  alpha_shift_mu_raw ~ normal(0, 1);   // Retained normal prior for alpha shift mean (raw)
  beta_mu_raw  ~ uniform(-4, 4);       // Uniform prior for beta mean (raw)
  kappa_mu_raw ~ normal(0, 1);         // Prior for perseveration mean (raw)

  // Group-level Standard Deviations (on the raw, unbounded scale, constrained positive)
  alpha_sd_raw ~ uniform(0.001, 3);       // Uniform prior (per prof. feedback)
  alpha_shift_sd_raw ~ uniform(0.001, 3); // Uniform prior (per prof. feedback)
  beta_sd_raw  ~ uniform(0.001, 3);       // Uniform prior (per prof. feedback)
  kappa_sd_raw ~ uniform(0.001, 3);       // Uniform prior for perseveration SD

  // Subject-level Raw Parameters (Centered Parameterization)
  alpha_subj_raw ~ normal(alpha_mu_raw, alpha_sd_raw);
  alpha_shift_subj_raw ~ normal(alpha_shift_mu_raw, alpha_shift_sd_raw);
  beta_subj_raw  ~ normal(beta_mu_raw, beta_sd_raw);
  kappa_subj_raw ~ normal(kappa_mu_raw, kappa_sd_raw);

  // --- Likelihood Calculation ---
  for (subi in 1:nSubs) {
    real current_beta_subj  = beta_subj_transformed[subi]; // Use pre-transformed beta
    real current_kappa_subj = kappa_subj_raw[subi];      // Use raw kappa

    // Initialize Q-values for this subject
    vector[2] qval = rep_vector(0.5, 2); // Q-values for two options
    real pe; // Prediction error
    int prev_choice_idx = -9; // Initialize previous choice, -9 if no valid previous choice

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

        // --- Policy (Softmax Choice Rule with Perseveration) ---
        choice_logits = current_beta_subj * qval; // Base logits from Q-values

        if (triali > 1 && prev_choice_idx != -9) { // If not the first trial and prev_choice was valid
          choice_logits[prev_choice_idx] += current_kappa_subj; // Add perseveration bonus to previously chosen action
        }

        choice[subi, triali] ~ categorical_logit(choice_logits);

        // --- Learning (Rescorla-Wagner Update) ---
        pe = current_reward_val - qval[current_choice_idx];
        qval[current_choice_idx] = qval[current_choice_idx] + trial_alpha_transformed * pe;

        prev_choice_idx = current_choice_idx; // Store current choice as previous for next trial
      } else {
        prev_choice_idx = -9; // If current trial is invalid, no valid previous choice for next trial
      }
    } // End trial loop
  } // End subject loop
}

generated quantities {
  // --- Transformed Group-level Parameters (for interpretation) ---
  real<lower=0, upper=1> alpha_mu = Phi(alpha_mu_raw);       // Transformed base learning rate mean
  real alpha_shift_mu_raw_gq = alpha_shift_mu_raw;           // Raw alpha shift mean (group level)
  real<lower=0, upper=10> beta_mu  = Phi(beta_mu_raw) * 10.0; // Scaled beta mean
  real kappa_mu_raw_gq = kappa_mu_raw;                       // Raw perseveration mean (group level)

  // --- Transformed/Raw Subject-level Parameters (for interpretation) ---
  vector<lower=0, upper=1>[nSubs] alpha;          // Transformed base learning rate per subject (non-salient feedback)
  vector[nSubs] alpha_shift_subj_raw_gq;          // Raw alpha shift per subject
  vector<lower=0, upper=10>[nSubs] beta;          // Scaled beta per subject
  vector[nSubs] kappa_subj_raw_gq;                // Raw perseveration strength per subject

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
    kappa_subj_raw_gq[subi] = kappa_subj_raw[subi];             // Storing raw kappa

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
    int prev_choice_idx_gq = -9;

    real current_beta_s_gq  = beta[subi]; // Uses subject's transformed beta from above
    real current_kappa_s_gq = kappa_subj_raw_gq[subi]; // Uses subject's raw kappa from above

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
        if (triali > 1 && prev_choice_idx_gq != -9) {
          gq_choice_logits[prev_choice_idx_gq] += current_kappa_s_gq;
        }

        vector[2] gq_choice_probs  = softmax(gq_choice_logits);

        pp_choice_stim2_prob[subi, triali] = gq_choice_probs[2];
        log_lik[subi] += categorical_logit_lpmf(observed_choice_idx | gq_choice_logits);
        predicted_choices[subi, triali] = categorical_logit_rng(gq_choice_logits);

        pe_gq = observed_reward_val - qval_gq[observed_choice_idx];
        qval_gq[observed_choice_idx] = qval_gq[observed_choice_idx] + gq_trial_alpha_transformed * pe_gq;

        prev_choice_idx_gq = observed_choice_idx;
      } else {
        prev_choice_idx_gq = -9;
      }
    }
  }
}
