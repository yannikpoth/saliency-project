/*      Reinforcement learning model: Model 6 - Full Model
Last edit:  2025/11/07
Authors:    Poth, Yannik (YP)
            Geysen, Steven (SG)
Notes:      - Factorial design model 6/6: Full model with all parameters
            - Parameters: alpha, beta, alpha_shift, kappa, kappa_shift
            - Updated priors per professor's specifications:
                - alpha_mu_raw, beta_mu_raw: uniform(-3, 3)
                - All SDs: uniform(0.0001, 10)
                - alpha_shift_mu_raw: normal(0, 1)
                - kappa_mu_raw: normal(0, 1)
                - kappa_shift_mu_raw: normal(0, 1)
            - Kappa is Phi-transformed to [0, 1] scale
            - Both alpha_shift and kappa_shift modulate based on salient feedback
            - Hierarchical structure with Centered Parameterization (CP)
To do:      - Test and validate
Comments:   - Complete model testing all hypotheses about salience effects
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
  real kappa_shift_mu_raw;    // Mean for perseveration shift (raw)

  real<lower=0> alpha_sd_raw;       // SD for base learning rate (raw)
  real<lower=0> alpha_shift_sd_raw; // SD for learning rate shift (raw)
  real<lower=0> beta_sd_raw;        // SD for inverse temperature (raw)
  real<lower=0> kappa_sd_raw;       // SD for perseveration strength (raw)
  real<lower=0> kappa_shift_sd_raw; // SD for perseveration shift (raw)

  // --- Subject-level Raw Parameters ---
  vector[nSubs] alpha_subj_raw;         // Subject-specific base learning rates (raw)
  vector[nSubs] alpha_shift_subj_raw;   // Subject-specific learning rate shifts (raw)
  vector[nSubs] beta_subj_raw;          // Subject-specific inverse temperatures (raw)
  vector[nSubs] kappa_subj_raw;         // Subject-specific perseveration strengths (raw)
  vector[nSubs] kappa_shift_subj_raw;   // Subject-specific perseveration shifts (raw)
}

transformed parameters {
  // --- Transformed Subject-level Parameters (for use in the model) ---
  vector<lower=0, upper=10>[nSubs] beta_subj_transformed;  // Scaled beta

  for (subi in 1:nSubs) {
    beta_subj_transformed[subi]  = Phi(beta_subj_raw[subi]) * 10.0; // Scale beta to [0, 10]
  }
}

model {
  // --- Priors ---
  // Group-level Means (on the raw, unbounded scale)
  alpha_mu_raw ~ uniform(-3, 3);       // Updated uniform prior
  alpha_shift_mu_raw ~ normal(0, 1);   // Normal prior for alpha shift mean (raw)
  beta_mu_raw  ~ uniform(-3, 3);       // Updated uniform prior
  kappa_mu_raw ~ normal(0, 1);         // Normal prior for perseveration mean (raw)
  kappa_shift_mu_raw ~ normal(0, 1);   // Normal prior for perseveration shift mean (raw)

  // Group-level Standard Deviations (on the raw, unbounded scale, constrained positive)
  alpha_sd_raw ~ uniform(0.0001, 10);       // Updated uniform prior
  alpha_shift_sd_raw ~ uniform(0.0001, 10); // Updated uniform prior
  beta_sd_raw  ~ uniform(0.0001, 10);       // Updated uniform prior
  kappa_sd_raw ~ uniform(0.0001, 10);       // Updated uniform prior
  kappa_shift_sd_raw ~ uniform(0.0001, 10); // Updated uniform prior

  // Subject-level Raw Parameters (Centered Parameterization)
  alpha_subj_raw ~ normal(alpha_mu_raw, alpha_sd_raw);
  alpha_shift_subj_raw ~ normal(alpha_shift_mu_raw, alpha_shift_sd_raw);
  beta_subj_raw  ~ normal(beta_mu_raw, beta_sd_raw);
  kappa_subj_raw ~ normal(kappa_mu_raw, kappa_sd_raw);
  kappa_shift_subj_raw ~ normal(kappa_shift_mu_raw, kappa_shift_sd_raw);

  // --- Likelihood Calculation ---
  for (subi in 1:nSubs) {
    real current_beta_subj  = beta_subj_transformed[subi];

    // Initialize Q-values for this subject
    vector[2] qval = rep_vector(0.5, 2); // Q-values for two options
    real pe; // Prediction error
    int prev_choice_idx = -9; // Initialize previous choice
    int prev_salient_feedback = -9; // Store previous trial's salience for kappa modulation

    for (triali in 1:subTrials[subi]) {
      // Skip trials with missing data
      if ((choice[subi, triali] >= 1) && (reward[subi, triali] >= 0) && (salient_feedback[subi, triali] >= 0)) {
        int current_choice_idx = choice[subi, triali]; // 1 or 2
        int current_reward_val = reward[subi, triali]; // 0 or 1
        int current_salient_feedback = salient_feedback[subi, triali]; // 0 or 1

        vector[2] choice_logits;

        // Calculate effective raw alpha for the trial (modulated by current salience)
        real effective_raw_alpha;
        if (current_salient_feedback == 1) {
          effective_raw_alpha = alpha_subj_raw[subi] + alpha_shift_subj_raw[subi];
        } else {
          effective_raw_alpha = alpha_subj_raw[subi];
        }
        // Transform effective alpha
        real trial_alpha_transformed = Phi(effective_raw_alpha);

        // --- Policy (Softmax Choice Rule with Perseveration modulated by previous salience) ---
        choice_logits = current_beta_subj * qval; // Base logits from Q-values

        if (triali > 1 && prev_choice_idx != -9 && prev_salient_feedback != -9) {
          // Calculate effective raw kappa based on previous trial's salience
          real effective_raw_kappa;
          if (prev_salient_feedback == 1) {
            effective_raw_kappa = kappa_subj_raw[subi] + kappa_shift_subj_raw[subi];
          } else {
            effective_raw_kappa = kappa_subj_raw[subi];
          }
          // Transform effective kappa and add perseveration bonus
          real trial_kappa_transformed = Phi(effective_raw_kappa);
          choice_logits[prev_choice_idx] += trial_kappa_transformed;
        }

        choice[subi, triali] ~ categorical_logit(choice_logits);

        // --- Learning (Rescorla-Wagner Update) ---
        pe = current_reward_val - qval[current_choice_idx];
        qval[current_choice_idx] = qval[current_choice_idx] + trial_alpha_transformed * pe;

        prev_choice_idx = current_choice_idx; // Store current choice as previous
        prev_salient_feedback = current_salient_feedback; // Store current salience
      } else {
        prev_choice_idx = -9; // Invalid trial, no valid previous choice
        prev_salient_feedback = -9;
      }
    } // End trial loop
  } // End subject loop
}

generated quantities {
  // --- Transformed Group-level Parameters (for interpretation) ---
  real<lower=0, upper=1> alpha_mu = Phi(alpha_mu_raw);
  real alpha_shift_mu_raw_gq = alpha_shift_mu_raw;
  real<lower=0, upper=10> beta_mu  = Phi(beta_mu_raw) * 10.0;
  real<lower=0, upper=1> kappa_mu = Phi(kappa_mu_raw);
  real kappa_shift_mu_raw_gq = kappa_shift_mu_raw;

  // --- Transformed/Raw Subject-level Parameters (for interpretation) ---
  vector<lower=0, upper=1>[nSubs] alpha;
  vector[nSubs] alpha_shift_subj_raw_gq;
  vector<lower=0, upper=10>[nSubs] beta;
  vector<lower=0, upper=1>[nSubs] kappa;
  vector[nSubs] kappa_shift_subj_raw_gq;

  // --- Interpretable Shift Parameters ---
  // Alpha shift
  vector<lower=0, upper=1>[nSubs] alpha_learning_rate_salient_subj;
  vector[nSubs] interpretable_alpha_shift_subj;
  real<lower=0, upper=1> alpha_learning_rate_salient_mu;
  real interpretable_alpha_shift_mu;

  // Kappa shift
  vector<lower=0, upper=1>[nSubs] kappa_perseveration_salient_subj;
  vector[nSubs] interpretable_kappa_shift_subj;
  real<lower=0, upper=1> kappa_perseveration_salient_mu;
  real interpretable_kappa_shift_mu;

  for (subi in 1:nSubs) {
    alpha[subi] = Phi(alpha_subj_raw[subi]);
    alpha_shift_subj_raw_gq[subi] = alpha_shift_subj_raw[subi];
    beta[subi]  = beta_subj_transformed[subi];
    kappa[subi] = Phi(kappa_subj_raw[subi]);
    kappa_shift_subj_raw_gq[subi] = kappa_shift_subj_raw[subi];

    // Calculate interpretable alpha shift components
    alpha_learning_rate_salient_subj[subi] = Phi(alpha_subj_raw[subi] + alpha_shift_subj_raw_gq[subi]);
    interpretable_alpha_shift_subj[subi] = alpha_learning_rate_salient_subj[subi] - alpha[subi];

    // Calculate interpretable kappa shift components
    kappa_perseveration_salient_subj[subi] = Phi(kappa_subj_raw[subi] + kappa_shift_subj_raw_gq[subi]);
    interpretable_kappa_shift_subj[subi] = kappa_perseveration_salient_subj[subi] - kappa[subi];
  }

  // Group level interpretable shifts
  alpha_learning_rate_salient_mu = Phi(alpha_mu_raw + alpha_shift_mu_raw_gq);
  interpretable_alpha_shift_mu = alpha_learning_rate_salient_mu - alpha_mu;

  kappa_perseveration_salient_mu = Phi(kappa_mu_raw + kappa_shift_mu_raw_gq);
  interpretable_kappa_shift_mu = kappa_perseveration_salient_mu - kappa_mu;

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
    int prev_salient_feedback_gq = -9;

    real current_beta_s_gq  = beta[subi];

    for (triali in 1:subTrials[subi]) {
      predicted_choices[subi, triali] = -9;
      pp_choice_stim2_prob[subi, triali] = -9.0;

      if ((choice[subi, triali] >= 1) && (reward[subi, triali] >= 0) && (salient_feedback[subi, triali] >= 0)) {
        int observed_choice_idx = choice[subi, triali];
        int observed_reward_val = reward[subi, triali];
        int gq_salient_feedback = salient_feedback[subi, triali];

        vector[2] gq_choice_logits;

        // Calculate effective raw alpha for GQ (modulated by current salience)
        real gq_effective_raw_alpha;
        if (gq_salient_feedback == 1) {
          gq_effective_raw_alpha = alpha_subj_raw[subi] + alpha_shift_subj_raw_gq[subi];
        } else {
          gq_effective_raw_alpha = alpha_subj_raw[subi];
        }
        real gq_trial_alpha_transformed = Phi(gq_effective_raw_alpha);

        gq_choice_logits = current_beta_s_gq * qval_gq;

        if (triali > 1 && prev_choice_idx_gq != -9 && prev_salient_feedback_gq != -9) {
          // Calculate effective raw kappa based on previous trial's salience
          real gq_effective_raw_kappa;
          if (prev_salient_feedback_gq == 1) {
            gq_effective_raw_kappa = kappa_subj_raw[subi] + kappa_shift_subj_raw_gq[subi];
          } else {
            gq_effective_raw_kappa = kappa_subj_raw[subi];
          }
          real gq_trial_kappa_transformed = Phi(gq_effective_raw_kappa);
          gq_choice_logits[prev_choice_idx_gq] += gq_trial_kappa_transformed;
        }

        vector[2] gq_choice_probs  = softmax(gq_choice_logits);

        pp_choice_stim2_prob[subi, triali] = gq_choice_probs[2];
        log_lik[subi] += categorical_logit_lpmf(observed_choice_idx | gq_choice_logits);
        predicted_choices[subi, triali] = categorical_logit_rng(gq_choice_logits);

        pe_gq = observed_reward_val - qval_gq[observed_choice_idx];
        qval_gq[observed_choice_idx] = qval_gq[observed_choice_idx] + gq_trial_alpha_transformed * pe_gq;

        prev_choice_idx_gq = observed_choice_idx;
        prev_salient_feedback_gq = gq_salient_feedback;
      } else {
        prev_choice_idx_gq = -9;
        prev_salient_feedback_gq = -9;
      }
    }
  }
}
