// basic_rl_model.stan

data {
  int<lower=1> nSubjects;       // Number of subjects
  int<lower=1> nTotalTrials;    // Total number of valid trials across all subjects
  int<lower=1, upper=nSubjects> subj_map[nTotalTrials]; // Map trial index to subject index
  int<lower=1> trial_lengths[nSubjects]; // Number of trials for each subject
  int<lower=1> trial_starts[nSubjects]; // Start index in long vectors for each subject's trials

  int<lower=1,upper=2> choice[nTotalTrials];     // vector of choices (1 or 2)
  real<lower=0, upper=1> reward[nTotalTrials]; // vector of rewards (0.0 or 1.0)
}

transformed data {
  vector[2] initV;  // initial values for V
  initV = rep_vector(0.5, 2);
}

parameters {
  // group-level parameters
  real alpha_mu_raw;
  real beta_mu_raw;
  real<lower=0> alpha_sd_raw;
  real<lower=0> beta_sd_raw;

  // subject-level raw parameters
  vector[nSubjects] alpha_raw;
  vector[nSubjects] beta_raw;
}

model {
  // Priors for group-level parameters
  alpha_mu_raw ~ normal(0, 1.5);
  beta_mu_raw  ~ normal(0, 1.5);
  alpha_sd_raw ~ normal(0, 1);
  beta_sd_raw  ~ normal(0, 1);

  // Priors for subject-level raw parameters (standard normal for non-centered parameterization)
  alpha_raw ~ normal(0, 1);
  beta_raw  ~ normal(0, 1);

  // Likelihood calculation
  for (s in 1:nSubjects) {
    vector[2] v = initV; // Initialize values for each subject
    real pe;             // Prediction error
    int start_idx = trial_starts[s];
    int end_idx = start_idx + trial_lengths[s] - 1;

    // Calculate subject-specific parameters (transformed)
    // Beta can be calculated once per subject as it's constant within a subject
    real beta_s = Phi_approx(beta_mu_raw + beta_sd_raw * beta_raw[s]) * 3;

    // Pre-calculate untransformed alpha for the subject
    real alpha_s_untransformed = alpha_mu_raw + alpha_sd_raw * alpha_raw[s];

    for (i in start_idx:end_idx) {
      int current_choice = choice[i];
      real current_reward = reward[i];

      // Calculate alpha for the current trial (allows for future trial-wise modulation)
      real alpha_i = Phi_approx(alpha_s_untransformed); // Transform here

      // Log-likelihood contribution for the current trial i
      // Use beta_s (subject-specific) and current state v
      target += categorical_logit_lpmf(current_choice | beta_s * v);

      // Update chosen value
      pe = current_reward - v[current_choice];
      // Use alpha_i (potentially trial-specific) for the update
      v[current_choice] = v[current_choice] + alpha_i * pe;
    }
  }
}

generated quantities {
  // Group means
  real<lower=0,upper=1> alpha_mu = Phi_approx(alpha_mu_raw);
  real<lower=0,upper=3> beta_mu  = Phi_approx(beta_mu_raw) * 3;

  // Subject-level parameters (transformed) - useful for checking individual estimates
  vector<lower=0, upper=1>[nSubjects] alpha_subj;
  vector<lower=0, upper=3>[nSubjects] beta_subj;
  for (s in 1:nSubjects) {
      // Note: alpha_subj here represents the base alpha for the subject,
      // calculated before any potential trial-wise modulation.
      alpha_subj[s] = Phi_approx(alpha_mu_raw + alpha_sd_raw * alpha_raw[s]);
      beta_subj[s]  = Phi_approx(beta_mu_raw + beta_sd_raw * beta_raw[s]) * 3;
  }


  // Predicted choices for posterior predictive checks
  int y_pred[nTotalTrials];

  { // Local block for predictions
    y_pred = rep_array(-999, nTotalTrials); // Initialize with placeholder

    for (s in 1:nSubjects) {
      vector[2] v = initV; // Initialize values for each subject
      real pe;
      int start_idx = trial_starts[s];
      int end_idx = start_idx + trial_lengths[s] - 1;

      // Calculate subject-specific parameters (transformed)
      real beta_s = beta_subj[s]; // Use pre-calculated beta_subj
      real alpha_s_untransformed = alpha_mu_raw + alpha_sd_raw * alpha_raw[s]; // Recalculate raw for consistency

      for (i in start_idx:end_idx) {
        // Calculate alpha for the current trial
        real alpha_i = Phi_approx(alpha_s_untransformed); // Transform here

        // Use observed choice and reward for updating V to predict next trial's choice
        int current_choice = choice[i];
        real current_reward = reward[i];

        // Generate prediction based on current V and subject's beta
        y_pred[i] = categorical_logit_rng(beta_s * v);

        // Update Value based on observed choice and reward
        pe = current_reward - v[current_choice];
        v[current_choice] = v[current_choice] + alpha_i * pe; // Use alpha_i
      }
    }
  }
} 