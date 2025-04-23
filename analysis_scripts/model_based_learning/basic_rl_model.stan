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
  real<lower=0.001, upper=3> alpha_sd_raw; // Use bounds directly from prior
  real<lower=0.001, upper=3> beta_sd_raw;  // Use bounds directly from prior

  // Subject-level parameters (on the raw, unbounded scale)
  vector[nSubjects] alpha_subj_raw;
  vector[nSubjects] beta_subj_raw;
}

transformed parameters {
  // Transform group means to the desired scale [0,1] for alpha, [0,3] for beta
  real<lower=0, upper=1> alpha_mu = Phi_approx(alpha_mu_raw);
  real<lower=0, upper=3> beta_mu  = Phi_approx(beta_mu_raw) * 3;

  // Subject-level parameters (transformed to desired scale)
  // These are now derived from the raw subject parameters
  vector<lower=0, upper=1>[nSubjects] alpha_subj;
  vector<lower=0, upper=3>[nSubjects] beta_subj;
  for (s in 1:nSubjects) {
    alpha_subj[s] = Phi_approx(alpha_subj_raw[s]);
    beta_subj[s]  = Phi_approx(beta_subj_raw[s]) * 3;
  }
}

model {
  // Priors for group-level raw parameters (as specified)
  alpha_mu_raw ~ uniform(-3, 3);
  beta_mu_raw  ~ uniform(-3, 3);



  // Centered Parameterization:
  alpha_subj_raw ~ normal(alpha_mu_raw, alpha_sd_raw);
  beta_subj_raw  ~ normal(beta_mu_raw, beta_sd_raw);


  // Likelihood calculation
  for (s in 1:nSubjects) {
    vector[2] v = initV; // Initialize values for each subject
    real pe;             // Prediction error
    int start_idx = trial_starts[s];
    int end_idx = start_idx + trial_lengths[s] - 1;

    // Use pre-calculated transformed subject parameters
    real alpha_s = alpha_subj[s]; // Directly use the transformed subject alpha
    real beta_s = beta_subj[s];   // Directly use the transformed subject beta

    for (i in start_idx:end_idx) {
      int current_choice = choice[i];
      real current_reward = reward[i];

      // Log-likelihood contribution for the current trial i
      target += categorical_logit_lpmf(current_choice | beta_s * v);

      // Update chosen value
      pe = current_reward - v[current_choice];
      // Use subject-specific alpha_s for the update
      v[current_choice] = v[current_choice] + alpha_s * pe;
    }
  }
}

generated quantities {
  // Group means (already calculated in transformed parameters)
  // real<lower=0,upper=1> alpha_mu = Phi_approx(alpha_mu_raw); // Recalculated here just for clarity if needed, but redundant
  // real<lower=0,upper=3> beta_mu  = Phi_approx(beta_mu_raw) * 3; // Recalculated here just for clarity if needed, but redundant

  // Subject-level parameters (transformed) - already calculated in transformed parameters
  // vector<lower=0, upper=1>[nSubjects] alpha_subj; // Already available
  // vector<lower=0, upper=3>[nSubjects] beta_subj; // Already available

  // Predicted choices for posterior predictive checks
  int y_pred[nTotalTrials];

  { // Local block for predictions
    y_pred = rep_array(-999, nTotalTrials); // Initialize with placeholder

    for (s in 1:nSubjects) {
      vector[2] v = initV; // Initialize values for each subject
      real pe;
      int start_idx = trial_starts[s];
      int end_idx = start_idx + trial_lengths[s] - 1;

      // Use transformed subject-specific parameters from the transformed parameters block
      real alpha_s = alpha_subj[s];
      real beta_s = beta_subj[s];

      for (i in start_idx:end_idx) {
        // Use observed choice and reward for updating V to predict next trial's choice
        int current_choice = choice[i];
        real current_reward = reward[i];

        // Generate prediction based on current V and subject's beta
        y_pred[i] = categorical_logit_rng(beta_s * v);

        // Update Value based on observed choice and reward
        pe = current_reward - v[current_choice];
        v[current_choice] = v[current_choice] + alpha_s * pe;
      }
    }
  }
} 