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
  // Group-level Raw Parameters (means)
  real alpha_mu_raw;
  real beta_mu_raw;

  // Group-level Raw Parameters (standard deviations)
  // Priors set via constraints based on professor feedback
  real<lower=0.001, upper=3> alpha_sd_raw;
  real<lower=0.001, upper=3> beta_sd_raw;

  // Subject-level Raw Parameters (for Centered Param.)
  vector[nSubjects] alpha_subj_raw; 
  vector[nSubjects] beta_subj_raw; 
}

model {
  // --- Priors ---
  // Group-level Means (Uniform as requested)
  alpha_mu_raw ~ uniform(-3, 3);
  beta_mu_raw  ~ uniform(-3, 3);
  // Group SD priors are handled by constraints in parameters block

  // Subject-level Raw Parameters (Priors for Centered Parameterization)
  alpha_subj_raw ~ normal(alpha_mu_raw, alpha_sd_raw);
  beta_subj_raw  ~ normal(beta_mu_raw, beta_sd_raw);

  // --- Likelihood Calculation ---
  for (s in 1:nSubjects) {
    // --- Subject-specific Parameters (Calculated Inline) ---
    // Directly transform the subject's raw parameter (Centered Parameterization)
    real alpha_subj = Phi_approx(alpha_subj_raw[s]);
    real beta_subj  = Phi_approx(beta_subj_raw[s]) * 3; 

    // --- Trial Loop ---
    vector[2] v = initV; // Initialize/reset Values for each subject
    real pe;             // Prediction error
    int start_idx = trial_starts[s];
    int end_idx = start_idx + trial_lengths[s] - 1;

    for (i in start_idx:end_idx) {
      int current_choice = choice[i];
      real current_reward = reward[i];

      // Log-likelihood contribution for the current trial i
      // Use the calculated alpha_subj and beta_subj for this subject
      target += categorical_logit_lpmf(current_choice | beta_subj * v);

      // Update chosen value
      pe = current_reward - v[current_choice];
      // Use subject-specific transformed alpha_subj for the update
      v[current_choice] = v[current_choice] + alpha_subj * pe;
    } // End trial loop
  } // End subject loop
}

generated quantities {
  // --- Transformed Group-level Parameters ---
  // Calculate interpretable group means after transformation
  real<lower=0, upper=1> alpha_mu = Phi_approx(alpha_mu_raw);
  real<lower=0, upper=3> beta_mu  = Phi_approx(beta_mu_raw) * 3; // Use same scaling factor

  // --- Transformed Subject-level Parameters ---
  // Calculate interpretable subject parameters after transformation
  vector<lower=0, upper=1>[nSubjects] alpha;
  vector<lower=0, upper=3>[nSubjects] beta;
  for (s in 1:nSubjects) {
    // Calculate using the centered subject raw parameters
    alpha[s] = Phi_approx(alpha_subj_raw[s]);
    beta[s]  = Phi_approx(beta_subj_raw[s]) * 3;
  }

  // --- Posterior Predictive Check ---
  int y_pred[nTotalTrials];
  { // Local block for predictions
    y_pred = rep_array(-999, nTotalTrials); // Initialize with placeholder

    for (s in 1:nSubjects) {
      vector[2] v = initV; // Initialize values for each subject
      real pe;
      int start_idx = trial_starts[s];
      int end_idx = start_idx + trial_lengths[s] - 1;

      // Use the specific subject's transformed parameters calculated above
      real alpha_subj = alpha[s];
      real beta_subj = beta[s];

      for (i in start_idx:end_idx) {
        // Use observed choice and reward for updating V to predict next trial's choice
        int current_choice = choice[i];
        real current_reward = reward[i];

        // Generate prediction based on current V and subject's beta
        y_pred[i] = categorical_logit_rng(beta_subj * v);

        // Update Value based on observed choice and reward
        pe = current_reward - v[current_choice];
        v[current_choice] = v[current_choice] + alpha_subj * pe;
      } // End trial loop
    } // End subject loop
  } // End local block
} 
