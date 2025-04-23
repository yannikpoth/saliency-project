// Hierarchical Q-learning model with salient feedback modulating learning rate

data {
  int<lower=1> N;              // Total number of trials across all subjects
  int<lower=1> N_subj;         // Number of subjects
  array[N] int<lower=1, upper=N_subj> subj_id; // Subject ID for each trial
  array[N] int<lower=0, upper=1> choice; // choice (0 for arm 1, 1 for arm 2)
  array[N] int<lower=0, upper=1> outcome; // reward (0 or 1) - Stan allows int here too
  array[N] int<lower=0, upper=1> salient; // salient feedback cue present (0 or 1)
}

parameters {
  // Group-level parameters (hyperparameters)
  vector[3] mu_pr; // Group means (on unconstrained scale) for latent_alpha, latent_alpha_shift, log_beta
  vector<lower=0>[3] sigma; // Group standard deviations

  // Cholesky factor of the correlation matrix for individual parameters
  cholesky_factor_corr[3] L_Omega;

  // Subject-level parameters (standardized deviations from group means)
  matrix[3, N_subj] z; // Standardized random effects
}

transformed parameters {
  // Transform standardized effects z into actual subject-level parameters
  // using non-centered parameterization
  vector[N_subj] latent_alpha;      // subject-level latent alpha (unbounded)
  vector[N_subj] latent_alpha_shift;// subject-level latent alpha shift (unbounded)
  vector<lower=0>[N_subj] beta;     // subject-level inverse temperature (positive)

  // Calculate correlated random effects using Cholesky factor
  { // Local scope for matrix calculation
    matrix[3, N_subj] subj_effects_pr = diag_pre_multiply(sigma, L_Omega) * z;
    for (s in 1:N_subj) {
      latent_alpha[s] = mu_pr[1] + subj_effects_pr[1, s];
      latent_alpha_shift[s] = mu_pr[2] + subj_effects_pr[2, s];
      // Beta needs exp() because we model log_beta hierarchically for positivity
      beta[s] = exp(mu_pr[3] + subj_effects_pr[3, s]);
    }
  }
}

model {
  // --- Priors --- //
  // Group-level means (on unconstrained scale)
  mu_pr[1] ~ normal(0, 1); // Prior for mu_latent_alpha
  mu_pr[2] ~ normal(0, 1); // Prior for mu_latent_alpha_shift
  mu_pr[3] ~ normal(log(1), 1); // Prior for mu_log_beta (median beta = 1)

  // Group-level standard deviations (must be positive)
  sigma ~ normal(0, 1); // Truncated normal implicitly via <lower=0>

  // Correlation matrix prior
  L_Omega ~ lkj_corr_cholesky(2); // LKJ prior, eta=2 pushes slightly away from perfect correlations

  // Standardized subject-level effects prior (required for non-centered)
  to_vector(z) ~ std_normal(); // Each element of z is N(0,1)

  // --- Likelihood --- //
  // Q-values: matrix[N_subj, 2 arms]
  // Initialize Q values (e.g., to 0.5, representing uncertainty)
  matrix[N_subj, 2] Q = rep_matrix(0.5, N_subj, 2);

  // Loop through all trials
  for (n in 1:N) {
    int s = subj_id[n]; // Get subject for this trial
    real q_diff;
    real logit_p;
    real pe;           // Prediction error
    real effective_alpha; // Trial-specific learning rate
    real current_latent_lr; // Latent LR for this trial
    int current_choice = choice[n]; // 0 or 1
    int chosen_arm_idx = current_choice + 1; // Map choice (0/1) to Q column index (1/2)

    // Calculate Q-value difference (Q[arm 2] - Q[arm 1])
    q_diff = Q[s, 2] - Q[s, 1];

    // Calculate logit of choice probability for arm 2 (choice=1) using subject's beta
    logit_p = beta[s] * q_diff;

    // --- Likelihood Contribution from Choice --- //
    // Use the choice (0 for arm 1, 1 for arm 2) directly with bernoulli_logit
    current_choice ~ bernoulli_logit(logit_p);

    // --- Q-value Update --- //
    // Determine the effective learning rate for this trial using Phi_approx()
    current_latent_lr = latent_alpha[s]; // Start with base latent alpha
    // Add shift ONLY if it was a WINNING trial with salient feedback cue
    if (salient[n] == 1 && outcome[n] == 1) {
      current_latent_lr += latent_alpha_shift[s];
    }
    // Equivalent one-liner:
    // current_latent_lr = latent_alpha[s] + salient[n] * outcome[n] * latent_alpha_shift[s];

    effective_alpha = Phi_approx(current_latent_lr); // Transform latent value to 0-1 range

    // Calculate Prediction Error for the chosen arm
    pe = outcome[n] - Q[s, chosen_arm_idx];

    // Update Q-value for the chosen arm
    Q[s, chosen_arm_idx] = Q[s, chosen_arm_idx] + effective_alpha * pe;

    // Q-value for unchosen arm remains the same implicitly
  } // end trial loop
}

generated quantities {
  // Calculate interpretable parameters (on the 0-1 scale for alpha)
  vector[N_subj] alpha_base; // Effective base learning rate (non-salient trial or loss)
  vector[N_subj] alpha_salient_win; // Effective learning rate for salient wins

  // Calculate group-level interpretable parameters
  real mu_alpha_base = Phi_approx(mu_pr[1]); // Group average base alpha
  real mu_alpha_salient_win = Phi_approx(mu_pr[1] + mu_pr[2]); // Group average salient win alpha
  real mu_beta = exp(mu_pr[3]); // Group average beta

  // Calculate log-likelihood for each trial (for LOO/WAIC) and posterior predictions
  vector[N] log_lik;
  array[N] int<lower=0, upper=1> y_pred; // Posterior predictive choices

  // Initialize Q-values matrix for generation
  matrix[N_subj, 2] Q_gen = rep_matrix(0.5, N_subj, 2);

  for (s in 1:N_subj) {
    // Calculate subject-level interpretable alphas
    alpha_base[s] = Phi_approx(latent_alpha[s]);
    alpha_salient_win[s] = Phi_approx(latent_alpha[s] + latent_alpha_shift[s]);
  }

  // Loop through all trials to calculate log_lik and y_pred
  for (n in 1:N) {
      int s = subj_id[n]; // Get subject for this trial
      real q_diff_gen;
      real logit_p_gen;
      real pe_gen;
      real effective_alpha_gen;
      real current_latent_lr_gen;
      int current_choice = choice[n]; // Observed choice (0 or 1)
      int chosen_arm_idx = current_choice + 1; // Index (1 or 2)

      // Calculate logit probability based on current Q_gen values
      q_diff_gen = Q_gen[s, 2] - Q_gen[s, 1];
      logit_p_gen = beta[s] * q_diff_gen;

      // Calculate log likelihood for the observed choice
      log_lik[n] = bernoulli_logit_lpmf(current_choice | logit_p_gen);

      // Generate a predicted choice from the model
      y_pred[n] = bernoulli_logit_rng(logit_p_gen);

      // --- Q-value Update (Replicating model logic) --- //
      current_latent_lr_gen = latent_alpha[s];
      if (salient[n] == 1 && outcome[n] == 1) {
         current_latent_lr_gen += latent_alpha_shift[s];
      }
      // Equivalent one-liner:
      // current_latent_lr_gen = latent_alpha[s] + salient[n] * outcome[n] * latent_alpha_shift[s];

      effective_alpha_gen = Phi_approx(current_latent_lr_gen);
      pe_gen = outcome[n] - Q_gen[s, chosen_arm_idx]; // PE based on *observed* choice and outcome
      Q_gen[s, chosen_arm_idx] = Q_gen[s, chosen_arm_idx] + effective_alpha_gen * pe_gen;
  }
} 
