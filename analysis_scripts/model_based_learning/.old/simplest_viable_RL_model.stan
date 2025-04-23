// Simplest Viable Hierarchical Q-learning model
// - Models alpha, alpha_shift, beta hierarchically
// - Uses proper transformation for alpha_shift constraint
// - Uses half-normal priors for sigmas

data {
  int<lower=1> N;              // Total number of trials across all subjects
  int<lower=1> N_subj;         // Number of subjects
  array[N] int<lower=1, upper=N_subj> subj_id; // Subject ID for each trial
  array[N] int<lower=0, upper=1> choice; // choice (0 for arm 1, 1 for arm 2)
  array[N] int<lower=0, upper=1> outcome; // reward (0 or 1)
  array[N] int<lower=0, upper=1> salient; // salient feedback cue present (0 or 1)
}

parameters {
  // Group-level means (hyperparameters)
  real mu_logit_alpha;           // Group mean for logit(alpha)
  real mu_alpha_shift_latent;  // Group mean for latent alpha_shift (unconstrained scale)
  real mu_log_beta;            // Group mean for log(beta)

  // Group-level standard deviations (hyperparameters)
  real<lower=0> sigma_logit_alpha;
  real<lower=0> sigma_alpha_shift_latent;
  real<lower=0> sigma_log_beta;

  // Subject-level parameters (latent scale for non-centered parameterization)
  vector[N_subj] logit_alpha_raw;         // Subject deviations for logit(alpha) (raw)
  vector[N_subj] alpha_shift_latent_raw;  // Subject deviations for latent alpha_shift (raw)
  vector[N_subj] log_beta_raw;            // Subject deviations for log(beta) (raw)
}

transformed parameters {
  // Subject-level parameters on their natural/constrained scale
  vector<lower=0, upper=1>[N_subj] alpha;           // Subject base learning rate (0-1)
  vector<lower=-1, upper=1>[N_subj] alpha_shift;    // Subject shift in learning rate [-1, 1]
  vector<lower=0>[N_subj] beta;                     // Subject inverse temperature (positive)

  // Apply non-centered parameterization to latent variables
  vector[N_subj] logit_alpha = mu_logit_alpha + logit_alpha_raw * sigma_logit_alpha;
  vector[N_subj] alpha_shift_latent = mu_alpha_shift_latent + alpha_shift_latent_raw * sigma_alpha_shift_latent;
  vector[N_subj] log_beta = mu_log_beta + log_beta_raw * sigma_log_beta;

  // Transform to constrained scales
  for (s in 1:N_subj) {
    alpha[s] = inv_logit(logit_alpha[s]);
    // Transform latent shift to [-1, 1] using scaled inverse logit
    alpha_shift[s] = 2 * inv_logit(alpha_shift_latent[s]) - 1;
    beta[s] = exp(log_beta[s]);
  }
}

model {
  // --- Priors --- //
  // Group-level means
  mu_logit_alpha ~ normal(0, 1);         // Prior centered on alpha=0.5
  mu_alpha_shift_latent ~ normal(0, 1.5);  // Prior on latent scale, slightly wider than default N(0,1)
                                         // Centered at 0 -> median alpha_shift = 0
  mu_log_beta ~ normal(log(1), 1);     // Prior centered on beta=1

  // Group-level standard deviations - Use Half-Normal (via constraint + normal)
  sigma_logit_alpha ~ normal(0, 1); // Weakly informative positive prior (due to lower=0 constraint)
  sigma_alpha_shift_latent ~ normal(0, 1); // Weakly informative positive prior (due to lower=0 constraint)
  sigma_log_beta ~ normal(0, 1);        // Weakly informative positive prior (due to lower=0 constraint)

  // Subject-level raw parameters (standard normal for non-centered parameterization)
  logit_alpha_raw ~ std_normal();
  alpha_shift_latent_raw ~ std_normal();
  log_beta_raw ~ std_normal();

  // --- Likelihood --- //
  // Q-values: matrix[N_subj, 2 arms]
  // Initialize Q values (e.g., to 0.5, representing uncertainty)
  matrix[N_subj, 2] Q = rep_matrix(0.5, N_subj, 2);

  // Loop through all trials
  for (n in 1:N) {
    int s = subj_id[n]; // Get subject for this trial
    int current_choice = choice[n]; // 0 or 1
    int chosen_arm_idx = current_choice + 1; // Map choice (0/1) to Q column index (1/2)
    real q_diff;
    real logit_p;
    real pe;           // Prediction error
    real effective_alpha; // Trial-specific learning rate

    // Calculate Q-value difference (Q[arm 2] - Q[arm 1])
    q_diff = Q[s, 2] - Q[s, 1];

    // Calculate logit of choice probability for arm 2 (choice=1) using subject's beta
    logit_p = beta[s] * q_diff;

    // --- Likelihood Contribution from Choice --- //
    // Use the choice (0 for arm 1, 1 for arm 2) directly with bernoulli_logit
    target += bernoulli_logit_lpmf(current_choice | logit_p);

    // --- Q-value Update --- //
    // Determine the effective learning rate for this trial
    effective_alpha = alpha[s]; // Start with base alpha
    // Add shift ONLY if it was a WINNING trial with salient feedback
    if (salient[n] == 1 && outcome[n] == 1) {
      effective_alpha = effective_alpha + alpha_shift[s]; // Use the properly transformed alpha_shift
    }

    // Clamp effective_alpha to be within [0, 1] - still necessary!
    effective_alpha = fmax(0.0, fmin(1.0, effective_alpha));

    // Calculate Prediction Error for the chosen arm
    pe = outcome[n] - Q[s, chosen_arm_idx];

    // Update Q-value for the chosen arm
    // Create a temporary copy to avoid self-assignment issues if Stan version is sensitive
    real Q_chosen_old = Q[s, chosen_arm_idx];
    Q[s, chosen_arm_idx] = Q_chosen_old + effective_alpha * pe;

  } // end trial loop
}

generated quantities {
  // Calculate interpretable group-level parameters
  real mu_alpha = inv_logit(mu_logit_alpha);                 // Group average base alpha
  real mu_alpha_shift = 2 * inv_logit(mu_alpha_shift_latent) - 1; // Implied group average alpha_shift [-1, 1]
  real mu_beta = exp(mu_log_beta);                          // Group average beta

  // Calculate subject-level effective alpha for salient wins (optional)
  vector<lower=0, upper=1>[N_subj] alpha_salient_win_eff;
  for (s in 1:N_subj) {
      alpha_salient_win_eff[s] = fmax(0.0, fmin(1.0, alpha[s] + alpha_shift[s]));
  }

  // Calculate log-likelihood for each trial (for LOO/WAIC)
  vector[N] log_lik;
  // Posterior predictive checks (optional, can be intensive)
  // array[N] int<lower=0, upper=1> y_pred;

  { // Local scope for Q value generation
    matrix[N_subj, 2] Q_gen = rep_matrix(0.5, N_subj, 2);
    for (n in 1:N) {
      int s = subj_id[n];
      int current_choice = choice[n];
      int chosen_arm_idx = current_choice + 1;
      real q_diff_gen = Q_gen[s, 2] - Q_gen[s, 1];
      real logit_p_gen = beta[s] * q_diff_gen;
      real pe_gen;
      real effective_alpha_gen;

      // Calculate log likelihood for the observed choice based on Q values *at that trial*
      log_lik[n] = bernoulli_logit_lpmf(current_choice | logit_p_gen);

      // Generate prediction (optional)
      // y_pred[n] = bernoulli_logit_rng(logit_p_gen);

      // --- Q-value Update (Replicating model logic for next trial's likelihood) --- //
      effective_alpha_gen = alpha[s];
      if (salient[n] == 1 && outcome[n] == 1) {
        effective_alpha_gen += alpha_shift[s];
      }
      effective_alpha_gen = fmax(0.0, fmin(1.0, effective_alpha_gen));

      pe_gen = outcome[n] - Q_gen[s, chosen_arm_idx];
      real Q_chosen_gen_old = Q_gen[s, chosen_arm_idx];
      Q_gen[s, chosen_arm_idx] = Q_chosen_gen_old + effective_alpha_gen * pe_gen;
    }
  } // end local scope
} 
