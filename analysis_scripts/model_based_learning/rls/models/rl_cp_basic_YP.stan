/*      Reinforcement learning model: Basic -- Version YP (Adapted from SG)
Last edit:  2025/04/29
Authors:    Poth, Yannik (YP)
            Geysen, Steven (SG)
Notes:      - A basic version of the Rescorla-Wagner model for the saliency
                slot machine task, using Centered Parameterization (CP).
            - Combines structure from SG's version with YP's original parameterization.
To do:      - Fit model
Comments:   - Uses subject matrix format for data input (like SG).
            - Uses original priors (uniform for means, constrained for SDs).
            - Uses original transformations (Phi_approx, beta scaling).
Sources:    Internal project files, Stan documentation
*/



/*** Model ***/
///////////////


data {
  // Number of subjects
  int<lower=1> nSubs;
  // Maximum number of trials for padding
  int<lower=1> maxTrials;
  // Number of actual trials for each subject
  int<lower=1> subTrials[nSubs];
  // Matrix of rewards (0 or 1, -9 for missing)
  int<lower=-9, upper=1> reward[nSubs, maxTrials];
  // Matrix of choices (1 or 2, -9 for missing)
  int<lower=-9,upper=2> choice[nSubs, maxTrials];
}

parameters {
  // --- Group-level Raw Parameters ---
  // Means (on the unbounded scale)
  real alpha_mu_raw;
  real beta_mu_raw;
  // Standard deviations (constrained)
  //real<lower=0.001, upper=3> alpha_sd_raw;
  //real<lower=0.001, upper=3> beta_sd_raw;
  real<lower=0> alpha_sd_raw;
  real<lower=0> beta_sd_raw;

  // --- Subject-level Raw Parameters ---
  // Individual deviations from group mean (Centered Parameterization)
  vector[nSubs] alpha_subj_raw;
  vector[nSubs] beta_subj_raw;
}


model {
  // --- Priors ---
  // Group-level Means (Uniform as originally specified)
  alpha_mu_raw ~ uniform(-3, 3);
  beta_mu_raw  ~ uniform(-3, 3);
  // Group SD priors are handled by constraints in the parameters block.

  // Subject-level Raw Parameters (Centered Parameterization)
  alpha_subj_raw ~ normal(alpha_mu_raw, alpha_sd_raw);
  beta_subj_raw  ~ normal(beta_mu_raw, beta_sd_raw);

  // --- Likelihood Calculation ---
  for (subi in 1:nSubs) {
    // --- Subject-specific Parameter Transformations ---
    // Transform raw parameters to the required scale for this subject
    real alpha_subj = Phi_approx(alpha_subj_raw[subi]); // Learning rate [0, 1]
    real beta_subj  = Phi_approx(beta_subj_raw[subi]) * 4; // Inverse temperature [0, 4]

    // --- Trial Loop ---
    // Initialize Q-values for this subject directly
    vector[2] qval = [0.5, 0.5]'; // Initialize directly
    // Prediction error
    real pe;

    for (triali in 1:subTrials[subi]) {
      // Skip trials with missing data (-9)
      if ((choice[subi, triali] >= 1) && (reward[subi, triali] >= 0)) {
        // Get data for the current trial
        int current_choice = choice[subi, triali];
        int current_reward = reward[subi, triali]; // Stan handles int to real conversion

        // --- Policy (Softmax Choice Rule) ---
        // Calculate log-likelihood contribution using transformed parameters
        choice[subi, triali] ~ categorical_logit(beta_subj * qval);

        // --- Learning (Rescorla-Wagner Update) ---
        // Calculate prediction error
        pe = current_reward - qval[current_choice];
        // Update the Q-value for the chosen option using transformed alpha
        qval[current_choice] = qval[current_choice] + alpha_subj * pe;
      }
    // End trial loop
    }
  // End subject loop
  }
}


generated quantities {
  // --- Transformed Group-level Parameters ---
  // Calculate interpretable group means after transformation
  real<lower=0, upper=1> alpha_mu = Phi_approx(alpha_mu_raw);
  real<lower=0, upper=4> beta_mu  = Phi_approx(beta_mu_raw) * 4;

  // --- Transformed Subject-level Parameters ---
  // Calculate interpretable subject parameters after transformation
  vector<lower=0, upper=1>[nSubs] alpha;
  vector<lower=0, upper=4>[nSubs] beta;
  for (subi in 1:nSubs) {
    alpha[subi] = Phi_approx(alpha_subj_raw[subi]);
    beta[subi]  = Phi_approx(beta_subj_raw[subi]) * 4;
  }

  // --- Log-Likelihood Calculation ---
  // For model comparison (e.g., WAIC, LOO)
  real log_lik[nSubs];

  // --- Posterior Predictive Check ---
  // Simulate choices based on the fitted model
  int predicted_choices[nSubs, maxTrials];
  real pp_choice_stim2_prob[nSubs, maxTrials]; // ADDED: For probability of choosing stimulus 2


  // Loop through subjects and trials again
  for (subi in 1:nSubs) {
    // Initialize values and log-likelihood for this subject directly
    vector[2] qval = [0.5, 0.5]'; // Initialize directly
    real pe;
    log_lik[subi] = 0;

    // Use this subject's transformed parameters calculated above
    real alpha_s = alpha[subi];
    real beta_s  = beta[subi];

    for (triali in 1:subTrials[subi]) {
      // Initialize prediction placeholder
      predicted_choices[subi, triali] = -9; // Use -9 for consistency
      pp_choice_stim2_prob[subi, triali] = -9.0; // Initialize with a sentinel value

      // Skip trials with missing data (-9 for choice/reward by convention, StanList uses 1/2 for choice)
      if ((choice[subi, triali] >= 1) && (reward[subi, triali] >= 0)) { // Ensure valid choice index
         // Get observed data for this trial
        int current_choice = choice[subi, triali]; // Observed choice (1 or 2)
        int current_reward = reward[subi, triali];

        // Calculate choice probabilities based on current qval and beta_s
        vector[2] raw_log_probs = beta_s * qval;
        vector[2] choice_probabilities = softmax(raw_log_probs);
        pp_choice_stim2_prob[subi, triali] = choice_probabilities[2]; // Prob of choosing stimulus 2 (index 2)

        // --- Generate Predicted Choice ---
        // Simulate choice based on current Q-values and subject's beta
        predicted_choices[subi, triali] = categorical_logit_rng(raw_log_probs); // Use raw_log_probs

        // --- Calculate Log Likelihood ---
        // Accumulate log probability of the *observed* choice given the model
        log_lik[subi] += categorical_logit_lpmf(current_choice | raw_log_probs); // Use raw_log_probs

        // --- Update Q-values (based on *observed* data) ---
        // Use observed choice and reward to update Q-values for the next prediction
        pe = current_reward - qval[current_choice];
        qval[current_choice] = qval[current_choice] + alpha_s * pe;
      }
    // End trial loop
    }
  // End subject loop
  }
} 
