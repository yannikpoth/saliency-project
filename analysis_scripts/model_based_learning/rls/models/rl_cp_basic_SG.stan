/*      Reinforcement learning model: Basic -- Version 1
Last edit:  2025/04/25
Authors:    Poth, Yannik (YP)
            Geysen, Steven (SG)
Notes:      - A basic version of the Rescorla-Wagner model for the saliency
                slot machine task.
            - Release notes:
                * Initial commit
To do:      - Fit model
Comments:   SG: I made some adjustments to the model of Yannik. It is mainly
                to clarify things for me. The important parts stayed the same.
Sources:    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
            https://groups.google.com/g/stan-users/c/JI6yrSZz_Xg
*/



/*** Model ***/
///////////////


data {
  // Number of subjects
  int<lower=1> nSubs;
  // Total number of valid trials across all subjects
  int<lower=1> maxTrials;
  // Number of trials for each subject
  int<lower=1> subTrials[nSubs];
  // Vector of rewards (0 or 1)
  int<lower=-9, upper=1> reward[nSubs, maxTrials];
  // Vector of choices (1 or 2)
  int<lower=-9,upper=2> choice[nSubs, maxTrials];
}


parameters {
  // Group-level Raw Parameters (means)
  // real alpha_mu_raw;
  // real beta_mu_raw;
  real<lower=-5, upper=5> alpha_mu_raw;
  real<lower=0> beta_mu_raw;
  // Group-level Raw Parameters (standard deviations)
  ////YP: Priors set via constraints based on professor Jan's feedback.
  // real<lower=0.001, upper=3> alpha_sd_raw;
  // real<lower=0.001, upper=3> beta_sd_raw;
  real<lower=0> alpha_sd_raw;
  real<lower=0> beta_sd_raw;
  // Subject-level Raw Parameters (for Centered Param.)
  vector<lower=-5, upper=5> [nSubs] alpha_subj_raw; 
  vector<lower=0> [nSubs] beta_subj_raw; 
}



model {
  // Define values
  real alpha[nSubs];
  // real beta[nSubjects];

  // --- Priors ---
  // Group-level Means
  ////YP: Uniform as requested.
  // alpha_mu_raw ~ uniform(-3, 3);
  // beta_mu_raw  ~ uniform(-3, 3);
  alpha_mu_raw ~ normal(0,5);
  beta_mu_raw  ~ normal(0,5)T[0, ];
  ////YP: Group SD priors are handled by constraints in parameters block.
  alpha_sd_raw ~ normal(0,5)T[0, ];
  beta_sd_raw  ~ normal(0,5)T[0, ];

  // Subject-level Raw Parameters (Priors for Centered Parameterization)
  // alpha_subj_raw ~ normal(alpha_mu_raw, alpha_sd_raw);
  // beta_subj_raw  ~ normal(beta_mu_raw, beta_sd_raw);
  for (subi in 1:nSubs){
    alpha_subj_raw[subi] ~ normal(alpha_mu_raw, alpha_sd_raw);
    beta_subj_raw[subi] ~ normal(beta_mu_raw, beta_sd_raw)T[0, ];
  }

  // --- Likelihood Calculation ---
  for (subi in 1:nSubs) {
    // --- Subject-specific Parameters (Calculated Inline) ---
    // Directly transform the subject's raw parameter (Centered Parameterization)
    // real alpha_subj = Phi_approx(alpha_subj_raw[s]);
    // real beta_subj  = Phi_approx(beta_subj_raw[s]) * 3; 
    alpha[subi] = Phi(alpha_subj_raw[subi]);
    
    // --- Trial Loop ---
    // Initialize/reset Values for each subject
    // vector[2] v = initV;
    // vector[2] qval;
    // qval[1] = 0.5;
    // qval[2] = 0.5;
    vector[2] qval = [0.5, 0.5]';
    // Prediction error
    real pe;

    for (triali in 1:subTrials[subi]) {
      ////SG: Skip trials with missing values.
      if ((choice[subi, triali] >= 0) && (reward[subi, triali] >= 0)) {
        int current_reward = reward[subi, triali];
        // int true_choice = choice[subi, triali];
  
        // Log-likelihood contribution for the current trial i
        ////YP: Use the calculated alpha_subj and beta_subj for this subject
        // target += categorical_logit_lpmf(current_choice | beta_subj_raw[s] * v);
        // Policy
        choice[subi, triali] ~ categorical_logit(beta_subj_raw[subi] * qval);
  
        // Update chosen value
        pe = current_reward - qval[choice[subi, triali]];
        // Use subject-specific transformed alpha_subj for the update
        qval[choice[subi, triali]] = (
          qval[choice[subi, triali]] + (alpha[subi] * pe)
        );
      }
    // End trial loop
    }
  // End subject loop
  }
}


generated quantities {
  // Define values
  real alpha[nSubs];
  // real beta[nSubjects];
  int predicted_choices[nSubs, maxTrials];
  real log_lik[nSubs];
  real pp_choice_stim2_prob[nSubs, maxTrials];
  // --- Transformed Group-level Parameters ---
  real<lower=0, upper=1> alpha_mu = Phi(alpha_mu_raw); // Added transformed group alpha
  
  
  // --- Posterior Predictive Check ---
  for (subi in 1:nSubs) {
    // Initialize values for each subject
    vector[2] qval = [0.5, 0.5]';
    real pe;

    ////YP: Use the specific subject's transformed parameters calculated above.
    alpha[subi] = Phi(alpha_subj_raw[subi]);
    log_lik[subi] = 0;

   for (triali in 1:subTrials[subi]) {
      predicted_choices[subi, triali] = -999;
      pp_choice_stim2_prob[subi, triali] = -9.0;

      ////SG: Skip trials with missing values. (choice is 1 or 2, reward is 0 or 1 from StanList)
      if ((choice[subi, triali] >= 1) && (reward[subi, triali] >= 0)) { // Ensure valid choice index
        ////YP: Use observed choice and reward for updating the Q value to
            // predict next trial's choice.
        int current_reward = reward[subi, triali];
        // int true_choice = choice[subi, triali]; // This is the observed choice (1 or 2)
        
        // Calculate choice probabilities based on current qval and beta
        vector[2] raw_log_probs = beta_subj_raw[subi] * qval;
        vector[2] choice_probabilities = softmax(raw_log_probs);
        pp_choice_stim2_prob[subi, triali] = choice_probabilities[2]; // Prob of choosing stimulus 2 (index 2)

        // Simulate choices
        predicted_choices[subi, triali] = categorical_logit_rng(
          raw_log_probs // Use raw_log_probs
        );
        // Log likelihood
        log_lik[subi] = log_lik[subi] + categorical_logit_lpmf(
          choice[subi, triali] | raw_log_probs // Use raw_log_probs
        );
  
        // Update Value based on observed choice and reward
        // Note: choice[subi, triali] will be 1 or 2, correctly indexing qval
        pe = current_reward - qval[choice[subi, triali]];
        qval[choice[subi, triali]] = (
          qval[choice[subi, triali]] + (alpha[subi] * pe)
        );
      }
    // End trial loop
    }
  // End subject loop
  }
} 
