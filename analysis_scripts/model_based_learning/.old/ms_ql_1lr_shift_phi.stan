data {
  // Data dimensions and trial-level variables
  int<lower=1> nTrials;                // Number of trials per subject
  int<lower=1> nSubjects;              // Number of subjects
  // Choices: 0 indicates a missed trial; 1 or 2 for valid responses.
  int<lower=0,upper=2> choice[nSubjects, nTrials];  
  real<lower=0,upper=1> reward[nSubjects, nTrials]; // Rewards (0 or 1)
  // Salient feedback indicator: 0 = non-salient, 1 = salient, 2 = missed trial.
  int<lower=0,upper=2> cue[nSubjects, nTrials];     
}

transformed data {
  // Initial values for both arms (e.g., 0.5 for an unbiased start)
  vector[2] initV = rep_vector(0.5, 2);
}

parameters {
  // Group-level hyperparameters for the base learning rate (α)
  real mu_alpha;
  real<lower=0> sigma_alpha;
  
  // Group-level hyperparameters for the modulation (α_shift)
  real mu_alpha_shift;
  real<lower=0> sigma_alpha_shift;
  
  // Group-level hyperparameters for inverse temperature (β)
  real<lower=0> mu_beta;
  real<lower=0> sigma_beta;
  
  // Subject-level parameters
  vector[nSubjects] alpha;        // Base learning rate (unconstrained; will be mapped via Phi)
  vector[nSubjects] alpha_shift;  // Shift in learning rate due to salient feedback (unconstrained)
  vector<lower=0>[nSubjects] beta;  // Inverse temperature (β > 0)
}

model {
  // --- Priors for group-level parameters ---
  mu_alpha ~ normal(0, 1);
  sigma_alpha ~ cauchy(0, 2);
  
  mu_alpha_shift ~ normal(0, 1);
  sigma_alpha_shift ~ cauchy(0, 2);
  
  mu_beta ~ normal(1, 1);
  sigma_beta ~ cauchy(0, 2);
  
  // --- Hierarchical priors for subject-level parameters ---
  alpha ~ normal(mu_alpha, sigma_alpha);
  alpha_shift ~ normal(mu_alpha_shift, sigma_alpha_shift);
  beta ~ lognormal(log(mu_beta), sigma_beta);
  
  // --- Likelihood: Reinforcement Learning Update and Choice Rule ---
  for (s in 1:nSubjects) {
    vector[2] v = initV;  // Initialize values for both arms at the start
    for (t in 1:nTrials) {
      // Only process valid trials (i.e., where a choice was made)
      if (choice[s, t] > 0) {
        // Model the choice using a softmax (categorical_logit) function
        choice[s, t] ~ categorical_logit(beta[s] * v);
        
        {
          // Compute prediction error for the chosen option
          real pe = reward[s, t] - v[choice[s, t]];
          // Determine effective learning rate:
          // If salient feedback (cue == 1), add alpha_shift; if non-salient (cue == 0), use base alpha.
          real effective_alpha = (cue[s, t] == 1) ? (alpha[s] + alpha_shift[s]) : alpha[s];
          // Constrain effective learning rate between 0 and 1 via the Phi transformation.
          real lr = Phi(effective_alpha);
          // Update only the chosen arm; the unchosen arm remains unchanged.
          v[choice[s, t]] = v[choice[s, t]] + lr * pe;
        }
      }
      // For missed trials (choice[s,t] == 0), skip likelihood and update.
    }
  }
}

generated quantities {
  // Compute log likelihood per subject for model evaluation
  vector[nSubjects] log_lik;
  for (s in 1:nSubjects) {
    vector[2] v = initV;
    log_lik[s] = 0;
    for (t in 1:nTrials) {
      if (choice[s, t] > 0) {
        log_lik[s] += categorical_logit_lpmf(choice[s, t] | beta[s] * v);
        {
          real pe = reward[s, t] - v[choice[s, t]];
          real effective_alpha = (cue[s, t] == 1) ? (alpha[s] + alpha_shift[s]) : alpha[s];
          real lr = Phi(effective_alpha);
          v[choice[s, t]] = v[choice[s, t]] + lr * pe;
        }
      }
      // For missed trials, do not contribute to the log likelihood.
    }
  }
}
