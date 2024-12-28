data {
  int nTrials;                                     	// number of trials
  int<lower=1> nSubjects;                          	// number of subjects
  int<lower=1, upper=2> choice[nSubjects, nTrials]; // vector of choices
  real<lower=0, upper=1> reward[nSubjects, nTrials];// vector of rewards
  int<lower=0, upper=1> cue[nSubjects, nTrials];   	// vector of saliency
}

transformed data {
  vector[2] initV;   								// initial values for V for each arm
  initV = rep_vector(0.5, 2);
}

parameters {
  real<lower=0, upper=1> alpha[nSubjects];         	// Base learning rate
  real alpha_s_raw[nSubjects];                     	// Unconstrained shift for salient feedback
  real<lower=0> beta[nSubjects];                   	// Inverse temperature
}

transformed parameters {
  real<lower=-1, upper=1> alpha_s[nSubjects];      	// Constrained alpha shift
  real alpha_total[nSubjects, nTrials];             // Combined learning rate (alpha + alpha_s)

  for (s in 1:nSubjects) {
    // Constrain alpha_s to ensure the sum of alpha and alpha_s is within [0, 1]
    alpha_s[s] = fmin(1 - alpha[s], fmax(-alpha[s], alpha_s_raw[s]));
    for (t in 1:nTrials) {
      // Compute the total learning rate for each trial
      alpha_total[s, t] = alpha[s] + cue[s, t] * alpha_s[s];
    }
  }
}

model {
	// Priors
  	alpha ~ beta(2, 2);                                // Prior for base learning rate: centered on 0.5
  	alpha_s_raw ~ normal(0, 0.5);                      // Prior for shift: encourages values near 0
  	beta ~ normal(4, 2);                               // Prior for inverse temperature: moderate values

	for (s in 1:nSubjects) {
		vector[2] v[nTrials + 1];          // Value
		real pe[nTrials];                  // Prediction error

		// Initialize values
		v[1] = initV;

		for (t in 1:nTrials) {
			if (choice[s, t] != 999) {
				// Model choice as a function of inverse temperature and value difference
				choice[s, t] ~ categorical_logit(beta[s] * v[t]);

				// Compute prediction error
				pe[t] = reward[s, t] - v[t, choice[s, t]];
			}

			// Update values
			v[t + 1] = v[t];
			if (choice[s, t] != 999) {
				v[t + 1, choice[s, t]] = v[t, choice[s, t]] + alpha_total[s, t] * pe[t];
			}
		}
	}
}
