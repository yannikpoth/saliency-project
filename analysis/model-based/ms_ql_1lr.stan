
data {
	int nTrials; 										//number of trials
	int<lower=1> nSubjects; 							// number of subjects
	int choice[nSubjects, nTrials]; 					// vector of choices
	real<lower=0, upper=1> reward[nSubjects, nTrials]; 	// vector of rewards
}

transformed data {
	vector[2] initV;  									// initial values for V for each arm
	initV = rep_vector(0.5, 2);
}

parameters {
	real<lower=0,upper=1> alpha[nSubjects];				// learning rate
	real <lower=0> beta[nSubjects];						// inverse temperature 
}


model {
  
	for (s in 1:nSubjects){
		vector[2] v[nTrials+1]; 		// value
		real pe[nSubjects, nTrials];  	// prediction error

			v[1] = initV;
		
			for (t in 1:nTrials){
			
				if (choice[s,t] != 999) {
			
					// choice 
					choice[s, t] ~ categorical_logit(beta[s] * v[t]);
					
					// prediction error
					pe[s, t] = reward[s, t] - v[t,choice[s, t]];
				
				}
			
				// value updating (learning) 
				v[t+1] = v[t];
		
				if (choice[s,t] != 999) {
		
					v[t+1, choice[s, t]] = v[t, choice[s, t]] + alpha[s] * pe[s, t];
		
				}
			}
	}
}
