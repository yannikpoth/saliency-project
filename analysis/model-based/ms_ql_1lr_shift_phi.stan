
data {
	int nTrials; 										//number of trials
	int<lower=1> nSubjects; 							// number of subjects
	int<lower=1, upper=2> choice[nSubjects, nTrials]; 	// vector of choices
	real<lower=0, upper=1> reward[nSubjects, nTrials]; 	// vector of rewards
	int<lower=0, upper=1> cue[nSubjects, nTrials];		// vector of saliency
}

transformed data {
	vector[2] initV;  					// initial values for V for each arm
	initV = rep_vector(0.5, 2);
}

parameters {
  
	// learning rate
	real<lower=-5,upper=5> alpha[nSubjects];
  
	// learning rate shift
	real<lower=-1,upper=1> alpha_s[nSubjects];

	// inverse temperature 
	real <lower=0> beta[nSubjects];
}


model {
  
  
  
  for (s in 1:nSubjects){
    
    vector[2] v[nTrials+1]; 			// value
    real pe[nSubjects, nTrials];       	// prediction error
  
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
      
        v[t+1, choice[s, t]] = v[t, choice[s, t]] + Phi(alpha[s] + cue[s,t]*alpha_s[s]) * pe[s, t];
      
      }
	  }
  }
}
  
// 
// generated quantities {
//   real log_lik[nSubjects, nTrials];
//   int predicted_choices[nSubjects, nTrials];
//   vector[4] v[nTrials+1]; // value
//   real pe[nSubjects, nTrials];       // prediction error
// 
// 	for (s in 1:nSubjects){
// 	  
//   	v[1] = initV;
// 
//   	for (t in 1:nTrials){
//   	  
//   	  if (choice[s,t] != 0) {
//   	  
//     	  // choice 
//     		log_lik[s, t] = categorical_logit_lpmf(choice[s, t] | beta[s] * v[t]);
//     		predicted_choices[s, t] = categorical_logit_rng(beta[s] * v[t]);
//     		 	
//     		// prediction error
//     		pe[s, t] = reward[s, t] - v[t,choice[s, t]];
//   	  
//   	  }	
//   		
//   	  // value updating (learning) 
//       v[t+1] = v[t];
//       
//       if (choice[s,t] != 0) {
//       
//       v[t+1, choice[s, t]] = v[t, choice[s, t]] + alpha[s] * pe[s, t];
//   	  
//   	  }
//   	}
//   }
// }
// 
