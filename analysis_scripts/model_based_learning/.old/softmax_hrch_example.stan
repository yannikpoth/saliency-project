data {

  // data in long format
  int<lower=1> data_length; // length of data
  int<lower=1> sub_no[data_length]; // participant number
  int<lower=1> n_subs; // number of participants
  int<lower=0> choice[data_length]; // choice vector
  real<lower=0> ss[data_length]; // amount of smaller-sooner (SS) option
  real<lower=0> ll[data_length]; // amount of larger-later (LL) option
  real<lower=0> delay[data_length]; // delay of LL option
  int<lower=0> mag_cond[data_length]; // magnitude condition, 0: low, 1: high
  int<lower=0> drug_cond[data_length]; // drug condition, 0: placebo, 1: ldopa
  
} 

parameters {

  // group-level parameters
  real k_mu; // discount rate
  real s_mu; // k shift
  real temp_mu; // inverse temperature
  real temp_s_mu; // temp shift
  real me_mu; // magnitude effect
  real me_s_mu; // shift in magnitude effect
  real<lower=0> k_sd;
  real<lower=0> s_sd;
  real<lower=0> temp_sd;
  real<lower=0> temp_s_sd;
  real<lower=0> me_sd;
  real<lower=0> me_s_sd;
    
  // subject-level parameters
  real k[n_subs];
  real s[n_subs];
  real temp[n_subs];
  real temp_s[n_subs];
  real me[n_subs];
  real me_s[n_subs];
  
}

transformed parameters {

  vector[data_length] sv;
  vector[data_length] this_temp;
  vector[data_length] p;
  
  // boundary for temp
  real tbound = 30;
  real this_me;
  real this_k;
 
  // trial-wise drug effects
  for (idx in 1:data_length) {
    this_me = mag_cond[idx] * (me[sub_no[idx]] + me_s[sub_no[idx]] * drug_cond[idx]);
    this_k = k[sub_no[idx]] + s[sub_no[idx]] * drug_cond[idx] + this_me;
    this_temp[idx] = tbound * Phi(temp[sub_no[idx]] + temp_s[sub_no[idx]] * drug_cond[idx]);
  }

  // vectorisation
  // subjective value of LL option
  sv = to_vector(ll) ./ (1 + exp(this_k) .* to_vector(delay)); 
  // probability to choose LL option
  p = exp(this_temp .* sv) ./ (exp(this_temp .* to_vector(ss)) + exp(this_temp .* sv));
  
}

model {
 
  // priors for group-level parameters
  k_mu ~ uniform(-20, 3);
  s_mu ~ normal(0, 0.5);
  temp_mu ~ uniform(-5, 5);
  temp_s_mu ~ normal(0, 0.5);
  me_mu ~ normal(0, 0.5);
  me_s_mu ~ normal(0, 0.5);
  k_sd ~ uniform(.001, 10);
  s_sd ~ uniform(.001, 10);
  temp_sd ~ uniform(.001, 50);
  temp_s_sd ~ uniform(.001, 10);
  me_sd ~ uniform(0.001, 25);
  me_s_sd ~ uniform(0.001, 25);

  // priors for subject-level parameters
  for (sub in 1:n_subs) {
    k[sub] ~ normal(k_mu, k_sd);
    s[sub] ~ normal(s_mu, s_sd);
    temp[sub] ~ normal(temp_mu, temp_sd);
    temp_s[sub] ~ normal(temp_s_mu, temp_s_sd);
    me[sub] ~ normal(me_mu, me_sd);
    me_s[sub] ~ normal(me_s_mu, me_s_sd);
  }

  // likelihood
  choice ~ bernoulli_logit(this_temp  .* (sv - to_vector(ss))); // vectorised

}

generated quantities {

  real log_lik[n_subs];
  vector[data_length] pred;
  
  // initialise log_lik with zeros
  log_lik = rep_array(0, n_subs);

  for (idx in 1:data_length) {
    // calculate log-likelihood
    log_lik[sub_no[idx]] = log_lik[sub_no[idx]] + bernoulli_logit_lpmf(choice[idx] | this_temp[idx] * (sv[idx] - ss[idx]));
    // predict choices for each trial
    pred[idx] = bernoulli_logit_rng(this_temp[idx] * (sv[idx] - ss[idx]));
  }

}
