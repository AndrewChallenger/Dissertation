data {
  //sizes of data
  int<lower=0> m; //number of races
  int<lower=0> N; //number of drivers
  int<lower=0> S; //number of teams
  int<lower=0> Nmax; //max number of drivers per race
  
  //results
  int<lower=0> results_driver[Nmax,m]; //driver results
  int<lower=0> results_team[Nmax,m]; //team results
  int<lower=0> failures[Nmax,m]; //covariate of whether a failure occured at end of the race
  int<lower=0> no_drivers[m]; //number of drivers per race
  
  //hyperparameters for variance terms 
  real<lower=0> a;
  real<lower=0> b;
  real<lower=0> c;
  real<lower=0> d;
  real e;
  real<lower=0> f;
  
}

parameters {
  
  //varinaces of gamma and lambda
  real<lower=0> sigma_lambda;
  real<lower=0> sigma_gamma;
  
  //skill of drivers and constructors before centering 
  vector[N] lambda_raw; 
  vector[S] gamma_raw;
  
  //covariate for failure
  real beta;
  
}

transformed parameters { //centering
  vector[N] lambda = lambda_raw - mean(lambda_raw);
  vector[S] gamma = gamma_raw - mean(gamma_raw);
}


model {
  
  //for saving the probabilites and sum of skills and covariate
  vector[N] probs;
  vector[N] total_skill;
  
  //priors for skills
  
  target += gamma_lpdf(sigma_lambda | a,b);
  target += gamma_lpdf(sigma_gamma | c,d);
  
  target += normal_lpdf(lambda_raw | 0, sigma_lambda);
  target += normal_lpdf(gamma_raw | 0, sigma_gamma);
  
  //prior for beta
  target += normal_lpdf(beta | e, f);
  
  
  //forming the likelihood
  
  for (i in 1:m){ //cycle through races
    
    //filling in total skill for all the drivers in the race - the rest are set to 0 
    total_skill = rep_vector(0, N);
    for (r in 1:no_drivers[i]){
      total_skill[results_driver[r,i]] = exp(lambda[results_driver[r,i]] + 
      gamma[results_team[r,i]] + beta*failures[r,i]);
    }
    
    probs = rep_vector(0, N);
    for (r in 1:no_drivers[i]){ //each position
      probs = total_skill/sum(total_skill);
      target += categorical_lpmf(results_driver[r,i] | probs);
      total_skill[results_driver[r,i]] = 0; //as not needed for next iteration
    }
  }
  
}

generated quantities { //for estimating marginal likelihoods
  
  vector[N] probs;
  vector[N] total_skill;
  vector[sum(no_drivers) - m] log_lik;
  int count = 1;
  
  for (i in 1:m){ //cycle through races
    
    //filling in total skill for all the drivers in the race - the rest are set to 0 
    total_skill = rep_vector(0, N);
    for (r in 1:no_drivers[i]){
      total_skill[results_driver[r,i]] = exp(lambda[results_driver[r,i]] + 
      gamma[results_team[r,i]] + beta*failures[r,i]);
    }
    
    probs = rep_vector(0, N);
    for (r in 1:(no_drivers[i]-1)){ //each position
      probs = total_skill/sum(total_skill);
      log_lik[count] = categorical_lpmf(results_driver[r,i] | probs);
      total_skill[results_driver[r,i]] = 0; //as not needed for next iteration
      count = count + 1;
    }
  }
 
}








