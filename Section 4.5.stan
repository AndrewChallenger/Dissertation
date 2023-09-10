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
  //for beta
  real e;
  real<lower=0> f;
  
}

parameters { 
  
  //not that 'gamma' refers to 'U' from subection 3.2.5
  real<lower=0> sigma_lambda;
  real<lower=0> sigma_gamma;
  
  vector[N] lambda_raw;
  vector[choose(S,2)] gamma_raw; //vector of effective parameters for pairwise dependence
  
  //covariate for failure
  real beta;
  
}

transformed parameters {
  
  vector[S] gamma [S]; // U matrix
  vector[N] lambda = lambda_raw - mean(lambda_raw);
  
  //fill gamma (U) in with correct values lower diagonal
  gamma[1,1] = 0;
  gamma[S,S] = 0;
  gamma[1, 2:S] = gamma_raw[1:(S-1)];
  
  for (i in 2:(S-1)){
    gamma[i,i] = 0;
    gamma[i,(i+1):S] = gamma_raw[((i-1)*S - choose(i,2) + 1):(i*S - choose(i+1,2))];
  }
  //filling in upper diagonal
  for (i in 2:S){
    for (j in 1:(i-1)){
      gamma[i,j] = -gamma[j,i];
    }
  }

}


model {
  
  //for saving the probabilites and sum of skills and covariate
  vector[N] probs;
  vector[N] total_skill;
  
  //prios for skills
  target += gamma_lpdf(sigma_lambda | a,b);
  target += gamma_lpdf(sigma_gamma | c,d);
  
  target += normal_lpdf(lambda_raw | 0, sigma_lambda);
  target += normal_lpdf(gamma_raw | 0, sigma_gamma);
  
  //prior for beta
  target += normal_lpdf(beta | e, f);
  
  
  //forming the likelihood
  
  
  for (i in 1:m){ //cycle through races
    
    //filling in total skill for all the drivers in the race - the rest are set to 0 
    for (r in 1:(no_drivers[i]-1)){
      total_skill = rep_vector(0, N);
      for (k in r:no_drivers[i]){
        for (j in r:no_drivers[i]){ //latent CRS likelihood
          total_skill[results_driver[k,i]] = total_skill[results_driver[k,i]] +
          gamma[results_team[k,i], results_team[j,i]];
        }
        total_skill[results_driver[k,i]] = exp(total_skill[results_driver[k,i]] + 
        lambda[results_driver[k,i]] + beta*failures[k,i]);
      }
      probs = total_skill/sum(total_skill);
      target += categorical_lpmf(results_driver[r,i] | probs);
    
  }
  
}

}

generated quantities { //for marginal likelihoods

  vector[N] probs;
  vector[N] total_skill;
  vector[sum(no_drivers) - m] log_lik;
  int count = 1;

   for (i in 1:m){ //cycle through races
    
    //filling in total skill for all the drivers in the race - the rest are set to 0 
    for (r in 1:(no_drivers[i]-1)){
       total_skill = rep_vector(0, N);
      for (k in r:no_drivers[i]){
        for (j in r:no_drivers[i]){ //latent CRS likelihood
          total_skill[results_driver[k,i]] = total_skill[results_driver[k,i]] +
          gamma[results_team[k,i], results_team[j,i]];
        }
        total_skill[results_driver[k,i]] = exp(total_skill[results_driver[k,i]] + 
        lambda[results_driver[k,i]] + beta*failures[k,i]);
      }
      probs = total_skill/sum(total_skill);
      log_lik[count] = categorical_lpmf(results_driver[r,i] | probs);
      count = count + 1;
    
  }

}
}




