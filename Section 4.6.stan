data {
  //sizes of data
  int<lower=0> m; //number of races
  int<lower=0> N; //number of drivers
  int<lower=0> S; //number of teams
  int<lower=0> T; //number of time-steps for driver
  int<lower=0> U; //number of time-steps for team
  int<lower=0> Nmax; //max number of drivers per race
  int<lower=0> M; //this is just S^2
  
  //results
  int<lower=0> results_driver[Nmax,m]; //driver results
  int<lower=0> results_team[Nmax,m]; //team results
  int<lower=0> failures[Nmax,m]; //covariate of whether a failure occured at end of the race
  int<lower=0> no_drivers[m]; //number of drivers per race
  int<lower=0> t[m]; //time-steps for driver
  int<lower=0> u[m]; //time-steps for team - in dissertation this is just the same as t
  
  //hyperparameters for variance terms 
  real<lower=0> a;
  real<lower=0> b;
  real<lower=0> c;
  real<lower=0> d;
  //for beta
  real e;
  real<lower=0> f;
  
}

transformed data{
  int H = choose(S,2); 
}

parameters {
  
  // uncentered skills - again 'gamma' refers to 'U'
  vector[N*T] lambda_raw;
  vector[H*U] gamma_raw;
  
  //autoregresive means and variances
  real<lower=0, upper=1> nu;
  real<lower=0> sigma;
  real<lower=0,upper=1> theta;
  real<lower=0> tau;
  
  //covariate for failure
  real beta;
  
}

transformed parameters {
  
  vector[N*T] means_lambda;
  vector[H*U] means_gamma;
  vector[N*T] vars_lambda;
  vector[H*U] vars_gamma;
  
  
  //skill of drivers and teams
  vector[N] lambda[T];
  vector[M] gamma[U]; //for each time period we have M=S^2 elements of gamma, so each U^{(t)} matrix is parametarised as a 
  //vector
  
  means_lambda[1:N] = rep_vector(0, N);
  means_gamma[1:H] = rep_vector(0, H);
  
  lambda[1] = lambda_raw[1:N] - mean(lambda_raw[1:N]);
  
  //AR(1) process for drivers
  for (i in 2:T){
    means_lambda[(((i-1)*N)+1):(i*N)] = nu*lambda[i-1];
    lambda[i] = lambda_raw[(((i-1)*N)+1):(i*N)] - mean(lambda_raw[(((i-1)*N)+1):(i*N)]);
  }
  
  //AR(1) process for teams
  for (i in 2:U){
    means_gamma[(((i-1)*H)+1):(i*H)] = theta*gamma_raw[(((i-2)*H)+1):((i-1)*H)];
  }
  
  vars_lambda[1:N] = rep_vector(sqrt((sigma^2)/(1 - nu^2)), N);
  vars_lambda[(N+1):(T*N)] = rep_vector(sigma, N*(T-1));
  vars_gamma[1:H] = rep_vector(sqrt((tau^2)/(1 - theta^2)), H);
  vars_gamma[(H+1):(U*H)] = rep_vector(tau, H*(U-1));
  
  //filling in U matrix for each time-period from gamma_raw
  for (k in 1:U){
    gamma[k,1] = 0;
    gamma[k,2:S] = gamma_raw[(H*(k-1) + 1):(H*(k-1) + (S-1))];
    
    for (i in 2:(S-1)){
      for (j in 1:(i-1)){
        gamma[k,(i-1)*S + j] = -gamma[k,(j-1)*S + i];
      }
      gamma[k,(i-1)*S + i] = 0;
      gamma[k,((i-1)*S + (i+1)):(i*S)] = gamma_raw[(H*(k-1) + (i-1)*S - choose(i,2)+1):(H*(k-1) + i*S - choose(i+1,2))];
    }
    for (j in 1:(S-1)){
      gamma[k,(S*(S-1) + j)] = -gamma[k,j*S];
    }
    gamma[k,M] = 0;
  }
  
}


model {
  
  //for saving the probabilites and sum of skills and covariate
  vector[N] probs;
  vector[N] total_skill;
  
  //autoregressive priors
  target += gamma_lpdf(tau | c,d);
  target += gamma_lpdf(sigma | a,b);
  target += uniform_lpdf(nu | 0,1);
  target += uniform_lpdf(theta | 0,1);
  
  //prior for beta
  target += normal_lpdf(beta| e,f);
  
  //priors for skills
  target += normal_lpdf(lambda_raw | means_lambda, vars_lambda);
  target += normal_lpdf(gamma_raw | means_gamma, vars_gamma);
  
   //forming the likelihood
  
  for (i in 1:m){ //cycle through races
    
    //filling in total skill for all the drivers in the race - the rest are set to 0 
    for (r in 1:(no_drivers[i]-1)){
       total_skill = rep_vector(0, N);
      for (k in r:no_drivers[i]){
        for (j in r:no_drivers[i]){ //latent CRS likelihood
          total_skill[results_driver[k,i]] = total_skill[results_driver[k,i]] +
          gamma[u[i], (results_team[k,i]-1)*S + results_team[j,i]];
        }
        total_skill[results_driver[k,i]] = exp(total_skill[results_driver[k,i]] + 
        lambda[t[i],results_driver[k,i]] + beta*failures[k,i]);
      }
      probs = total_skill/sum(total_skill);
      target += categorical_lpmf(results_driver[r,i] | probs);
    
  }
  
}
  
}



generated quantities { //for marginal likelihood

  vector[N] probs;
  vector[N] total_skill;
  vector[sum(no_drivers) - m] log_lik;
  int result[Nmax];
  int count = 1;

   for (i in 1:m){ //cycle through races
    
    //filling in total skill for all the drivers in the race - the rest are set to 0 
    for (r in 1:(no_drivers[i]-1)){
       total_skill = rep_vector(0, N);
      for (k in r:no_drivers[i]){
        for (j in r:no_drivers[i]){ //latent CRS likelihood
          total_skill[results_driver[k,i]] = total_skill[results_driver[k,i]] +
          gamma[u[i], (results_team[k,i]-1)*S + results_team[j,i]];
        }
        total_skill[results_driver[k,i]] = exp(total_skill[results_driver[k,i]] + 
        lambda[t[i],results_driver[k,i]] + beta*failures[k,i]);
      }
      probs = total_skill/sum(total_skill);
      log_lik[count] = categorical_lpmf(results_driver[r,i] | probs);
      count = count+1;
    
  }
  
}

}










