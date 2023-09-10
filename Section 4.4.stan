data {
  //sizes of data
  int<lower=0> m; //number of races
  int<lower=0> N; //number of drivers
  int<lower=0> S; //number of teams
  int<lower=0> T;
  int<lower=0> U; //number of time-steps for team
  int<lower=0> Nmax; //max number of drivers per race
  int<lower=0> Ndriver_max; //max number of drivers for each team in each time period
  
  //results
  int<lower=0> results_driver[Nmax,m]; //driver results
  int<lower=0> results_team[Nmax,m]; //team results
  int<lower=0> failures[Nmax,m]; //covariate of whether a failure occured at end of the race
  int<lower=0> no_drivers[m]; //number of drivers per race
  int<lower=0> u[m]; //time-steps for team
  int<lower=0> t[m]; //time-steps for team
  int<lower=0> no_drivers_per_team[T,S,1]; //number of drivers for each team in each time period
  int<lower=0> drivers_per_team[T,S,Ndriver_max]; //which drivers drove for each team in each time period
  
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
  
  // uncentered skills
  vector[N*T] lambda_raw;
  vector[S*U] gamma_raw;
  
  //autoregresive means and variances
  real<lower=0,upper=1> theta;
  real<lower=0> tau;
  real<lower=0,upper=1> nu;
  real<lower=0> sigma;
  
  //covariate for failure
  real beta;
  
}

transformed parameters {
  
  //means and variances for AR(1) processes
  vector[S*U] means_gamma;
  vector[S*U] vars_gamma;
  vector[N*T] means_lambda;
  vector[N*T] vars_lambda;
  
  
  //skill of drivers and constructors
  vector[N] lambda[T];
  vector[S] gamma[U];
  
  //AR(1) process for teams
  means_gamma[1:S] = rep_vector(0, S);
  vars_gamma[1:S] = rep_vector(sqrt((tau^2)/(1 - theta^2)), S);
  vars_gamma[(S+1):(U*S)] = rep_vector(tau, S*(U-1));
  
  gamma[1] = gamma_raw[1:S] - mean(gamma_raw[1:S]);
  
  for (i in 2:U){
    means_gamma[(((i-1)*S)+1):(i*S)] = theta*gamma[i-1]; 
    gamma[i] = gamma_raw[(((i-1)*S)+1):(i*S)] - mean(gamma_raw[(((i-1)*S)+1):(i*S)]);
  }
  
  
  
//AR(1) process for drivers
means_lambda[1:N] = rep_vector(0, N);
lambda[1] = lambda_raw[1:N];
  
for (i in 2:T){
    lambda[i] = lambda_raw[(((i-1)*N)+1):(i*N)];
  }
  
  
  //now center drivers in each team
  for (i in 1:T){
    for (j in 1:S){
      if (no_drivers_per_team[i,j,1] > 0){
        lambda[i, drivers_per_team[i,j,1:no_drivers_per_team[i,j,1]]] = lambda[i, drivers_per_team[i,j,1:no_drivers_per_team[i,j,1]]] -
        mean(lambda[i, drivers_per_team[i,j,1:no_drivers_per_team[i,j,1]]]);
      }
    }
  }
  
  for (i in 2:T){
    means_lambda[(((i-1)*N)+1):(i*N)] = nu*lambda[i-1]; 
  }
  
  vars_lambda[1:N] = rep_vector(sqrt((sigma^2)/(1 - nu^2)), N);
  vars_lambda[(N+1):(T*N)] = rep_vector(sigma, N*(T-1));
  
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
    total_skill = rep_vector(0, N);
    for (r in 1:no_drivers[i]){
      total_skill[results_driver[r,i]] = exp(lambda[t[i],results_driver[r,i]] + 
      gamma[u[i], results_team[r,i]] + beta*failures[r,i]);
    }
    
    probs = rep_vector(0, N);
    for (r in 1:no_drivers[i]){ //each position
      probs = total_skill/sum(total_skill);
      results_driver[r,i] ~ categorical(probs);
      total_skill[results_driver[r,i]] = 0; //as not needed for next iteration
    }
  }
  
}



generated quantities { //for marignal likelihood
  
  vector[N] probs;
  vector[N] total_skill;
  vector[sum(no_drivers) - m] log_lik;
  int count = 1;
  
  for (i in 1:m){ //cycle through races
    
    //filling in total skill for all the drivers in the race - the rest are set to 0 
    total_skill = rep_vector(0, N);
    for (r in 1:(no_drivers[i]-1)){
      total_skill[results_driver[r,i]] = exp(lambda[t[i],results_driver[r,i]] + 
      gamma[u[i], results_team[r,i]] + beta*failures[r,i]);
    }
    
    probs = rep_vector(0, N);
    for (r in 1:(no_drivers[i]-1)){ //each position
      probs = total_skill/sum(total_skill);
      log_lik[count] = categorical_lpmf(results_driver[r,i] | probs);
      //results_driver[r,i] ~ categorical(probs);
      total_skill[results_driver[r,i]] = 0; //as not needed for next iteration
      count = count + 1;
    }
  }
 
}



