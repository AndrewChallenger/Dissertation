library(dplyr)
library(rstan)
library(HDInterval)


load('data.RData')

m <- length(data$results_driver) #number of races
N <- length(data$key_drivers$driver_name) #number of drivers
S <- length(unique(data$key_constructors$constructor_id)) #number of teams 
T <- data$t[m] #number of time steps for divers
U <- data$u[m] #number of time steps for constructors
Nmax <- length(data$results_driver[,1]) #max number of drivers per race
M <- S^2
u <- data$u
t <- data$t
failures <- data$failures
no_drivers <- data$no_drivers


#Simulate z2

setwd("~/Oxford/Dissertation/PL/Model4/pl_model4")
#assumes 'samples' has already been saved - just posterior samples from model 4
load('samples')
names(samples) <- c('beta', 'lambda', 'gamma')

#just want final sample of relevant parameters
final_ind <- length(samples$beta)
beta <- samples$beta[final_ind]
lambda <- samples$lambda[final_ind,,]
U <- samples$gamma[final_ind,,]


#now simulate full dataset from these final samples

#for saving the results
sim_results_driver <- as.data.frame(matrix(0, nrow = nrow(data$results_driver), ncol = ncol(data$results_driver)))
sim_results_team <- as.data.frame(matrix(0, nrow = nrow(data$results_team), ncol = ncol(data$results_team)))

#function that simulates a race outcome using CRS likelihood, given parameter values 
sim_race <- function(beta, lambda_race, U_race, i #race index, i.e. from 1 to 160
                     ){
  
  #drivers teams and failures for race i
  race_drivers <- data$results_driver[,i][data$results_driver[,i] !=0]
  race_teams <- data$results_team[,i][data$results_team[,i] != 0]
  race_failures <- data$failures[,i]
  n_drivers <- data$no_drivers[i]
  
  #to save the race result
  race_result_drivers <- rep(0, length(race_failures))
  race_result_team <- rep(0, length(race_failures))
  
  #simulate race by applying the model 4 likelihood
  for (r in 1:(n_drivers-1)){
    total_skill <- rep(0, n_drivers - (r-1))
    count2 <- 1
    for (k in r:n_drivers){
      for (j in r:n_drivers){
        #interaction terms
        total_skill[count2] <- total_skill[count2] + U_race[u[i], (race_teams[k]-1)*S + race_teams[j]] 
      }
      pos <- which(race_drivers[k] == data$results_driver[,i]) #finishing position of k'th driver
      
      #driver and failire terms
      total_skill[count2] <- exp(total_skill[count2] + lambda_race[t[i], race_drivers[k]] + beta*race_failures[pos])
      count2 <- count2 + 1
    }
    probs <- total_skill/sum(total_skill)
    
    #simulate result
    race_result_drivers[r] <- sample(race_drivers[r:n_drivers], size = 1, prob = probs)
    
    pos_r <- which(race_drivers == race_result_drivers[r])
    race_result_team[r] <- race_teams[pos_r]
    
    #put driver that finished in position r in the right position
    temp <- race_drivers[r]
    race_drivers[r] <- race_drivers[pos_r]
    race_drivers[pos_r] <- temp
    
    #and for the teams
    temp <- race_teams[r]
    race_teams[r] <- race_teams[pos_winner]
    race_teams[pos_winner] <- temp
  }
  race_result_drivers[n_drivers] <- race_drivers[n_drivers]
  race_result_team[n_drivers] <- race_teams[n_drivers]
  
  return(list(race_result_drivers, race_result_team))
  
}

set.seed(26)

#simulating the full dataset
for (i in 1:ncol(data$results_driver)){
  race_outcome <- sim_race(beta, lambda, U, i)
  sim_results_driver[,i] <- race_outcome[[1]]
  sim_results_team[,i] <- race_outcome[[2]]
}

#creating a new list for the simulated data
sim_data <- data
sim_data$results_driver <- sim_results_driver
sim_data$results_team <- sim_results_team

#also save parameter values
sim_data$lambda <- lambda
sim_data$U <- U
sim_data$beta <- beta

#save the simulated data z2 so we can refit model 2 and model 4 to it
setwd("~/Oxford/Dissertation/PL/GOF/CRS")
save(sim_data, file = 'sim_data_CRS.RData')
















#Repeat the process for simulating z1

setwd("~/Oxford/Dissertation/PL/Model2/pl_model2")
load('samples')
#only get relevant samples
samples <- list(samples$beta, samples$lambda, samples$gamma)

names(samples) <- c('beta', 'lambda', 'gamma')

#just want final sample of relevant parameters
final_ind <- length(samples$beta)
beta <- samples$beta[final_ind]
lambda <- samples$lambda[final_ind,,]
gamma <- samples$gamma[final_ind,,]


#now simulate data from these final samples

sim_results_driver <- as.data.frame(matrix(0, nrow = nrow(data$results_driver), ncol = ncol(data$results_driver)))
sim_results_team <- as.data.frame(matrix(0, nrow = nrow(data$results_team), ncol = ncol(data$results_team)))


sim_race <- function(beta, lambda_race, gamma_race, i){
  
  race_drivers <- data$results_driver[,i][data$results_driver[,i] !=0]
  race_teams <- data$results_team[,i][data$results_team[,i] != 0]
  race_failures <- data$failures[,i]
  n_drivers <- data$no_drivers[i]
  race_result_drivers <- rep(0, length(race_failures))
  race_result_team <- rep(0, length(race_failures))
  
  
  for (r in 1:(n_drivers-1)){
    total_skill <- rep(0, n_drivers - (r-1))
    count2 <- 1
    for (k in r:n_drivers){
      pos <- which(race_drivers[k] == data$results_driver[,i])
      total_skill[count2] <- exp(lambda_race[t[i], race_drivers[k]] + gamma_race[u[i], race_teams[k]] + beta*race_failures[pos])
      count2 <- count2 + 1
    }
    probs <- total_skill/sum(total_skill)
    race_result_drivers[r] <- sample(race_drivers[r:n_drivers], size = 1, prob = probs)
    
    pos_winner <- which(race_drivers == race_result_drivers[r])
    race_result_team[r] <- race_teams[pos_winner]
    
    #put guy who won at front
    temp <- race_drivers[r]
    race_drivers[r] <- race_drivers[pos_winner]
    race_drivers[pos_winner] <- temp
    
    temp <- race_teams[r]
    race_teams[r] <- race_teams[pos_winner]
    race_teams[pos_winner] <- temp
  }
  race_result_drivers[n_drivers] <- race_drivers[n_drivers]
  race_result_team[n_drivers] <- race_teams[n_drivers]
  
  return(list(race_result_drivers, race_result_team))
  
}

set.seed(26)
for (i in 1:ncol(data$results_driver)){
  race_outcome <- sim_race(beta, lambda, gamma, i)
  sim_results_driver[,i] <- race_outcome[[1]]
  sim_results_team[,i] <- race_outcome[[2]]
}


sim_data <- data
sim_data$results_driver <- sim_results_driver
sim_data$results_team <- sim_results_team

#also save parameter values
sim_data$lambda <- lambda
sim_data$gamma <- gamma
sim_data$beta <- beta

#save z1
setwd("~/Oxford/Dissertation/PL/GOF/PL")
save(sim_data, file = 'sim_data_PL.RData')























