library(rstan)
library(dplyr)
library(bridgesampling)
library(loo)

options(mc.cores = parallel::detectCores())

load('data.RData')

m <- length(data$results_driver) #number of races
N <- length(data$key_drivers$driver_name) #number of drivers
S <- length(unique(data$key_constructors$constructor_id)) #number of teams 
U <- data$u[m] #number of time steps for constructors
T <- data$t[m]
Nmax <- length(data$results_driver[,1]) #max number of drivers per race

#hyperparameters for AR(1) process variances
a <- 2
b <- 2 
c <- 2
d <- 2 

#mean and variance of beta
e <- -5
f <- 1

iters = 52000
burnin = 2000


stan_data <- list(m=m, N=N, S=S, T=T ,U=U, Nmax=Nmax, results_driver = data$results_driver, 
                  results_team = data$results_team, failures = data$failures, no_drivers = data$no_drivers,
                  t = data$t, u = data$u, a=a, b=b, c=c, d=d, e=e,f=f)


fit <- stan(file = 'pl_model2.stan', data = stan_data, iter = iters, warmup = burnin, seed = 26)

save(fit, file = 'pl_model2_fit')

sum <- rstan::summary(fit)$summary
save(sum, file = 'pl_model2_summary')

#Marginal likelihood
log_lik <- bridge_sampler(fit)
save(log_lik, file = 'pl_model2_loglik')
