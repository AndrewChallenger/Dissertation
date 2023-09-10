

library(rstan)
library(dplyr)
library(bridgesampling)
library(loo)

options(mc.cores = parallel::detectCores())

load('data.RData')

m <- length(data$results_driver) #number of races
N <- length(data$key_drivers$driver_name) #number of drivers
S <- length(unique(data$key_constructors$constructor_id)) #number of teams 
T <- data$t[m] #number of time steps for divers
U <- data$u[m] #number of time steps for constructors
Nmax <- length(data$results_driver[,1]) #max number of drivers per race
M <- S^2

#hyperparameters for AR(1) variances
a <- 2
b <- 2
c <- 3
d <- 25
#for beta
e <- -5
f <- 1

stan_data <- list(m=m, N=N, S=S ,U=U,T=T, Nmax=Nmax, results_driver = data$results_driver, 
                  results_team = data$results_team, failures = data$failures, no_drivers = data$no_drivers,
                  u = data$u,t=data$t,a=a,b=b, c=c, d=d, e=e, f=f, M=M)

iters = 52000
burnin = 2000

fit <- stan(file = 'pl_model4.stan', data = stan_data, iter = iters, warmup = burnin, seed = 26)

save(fit, file = 'pl_model4_fit')

sum <- rstan::summary(fit)$summary
save(sum, file = 'pl_model4_summary')

#Marginal likelihood
log_lik <- bridge_sampler(fit)
save(log_lik, file = 'pl_model4_loglik')






