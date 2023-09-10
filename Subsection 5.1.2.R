setwd("~/Oxford/Dissertation/PL/Data")

load('data.RData')

library(dplyr)
library(bridgesampling)
library(loo)
library(ggplot2)
library(kableExtra)
library(knitr)

m <- length(data$results_driver) #number of races
N <- length(data$key_drivers$driver_name) #number of drivers
U <- data$u[m]
T <- data$t[m]
S <- length(unique(data$key_constructors$constructor_id)) #number of teams 
Nmax <- length(data$results_driver[,1]) #max number of drivers per race

setwd("~/Oxford/Dissertation/PL/Model1/pl_model1")

load('pl_model1_summary')
load('pl_model1_loglik')
model1_sum <- data.frame(sum)
model1_loglik <- log_lik

setwd("~/Oxford/Dissertation/PL/Model2/pl_model2")

load('pl_model2_summary')
load('pl_model2_loglik')
model2_sum <- data.frame(sum)
model2_loglik <- log_lik

setwd("~/Oxford/Dissertation/PL/Model2/pl_model2_alt")

load('pl_model2_summary')
load('pl_model2_loglik')
model2_alt_sum <- data.frame(sum)
model2_alt_loglik <- log_lik

setwd("~/Oxford/Dissertation/PL/Model3/pl_model3")

load('pl_model3_summary')
load('pl_model3_loglik')
model3_sum <- data.frame(sum)
model3_loglik <- log_lik


setwd("~/Oxford/Dissertation/PL/Model4/pl_model4")

load('pl_model4_summary')
load('pl_model4_loglik')
model4_sum <- data.frame(sum)
model4_loglik <- log_lik


#comparing marginal likelihood - ignore model 4 alt
marg_lik <- round(c(model1_loglik$logml, model2_loglik$logml, model2_alt_loglik$logml, model3_loglik$logml, model4_loglik$logml,
              model4_alt_loglik$logml),1)

#percentage errors
err <- round(100*c(error_measures(model1_loglik)$cv, error_measures(model2_loglik)$cv, error_measures(model2_alt_loglik)$cv,
         error_measures(model3_loglik)$cv, error_measures(model4_loglik)$cv, error_measures(model4_alt_loglik)$cv
         ),2)

table_ml <- data.frame(marg_lik = marg_lik, err = err)
row.names(table_ml) <- c('Model 1 (fixed-time PL)', 'Model 2 (dynamic PL)','Model 2 alt',
                         'Model 3 (fixed-time CRS)', 'Model 4 (dynamic CRS)', 'Model 4 alt'
                         )
colnames(table_ml) <- c('Estimate of marginal likelihood',  'Estimated percentage error')

#table 5.2
kable(table_ml) %>% kable_styling()










