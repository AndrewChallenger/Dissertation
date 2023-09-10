setwd("~/Oxford/Dissertation/PL/Data")

load('data.RData')

library(dplyr)
library(bridgesampling)
library(loo)
library(ggplot2)
library(kableExtra)
library(knitr)
library(patchwork)
library(gridExtra)
library(HDInterval)

m <- length(data$results_driver) #number of races
N <- length(data$key_drivers$driver_name) #number of drivers
U <- data$u[m]
T <- data$t[m]
S <- length(unique(data$key_constructors$constructor_id)) #number of teams 
Nmax <- length(data$results_driver[,1]) #max number of drivers per race

setwd("~/Oxford/Dissertation/PL/GOF/PL/model2")

#Model PL z1
load('pl_model2_summary')
load('pl_model2_loglik')
model2_PL_sum <- as.data.frame(sum)
model2_PL_loglik <- log_lik

setwd("~/Oxford/Dissertation/PL/GOF/PL/model4")

#Model CRS z1
load('pl_model4_summary')
load('pl_model4_loglik')
model4_PL_sum <- as.data.frame(sum)
model4_PL_loglik <- log_lik

setwd("~/Oxford/Dissertation/PL/GOF/CRS/model2")

#Model PL z2
load('pl_model2_summary')
load('pl_model2_loglik')
model2_CRS_sum <- as.data.frame(sum)
model2_CRS_loglik <- log_lik
model2_CRS_oos <- oos

setwd("~/Oxford/Dissertation/PL/GOF/CRS/model4")

#Model CRS z2
load('pl_model4_summary')
load('pl_model4_loglik')
model4_CRS_sum <- as.data.frame(sum)
model4_CRS_loglik <- log_lik
model4_CRS_oos <- oos



#comparing marginal likelihood for simulation study - table 5.1
marg_lik <- round(c(model2_PL_loglik$logml, model4_PL_loglik$logml, model2_CRS_loglik$logml, model4_CRS_loglik$logml),1)

err <- round(100*c(error_measures(model2_PL_loglik)$cv, error_measures(model4_PL_loglik)$cv, 
                   error_measures(model2_CRS_loglik)$cv, error_measures(model4_CRS_loglik)$cv),2)

table_ml <- data.frame(marg_lik = marg_lik, err = err)
row.names(table_ml) <- c('Model 2 PL', 'Model 4 PL','Model 2 CRS', 'Model 4 CRS')
                         
colnames(table_ml) <- c('Estimate of marginal likelihood',  'Estimated percentage error')

kable(table_ml) %>% kable_styling()




#Figure 5.1

alpha <- 0.89 #for HPD set

#For figure 5.1

setwd("~/Oxford/Dissertation/PL/GOF/PL/model2")
load('samples') #samples from PL z1
names(samples) <- c('beta', 'lambda', 'gamma')

driver_names <- c('vettel','hamilton', 'max_verstappen')
driver_ids <- rep(0,length(driver_names))

for (i in 1:length(driver_names)){
  driver_ids[i] <- data$key_drivers$driver_id[data$key_drivers$driver_name == driver_names[i]]
}

seasons <- rep(2014:2021, length(driver_names))
names <- rep(driver_names, each = 8)
means <- rep(0, length(driver_names)*8)

lower <- rep(0, length(driver_names)*8) #for HPD upper and lower tails
upper <- rep(0, length(driver_names)*8)

#Compare to known values used to simulate data
setwd("~/Oxford/Dissertation/PL/GOF/PL")
load('sim_data_PL.RData')
real_lambdas <- sim_data$lambda
real_values <- rep(0, length(driver_names)*8)

for (i in 1:length(driver_names)){
  
  driver_data <- samples$lambda[,,driver_ids[i]]
  
  means[(((i-1)*8)+1):(i*8)] <- colMeans(driver_data)
  for (j in 1:8){
    lower[((i-1)*8)+j] <- hdi(driver_data[,j], credMass = alpha)[1] 
    upper[((i-1)*8)+j] <- hdi(driver_data[,j], credMass = alpha)[2] 
  }
  
  real_values[(((i-1)*8)+1):(i*8)] <- real_lambdas[,driver_ids[i]]
  
}

#Top of figure 5.1
driver_plot <- data.frame(Season = seasons, Mean = means, x = lower, y = upper, Driver = names, real = real_values)
driver_plot$Driver <- factor(driver_plot$Driver, levels = c('vettel', 'hamilton', 'max_verstappen'),
                             labels = c('Vettel', 'Hamilton', 'Verstappen'))

#Verstappen wasn't in F1 in 2014
driver_plot$Mean[driver_plot$Driver == 'Verstappen' & driver_plot$Season == 2014] <- NA
driver_plot$x[driver_plot$Driver == 'Verstappen' & driver_plot$Season == 2014] <- NA
driver_plot$y[driver_plot$Driver == 'Verstappen' & driver_plot$Season == 2014] <- NA
driver_plot$real[driver_plot$Driver == 'Verstappen' & driver_plot$Season == 2014] <- NA

pl <- ggplot(driver_plot, aes(x=Season)) + geom_line(aes(y=Mean, color = Driver)) +
  geom_ribbon(aes(ymin = x, ymax = y, fill = Driver), alpha = 0.2) + ylab('Lambda') +
  facet_grid(. ~ Driver,
             scales = 'free_x',
             switch = 'y',
             space = 'free',
  ) + 
  geom_line(aes(y = real, color = Driver), linetype=2) +
  theme(strip.placement = "outside",  # Position strip (driver name) outside the plot
        strip.background = element_blank()) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 16),
        strip.text = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10)) + ylim(-2,2) 

print(pl)



#Bottom of figure 5.1
alpha <- 0.89 #for HPD set

setwd("~/Oxford/Dissertation/PL/GOF/CRS/model4")
load('samples') #samples from CRS z2
names(samples) <- c('beta', 'lambda', 'gamma')

driver_names <- c('vettel','hamilton', 'max_verstappen')
driver_ids <- rep(0,length(driver_names))

for (i in 1:length(driver_names)){
  driver_ids[i] <- data$key_drivers$driver_id[data$key_drivers$driver_name == driver_names[i]]
}

seasons <- rep(2014:2021, length(driver_names))
names <- rep(driver_names, each = 8)
means <- rep(0, length(driver_names)*8)

lower <- rep(0, length(driver_names)*8) #for HPD upper and lower tails
upper <- rep(0, length(driver_names)*8)

#Known parameter values
setwd("~/Oxford/Dissertation/PL/GOF/CRS")
load('sim_data_CRS.RData')
real_lambdas <- sim_data$lambda
real_values <- rep(0, length(driver_names)*8)

for (i in 1:length(driver_names)){
  
  driver_data <- samples$lambda[,,driver_ids[i]]
  
  means[(((i-1)*8)+1):(i*8)] <- colMeans(driver_data)
  for (j in 1:8){
    lower[((i-1)*8)+j] <- hdi(driver_data[,j], credMass = alpha)[1] #quantile(driver_data[,j], alpha)
    upper[((i-1)*8)+j] <- hdi(driver_data[,j], credMass = alpha)[2] #quantile(driver_data[,j], 1-alpha)
  }
  
  real_values[(((i-1)*8)+1):(i*8)] <- real_lambdas[,driver_ids[i]]
  
}


driver_plot <- data.frame(Season = seasons, Mean = means, x = lower, y = upper, Driver = names, real = real_values)
driver_plot$Driver <- factor(driver_plot$Driver, levels = c('vettel', 'hamilton', 'max_verstappen'),
                             labels = c('Vettel', 'Hamilton', 'Verstappen'))

#Verstappen wasn't in F1 in 2014
driver_plot$Mean[driver_plot$Driver == 'Verstappen' & driver_plot$Season == 2014] <- NA
driver_plot$x[driver_plot$Driver == 'Verstappen' & driver_plot$Season == 2014] <- NA
driver_plot$y[driver_plot$Driver == 'Verstappen' & driver_plot$Season == 2014] <- NA
driver_plot$real[driver_plot$Driver == 'Verstappen' & driver_plot$Season == 2014] <- NA

#Bottom of figure 5.1

pl2 <- ggplot(driver_plot, aes(x=Season)) + geom_line(aes(y=Mean, color = Driver)) +
  geom_ribbon(aes(ymin = x, ymax = y, fill = Driver), alpha = 0.2) + ylab('Lambda') +
  facet_grid(. ~ Driver,
             scales = 'free_x',
             switch = 'y',
             space = 'free',
  ) + 
  geom_line(aes(y = real, color = Driver), linetype=2) +
  theme(strip.placement = "outside",  # Position strip (driver name) outside the plot
        strip.background = element_blank()) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 16),
        strip.text = element_blank(), axis.title = element_text(size = 14), axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) + ylim(-2,2)



print(pl2)

#Figure 5.1

grid.arrange(pl, pl2, nrow = 2)




#what proportion of known parameters are in HPS sets

#For PL z1

setwd("~/Oxford/Dissertation/PL/GOF/PL/model2")
load('samples')
names(samples) <- c('beta', 'lambda', 'gamma')

setwd("~/Oxford/Dissertation/PL/GOF/PL")
load('sim_data_PL.RData')
real_lambdas <- sim_data$lambda
real_gammas <- sim_data$gamma


#Lambda parameters

intervals_driver <- as.data.frame(matrix(0, nrow = N*T, ncol = 2))
count <- 1
count2 <- 0
for (i in 1:N){
  for (j in 1:T){
    intervals_driver[count,1] <- hdi(samples$lambda[,j,i], credMass = alpha)[1]
    intervals_driver[count,2] <- hdi(samples$lambda[,j,i], credMass = alpha)[2]
    if ((real_lambdas[j,i] < intervals_driver[count,2]) & (real_lambdas[j,i] > intervals_driver[count,1])){
      count2 <- count2 + 1
    }
    count <- count + 1
  }
}

# Gamma parameters

intervals_team <- as.data.frame(matrix(0, nrow = S*U, ncol = 2))
count <- 1
for (i in 1:S){
  for (j in 1:U){
    intervals_team[count,1] <- hdi(samples$gamma[,j,i], credMass = alpha)[1]
    intervals_team[count,2] <- hdi(samples$gamma[,j,i], credMass = alpha)[2]
    if ((real_gammas[j,i] < intervals_team[count,2]) & (real_gammas[j,i] > intervals_team[count,1])){
      count2 <- count2 + 1
    }
    count <- count + 1
  }
}

#Overall recovery
count2/(T*(N+S)) * 100 #81.1% recovery

# Same fot model CRS z2

setwd("~/Oxford/Dissertation/PL/GOF/CRS")
load('sim_data_CRS.RData')
real_lambdas <- sim_data$lambda


setwd("~/Oxford/Dissertation/PL/GOF/CRS/model4")
load('samples')
names(samples) <- c('beta', 'lambda', 'gamma')

setwd("~/Oxford/Dissertation/PL/GOF/CRS")
load('sim_data_CRS.RData')
real_lambdas <- sim_data$lambda
real_gammas <- sim_data$U

intervals_driver <- as.data.frame(matrix(0, nrow = N*T, ncol = 2))
count <- 1
count2 <- 0
for (i in 1:N){
  for (j in 1:T){
    intervals_driver[count,1] <- hdi(samples$lambda[,j,i], credMass = alpha)[1]
    intervals_driver[count,2] <- hdi(samples$lambda[,j,i], credMass = alpha)[2]
    if ((real_lambdas[j,i] < intervals_driver[count,2]) & (real_lambdas[j,i] > intervals_driver[count,1])){
      count2 <- count2 + 1
    }
    count <- count + 1
  }
}

#For U parameters

intervals_team <- as.data.frame(matrix(0, nrow = choose(S,2)*U, ncol = 2))
count <- 1
for (i in 1:(S-1)){
  for (k in (i+1):S){
    for (j in 1:U){
      intervals_team[count,1] <- hdi(samples$gamma[,j,(i-1)*S + k], credMass = alpha)[1]
      intervals_team[count,2] <- hdi(samples$gamma[,j,(i-1)*S + k], credMass = alpha)[2]
      if ((real_gammas[j,(i-1)*S + k] < intervals_team[count,2]) & (real_gammas[j,(i-1)*S + k] > intervals_team[count,1])){
        count2 <- count2 + 1
      }
      count <- count + 1
    }
  }
}

#Overall recovery
count2/(T*(N+choose(S,2))) * 100 #88.3% recovery






