

load('data.RData')

library(dplyr)
library(ggplot2)
library(rstan)
library(patchwork)
library(gridExtra)
library(HDInterval)

m <- length(data$results_driver) #number of races
N <- length(data$key_drivers$driver_name) #number of drivers
U <- data$u[m]
T <- data$t[m]
S <- length(unique(data$key_constructors$constructor_id)) #number of teams 
Nmax <- length(data$results_driver[,1]) #max number of drivers per race


#load previously saved posterior samples, alternatively
# load('pl_model4_fit')
# samples <- extract(fit)
# samples <- list(samples$beta, samples$lambda, samples$gamma)

load('samples')
names(samples) <- c('beta', 'lambda', 'gamma')



#driver plot figure 5.3

alpha <- 0.89 #size of HPD set


driver_names <- c('vettel','hamilton', 'max_verstappen')
driver_ids <- rep(0,length(driver_names))

for (i in 1:length(driver_names)){
  driver_ids[i] <- data$key_drivers$driver_id[data$key_drivers$driver_name == driver_names[i]]
}

#saving data needed for plot
seasons <- rep(2014:2021, length(driver_names))
names <- rep(driver_names, each = 8)
means <- rep(0, length(driver_names)*8)

lower <- rep(0, length(driver_names)*8) #for HPD upper and lower tails
upper <- rep(0, length(driver_names)*8)

for (i in 1:length(driver_names)){
  
  driver_data <- samples$lambda[,,driver_ids[i]]
  
  means[(((i-1)*8)+1):(i*8)] <- colMeans(driver_data)
  for (j in 1:8){
    #HPD intervals
    lower[((i-1)*8)+j] <- hdi(driver_data[,j], credMass = alpha)[1] 
    upper[((i-1)*8)+j] <- hdi(driver_data[,j], credMass = alpha)[2] 
  }
  
}

driver_plot <- data.frame(Season = seasons, Mean = means, x = lower, y = upper, Driver = names)
driver_plot$Driver <- factor(driver_plot$Driver, levels = c('vettel', 'hamilton', 'max_verstappen'),
                             labels = c('Vettel', 'Hamilton', 'Verstappen'))

#Verstappen wasn't in F1 in 2014 so remove him from these seasons
driver_plot$Mean[driver_plot$Driver == 'Verstappen' & driver_plot$Season == 2014] <- NA
driver_plot$x[driver_plot$Driver == 'Verstappen' & driver_plot$Season == 2014] <- NA
driver_plot$y[driver_plot$Driver == 'Verstappen' & driver_plot$Season == 2014] <- NA

#create figure 5.3

pl <- ggplot(driver_plot, aes(x=Season)) + geom_line(aes(y=Mean, color = Driver)) +
  geom_ribbon(aes(ymin = x, ymax = y, fill = Driver), alpha = 0.2) + ylab('Posterior mean of lambda') +
  facet_grid(. ~ Driver,
             scales = 'free_x',
             switch = 'y',
             space = 'free',
  ) + 
  theme(strip.placement = "outside",  # Position strip (driver name) outside the plot
        strip.background = element_blank()) +
  theme(legend.position = "none",
        strip.text = element_text(size = 14), axis.title = element_text(size = 14), axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

print(pl)








#Now for the team plot - figure 5.4

team_names <- c('Mercedes', 'Red Bull', 'Ferrari')
team_ids <- rep(0,length(team_names))

for (i in 1:length(team_names)){
  team_ids[i] <- data$key_constructors$constructor_id[data$key_constructors$constructor_name == team_names[i]]
}

seasons <- rep(2014:2021, length(team_names))
names <- rep(team_names, each = 8)
means <- rep(0, length(team_names)*8)
lower <- rep(0, length(team_names)*8)
upper <- rep(0, length(team_names)*8)

for (i in 1:length(team_names)){
  
  team_data <- matrix(0, nrow = length(samples$beta), ncol = 8)
  
  team_index <- ((team_ids[i]-1)*S + 1):(team_ids[i]*S)
  
  #posterior of absolute utilities
  for (j in 1:8){
    team_index2 <- sort(team_index[data$teams_per_season[,j]])
    team_data_season <- samples$gamma[,j,team_index2]
    team_data[,j] <- rowSums(team_data_season)
  }
  
  means[(((i-1)*8)+1):(i*8)] <- colMeans(team_data)
  for (j in 1:8){
    lower[((i-1)*8)+j] <- hdi(team_data[,j], credMass = alpha)[1]
    upper[((i-1)*8)+j] <- hdi(team_data[,j], credMass = alpha)[2]
  }
  
}

teams_plot <- data.frame(Season = seasons, Mean = means, x = lower, y = upper, Team = names)
teams_plot$Team <- factor(teams_plot$Team)

#Creating figure 5.4

pl2 <- ggplot(teams_plot, aes(x=Season)) + geom_line(aes(y=Mean, color = Team)) +
  geom_ribbon(aes(ymin = x, ymax = y, fill = Team), alpha = 0.2) + ylab('Posterior mean of omega') +
  facet_grid(. ~ Team,
             scales = 'free_x',
             switch = 'y',
             space = 'free',
  ) + 
  theme(strip.placement = "outside",  # Position strip (driver name) outside the plot
        strip.background = element_blank()) +
  theme(legend.position = "none",
        strip.text = element_text(size = 14), axis.title = element_text(size = 14), axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

print(pl2)









# Now competitor plot figure A.2

driver_names <- c('vettel', 'hamilton', 'max_verstappen')
driver_ids <- rep(0,length(driver_names))

for (i in 1:length(driver_names)){
  driver_ids[i] <- data$key_drivers$driver_id[data$key_drivers$driver_name == driver_names[i]]
}

team_ids <- data$teams_per_driver[data$teams_per_driver$driver_id %in% driver_ids,]      

seasons <- rep(2014:2021, length(driver_names))
names <- rep(driver_names, each = 8)
means <- rep(0, length(driver_names)*8)
lower <- rep(0, length(driver_names)*8)
upper <- rep(0, length(driver_names)*8)

for (i in 1:length(driver_names)){
  
  driver_data <- samples$lambda[,,driver_ids[i]]
  team_index <- subset(team_ids, driver_id == driver_ids[i])
  team_index$constructor_id <- as.numeric(team_index$constructor_id)
  all_data <- matrix(0, nrow = length(driver_data[,1]), ncol = 8)
  
  #get team data
  for (j in 1:length(team_index$t)){
    team_inds <- ((team_index[j,3]-1)*S + 1):(team_index[j,3]*S)
    team_inds <- sort(team_inds[data$teams_per_season[,team_index$t[j]]])
    team_data <- samples$gamma[,team_index$t[j],team_inds]
    team_data <- rowSums(team_data)
    
    all_data[,team_index$t[j]] <- team_data
  }
  
  #competitor skills
  all_data <- all_data + driver_data
  
  means[(((i-1)*8)+1):(i*8)] <- colMeans(all_data)
  for (j in 1:8){
    lower[((i-1)*8)+j] <- hdi(all_data[,j], credMass = alpha)[1]
    upper[((i-1)*8)+j] <- hdi(all_data[,j], credMass = alpha)[2]
  }
  
}

comb_plot <- data.frame(Season = seasons, Mean = means, x = lower, y = upper, Driver = names)
comb_plot$Driver <- factor(comb_plot$Driver, levels = c('vettel', 'hamilton', 'max_verstappen'),
                           labels = c('Vettel + team', 'Hamilton + team', 'Verstappen + team'))
comb_plot$Mean[comb_plot$Driver == 'Verstappen + team' & comb_plot$Season == 2014] <- NA
comb_plot$x[comb_plot$Driver == 'Verstappen + team' & comb_plot$Season == 2014] <- NA
comb_plot$y[comb_plot$Driver == 'Verstappen + team' & comb_plot$Season == 2014] <- NA

#Figure A.2

pl4 <- ggplot(comb_plot, aes(x=Season)) + geom_line(aes(y=Mean, color = Driver)) +
  geom_ribbon(aes(ymin = x, ymax = y, fill = Driver), alpha = 0.2) + ylab('Posterior mean of lambda+omega') +
  facet_grid(. ~ Driver,
             scales = 'free_x',
             switch = 'y',
             space = 'free',
  ) + 
  theme(strip.placement = "outside",  
        strip.background = element_blank()) +
  theme(legend.position = "none",
        strip.text = element_text(size = 14), axis.title = element_text(size = 14), axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

print(pl4)






#Now some interaction terms figure 5.5

pairs_names <- data.frame(pair1 = c('Mercedes', 'Ferrari'), pair2 = c('Mercedes', 'Red Bull'), pair3 = c('Mercedes', 'Williams'))
pairs_ids <- data.frame(matrix(rep(0,length(pairs_names)*length(pairs_names$pair1)), nrow = length(pairs_names$pair1)))

for (i in 1:length(pairs_names)){
  for (j in 1:length(pairs_names$pair1)){
    pairs_ids[j,i] <- data$key_constructors$constructor_id[data$key_constructors$constructor_name == pairs_names[j,i]]
  }
}

seasons <- rep(2014:2021, length(team_names))
names <- rep(paste(pairs_names[1,], pairs_names[2,], sep = ":"), each = 8)
means <- rep(0, length(team_names)*8)
lower <- rep(0, length(team_names)*8)
upper <- rep(0, length(team_names)*8)

for (i in 1:length(pairs_names)){
  
  pairs_index <- (pairs_ids[1,i]-1)*S + pairs_ids[2,i]
  pairs_data <- samples$gamma[,,pairs_index] 
  
  means[(((i-1)*8)+1):(i*8)] <- colMeans(pairs_data)
  for (j in 1:8){
    lower[((i-1)*8)+j] <- hdi(pairs_data[,j], credMass = alpha)[1]
    upper[((i-1)*8)+j] <- hdi(pairs_data[,j], credMass = alpha)[2]
  }
  
}

pairs_plot <- data.frame(Season = seasons, Mean = means, x = lower, y = upper, Teams = names)
pairs_plot$Teams <- factor(pairs_plot$Teams)

#Figure 5.5

pl3 <- ggplot(pairs_plot, aes(x=Season)) + geom_line(aes(y=Mean, color = Teams)) +
  geom_ribbon(aes(ymin = x, ymax = y, fill = Teams), alpha = 0.2) + ylab('Posterior mean of interaction') +
  facet_grid(. ~ Teams,
             scales = 'free_x',
             switch = 'y',
             space = 'free',
  ) + 
  theme(strip.placement = "outside",  # Position strip (driver name) outside the plot
        strip.background = element_blank()) +
  theme(legend.position = "none",
        strip.text = element_text(size = 14), axis.title = element_text(size = 14), axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

print(pl3)







#Creating figure 5.6 - so need to simulate toy example race for cases 1 and 2


#For competitors in team_ids get prob of Hamilton beating both Vettel and Riakonnen 
#for posterior sample with index 'sample_number'
get_prob <- function(teams_ids, sample_number){
  #get posterior samples
  
  num_drivers <- nrow(team_ids)
  
  M <- 1000 #number of races to simulate
  results <- rep(0, M)
  for (m in 1:M){
    #simulate race outcome using model 4
    
    drivers_in_race <- as.numeric(team_ids$driver_id)
    teams_in_race <- as.numeric(team_ids$constructor_id)
    race_outcome <- rep(0,num_drivers)
    for (i in 1:(num_drivers-1)){ #drivers left in race
      skills <- rep(0, num_drivers-(i-1))
      for (j in 1:length(drivers_in_race)){#skill for each driver in race
        for (k in 1:length(drivers_in_race)){#team skill
          u_jk_index <- (teams_in_race[j]-1)*S + teams_in_race[k]
          skills[j] <- skills[j] + samples$gamma[sample_number,2,u_jk_index]
        }
        skills[j] <- skills[j] + samples$lambda[sample_number,2,drivers_in_race[j]] #driver skill
        
      }
      skills <- exp(skills)
      probs <- skills/sum(skills)
      
      #simulate race
      next_driver <- sample(1:length(drivers_in_race), size = 1, prob = probs)
      race_outcome[i] <- drivers_in_race[next_driver]
      drivers_in_race <- drivers_in_race[-next_driver]
      teams_in_race <- teams_in_race[-next_driver]
    }
    race_outcome[num_drivers] <- drivers_in_race
    
    #see if Hamilton beats both ferrari's
    index_ham <- which(race_outcome == 1)
    index_vet <- which(race_outcome == 9)
    index_rai <- which(race_outcome == 4)
    
    if (index_ham < index_vet & index_ham < index_rai){
      results[m] <- 1
    }
    
  }
  #find prob
  return(mean(results))
  
}



#Case 2

driver_names <- c('hamilton','vettel', 'raikkonen', 'ricciardo', 'alonso')
driver_ids <- rep(0,length(driver_names))

for (i in 1:length(driver_names)){
  driver_ids[i] <- data$key_drivers$driver_id[data$key_drivers$driver_name == driver_names[i]]
}

#Teams that these drivers were with in 2015
team_ids <- data$teams_per_driver[data$teams_per_driver$driver_id %in% driver_ids,] 
team_ids <- subset(team_ids, t == 2)

no_samples <- length(samples$beta)
samples_inds <- sample(1:no_samples, size = 1000) #1000 samples
post_probs <- rep(0, length(samples_inds))

#simulate race 1000 times
count <- 1
for (sample in samples_inds){
  post_probs[count] <- get_prob(team_ids, sample)
  count <- count+1
}

print(mean(post_probs)) #posterior mean for probability that Hamilton beats both Ferrari's

#get plot of posterior probs - bottom of figure 5.6
plot_data <- as.data.frame(post_probs)

pl1 <- ggplot(data = plot_data, aes(x = post_probs)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.6) +
  theme_minimal() +
  labs(x = "Probability Hamilton beats both Vettel and Raikkonen", y = "Posterior density") + xlim(0,1)





#Case 2

driver_names <- c('hamilton','vettel', 'raikkonen', 'ricciardo', 'alonso', 'perez')
driver_ids <- rep(0,length(driver_names))

for (i in 1:length(driver_names)){
  driver_ids[i] <- data$key_drivers$driver_id[data$key_drivers$driver_name == driver_names[i]]
}


team_ids <- data$teams_per_driver[data$teams_per_driver$driver_id %in% driver_ids,] 
team_ids <- subset(team_ids, t == 2)

no_samples <- length(samples$beta)
samples_inds <- sample(1:no_samples, size = 1000)#1000 samples
post_probs <- rep(0, length(samples_inds))

count <- 1
for (sample in samples_inds){
  post_probs[count] <- get_prob(team_ids, sample)
  count <- count+1
}

print(mean(post_probs))

#get plot of posterior probs - top of figure 5.6
plot_data <- as.data.frame(post_probs)

pl2 <- ggplot(data = plot_data, aes(x = post_probs)) +
  geom_density(fill = "#1E90FF", color = "black", alpha = 0.6) +
  theme_minimal() +
  labs(x = "Probability Hamilton beats both Vettel and Raikkonen", y = "Posterior density") + xlim(0,1)



#Create figure 5.6
grid.arrange(pl2,pl1,nrow=2)












#trace plots - figure A.1
load('pl_model4_fit')
pl <- traceplot(fit, pars = c('lambda[1,1]', 'lambda[6,9]', 'gamma[2,37]', 'gamma[8,50]'))

levels(pl$data$parameter)[3] <- 'u[2,4,1]'
levels(pl$data$parameter)[4] <- 'u[8,5,2]'

print(pl)






