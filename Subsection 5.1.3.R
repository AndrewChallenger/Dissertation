library(dplyr)
library(rstan)
library(HDInterval)
library(ggplot2)


load('data.RData')

m <- length(data$results_driver) #number of races
N <- length(data$key_drivers$driver_name) #number of drivers
S <- length(unique(data$key_constructors$constructor_id)) #number of teams 
T <- data$t[m] #number of time steps for divers
U <- data$u[m] #number of time steps for constructors
Nmax <- length(data$results_driver[,1]) #max number of drivers per race
M <- S^2
u <- data$u

points_per_race <- c(25,18,15,12,10,8,6,4,2,1) #number of points scored for each finishing position


#load previously saved posterior samples, alternatively
# load('pl_model4_fit')
# samples <- extract(fit)
# samples <- list(samples$beta, samples$lambda, samples$gamma)

load('samples')
names(samples) <- c('beta', 'lambda', 'gamma')

set.seed(26)

no_smpls <- 2000 #how many samples to take
total_smpls <- length(samples$beta) #how many samples there are in total
ind <- sample(1:total_smpls, size = no_smpls) #random samples 

#take the posterior samples
gamma_smpls <- samples$gamma[ind,,]
lambda_smpls <- samples$lambda[ind,,]
beta_smpls <- samples$beta[ind]

#matrix to store the points for each sample
points_store <- matrix(rep(0,N*no_smpls), nrow = N)

#Simulate 2021 season
for (n in 1:no_smpls){
  #n'th sample
  
  #n'th posterior sample
  test_gamma <- gamma_smpls[n,,]
  test_lambda <- lambda_smpls[n,,]
  test_beta <- beta_smpls[n]
  
  points <- rep(0,N)
  
  
  for (i in 139:160){ #races in 2021 season
    
    #which drivers and teams were in the race
    test_drivers <- data$results_driver[,i][data$results_driver[,i] !=0]
    test_teams <- data$results_team[,i][data$results_team[,i] != 0]
    test_failures <- data$failures[,i]
    n_drivers <- data$no_drivers[i]
    
    #this is result of i'th race that we will simulate
    rank <- rep(0, n_drivers)
    
    #Simulating with the CRS likelihood
    for (r in 1:(n_drivers-1)){
      total_skill <- rep(0, n_drivers - (r-1))
      count2 <- 1
      for (k in r:n_drivers){
        for (j in r:n_drivers){
          #pairwise interaction terms of teams remaining in race
          total_skill[count2] <- total_skill[count2] + test_gamma[u[i], (test_teams[k]-1)*S + test_teams[j]] 
        }
        pos <- which(test_drivers[k] == data$results_driver[,i]) #finishing position of k'th driver
        
        #full skill of competitor
        total_skill[count2] <- exp(total_skill[count2] + test_lambda[8, test_drivers[k]] + test_beta*test_failures[pos])
        count2 <- count2 + 1
      }
      probs <- total_skill/sum(total_skill)
      
      #simulate r'th position
      rank[r] <- sample(test_drivers[r:n_drivers], size = 1, prob = probs)
      
      pos_r <- which(test_drivers == rank[r]) #driver that finished in position r
      
      #put driver in correct place
      temp <- test_drivers[r]
      test_drivers[r] <- test_drivers[pos_r]
      test_drivers[pos_r] <- temp
      
      #put team in correct place
      temp <- test_teams[r]
      test_teams[r] <- test_teams[pos_r]
      test_teams[pos_r] <- temp
    }
    
    #assign points for race half points for spa
    if (i == 150){
      for (k in 1:10){
        points[rank[k]] <- points[rank[k]] + points_per_race[k]/2
      }
    }
    else{
      for (k in 1:10){
        points[rank[k]] <- points[rank[k]] + points_per_race[k]
      }
    }
    
  }
  points_store[,n] <- points
}

#Create figure 5.2
all_2021_results <- c(as.matrix(data$results_driver[,139:160]))
drivers_21 <- unique(all_2021_results)
drivers_21 <- drivers_21[drivers_21 != 0] 
driver_names <- rep(0, length(drivers_21))
for (i in 1:length(drivers_21)){
  driver_names[i] <- data$key_drivers$driver_name[data$key_drivers$driver_id == drivers_21[i]]
}

drivers_21 <- data.frame(drivers = driver_names, driver_id = drivers_21)

#Actual number of points
drivers_21$points <- c(387.5, 395.5, 226, 160, 190, 159, 115, 164.5, 32, 34, 10, 3, 74, 16, 43, 0, 110, 7, 81, 0, 0)

#fixing fastest laps and sprint
#bottas 10
#hamilton 8
#max 12
#gasly 1
#ricciardo 2
#norris 1
#perez 1
#sainz 1
drivers_21$points[1] <- 379.5
drivers_21$points[2] <- 383.5
drivers_21$points[3] <- 216
drivers_21$points[4] <- 159
drivers_21$points[5] <- 189
drivers_21$points[7] <- 113
drivers_21$points[8] <- 163.5
drivers_21$points[1] <- 379.5

#Get means and HPD sets
points_sum <- matrix(rep(0,3*length(drivers_21$drivers)), ncol = 3)
points_store <- points_store[drivers_21$driver_id,]
for (i in 1:length(drivers_21$drivers)){
  points_sum[i,2] <- mean(points_store[i,])
  points_sum[i,1] <- hdi(points_store[i,], credMass = 0.89)[1]
  points_sum[i,3] <- hdi(points_store[i,], credMass = 0.89)[2]
}

points_sum <- data.frame(points_sum)
colnames(points_sum) <- c('lower', 'mean', 'upper')
points_sum$drivers <- drivers_21$drivers

result_21 <- merge(drivers_21, points_sum, by = 'drivers')

result_21 <- select(result_21, -driver_id) 
result_21 <- arrange(result_21, desc(mean))

#Figure 5.2

pl <- ggplot(result_21, aes(x=drivers)) + geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin = lower, ymax = upper)) + ylab('Points') +
  geom_point(aes(y = points), shape = 4, color = 'blue', size = 3) +
  xlab('Drivers') +
   theme(axis.title.y = element_text(size = 16), axis.title.x = element_text(size = 16),
       axis.text.y = element_text(size = 14),
       axis.text.x = element_text(size = 14, angle = 45, hjust = 1))  + 
  scale_x_discrete(limits = result_21 %>% arrange(desc(mean)) %>% pull(drivers))

print(pl)






























