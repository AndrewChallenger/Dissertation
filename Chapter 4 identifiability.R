library(Matrix)

load('data.RData')

m <- length(data$results_driver) #number of races
N <- length(data$key_drivers$driver_name) #number of drivers
S <- length(unique(data$key_constructors$constructor_id)) #number of teams 
T <- data$t[m] #number of time steps for divers
U <- data$u[m] #number of time steps for constructors
Nmax <- length(data$results_driver[,1]) #max number of drivers per race

results_driver <- data$results_driver
results_team <- data$results_team
no_drivers <- data$no_drivers


#get matrix from identitifability proof for model1
model1_mtx <- matrix(0, nrow = sum(no_drivers)+2, ncol = N+S)
count <- 1

#fill in driver-team pairings - no need for only unique pairings as the rank won't change
for (i in 1:length(no_drivers)){
  for (j in 1:no_drivers[i]){
    model1_mtx[count,results_driver[j,i]] <- 1
    model1_mtx[count,N+results_team[j,i]] <- 1
    count <- count + 1
  }
}
#final two rows for centering
model1_mtx[2713, 1:N] <- 1
model1_mtx[2714, (N+1):(N+S)] <- 1

rankMatrix(model1_mtx) == N+S #identifiable





#now rank for each season for model 2
model2_ranks <- rep(0, T) #the ranks for each season
model2_fullranks <- rep(0,T) #the full ranks for each season

count <- 1
for (t in 1:T){
  count2 <- 1
  no_races <- length(data$t[data$t == t])
  
  #rsults for season t
  results_driver_season <- results_driver[,count:(count+no_races-1)]
  results_team_season <- results_team[,count:(count+no_races-1)]
  no_drivers_season <- no_drivers[count:(count+no_races-1)]
  
  #the A matrix for each season
  model2_mtx <- matrix(0, nrow = sum(no_drivers_season)+2, ncol = N+S)
  
  for (i in 1:length(no_drivers_season)){
    for (j in 1:no_drivers_season[i]){
      model2_mtx[count2, results_driver_season[j,i]] <- 1
      model2_mtx[count2, N+results_team_season[j,i]] <- 1
      count2 <- count2+1
    }
  }
  count <- count + no_races
  
  # count zero columns
  non_zero_cols <- colSums(model2_mtx != 0) > 0
  model2_fullranks[t] <- length(non_zero_cols[non_zero_cols == TRUE]) #only want to learn parameters that are seen in likelihood
  
  #last two rows for centering
  model2_mtx[nrow(model2_mtx) - 1, 1:N] <- 1
  model2_mtx[nrow(model2_mtx), (N+1):(N+S)] <- 1
  
  #get rid of zero columns before calculating rank
  model2_mtx <- model2_mtx[,non_zero_cols]
  
 model2_ranks[t] <- rankMatrix(model2_mtx)

}

model2_fullranks == model2_ranks #nothing identifiable!



#now for model2_alt
model2_ranks <- rep(0, T)
model2_fullranks <- rep(0,T)
count <- 1
for (t in 1:T){
  count2 <- 1
  no_races <- length(data$t[data$t == t])
  
  results_driver_season <- results_driver[,count:(count+no_races-1)]
  results_team_season <- results_team[,count:(count+no_races-1)]
  no_drivers_season <- no_drivers[count:(count+no_races-1)]
  
  model2_mtx <- matrix(0, nrow = sum(no_drivers_season)+S+1, ncol = N+S)
  
  for (i in 1:length(no_drivers_season)){
    for (j in 1:no_drivers_season[i]){
      model2_mtx[count2, results_driver_season[j,i]] <- 1
      model2_mtx[count2, N+results_team_season[j,i]] <- 1
      count2 <- count2+1
    }
  }
  count <- count + no_races
  
  #centering as in model2 alt
  count2 <- nrow(model2_mtx) - S
  for (i in 1:S){
    if (data$no_drivers_per_team[t,i,1] > 0){
      for (j in 1:data$no_drivers_per_team[t,i,1]){
        model2_mtx[count2, data$drivers_per_team[t,i,j]] <- 1
      }
    }
    count2 <- count2 + 1
  }
  
  # count zero columns
  non_zero_cols <- colSums(model2_mtx != 0) > 0
  model2_fullranks[t] <- length(non_zero_cols[non_zero_cols == TRUE])
  
  
  #still zero sum gammas
  model2_mtx[nrow(model2_mtx), (N+1):(N+S)] <- 1
  
  #get rid of zero columns before calculating rank
  model2_mtx <- model2_mtx[,non_zero_cols]
  
  model2_ranks[t] <- rankMatrix(model2_mtx)
  
}

model2_fullranks == model2_ranks #everything identifiable!








#now model 3 

rows_per_race <- rep(0, length(no_drivers))
for (i in 1:length(rows_per_race)){
  count <- 0
  for (j in 1:(no_drivers[i]-1)){
    count <- count + (j+1)
  }
  rows_per_race[i] <- count
}


model3_mtx <- matrix(0, nrow = sum(rows_per_race)+1, ncol = N+choose(S,2))
count <- 1

for (i in 1:length(no_drivers)){ #go through the races
  for (j in 1:(no_drivers[i]-1)){ #for each race go through the drivers
  
    teams_left <- results_team[j:no_drivers[i],i]
    for (k in 1:length(teams_left)){
      
      #pairwise indices
      for (r in 1:length(teams_left)){
        team1 <- teams_left[k]
        team2 <- teams_left[r]
        
        ind <- N + (min(team1, team2)-1)*S - ifelse(min(team1, team2) == 1, 0, sum(1:(min(team1, team2)-1))) + max(team1, team2) - min(team1, team2)
        if (team1 < team2){
          model3_mtx[count, ind] <- model3_mtx[count, ind] + 1
        }
        else if (team1 > team2){
          model3_mtx[count, ind] <- model3_mtx[count, ind] - 1
        }
      }
      #driver indices
      model3_mtx[count,results_driver[k + (j-1),i]] <- 1
      count <- count + 1
    }
    
  }
}

#zero sum lambdas
model3_mtx[nrow(model3_mtx), 1:N] <- 1

rankMatrix(model3_mtx) == N + choose(S,2) 
#FALSE which makes it seem unidentifiable, however...

which(apply(model3_mtx, 2, function(col) all(col == 0)))
#there's a parameter between 5 and 8 e.g. Caterham and Haas - as Caterham and Haas never raced each other this 
#parameter is what is unidentitiable but that's fine as we don't care about it anyway 
#- so when ignoring this model 3 is identifiable, i.e.

rankMatrix(model3_mtx) == N + choose(S,2) -1
#TRUE so identifiable!





#for model 4

model4_ranks <- rep(0, T)
model4_fullranks <- rep(0,T)
count <- 1
for (t in 1:T){
  count2 <- 1
  no_races <- length(data$t[data$t == t])
  
  #Results for season t
  results_driver_season <- results_driver[,count:(count+no_races-1)]
  results_team_season <- results_team[,count:(count+no_races-1)]
  no_drivers_season <- no_drivers[count:(count+no_races-1)]
  rows_per_race_season <- rows_per_race[data$t == t]
  
  model4_mtx <- matrix(0, nrow = sum(rows_per_race_season)+1, ncol = N+choose(S,2))
  
  for (i in 1:length(no_drivers_season)){ #go through the races
    for (j in 1:(no_drivers_season[i]-1)){#for each race go through the drivers
      
      teams_left <- results_team_season[j:no_drivers_season[i],i]
      for (k in 1:length(teams_left)){
        for (r in 1:length(teams_left)){
          team1 <- teams_left[k]
          team2 <- teams_left[r]
          
          ind <- N + (min(team1, team2)-1)*S - ifelse(min(team1, team2) == 1, 0, sum(1:(min(team1, team2)-1))) + max(team1, team2) - min(team1, team2)
      
          if (team1 < team2){
            model4_mtx[count2, ind] <- model4_mtx[count2, ind] + 1
          }
          else if (team1 > team2){
            model4_mtx[count2, ind] <- model4_mtx[count2, ind] - 1
          }
        }
        model4_mtx[count2,results_driver_season[k + (j-1),i]] <- 1
        count2 <- count2 + 1
      }
      
    }
  }
  
  count <- count + no_races
  
  # count zero columns
  non_zero_cols <- !apply(model4_mtx, 2, function(col) all(col == 0))
  model4_fullranks[t] <- length(non_zero_cols[non_zero_cols == TRUE])
  
  #center lambdas
  model4_mtx[nrow(model4_mtx), 1:N] <- 1
  
  #remove zero columns 
  model4_mtx <- model4_mtx[, non_zero_cols]
  
  model4_ranks[t] <- rankMatrix(model4_mtx)
  
}

model4_ranks == model4_fullranks
#TRUE so identifiable!








