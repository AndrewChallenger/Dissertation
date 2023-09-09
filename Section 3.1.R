
library(lubridate)
library(dplyr)
library(tidyverse)

#This function generates the data needed - for full data (2014-2021) type='full' otherwise for test data (2021-2022) 
#type = 'test'
get_data <- function(type){
  
  #load in required data from Ergast API dataset
  constructors <- read.csv('constructors.csv')
  drivers <- read.csv('drivers.csv')
  races <- read.csv('races.csv')
  results <- read.csv('results.csv')
  
  races$date <- ymd(races$date)
  if (type == 'full'){
    ids <- races$raceId[year(races$date) >= 2014 & year(races$date) <= 2021]
  }
  else {
    ids <- races$raceId[year(races$date) >= 2021]
  }
  
  #combining data and obtaining only relevant columns
  results <- subset(results, raceId %in% ids) 
  results <- merge(results, races, by = 'raceId')
  results <- merge(results, constructors, by = 'constructorId')
  results <- merge(results, drivers, by = 'driverId')
  results <- results %>% select(driverId, constructorId, raceId, position, year, name.y,
                                date, driverRef, statusId)
  results$statusId <- as.integer(results$statusId)
  results$date <- ymd(results$date)
  
  #correcting an error in the data
  k <- which(results$driverRef == 'kubica' & results$date == ymd('2019-10-13'))
  results[k,4] <- 17
  
  #seeing how many 'failures' before removing DNFs for table A.1
  statusid <- results %>% group_by(statusId) %>% summarise(count = length(statusId))
  
  #removing DNFs
  results <- results %>%  subset(position != '\\N') 
  
  #generating a key for the drivers and teams
  key_drivers <- data.frame(driver_name = unique(results$driverRef), 
                            driver_id = 1:length(unique(results$driverId)))
  key_constructors <- data.frame(constructor_name = sort(unique(results$name.y)), constructor_id = 
                                   1:length(unique(results$constructorId))) 
  
  #setting rebrended teams to be the same team
  if (type == 'full'){
    key_constructors$constructor_id[c(3,9,16)] <- 3 #renault alpine #lotus
    key_constructors$constructor_id[c(10,11)] <- 10#key_constructors$constructor_id[11] #virgin massuria
    key_constructors$constructor_id[c(4,7,14)] <- 4#key_constructors$constructor_id[13] #aston matrin racing point force india 
    key_constructors$constructor_id[c(17)] <- 1#key_constructors$constructor_id[9] #alpha romeo sauber
    key_constructors$constructor_id[c(18,2)] <- 2#key_constructors$constructor_id[18] #toro rosse alpha tauri
    key_constructors$constructor_id[c(15)] <- 7
    key_constructors$constructor_id[c(13)] <- 9
    key_constructors$constructor_id[c(19)] <- 11
  }
  
  
  #columns for time series element
  results$t <- 1
  results$u <- 1 #u is just the same as t
  step_t <- 365 #update driver skill every year for now
  step_u <- 365 #update car skill every year as well
  first_date_t <- min(results$date)
  first_date_u <- dmy('15-02-2014') #starting date
  last_date_t <- first_date_t + step_t
  last_date_u <- first_date_u + step_u
  
  count <- 1
  while (first_date_t < max(results$date)){
    results$t[results$date > first_date_t & results$date <= last_date_t] <- count
    first_date_t <- first_date_t + step_t
    last_date_t <- last_date_t + step_t
    count <- count + 1
  }
  count <- 1
  while (first_date_u < max(results$date)){
    results$u[results$date > first_date_u & results$date <= last_date_u] <- count
    first_date_u <- first_date_u + step_u
    last_date_u <- last_date_u + step_u
    count <- count + 1
  }
  
  results$failure <- ifelse(results$statusId %in% c(1,11:19), 0,1) #creates the 'failure' variable
  
  #number of drivers that finished each race
  results <- results %>% arrange(date)
  no_of_drivers <- results %>% group_by(raceId) %>% 
    summarise(no_drivers = length(raceId))
  
  
  #need a matrix of all race results for drivers and teams
  results_driver <- matrix(rep(0, max(no_of_drivers$no_drivers)*length(unique(results$raceId))), nrow = 
                             max(no_of_drivers$no_drivers))
  results_driver <- as.data.frame(results_driver)
  results_team <- matrix(rep(0, max(no_of_drivers$no_drivers)*length(unique(results$raceId))), nrow = 
                           max(no_of_drivers$no_drivers))
  results_team <- as.data.frame(results_team)
  
  
  #save values of t, u, date and failure for each race
  t <- rep(0, length(unique(results$raceId)))
  u <- rep(0, length(unique(results$raceId)))
  no_drivers2 <- rep(0, length(unique(results$raceId)))
  dates <- rep(ymd('1999-09-26'), length(unique(results$raceId)))
  #results$failure <- ifelse(results$statusId %in% c(1,11:19), 0,1) #covers finishing or being lapped
  failures <- matrix(rep(0, max(no_of_drivers$no_drivers)*length(unique(results$raceId))), nrow = 
                      max(no_of_drivers$no_drivers))
  failures <- as.data.frame(failures)
  
  
  results$position <- as.integer(results$position)
  
  
  #generating desired data
  count <- 1
  for (i in unique(results$raceId)){
    race <- subset(results, raceId == i)
    no_drivers <- no_of_drivers$no_drivers[no_of_drivers$raceId == i]
    for (j in 1:no_drivers){
      #drivers
      driver <- race$driverRef[race$position == j]
      driver_no <- key_drivers$driver_id[key_drivers$driver_name == driver]
      results_driver[j,count] <- driver_no
      #teams
      team <- race$name.y[race$position == j]
      team_no <- key_constructors$constructor_id[key_constructors$constructor_name == team]
      results_team[j, count] <- team_no
      #failure?
      failures[j, count] <- race$failure[race$driverRef == driver]
      
    }
    t[count] <- race$t[1]
    u[count] <- race$u[1]
    no_drivers2[count] <- length(race$position)
    dates[count] <- race$date[1]
    
    count <- count + 1
  }
  
  #get which teams were in each season
  teams_per_season <- as.data.frame(matrix(rep(0, 11*length(unique(results$year))), nrow = 11))
  for (season in unique(results$year)){
    races <- subset(results, year == season)
    teams <- unique(races$name.y)
    teams_no <- key_constructors$constructor_id[key_constructors$constructor_name %in% teams]
    teams_per_season[1:length(teams),season-2013] <- teams_no
  }
  
  #get the teams each driver was with for each season
  teams_per_driver <- data.frame()
  number_per_period <- c()
  T <- length(unique(t))
  
  for (i in 1:T){
    races_periodt <- subset(results, t == i)
    drivers_periodt <- key_drivers$driver_id[key_drivers$driver_name %in% unique(races_periodt$driverRef)] 
    teams_for_driver <- races_periodt %>% select(name.y, driverRef) %>% group_by(driverRef) %>%
      summarise(driver_team = unique(name.y)[1]) 
    #this gets the first team that each driver drove for in each time period instead of all the teams they drove for 
    
    for (j in 1:length(teams_for_driver$driverRef)){
      teams_for_driver$driverRef[j] <- key_drivers$driver_id[key_drivers$driver_name == teams_for_driver$driverRef[j]]
      teams_for_driver$driver_team[j] <- key_constructors$constructor_id[key_constructors$constructor_name == teams_for_driver$driver_team[j]]
    }
    
    teams_for_driver$t <- rep(i, length(teams_for_driver$driverRef))
    
    teams_per_driver <- rbind(teams_per_driver, teams_for_driver)
    number_per_period <- append(number_per_period, length(teams_for_driver$t))
    
  }
  
  teams_per_driver <- teams_per_driver %>% relocate(t, .before = driverRef)
  colnames(teams_per_driver) <- c('t', 'driver_id', 'constructor_id')
  teams_per_driver <- as.data.frame(teams_per_driver)
  
  
  #get the numer of drivers that drove for each team for each time-period
  S <- length(unique(key_constructors$constructor_id))
  no_drivers_per_team <- array(0, dim = c(T,S,1))
  for (i in 1:T){
    for (j in 1:S){
      no_drivers_per_team[i,j,1] <- length(subset(teams_per_driver, t==i & constructor_id == j)$t)
    }
  }
  #how big to make last element of drivers_per_team array
  Ndrivers_max <- max(no_drivers_per_team)
  
  #now get which drivers were part of each team
  drivers_per_team <- array(0, dim = c(T,S,Ndrivers_max))
  for (i in 1:T){
    for (j in 1:S){
      if (no_drivers_per_team[i,j,1] > 0){
        for (k in 1:no_drivers_per_team[i,j,1]){
          drivers_per_team[i,j,k] <- as.numeric(subset(teams_per_driver, t==i & constructor_id == j)$driver_id[k])
        }
      }
    }
  }
  
  
  #make into a list and save
  data <- list(results_driver = results_driver, results_team = results_team, failures = failures,
               no_drivers = no_drivers2, t = t, u = u, dates = dates, key_drivers = key_drivers,
               key_constructors = key_constructors, teams_per_season = teams_per_season,
                teams_per_driver = teams_per_driver, number_per_period = number_per_period,
               no_drivers_per_team = no_drivers_per_team, drivers_per_team = drivers_per_team)
  return(data)
}


data <- get_data('full')
save(data, file = 'data.RData')


#see how many failures are in data
statusid <- results %>% group_by(statusId) %>% summarise(count = length(statusId))











