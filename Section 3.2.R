library(ggplot2)
library(dplyr)
library(gridExtra)
library(igraph)

#Load the data that was saved from the code from section 3.1
load('data.RData')

m <- length(data$results_driver) #number of races
N <- length(data$key_drivers$driver_name) #number of drivers
S <- length(unique(data$key_constructors$constructor_id)) #number of teams 
T <- data$t[m] #number of time steps for divers
U <- data$u[m] #number of time steps for constructors
Nmax <- length(data$results_driver[,1]) #max number of drivers per race


#Creating figure 3.1

#Teams that we want to plot
teams <- c('Ferrari','Mercedes', 'Red Bull')
team_ids <- data$key_constructors$constructor_id[data$key_constructors$constructor_name %in% teams]

#Finding proportion of podiums for each team
podiums <- data.frame(Team = rep(teams, T),team_id = rep(team_ids, T), t = rep(1:T, each = length(teams)), 
                      years = rep(2014:2021, each = length(teams)), prop_podiums = rep(0, T*length(teams)))
for (i in 1:nrow(podiums)){
  race_index <- data$t == podiums$t[i]
  results_season <- data$results_team[,race_index]
  
  count <- 0
  
  for (j in 1:ncol(results_season)){
    if (podiums$team_id[i] %in% results_season[1:3,j]){
      count <- count + 1
    }
  }
  podiums$prop_podiums[i] <- count/ncol(results_season)
}

#create plot for teams
pl1 <- ggplot(podiums) + geom_line(aes(x = years, y=prop_podiums, color = Team)) + 
  ylab('Proportion of races with podium finishes') + 
  xlab('Season') + ylim(0,1) + labs(title = 'Teams') + theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
  )



#Now for drivers
drivers <- c('bottas','hamilton', 'max_verstappen', 'rosberg', 'vettel')
driver_ids <- rep(0, length(drivers))
for (i in 1:length(drivers)){
  driver_ids[i] <- data$key_drivers$driver_id[data$key_drivers$driver_name == drivers[i]]
}


podiums <- data.frame(Driver = rep(c('Bottas','Hamilton','Verstappen', 'Rosberg', 'Vettel'), T),
                      driver_id = rep(driver_ids, T), t = rep(1:T, each = length(drivers)), 
                      years = rep(2014:2021, each = length(drivers)), prop_podiums = rep(0, T*length(drivers)))

for (i in 1:nrow(podiums)){
  race_index <- data$t == podiums$t[i]
  results_season <- data$results_driver[,race_index]
  
  count <- 0
  
  for (j in 1:ncol(results_season)){
    if (podiums$driver_id[i] %in% results_season[1:3,j]){
      count <- count + 1
    }
  }
  podiums$prop_podiums[i] <- count/ncol(results_season)
}

podiums$prop_podiums[podiums$Driver == 'Verstappen' & podiums$years == 2014] <- NA
podiums$prop_podiums[podiums$Driver == 'Rosberg' & podiums$years %in% 2017:2021] <- NA

pl2 <- ggplot(podiums) + geom_line(aes(x = years, y=prop_podiums, color = Driver)) + 
  ylab('Proportion of races with podium finishes') + 
  xlab('Season') + ylim(0,1) + labs(title = 'Drivers') + theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
  )

#create figure 3.1
grid.arrange(pl1, pl2, ncol = 2)








#Create figure 3.2

#make an incidence matrix to form the network - an edge is present if two drivers were 
#teammates at some point
inc_matrix <- matrix(0, nrow = N, ncol = N)

for (i in 1:ncol(data$results_driver)){
  for (j in unique(data$results_team[,i])){
    if (j > 0){
      inds <- which(data$results_team[,i] == j)
      if (length(inds) > 1){
        inc_matrix[data$results_driver[inds[1], i], data$results_driver[inds[2], i]] <- 1
      }
    }
  }
}


#create network
graph <- graph_from_adjacency_matrix(inc_matrix)
components <- clusters(graph)
layout <- layout_with_fr(graph)



# Plot figure 3.2 with lines (no arrows) and labels
plot(graph, layout = layout, vertex.label = NA,
     vertex.size = 10, vertex.label.cex = 0.7, 
     vertex.label.color = "black", vertex.label.dist = 0, vertex.label.degree = 1,
     edge.arrow.mode = 0, edge.color = "black", edge.width = 1.5, margin = 0)









