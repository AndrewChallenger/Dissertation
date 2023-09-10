library(ggplot2)

#Likelihood of PL
pl_likelihood <- function(lam1, lam2){
  prob <- exp(lam1)/(exp(lam1)+exp(lam2))
  return(prob)
}

set.seed(26)
n <- 100000 #number of samples
lam1 <- rgamma(n, 2, 2)
lam2 <- rgamma(n, 2, 2)

#prob driver i beats driver j
probs <- as.data.frame(pl_likelihood(lam1, lam2))
colnames(probs) <- 'probs'

#figure 4.1
ggplot(probs, aes(x=probs)) +
  geom_density(fill = "#1E90FF", color = "black", alpha = 0.6) +
  theme_minimal() +
  labs(x = "Probability driver x_i beats driver x_j", y = "Prior density") + xlim(0,1)

#prior probability that the prob that driver i beats driver j is more than 0.95
length(probs$probs[probs$probs > 0.95])/n

