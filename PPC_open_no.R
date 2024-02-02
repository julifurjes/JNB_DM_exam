# Building models
install.packages("R2jags")
library(tidyverse)
library(R2jags)
library(dplyr)
library(readxl)

set.seed(1989) #(Taylor's version)
nsubjects <- 98

### Using the no parent model ###

real_openness_choice <- calc_scores$bin_open # Assuming that this is the choice; open or closed

bin_open # Is the simulated openness choice

# Posteriors density of the parameters
par(mfrow=c(2,2))
plot(density(openinfo_qual$BUGSoutput$sims.list$theta))
plot(density(openinfo_qual$BUGSoutput$sims.list$beta_rel_no))
plot(density(openinfo_qual$BUGSoutput$sims.list$beta_ip_no))

#----------Posterior predictive checks of descriptive accuracy

# theta_q decides for each participant whether to be open or not
theta_q_post <- openinfo_qual$BUGSoutput$sims.list$theta_q_no

# Plot probability of openness for participants 81, 82, 83, and 84
par(mfrow=c(2,2))
plot(density(theta_q_post[,81]))
plot(density(theta_q_post[,82]))
plot(density(theta_q_post[,83]))
plot(density(theta_q_post[,84]))

# Which option will be chosen?
bin_open[81:84]

# Is this a good prediction?
for (i in c(81, 82, 83, 84)) {
  print(MPD(theta_q_post[,i]))
  print(bin_open[i])
}

# Predicting responses for all subjects
x_predict <- array(nsubjects)

for (p in 1:nsubjects) {
  
  p_predict <- MPD(theta_q_post[,p])
  
  x_predict[p] <- ifelse(p_predict>0.5, 1, 0)
}

# How well did our model do?
# Simulated data
sum(x_predict==bin_open)
x_predict==bin_open

# On real data
sum(x_predict==real_openness_choice)
x_predict==real_openness_choice
