## Mother ##

install.packages("pacman")
pacman::p_load(tidyverse, R2jags, dplyr, readxl, parallel, polspline)

# Setting the working directory
setwd('/work/JNB exam/The_good_stuff')
set.seed(1989) #(Taylor's version)

# Maximum Posterior Density function
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

nruns <- 100 # should be 100 for good stuff, but using 5 while testing/playing with code
nsubjects <- 98

# Creating empty arrays where we'll store data later on
trueTheta <- rep(NA,nruns) # array for logging the true theta
inferredTheta <- rep(NA,nruns) # array for logging the inferred/estimated theta

trueBeta_rel <- rep(NA,nruns) # array for logging the true beta (rel)
inferredBeta_rel <- rep(NA,nruns) # array for logging the inferred/estimated beta

trueBeta_mom <- rep(NA,nruns) # array for logging the true beta (mom)
inferredBeta_mom <- rep(NA,nruns) # array for logging the inferred/estimated beta

trueBeta_ip <- rep(NA,nruns) # array for logging the true beta (ip)
inferredBeta_ip <- rep(NA,nruns) # array for logging the inferred/estimated beta (ip)

# Simulation
calc_scores <- read.csv('/work/JNB exam/calc_scores.csv')

# Loading variables from the output file
nsubjects <- nrow(calc_scores)
bin_mom <- calc_scores$bin_mom
bin_ip <- calc_scores$bin_ip
bin_rel <- calc_scores$bin_rel

# Checking the runtime on our parameter recovery
start_time = Sys.time()

# Sourcing our simulation function (located in a separate script)
source('openness_three_sim.R')

for (i in 1:nruns) {
  print(i)
  
  # Sampling (pseudo-)random values for our parameters
  theta_temp <- runif(1,0,1) # Generating a random value between 0 and 1 (excluding 0 and 1)
  beta_rel_temp <- runif(1,0,1)
  beta_temp <- runif(1,0,1)
  beta_ip_temp <- runif(1,0,1)
  
  # Constraining the sum of all weights to 1
  # This is similar to a dirichlect distribution
  theta <- theta_temp/(theta_temp + beta_rel_temp + beta_temp + beta_ip_temp)
  beta_rel_mom <- beta_rel_temp/(theta_temp + beta_rel_temp + beta_temp + beta_ip_temp)
  beta_mom <- beta_temp/(theta_temp + beta_rel_temp + beta_temp + beta_ip_temp)
  beta_ip_mom <- beta_ip_temp/(theta_temp + beta_rel_temp + beta_temp + beta_ip_temp)
  
  # Simulate
  sim_data <- openness_three_sim(theta, beta_rel_mom, beta_mom, beta_ip_mom,
                                 bin_rel, bin_mom, bin_ip, nsubjects)
  
  bin_open <- sim_data$bin_open
  
  # Feeding the simulated data to the jags model
  # This estimates (recovers) the parameters that led to those choices
  
  # Fitting data to the JAGS model
  data <- list("bin_open", "nsubjects", "bin_rel", "bin_mom", "bin_ip")
  params <- c("theta", "theta_q_mom", "beta_rel_mom","beta_mom", "beta_ip_mom")
  
  openinfo_qual <- jags.parallel(data, inits=NULL, params,
                                 model.file ="openness_jags_mom.txt",
                                 n.chains=1, n.iter=5000, n.burnin=1000, n.thin=1, n.cluster=1)
  
  # Parameter recovery
  trueTheta[i] <- theta
  trueBeta_rel[i] <- beta_rel_mom
  trueBeta_mom[i] <- beta_mom
  trueBeta_ip[i] <- beta_ip_mom
  
  X <- openinfo_qual$BUGSoutput$sims.list
  inferredTheta[i] <- MPD(X$theta)
  inferredBeta_rel[i] <- MPD(X$beta_rel_mom)
  inferredBeta_mom[i] <- MPD(X$beta_mom)
  inferredBeta_ip[i] <- MPD(X$beta_ip_mom)
}

end_time = Sys.time()
print("Runtime for model: ")
end_time - start_time

#Nanna trying:

par(mfrow=c(2,2))
traceplot(as.mcmc(openinfo_qual$BUGSoutput$sims.list$theta))
traceplot(as.mcmc(openinfo_qual$BUGSoutput$sims.list$beta_rel_mom))
traceplot(as.mcmc(openinfo_qual$BUGSoutput$sims.list$beta_ip_mom))
traceplot(as.mcmc(openinfo_qual$BUGSoutput$sims.list$beta_mom))




# Function to plot posterior distributions
plot_posterior <- function(params, title) {
  d <- density(param_mom)
  plot(d, main = title, xlab = "Value", ylab = "Density", type = "l", lwd = 2)
  abline(v = median(param_mom), col = "red")  # Adding a line for the median
}

# Plotting posterior distributions for each parameter
par(mfrow=c(3,2))
plot_posterior(X$theta, "Posterior of Theta")
plot_posterior(X$theta_q_mom, "Posterior of Theta_q for mom")
plot_posterior(X$beta_rel_mom, "Posterior of Beta_rel")
plot_posterior(X$beta_mom, "Posterior of Beta_mom")
plot_posterior(X$beta_ip_mom, "Posterior of Beta_ip")






###Analysing the outcome

# Plot true parameters against inferred/estimated parameter
# Shows whether recovery is good
par(mfrow=c(2,2))
plot(trueTheta,inferredTheta, main="theta")
plot(trueBeta_rel,inferredBeta_rel, main="beta_rel")
plot(trueBeta_mom,inferredBeta_mom, main="beta_mom")
plot(trueBeta_ip,inferredBeta_ip, main="beta_ip")

# Calculate the bias for each parameter
biasTheta <- mean(inferredTheta - trueTheta)
biasBeta_rel <- mean(inferredBeta_rel - trueBeta_rel)
biasBeta_mom <- mean(inferredBeta_mom - trueBeta_mom)
biasBeta_ip <- mean(inferredBeta_ip - trueBeta_ip)

# Calculate the precision for each parameter
precisionTheta <- sd(inferredTheta - trueTheta)
precisionBeta_rel <- sd(inferredBeta_rel - trueBeta_rel)
precisionBeta_mom <- sd(inferredBeta_mom - trueBeta_mom)
precisionBeta_ip <- sd(inferredBeta_ip - trueBeta_ip)

# Print the bias and precision for each parameter
print(paste("Bias for Theta: ", biasTheta))
print(paste("Precision for Theta: ", precisionTheta))
print(paste("Bias for Beta_rel: ", biasBeta_rel))
print(paste("Precision for Beta_rel: ", precisionBeta_rel))
print(paste("Bias for Beta_mom: ", biasBeta_mom))
print(paste("Precision for Beta_mom: ", precisionBeta_mom))
print(paste("Bias for Beta_ip: ", biasBeta_ip))
print(paste("Precision for Beta_ip: ", precisionBeta_ip))

# Assessing the model's tendency to overestimate or underestimate
overUnderTheta <- ifelse(biasTheta > 0, "Overestimation", "Underestimation")
overUnderBeta_rel <- ifelse(biasBeta_rel > 0, "Overestimation", "Underestimation")
overUnderBeta_mom <- ifelse(biasBeta_mom > 0, "Overestimation", "Underestimation")
overUnderBeta_ip <- ifelse(biasBeta_ip > 0, "Overestimation", "Underestimation")

# Print the over/underestimation tendency for each parameter
print(paste("Theta is generally being: ", overUnderTheta))
print(paste("Beta_rel is generally being: ", overUnderBeta_rel))
print(paste("Beta_mom is generally being: ", overUnderBeta_mom))
print(paste("Beta_ip is generally being: ", overUnderBeta_ip))