## Father ##

install.packages("pacman")
pacman::p_load(tidyverse, R2jags, dplyr, readxl, parallel, polspline)

# Setting the working directory
setwd('/work/JNB exam/The_good_stuff')
set.seed(1989) #(Taylor's version)

# Maximum Posterior Density function
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

nruns <- 100
nsubjects <- 98

# Creating empty arrays where we'll store data later on
trueTheta <- rep(NA,nruns) # array for logging the true theta
inferredTheta <- rep(NA,nruns) # array for logging the inferred/estimated theta

trueBeta_rel <- rep(NA,nruns) # array for logging the true beta (rel)
inferredBeta_rel <- rep(NA,nruns) # array for logging the inferred/estimated beta

trueBeta_dad <- rep(NA,nruns) # array for logging the true beta (dad)
inferredBeta_dad <- rep(NA,nruns) # array for logging the inferred/estimated beta

trueBeta_ip <- rep(NA,nruns) # array for logging the true beta (ip)
inferredBeta_ip <- rep(NA,nruns) # array for logging the inferred/estimated beta (ip)

# Simulation
calc_scores <- read.csv('/work/JNB exam/calc_scores.csv')

# Loading variables from the output file
nsubjects <- nrow(calc_scores)
bin_dad <- calc_scores$bin_dad
bin_ip <- calc_scores$bin_ip
bin_rel <- calc_scores$bin_rel

# Checking the runtime on our parameter recovery
start_time = Sys.time()

# Sourcing our simulation function (located in a separate script)
source('openness_three_sim.R')

for (i in 1:nruns) {
  print(i)
  
  # Sample (pseudo-)random values for the parameters
  theta_temp <- runif(1,0,1) # Generating a random value between 0 and 1 (excluding 0 and 1)
  beta_rel_temp <- runif(1,0,1)
  beta_temp <- runif(1,0,1)
  beta_ip_temp <- runif(1,0,1)
  
  # Constraining the sum of all weights to 1
  # This is similar to a dirichlect distribution
  theta <- theta_temp/(theta_temp + beta_rel_temp + beta_temp + beta_ip_temp)
  beta_rel_dad <- beta_rel_temp/(theta_temp + beta_rel_temp + beta_temp + beta_ip_temp)
  beta_dad <- beta_temp/(theta_temp + beta_rel_temp + beta_temp + beta_ip_temp)
  beta_ip_dad <- beta_ip_temp/(theta_temp + beta_rel_temp + beta_temp + beta_ip_temp)
  
  sim_data <- openness_three_sim(theta, beta_rel_dad, beta_dad, beta_ip_dad,
                                 bin_rel, bin_dad, bin_ip, nsubjects)
  
  bin_open <- sim_data$bin_open
  
  # Feeding the simulated data to the jags model
  # This estimates (recovers) the parameters that led to those choices
  
  # Fit data to the JAGS model
  data <- list("bin_open", "nsubjects", "bin_rel", "bin_dad", "bin_ip")
  params <- c("theta", "theta_q_dad", "beta_rel_dad","beta_dad", "beta_ip_dad")
  
  openinfo_qual <- jags.parallel(data, inits=NULL, params,
                                 model.file ="openness_jags_dad.txt",
                                 n.chains=1, n.iter=5000, n.burnin=1000, n.thin=1, n.cluster=1)
  
  # Parameter recovery
  trueTheta[i] <- theta
  trueBeta_rel[i] <- beta_rel_dad
  trueBeta_dad[i] <- beta_dad
  trueBeta_ip[i] <- beta_ip_dad
  
  X <- openinfo_qual$BUGSoutput$sims.list
  inferredTheta[i] <- MPD(X$theta)
  inferredBeta_rel[i] <- MPD(X$beta_rel_dad)
  inferredBeta_dad[i] <- MPD(X$beta_dad)
  inferredBeta_ip[i] <- MPD(X$beta_ip_dad)
}

end_time = Sys.time()
print("Runtime for model: ")
end_time - start_time

# Analysing the outcome

# Plot true parameters against inferred/estimated parameter
# Shows whether recovery is good
par(mfrow=c(2,2))
plot(trueTheta,inferredTheta, main="theta", xlim=c(0,1), ylim=c(0,1))
plot(trueBeta_rel,inferredBeta_rel, main="beta_rel")
plot(trueBeta_dad,inferredBeta_dad, main="beta_dad")
plot(trueBeta_ip,inferredBeta_ip, main="beta_ip")

# Calculate the bias for each parameter
biasTheta <- mean(inferredTheta - trueTheta)
biasBeta_rel <- mean(inferredBeta_rel - trueBeta_rel)
biasBeta_dad <- mean(inferredBeta_dad - trueBeta_dad)
biasBeta_ip <- mean(inferredBeta_ip - trueBeta_ip)

# Calculate the precision for each parameter
precisionTheta <- sd(inferredTheta - trueTheta)
precisionBeta_rel <- sd(inferredBeta_rel - trueBeta_rel)
precisionBeta_dad <- sd(inferredBeta_dad - trueBeta_dad)
precisionBeta_ip <- sd(inferredBeta_ip - trueBeta_ip)

# Print the bias and precision for each parameter
print(paste("Bias for Theta: ", biasTheta))
print(paste("Precision for Theta: ", precisionTheta))
print(paste("Bias for Beta_rel: ", biasBeta_rel))
print(paste("Precision for Beta_rel: ", precisionBeta_rel))
print(paste("Bias for Beta_dad: ", biasBeta_dad))
print(paste("Precision for Beta_dad: ", precisionBeta_dad))
print(paste("Bias for Beta_ip: ", biasBeta_ip))
print(paste("Precision for Beta_ip: ", precisionBeta_ip))

# Assessing the model's tendency to overestimate or underestimate
overUnderTheta <- ifelse(biasTheta > 0, "Overestimation", "Underestimation")
overUnderBeta_rel <- ifelse(biasBeta_rel > 0, "Overestimation", "Underestimation")
overUnderBeta_dad <- ifelse(biasBeta_dad > 0, "Overestimation", "Underestimation")
overUnderBeta_ip <- ifelse(biasBeta_ip > 0, "Overestimation", "Underestimation")

# Print the over/underestimation tendency for each parameter
print(paste("Theta is generally being: ", overUnderTheta))
print(paste("Beta_rel is generally being: ", overUnderBeta_rel))
print(paste("Beta_dad is generally being: ", overUnderBeta_dad))
print(paste("Beta_ip is generally being: ", overUnderBeta_ip))