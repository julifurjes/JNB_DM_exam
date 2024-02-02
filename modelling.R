# Setting the working directory
setwd('/work/JNB exam/The_good_stuff')

# Load packages
install.packages("pacman")
pacman::p_load(tidyverse, R2jags, dplyr, readxl, parallel, polspline)
set.seed(1989) #(Taylor's version)

# Checking R version
version$version.string

# Load preprocessed data
calc_scores <- read.csv("/work/JNB exam/calc_scores.csv")

# Maximum Posterior Density function
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

# Defining the common parameters for the models
nsubjects <- nrow(calc_scores)
bin_open <- calc_scores$bin_open
theta <- 0.3 # Because we saw in preprocessing that 30/100 people were open
bin_ip <- calc_scores$bin_ip
bin_rel <- calc_scores$bin_rel

###### Model - Mother ######

bin_mom <- calc_scores$bin_mom

data_mom <- list("bin_open", "nsubjects", "bin_rel", "bin_mom", "bin_ip")
params_mom <- c("theta", "theta_q_mom", "beta_rel_mom","beta_mom", "beta_ip_mom")

start_time = Sys.time()
openinfo_qual_mom <- jags.parallel(data_mom, inits=NULL, params_mom, model.file="openness_jags_mom.txt", n.chains = 3, 
                                   n.iter = 5000, n.burnin = 1000, n.thin=1, n.cluster=3)
end_time = Sys.time()
end_time - start_time

X_mom <- openinfo_qual_mom$BUGSoutput$sims.list
theta_recov_mom <- MPD(X_mom$theta)
theta_q_mom_recov <- MPD(X_mom$theta_q_mom)
beta_rel_recov_mom <- MPD(X_mom$beta_rel_mom)
beta_mom_recov_mom <- MPD(X_mom$beta_mom)
beta_ip_recov_mom <- MPD(X_mom$beta_ip_mom)

###### Model - Father ######

bin_dad <- calc_scores$bin_dad

data_dad <- list("bin_open", "nsubjects", "bin_rel", "bin_dad", "bin_ip")
params_dad <- c("theta", "theta_q_dad", "beta_rel_dad","beta_dad", "beta_ip_dad")

start_time = Sys.time()
openinfo_qual_dad <- jags(data_dad, inits=NULL, params_dad, model.file="openness_jags_dad.txt", n.chains = 3, 
                          n.iter = 5000, n.burnin = 1000, n.thin=1)
end_time = Sys.time()
end_time - start_time

X_dad <- openinfo_qual_dad$BUGSoutput$sims.list
theta_recov_dad <- MPD(X_dad$theta)
theta_q_dad_recov <- MPD(X_dad$theta_q_dad)
beta_rel_recov_dad <- MPD(X_dad$beta_rel_dad)
beta_dad_recov_dad <- MPD(X_dad$beta_dad)
beta_ip_recov_dad <- MPD(X_dad$beta_ip_dad)

###### Model - No Parents ######

data_no <- list("bin_open", "nsubjects", "bin_rel", "bin_ip")
params_no <- c("theta", "theta_q_no", "beta_rel_no", "beta_ip_no")

start_time = Sys.time()
openinfo_qual_no <- jags(data_no, inits=NULL, params_no, model.file="openness_jags_no.txt", n.chains = 3, 
                         n.iter = 5000, n.burnin = 1000, n.thin=1)
end_time = Sys.time()
end_time - start_time

X_no <- openinfo_qual_no$BUGSoutput$sims.list
theta_recov_no <- MPD(X_no$theta)
theta_q_no_recov <- MPD(X_no$theta_q_no)
beta_rel_recov_no <- MPD(X_no$beta_rel_no)
beta_ip_recov_no <- MPD(X_no$beta_ip_no)

##### Visualisations #####

## Trace plots
# To assess the convergence of your MCMC chains, trace plots can be very informative.
# They show the sampled values of a parameter over each iteration of the MCMC.
library(coda)

# MOM
par(mfrow=c(2,2))
traceplot(as.mcmc(openinfo_qual_mom$BUGSoutput$sims.list$theta))
traceplot(as.mcmc(openinfo_qual_mom$BUGSoutput$sims.list$beta_rel_mom))
traceplot(as.mcmc(openinfo_qual_mom$BUGSoutput$sims.list$beta_ip_mom))
traceplot(as.mcmc(openinfo_qual_mom$BUGSoutput$sims.list$beta_mom))

# DAD
par(mfrow=c(2,2))
traceplot(as.mcmc(openinfo_qual_dad$BUGSoutput$sims.list$theta))
traceplot(as.mcmc(openinfo_qual_dad$BUGSoutput$sims.list$beta_rel_dad))
traceplot(as.mcmc(openinfo_qual_dad$BUGSoutput$sims.list$beta_ip_dad))
traceplot(as.mcmc(openinfo_qual_dad$BUGSoutput$sims.list$beta_dad))

# NO PARENT
par(mfrow=c(2,2))
traceplot(as.mcmc(openinfo_qual_no$BUGSoutput$sims.list$theta))
traceplot(as.mcmc(openinfo_qual_no$BUGSoutput$sims.list$beta_rel_no))
traceplot(as.mcmc(openinfo_qual_no$BUGSoutput$sims.list$beta_ip_no))

## Posterior distribution plots
# MOM
X_mom <- openinfo_qual_mom$BUGSoutput$sims.list

# Function to plot posterior distributions
plot_posterior_mom <- function(param_mom, title) {
  d <- density(param_mom)
  plot(d, main = title, xlab = "Value", ylab = "Density", type = "l", lwd = 2)
  abline(v = median(param_mom), col = "red")  # Adding a line for the median
}

# Plotting posterior distributions for each parameter
par(mfrow=c(3,2))
plot_posterior_mom(X_mom$theta, "Posterior of Theta")
plot_posterior_mom(X_mom$theta_q_mom, "Posterior of Theta_q for mom")
plot_posterior_mom(X_mom$beta_rel_mom, "Posterior of Beta_rel")
plot_posterior_mom(X_mom$beta_mom, "Posterior of Beta_mom")
plot_posterior_mom(X_mom$beta_ip_mom, "Posterior of Beta_ip")

# DAD
X_dad <- openinfo_qual_dad$BUGSoutput$sims.list

# Function to plot posterior distributions
plot_posterior_dad <- function(param_dad, title) {
  d <- density(param_dad)
  plot(d, main = title, xlab = "Value", ylab = "Density", type = "l", lwd = 2)
  abline(v = median(param_dad), col = "red")  # Adding a line for the median
}

# Plotting posterior distributions for each parameter
par(mfrow=c(3,2))
plot_posterior_dad(X_dad$theta, "Posterior of Theta")
plot_posterior_dad(X_dad$theta_q_dad, "Posterior of Theta_q dad")
plot_posterior_dad(X_dad$beta_rel_dad, "Posterior of Beta_rel")
plot_posterior_dad(X_dad$beta_dad, "Posterior of Beta_dad")
plot_posterior_dad(X_dad$beta_ip_dad, "Posterior of Beta_ip")

# NO PARENT
X_no <- openinfo_qual_no$BUGSoutput$sims.list

# Function to plot posterior distributions
plot_posterior_no <- function(param_no, title) {
  d <- density(param_no)
  plot(d, main = title, xlab = "Value", ylab = "Density", type = "l", lwd = 2)
  abline(v = median(param_no), col = "red")  # Adding a line for the median
}

# Plotting posterior distributions for each parameter
par(mfrow=c(2,2))
plot_posterior_no(X_no$theta, "Posterior of Theta")
plot_posterior_no(X_no$theta_q_no, "Posterior of Theta_q No parents")
plot_posterior_no(X_no$beta_rel_no, "Posterior of Beta_rel")
plot_posterior_no(X_no$beta_ip_no, "Posterior of Beta_ip")

## Heatmaps for Correlation Matrix
# If you have multiple parameters, a heatmap of their correlation matrix can be insightful
# I am now comparing the issues and the relationship quality
install.packages('reshape2')
library(reshape2)

# MOM
X_mom <- openinfo_qual_mom$BUGSoutput$sims.list

params_df_mom <- data.frame(X_mom$beta_rel_mom, X_mom$beta_mom, X_mom$beta_ip_mom)
# Calculate the correlation matrix
corr_matrix_mom <- cor(params_df_mom)

# Melt the correlation matrix for use with ggplot
melted_corr_matrix_mom <- melt(corr_matrix_mom)

# Create the heatmap
ggplot(melted_corr_matrix_mom, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white", size = 0.1) + # Adding white lines to separate tiles
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    axis.title.x = element_blank(), # Remove x-axis label
    axis.title.y = element_blank()  # Remove y-axis label
  ) +
  coord_fixed() + # Ensure tiles are square
  labs(fill = "Correlation")

# DAD
X_dad <- openinfo_qual_dad$BUGSoutput$sims.list

params_df_dad <- data.frame(X_dad$beta_rel_dad, X_dad$beta_dad, X_dad$beta_ip_dad)
# Calculate the correlation matrix
corr_matrix_dad <- cor(params_df_dad)

# Melt the correlation matrix for use with ggplot
melted_corr_matrix_dad <- melt(corr_matrix_dad)

# Create the heatmap
ggplot(melted_corr_matrix_dad, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white", size = 0.1) + # Adding white lines to separate tiles
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    axis.title.x = element_blank(), # Remove x-axis label
    axis.title.y = element_blank()  # Remove y-axis label
  ) +
  coord_fixed() + # Ensure tiles are square
  labs(fill = "Correlation") # Label for the color scale

# NO PARENTS
X_no <- openinfo_qual_no$BUGSoutput$sims.list

params_df_no <- data.frame(X_no$beta_rel_no, X_no$beta_ip_no)
# Calculate the correlation matrix
corr_matrix_no <- cor(params_df_no)

# Melt the correlation matrix for use with ggplot
melted_corr_matrix_no <- melt(corr_matrix_no)

# Create the heatmap
ggplot(melted_corr_matrix_no, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white", size = 0.1) + # Adding white lines to separate tiles
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    axis.title.x = element_blank(), # Remove x-axis label
    axis.title.y = element_blank()  # Remove y-axis label
  ) +
  coord_fixed() + # Ensure tiles are square
  labs(fill = "Correlation") # Label for the color scale