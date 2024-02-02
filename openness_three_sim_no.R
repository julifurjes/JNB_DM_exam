openness_three_sim <- function(theta, beta_rel, beta_ip, 
                         bin_rel, bin_ip, nsubjects) {
  
  # arrays to populate for simulation
  theta_q <- array(NA, c(nsubjects))
  bin_open_q <- array(NA, c(nsubjects))
  
  for (p in 1:nsubjects) {
    
    theta_q[p] <- (1-(beta_rel+beta_ip))*theta + beta_rel*bin_rel[p] + beta_ip*bin_ip[p]
    
    bin_open_q[p] <- rbinom(1, 1, theta_q[p])
    
  }
  
  result <- list(bin_open_q=bin_open_q,
                 theta_q=theta_q)
  
  return(result)
  
}