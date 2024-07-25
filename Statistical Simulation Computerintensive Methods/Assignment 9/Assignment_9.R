# Gibbs Sampling
sample_theta1 <- function(theta2, rho) {
  mean_theta1 <- rho * theta2
  sd_theta1 <- sqrt(1 - rho^2)
  return(rnorm(1, mean = mean_theta1, sd = sd_theta1))
}

sample_theta2 <- function(theta1, rho) {
  mean_theta2 <- rho * theta1
  sd_theta2 <- sqrt(1 - rho^2)
  return(rnorm(1, mean = mean_theta2, sd = sd_theta2))
}


# Gibbs sampler
gibbs_sampler <- function(iterations, rho) {
  # Initialize
  theta <- c(0, 0)  # Initial values for the parameters
  samples <- matrix(0, nrow = iterations, ncol = 2)
  
  # Gibbs sampler
  for (i in 1:iterations) {
    # Sample Theta 1 given Theta 2
    theta[1] <- sample_theta1(theta[2], rho)
    
    # Sample Theta 2 given Theta 1
    theta[2] <- sample_theta2(theta[1], rho)
    
    # Store the sample
    samples[i, ] <- theta
  }
  return(samples)
}
# Set parameters
rho <- 0.5
chain_size <- 30000

# Run Gibbs sampler
set.seed(12223236)
gibbs_samples <- gibbs_sampler(chain_size, rho)

############################################################################################################
# Chain Diagnostics for Gibbs
par(mfrow = c(1, 2))
plot(gibbs_samples[, 1], 
     type = 'l', 
     xlab = 'Iteration', 
     ylab = expression(paste(theta,"1")),
     main = expression(paste('Trace Plot for ', theta,'1 (Gibbs)')))
plot(gibbs_samples[, 2], 
     type = 'l', 
     xlab = 'Iteration', 
     ylab = expression(paste(theta,"2")),
     main = expression(paste('Trace Plot for ', theta,'2 (Gibbs)')))
par(mfrow = c(1, 1))

# install.packages("mcmcplots")
library(mcmcplots)

rmeanplot(gibbs_samples[, 1],lwd=2,main=expression(paste("Run mean for ",theta,"1")),mar=c(2,2,1.5,1)+ 0.1)
rmeanplot(gibbs_samples[, 2],lwd=2,main=expression(paste("Run mean for ",theta,"2")),mar=c(2,2,1.5,1)+ 0.1)
############################################################################################################




# Function to calculate the joint probability density function
joint_pdf <- function(theta, rho) {
  numerator <- exp(-0.5 * (1 - rho^2) * (theta[1]^2 - 2 * rho * theta[1] * theta[2] + theta[2]^2))
  denominator <- 2 * pi * sqrt(1 - rho^2)
  return(numerator / denominator)
}

# Metropolis-Hastings algorithm with block-wise update
metropolis_hastings <- function(iterations, rho) {
  # Initialize
  theta <- c(0, 0)  # Initial values for the parameters
  samples <- matrix(0, nrow = iterations, ncol = 2)
  
  # Metropolis-Hastings algorithm
  for (i in 1:iterations) {
    # Block-wise update: propose new values for both parameters simultaneously
    proposal <- rnorm(2, mean = theta, sd = c(1, 1))  # You can adjust the proposal standard deviation

    # Calculate acceptance ratio
    alpha <- min(1, joint_pdf(proposal, rho) / joint_pdf(theta, rho))
    
    # Accept or reject the proposal
    if (runif(1) < alpha) {
      theta <- proposal
    }
    
    # Store the sample
    samples[i, ] <- theta
  }
  
  return(samples)
}

# Set parameters
rho <- 0.5
chain_size <- 30000

# Run Metropolis-Hastings algorithm
set.seed(12223236)
metropolis_samples <- metropolis_hastings(chain_size, rho)

# Plot the samples
par(mfrow = c(1, 2))
plot(metropolis_samples[, 1], 
     type = 'l', 
     xlab = 'Iteration', 
     ylab = expression(paste(theta,"1")),
     main = expression(paste('Trace Plot for ', theta,'1 (Metropolis Hastings)')))
plot(metropolis_samples[, 2], 
     type = 'l', 
     xlab = 'Iteration', 
     ylab = expression(paste(theta,"2")),
     main = expression(paste('Trace Plot for ', theta,'2 (Metropolis Hastings)')))
par(mfrow = c(1, 1))

par(mfrow = c(1, 1))

rmeanplot(metropolis_samples[, 1],
          lwd=2,
          main=expression(paste("Run mean for ",theta,"1")),
          mar=c(2,2,1.5,1)+ 0.1)
rmeanplot(metropolis_samples[, 2],
          lwd=2,
          main=expression(paste("Run mean for ",theta,"2")),
          mar=c(2,2,1.5,1)+ 0.1)






