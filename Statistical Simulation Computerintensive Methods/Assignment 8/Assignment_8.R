##########################################################################################
# Task 1 

# Task 1.1

total_tests_Germany <- 4068
positive_cases_Germany <- 4

y_prior <- positive_cases_Germany # slide 41
n <- total_tests_Germany # slide 41


# Parameters from the German study
alpha_prior <- y_prior+1
beta_prior <- n-y_prior+1
x <- seq(0, 1, by = 0.000001)

# Create a Beta prior distribution
prior_distribution <- dbeta(x, alpha_prior, beta_prior)

# Plot the Beta distribution
plot(x, prior_distribution, type = "l", col = "blue", lwd = 2,
     xlab = "Prevalence", ylab = "Density", main = "Beta Prior Distribution",
     xlim = c(0,0.006))

# Reweighted alpha and beta
# Create a scaled Beta prior distribution

prior_distribution_scaled <- dbeta(x, alpha_prior/10, beta_prior/10)

plot(x, prior_distribution_scaled, type = "l", col = "black", lwd = 2,
     xlab = "Prevalence", ylab = "Density", main = "Beta Prior Distribution",
     xlim = c(0,0.006))


# Task 1.2

total_tests_Austria <- 1279
positive_cases_Austria <- 0

likelihood <- dbinom(positive_cases_Austria, 
                     size = total_tests_Austria, 
                     prob = x) # Binomial Likelihood 

# Create a Beta posterior distribution
y_posterior <- positive_cases_Austria
alpha_posterior <- alpha_prior + y_posterior
beta_posterior <- beta_prior + n - y_posterior

posterior_distribution <- dbeta(x, alpha_posterior, beta_posterior)


# Task 1.3
# Plot the Beta posterior distribution
plot(x, posterior_distribution, type = "l", col = "black", lwd = 2,
     xlab = "Probability", ylab = "Density", main = "Beta Prior/Posterior Distribution",
     xlim = c(0,0.004))

# Mean
mean_posterior <- alpha_posterior/(alpha_posterior+beta_posterior)
# Standard Deviation
sd_posterior <- (alpha_posterior*beta_posterior)/((alpha_posterior+beta_posterior)^2*(alpha_posterior+beta_posterior+1))

quantiles <- qbeta(p = c(0.025,0.975), alpha_posterior, beta_posterior)



x_quantile <- seq(quantiles[1],quantiles[2],0.000001)
y_quantile <-  dbeta(x_quantile, alpha_posterior, beta_posterior)

x_quantile = c(quantiles[1],x_quantile,quantiles[2])
y_quantile = c(0,y_quantile,0)
polygon(x_quantile,y_quantile, col="yellow")

abline(v=mean_posterior,col="red",lwd=2)
lines(x,prior_distribution, col = "blue", lwd = 2)

legend ("topright",legend=c("Posterior", "Prior", "Mean" ,"HPD interval 95%"),
        lwd=4,
        col=c("black","blue","red","yellow"))

# Task 1.4 

# I do not know (TODO)

##########################################################################################
# Task 2
library(ISLR)
data(Auto)
df_auto <- Auto

# Summary of the data
head(df_auto)
summary(df_auto)
str(df_auto)

# Remove NAs, if there are
df_auto <- na.omit(df_auto)
##########################################################################################   

# Task 2.1 
# Compare different choice of prior parameters
# 3 cases
# 1st case -> informative
# 2nd case -> uninformative
# 3rd case -> weakly informative






