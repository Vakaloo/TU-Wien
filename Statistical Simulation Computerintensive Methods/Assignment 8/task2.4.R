# install.packages("rstanarm")
library(rstanarm)
library(ISLR)
library(ggplot2)
library("bayesplot")
data(Auto)
df_auto <- Auto

# Remove NAs, if there are
df_auto <- na.omit(df_auto)

# Summary of the data
head(df_auto)
summary(df_auto)
str(df_auto)




model_bayes <- stan_glm(mpg~horsepower, data=df_auto,  
                        prior = normal(50, 5),
                        prior_aux = exponential(0.0008),
                        seed=111,refresh = FALSE)
print(model_bayes, digits = 3)


mcmc_dens(model_bayes, pars = c("sigma"))
mcmc_dens(model_bayes, pars = c("horsepower"))

library(bayestestR)
install.packages("bayestestR")
hdi(model_bayes)
