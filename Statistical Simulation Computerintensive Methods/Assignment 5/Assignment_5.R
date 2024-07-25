# Task 1

x1 <- c(-0.673, -0.584, 0.572, -0.341, 
        -0.218, 0.603, -0.415, -0.013, 
        0.763, 0.804, 0.054, 1.746, 
        -0.472, 1.638, -0.578, 0.947, 
        -0.329, -0.188, 0.794, 0.894, 
        -1.227, 1.059)

x2 <- c(0.913, -0.639, 2.99, -5.004, 
        3.118, 0.1, 1.128, 0.579, 0.32, 
        -0.488, -0.994, -0.212, 0.413, 
        1.401, 0.007, 0.568, -0.005, 0.696)


# Task 1.1

# Create a data frame
df <- data.frame(
  Group = c(rep("x1", length(x1)), rep("x2", length(x2))),
  Values = c(x1, x2)
)

library(ggplot2)
ggplot(df, aes(x = Values, fill = Group)) +
  geom_histogram(binwidth = 0.5, position = "dodge", color = "black") +
  labs(title = "Histogram of Two Vectors", x = "Values", y = "Frequency") +
  scale_fill_manual(values = c("x1" = "red", "x2" = "lightgreen")) +
  geom_vline(xintercept = mean(x1), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(x2), color = "lightgreen", linetype = "dashed", linewidth = 1)


# Task 1.2
# (TODO)

# Task 1.3
Tstatistic <- function(x,y){
  n.x <- length(x)
  n.y <- length(y)
  
  sp <- sqrt(((n.x-1)*var(x)+(n.y-1)*var(y))/(n.x+n.y-2))
  t <- (mean(x)-mean(y))/(sp*sqrt(1/n.x+1/n.y))
  return(t)
}

bootstrap_samples <- 10000

n1 <- length(x1)
n2 <- length(x2)
sp <- sqrt(((n1-1)*var(x1)+(n2-1)*var(x2))/(n1+n2-2))
t <- (mean(x1)-mean(x2))/(sp*sqrt(1/n1+1/n2))
T.orig <- Tstatistic(x1,x2)


set.seed(12223236)
Tstar.parametric <- replicate(bootstrap_samples, Tstatistic(x = sample(x1,replace = TRUE), y = sample(x2, replace = TRUE)))



Tstar.nonparametric <- replicate(bootstrap_samples,
                                 Tstatistic(x = sample(c(x1-mean(x1),
                                                         x2-mean(x2)),
                                                       replace=TRUE)[1:n1],
                                            y=sample(c(x1-mean(x1),
                                                       x2-mean(x2)),
                                                     replace=TRUE)[(n1+1):(n1+n2)]))

# 95% CI

print(quantile(Tstar.parametric, c(0.025, 0.975)))
print(quantile(Tstar.nonparametric, c(0.025, 0.975)))

# 99% CI

print(quantile(Tstar.parametric, c(0.005, 0.995)))
print(quantile(Tstar.nonparametric, c(0.005, 0.995)))

# P value

p.value.parametric <- (sum(abs(Tstar.parametric) > abs(T.orig)) + 1) / (bootstrap_samples+1)
p.value.nonparametric <- (sum(abs(Tstar.nonparametric) > abs(T.orig)) + 1) / (bootstrap_samples+1)

print(p.value.parametric)
print(p.value.nonparametric)


# We cannot reject the null hypothesis for both alpha. 

# Task 1.4 (permutation)

# Step 1 (Calculate t-Test for the original samples x1 and x2 -> This was done in task 1.3)
print(T.orig)

# Step 2 (Create two new same samples x1.new and x2.new by merging the 2 original samples x1 and x2, so that we have the 2 new samples that they have same mean)
x1.new <- c(x1,x2)
x2.new <- c(x1,x2)

# Step 3 (assign to the x1.new sample the label 1 and to the x2.new sample the label 2. 
#         This step can be ignored because it is already known from which sample each observation belongs to)


# Step 4 (Sample with replacement from each new sample (x1.new and x2.new) and calculate the t-Test for the two samples. The sizes of samples with replacement will correspond to the sizes of the original samples x1 and x2)
# Step 5 (repeat the step 4 multiple times so that we have multiple test statistics)
# Step 4 and 5 can be done together by using the replicate function as it was done in the previous questions
# n1 is the length of x1 and n2 is the length of the x2 
set.seed(12223236)
permutation.tests <- replicate(bootstrap_samples, 
                               Tstatistic(x = sample(x1.new, size = n1, replace = TRUE), 
                                          y = sample(x2.new, size = n2,  replace = TRUE)))

# Step 6 (Calculate the p value based on the original t test and the t tests from the permuted t tests as it was done in the previous task and in the slides of the course)
p.value.permutation <- (sum(abs(permutation.tests) > abs(T.orig)) + 1) / (bootstrap_samples+1)
print(p.value.permutation)
# Step 7 (calculate the confidence intervals)

# 95% CI

print(quantile(permutation.tests, c(0.025, 0.975)))

# 99% CI

print(quantile(permutation.tests, c(0.005, 0.995)))

# step 8 (Conclusions)

ggplot(data = data.frame(permutation.tests), aes(x = permutation.tests)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.25, fill = "white", color = "black") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25) + 
  geom_vline(aes(xintercept=T.orig),
             color="red", linetype="solid", linewidth=1.5) +
  labs(
    x = "Permutation Tests",
    y = "Density"
  )

# Task 1.5


# Wilcox.Tstatistic <- function(x,y){
  
#  x.data <- data.frame(label = rep(1,length(x)), data=x)
#  y.data <- data.frame(label = rep(2,length(y)), data=y)
  
#  combined.data <- rbind(x.data,y.data)
#  combined.data <- combined.data[order(combined.data$data),]
 
#  w <- sum(which(combined.data$label == 1))  
#  return(w)
#}

# Use the rank command
# Take 1 to length(x) elements that correspond to the first sample
Wilcox.Tstatistic <- function(x,y){
  n.x <- length(x)
  n.y <- length(y)
  
  ranks <- rank(c(x,y))
  return(sum(ranks[1:n.x]))
}

W.orig <- Wilcox.Tstatistic(x1,x2)


set.seed(12223236)
W.parametric <- replicate(bootstrap_samples, Wilcox.Tstatistic(x = sample(x1,replace = TRUE), y = sample(x2, replace = TRUE)))

W.nonparametric <- replicate(bootstrap_samples, Wilcox.Tstatistic(x = sample(c(x1-mean(x1),x2-mean(x2)),replace=TRUE)[1:n1], y=sample(c(x1-mean(x1),x2-mean(x2)),replace=TRUE)[(n1+1):(n1+n2)]))

# 95% CI

print(quantile(W.parametric, c(0.025, 0.975)))
print(quantile(W.nonparametric, c(0.025, 0.975)))

# 99% CI

print(quantile(W.parametric, c(0.005, 0.995)))
print(quantile(W.nonparametric, c(0.005, 0.995)))

# P value

p.value.W.parametric <- (sum(W.parametric > W.orig) + 1) / (bootstrap_samples+1)
p.value.W.nonparametric <- (sum(W.nonparametric > W.orig) + 1) / (bootstrap_samples+1)

print(p.value.W.parametric)
print(p.value.W.nonparametric)


# Task 1.6

t.test(x1,x2)
wilcox.test(x1,x2)


# Task 2.1

set.seed(12223236)

n <- 200

x1 <- rnorm(n, mean = 2, sd = sqrt(3))

x2 <- runif(n, min = 2, max = 4)

epsilon <- rt(n, df = 5)

x3 <- runif(n, min = -2, max = 2)

y <- 3 + 2*x1 + x2 + epsilon

df <- data.frame(x1,x2,x3,epsilon,y)

# Task 2.2
library(boot)

model.lm <- lm(y~x1+x2+x3, data = df)

summary(model.lm)

res <- resid(model.lm) # Residuals
yhat <- fitted(model.lm) # fitted values

df2 <- data.frame(yhat, res)

coef.fun <- function(x){coef(lm(y~x1+x2+x3, data = x))}

res.sim <- function(x, resi){
  x$y <- resi$yhat + sample(resi$res,replace=TRUE)
  return(x)
}
set.seed(12223236)
parametric.boot <- boot(df, coef.fun, R=1000,sim="parametric", ran.gen=res.sim, mle = df2)
parametric.boot

# Intercept
boot.ci(boot.out =parametric.boot,conf = 0.95,type ="perc", index = 1)
# x1
boot.ci(boot.out =parametric.boot,conf = 0.95,type ="perc", index = 2)
# x2
boot.ci(boot.out =parametric.boot,conf = 0.95,type ="perc", index = 3)
# x3
boot.ci(boot.out =parametric.boot,conf = 0.95,type ="perc", index = 4)




####################################################################################################
pvalue.fun <- function(x){
  pvalue.x3 <- summary(lm(y~x1+x2+x3, data = x))$coefficients[,4][4]
  return(pvalue.x3)
}
parametric.boot.pvalue <- boot(df, pvalue.fun, R=1000,sim="parametric", ran.gen=res.sim, mle = df2)
parametric.boot.pvalue

# parametric.boot.pvalue$t -> this command prints the p value of x3 variable for each bootstrap sample

any(parametric.boot.pvalue$t < 0.05)
parametric.boot.pvalue$t[which(parametric.boot.pvalue$t < 0.05)]
length(parametric.boot.pvalue$t[which(parametric.boot.pvalue$t < 0.05)])
####################################################################################################

# Task 2.3

reg.fun <- function(x, i){
  x.i <- x[i,]
  x.i.reg <- lm(y~x1+x2+x3, data = x.i)
  result <- c(coef(x.i.reg))
  return(result)
}
set.seed(12223236)
nonparametric.boot <- boot(df, reg.fun, R=1000)
nonparametric.boot

# Intercept
boot.ci(boot.out =nonparametric.boot,conf = 0.95,type ="perc", index = 1)
# x1
boot.ci(boot.out =nonparametric.boot,conf = 0.95,type ="perc", index = 2)
# x2
boot.ci(boot.out =nonparametric.boot,conf = 0.95,type ="perc", index = 3)
# x3
boot.ci(boot.out =nonparametric.boot,conf = 0.95,type ="perc", index = 4)




# Task 2.4
# TODO -> RMD

# Task 3
# TODO -> RMD



