# Task 1
# function for Linear Congruential Random Number Generator
mc.gen <- function(n,m,a,c=0,x0){
  us <- numeric(n)
  for (i in 1:n){
    x0 <- (a*x0+c) %% m
    us[i] <- x0 / m
    }
  return(us)
}

# Simulation
n <- 100
m <- 7919 # 1000th prime number. I could use the student ID
a <- round(sqrt(m),2)
c <- 1158
x0 <- 25
mc.gen(n,m,a,c,x0)

# According to the available codes examples and instructions from the course about the method 
# Linear Congruential Random Number Generation Algorithm, we have to choose 
# the parameter m to be as large as possible in order to not obtain the first element of the sequence 
# after a number of iterations. Therefore, the whole method is characterized as cyclic 
# and the largest the m parameter the higher the cycle length of the random number generator sequence. 
# Furthermore, there is a second parameter, which is called "multiplier" a and is usually close to
# square root of m. Finally, the last parameter is the "increment" c, of which the value ranges between
# [0,m]. It is worth mentioning that first an initial integer value must be chosen in order to 
# start the procedure. The initial value is called "starting value" or "seed". Also, the parameter m must be 
# a prime number, which means that it is a number that can only be divided by itself and 1 without remainders.


# Try out and compare different values of m, a to illustrate 
# the behavior of the method as described on slide 18 (TODO)

# Task 2

exponential_rng <- function(sample,l){
  set.seed(12223236)
  variables <- runif(sample, min=0, max=1)
  x <- -log(1-variables)/l
  return(x)
}

lambda <- c(1,10,25)
sample <- 1000

simulations <- list()
real_exponential_distribution <- list()

set.seed(12223236)

for (l in lambda){
  simulations <- c(simulations, list(exponential_rng(sample,l)))
  
  real_exponential_distribution <- c(real_exponential_distribution, 
                                     list(rexp(sample, rate = l)))
  
  # qqnorm(exponential_rng(sample,l), main="Normal Q-Q plot")
  # qqline(exponential_rng(sample,l))
}

# Task 3

# Task 3.1

acceptance_rejection_method <- function(sample, a, b, c = 1){
  set.seed(12223236)  # Set a seed for reproducibility
  
  x <- numeric(sample)
  accepted <- 0
  rejection <- 0
  while (accepted < sample){
    
    u <- runif(1) # this is the u
    y <- runif(1) # this is g(x)
    f <- gamma(a+b)/(gamma(a)*gamma(b))*y^(a-1)*(1-y)^(b-1)
    
    # Check the acceptance
    if (u<=f/(c*y)){
      accepted <- accepted+1
      x[accepted] <- y
    }
    else{
      # Calculate the number of rejections in order to find rejection proportion 
      rejection <- rejection + 1
    }
  }
  return (list(x,rejection))
}

# initial values
sample <- 1000
a <- 2
b <- 2

# Simulations for constant c
con <- seq(1.5,20,0.1) # the 1.5 value is the maximum for the f function
rejections <- c()
for (c in con){
  rejections <- c(rejections, acceptance_rejection_method(sample, a, b, c)[[2]])
}

cat("The minimum c, for g(x) to be uniform distribution, is:", con[which.min(rejections)])
plot(con,rejections, type = "l", lty = 1)

# Task 3.2

f = function(y,a,b) {
  gamma(a+b)/(gamma(a)*gamma(b))*y^(a-1)*(1-y)^(b-1)
}
# Few demonstrations about the f function/beta distribution
# we take again uniform distribution
# Initial values
a <- 6
b <- 8

# find the maximum of f(x) within the interval [0, 1]
ans = optimize(f, c(0,1), maximum = TRUE, a = a,b = b)

x_max = ans$maximum
y_max = ans$objective

cat("The max value of the beta distribution is: ", y_max)

# plot f(x)
x = seq(0, 1, length.out = 100)
plot(x, f(x,a,b), type = 'l')

points(x_max, y_max, pch = 15)
text(x = x_max, y = y_max, labels = 'Maximum',
     pos = 4, col = 'blue')

abline(h = y_max, col = "red")

# Simulations for constant c
# We show that for c greater than the max of the function f, the rejections are increased
con <- seq(y_max,20,0.1) # the y_max value is the maximum for the f function
rejections <- c()
for (c in con){
  rejections <- c(rejections, acceptance_rejection_method(sample, a, b, c)[[2]])
}

cat("The minimum c, for g(x) to be uniform distribution, is:", con[which.min(rejections)])
plot(con,rejections, type = "l", lty = 1)


