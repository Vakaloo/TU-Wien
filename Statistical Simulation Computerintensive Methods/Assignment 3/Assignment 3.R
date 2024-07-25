# Task 1.1
b <- 6

# Monte Carlo integration
set.seed(12223236)
us <- runif(10000,min=1, max=b)
cat(paste("Monte Carlo Integral:",mean(exp(-us^3)*(b-1))))

# Function integrate
integrate(f = function(x){exp(-x^3)},lower = 1,upper = b)


# Task 1.2

# exponential distribution
set.seed(12223236)
x <- rexp(100000)
cat(paste("Monte Carlo Integral:",mean(exp(-(x+1)^3)/dexp(x))))


# Function integrate
integrate(function(x){exp(-x^3)},1,Inf)

# Task 1.3

# TODO write few things

# Task 2.1

r <- function(t){
  exp(cos(t))-2*cos(4*t)-sin((t/12))^5}

t <- seq(-pi,pi,length.out = 1000)

x <- r(t)*sin(t)
y <- r(t)*cos(t)

plot(x,y, type = "l")
polygon(x, y, col = "red")

# Task 2.2

indicator <- function(sample){
  set.seed(12223236)
  points <- data.frame(x = runif(sample, min = -3, max = 3),
                       y = runif(sample, min = -2, max = 3.5),
                       inside_area = logical(sample))
  
  for (i in 1:dim(points)[1]){
    if (points$y[i] > 0){
      degree <- 0
    }
    else {
      degree <- pi
    }
    theta_value <- atan(points$x[i]/points$y[i]) + degree
    r_value <- sqrt(points$x[i]^2 + points$y[i]^2)
    
    if (r(theta_value) > 0 & (r_value < abs(r(theta_value)))) {
      points$inside_area[i] <- TRUE
    } 
    else if (r(theta_value+pi) < 0 & (r_value < abs(r(theta_value + pi)))) {
      points$inside_area[i] <- TRUE
    } 
    else {
      points$inside_area[i] <- FALSE
    }
  }
  return(points)
}

# Task 2.3

library(ggplot2)

simulations <- c(100, 1000, 10000, 100000)

# Sample 100
points <- indicator(simulations[1])
ggplot()+
  geom_point(data= points, aes(x=x, y=y, color = inside_area), size = 0.1)+
  geom_path(aes(x=x, y=y))

# Sample 1000
points <- indicator(simulations[2])
ggplot()+
  geom_point(data= points, aes(x=x, y=y, color = inside_area), size = 0.1)+
  geom_path(aes(x=x, y=y))

# Sample 10000
points <- indicator(simulations[3])
ggplot()+
  geom_point(data= points, aes(x=x, y=y, color = inside_area), size = 0.1)+
  geom_path(aes(x=x, y=y))

# Sample 100000
points <- indicator(simulations[4])
ggplot()+
  geom_point(data= points, aes(x=x, y=y, color = inside_area), size = 0.1)+
  geom_path(aes(x=x, y=y))


percentage <- c()
estimated_area <- c()
for (sample in simulations){
  points <- indicator(sample)
  
  correct_points <- sum(points$inside_area==TRUE)
  per <- correct_points/sample
  
  percentage <- c(percentage,per)
  estimated_area <- c(estimated_area,per*6*5.5)  # Rectangle area
}

result.table <- data.frame(simulations, percentage, estimated_area)






