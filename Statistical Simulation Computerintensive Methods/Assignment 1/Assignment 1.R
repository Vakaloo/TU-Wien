############## Task 1##############

# Task 1.1

##Algorithm 0##

gold_standard <- function(x){
  return(var(x))
}

##Algorithm 1##

precise <- function(x){
  sample_mean <- sum(x)/length(x)
  variance <- sum((x-sample_mean)^2)/(length(x)-1)
  return(variance)
}

##Algorithm 2##

excel <- function(x){
  P1 <- sum(x^2)
  P2 <- (sum(x))^2/length(x)
  variance <- (P1-P2)/(length(x)-1)
  return(variance)
}

##Algorithm 3##

shift <- function(x, c){
  P1 <- sum((x-c)^2)
  P2 <- (sum(x-c))^2/length(x)
  variance <- (P1-P2)/(length(x)-1)
  return(variance)
}

##Algorithm 4##

online <- function(x){
  
  sample_mean <- (x[1]+x[2])/2
  variance <- ((x[1]-sample_mean)^2+(x[2]-sample_mean)^2)/(2-1)
  
  for (n in 3:length(x)){
    variance <- (n-2)/(n-1)*variance+(x[n]-sample_mean)^2/n
    sample_mean <- sample_mean + (x[n]-sample_mean)/n
  }
  return(variance)
}

# Task 1.2
wrapper_function <- function(x){
  results <- data.frame(
    Method = c("Gold Standard", "Precise", 
               "Excel", "Shift", "Online"),
    Variance = c(
      gold_standard(x),
      precise(x),
      excel(x),
      shift(x, x[1]),
      online(x)
    )
  )
  return(results)
}

set.seed(12223236)
x <- rnorm(100)
# Compare variance calculation methods
comparison_results <- wrapper_function(x)

############## Task 2############## 

# install.packages("microbenchmark")
library(microbenchmark)

# Table Summarize
mb <- microbenchmark(
  "Gold Standard" = gold_standard(x),
  "Precise" = precise(x),
  "Excel" = excel(x),
  "Shift" = shift(x,x[1]),
  "Online" = online(x),
  times = 100
)
print(mb)  

#Graphically summarize
boxplot(mb, main="Execution Time Comparison", ylab="Time (milliseconds)")


############## Task 3############## 
computational_comparison <- function(x, scale_invariance_property){
  
  shifted_variance <- c()
  
  for (c in scale_invariance_property){
    shifted_variance <- c(shifted_variance, shift(x,c))
  }
  
  mb <- microbenchmark(
    "Shift -1e10" = shift(x,scale_invariance_property[1]),
    "Shift Min" = shift(x,scale_invariance_property[2]),
    "Shift 1st Qu" = shift(x,scale_invariance_property[3]),
    "Shift Median" = shift(x,scale_invariance_property[4]),
    "Shift Mean" = shift(x,scale_invariance_property[5]),
    "Shift 3rd Qu" = shift(x,scale_invariance_property[6]),
    "Shift Max" = shift(x,scale_invariance_property[7]),
    "Shift 1e10" = shift(x,scale_invariance_property[8]),
    times = 100
  ) 
  return(list(shifted_variance = shifted_variance,mb = mb))
}
equality_comparison <- function(shifted_variance){
  # Initialize empty matrices to store comparison results
  equal_matrix <- matrix(FALSE, nrow = length(shifted_variance), ncol = length(shifted_variance))
  identical_matrix <- matrix(FALSE, nrow = length(shifted_variance), ncol = length(shifted_variance))
  all_equal_matrix <- matrix(FALSE, nrow = length(shifted_variance), ncol = length(shifted_variance))
  
  # Perform pairwise comparisons using nested loops
  for (i in 1:length(shifted_variance)) {
    for (j in 1:length(shifted_variance)) {
      # Using == operator
      equal_matrix[i, j] <- shifted_variance[i] == shifted_variance[j]
      
      # Using identical()
      identical_matrix[i, j] <- identical(shifted_variance[i], shifted_variance[j])
      
      # Using all.equal()
      all_equal_matrix[i, j] <- isTRUE(all.equal(shifted_variance[i], shifted_variance[j]))
    }
  }
  return(list(equal_matrix = equal_matrix,
              identical_matrix = identical_matrix,
              all_equal_matrix = all_equal_matrix)) 
}

scale_invariance_property <- c(-1e07, summary(x)[1],summary(x)[2],summary(x)[3],
                               summary(x)[4],summary(x)[5],summary(x)[6], 1e07)

# First part of the simulations (first data set)
set.seed(12223236)
x1 <- rnorm(100)
scale_invariance_property <- c(-1e07, summary(x1)[1],summary(x1)[2],summary(x1)[3],
                               summary(x1)[4],summary(x1)[5],summary(x1)[6], 1e07)

# Second part of the simulations (second data set)
set.seed(12223236)
x2 <- rnorm(100, mean=1000000)
scale_invariance_property <- c(-1e07, summary(x2)[1],summary(x2)[2],summary(x2)[3],
                               summary(x2)[4],summary(x2)[5],summary(x2)[6], 1e07)

# print(mb)
#Graphically summarize
# boxplot(mb, main="Execution Time Comparison", ylab="Time (milliseconds)")



############## Task 4############## 
condition_number <- function(x,c){
  return(sqrt(1+(length(x)*(mean(x)-c)^2)/(sum((x-mean(x))^2))))
}

for (c in scale_invariance_property){
  k <- condition_number(x1,c)
  print(k)
}

for (c in scale_invariance_property){
  k <- condition_number(x2,c)
  print(k)
}

set.seed(12223236)
x3 <- rnorm(100,mean = 1, sd = 100000)
scale_invariance_property <- c(-1e07, summary(x3)[1],summary(x3)[2],summary(x3)[3],
                               summary(x3)[4],summary(x3)[5],summary(x3)[6], 1e07)

for (c in scale_invariance_property){
  k <- condition_number(x3,c)
  print(k)
}

