##################Ratio of Fibonacci numbers##############################
#Question 1
Fib_for <- function(n){
  #start the sequence with 0 and 1
  fibseq <- c(1,1)
  rseq <- c(1)
  if (n>2){
    for (i in 3:(n+1)){
      fibseq[i] <- fibseq[i - 1] + fibseq[i - 2]
      rseq[i-1] <- fibseq[i]/fibseq[i-1]
    }
  }
  return(rseq)
}
Fib_while <- function(n){
  #start the sequence with 0 and 1
  fibseq <- c(1,1)
  rseq <- c(1)
  i<-3
  if (n>2){
    while (i<=(n+1)){
      fibseq[i] <- fibseq[i - 1] + fibseq[i - 2]
      rseq[i-1] <- fibseq[i]/fibseq[i-1]
      i <- i + 1
    }
  }
  return(rseq)
}

#Question 2
library(microbenchmark)
bench_for100 <- microbenchmark(Fib_for(100))
bench_while100 <- microbenchmark(Fib_while(100))

print(paste("The time for n=100 using the 'for' loop in seconds:",mean(bench_for100$time)/1e6, 
            "+/-", round(sd(bench_for100$time)/1e6,6), "seconds"))

print(paste("The time for n=100 using the 'while' loop in seconds:",mean(bench_while100$time)/1e6, 
            "+/-", round(sd(bench_while100$time)/1e6,6), "seconds"))


bench_for1000 <- microbenchmark(Fib_for(1000))
bench_while1000 <- microbenchmark(Fib_while(1000))

print(paste("The time for n=1000 using the 'for' loop in seconds:",mean(bench_for1000$time)/1e6, 
            "+/-", round(sd(bench_for1000$time)/1e6,6), "seconds"))

print(paste("The time for n=1000 using the 'while' loop in seconds:",mean(bench_while1000$time)/1e6, 
            "+/-", round(sd(bench_while1000$time)/1e6,6), "seconds"))

#Question 3

fib100 <- Fib_for(100)
plot(c(1:100),fib100,type = "l", ylab = "Sequence", xlab = "Number of n")
#The number that starts to converge is 1.618034 and n = 16
##########################################################################

######################The golden ratio####################################

n <- c(1:100)
FI <- (sqrt(5)+1)/2
left_part <- sapply(n, function(x){FI^(x+1)})
right_part <- sapply(n, function(x){FI^x+FI^(x-1)})

left_part == right_part
all.equal(left_part, right_part)

##########################################################################

######################Game of craps####################################

dice1 <- floor(runif(1, min=1, max=7))
dice2 <- floor(runif(1, min=1, max=7))
x <- dice1+dice2

if (x==7|x==11){
  print("You won the game of craps!")
}else{
  while(TRUE){
    dice1 <- floor(runif(1, min=1, max=7))
    dice2 <- floor(runif(1, min=1, max=7))
    x_temp <- dice1+dice2
    print(paste("You roll: ", x_temp))
    if (x==x_temp){
      print("You won the game of craps!")
      break
    }else if (x_temp==7|x_temp==11){
      print("You lost the game of craps!")
      break
    }else{
      print("Keep rolling the dice")
    }
  }
}

##########################################################################

######################Readable and efficient code####################################
#Part 1
foobar0 <- function(x,z){
  if (sum(x >= .001) < 1) {
    stop("step 1 requires 1 observation(s) with value >= .001")
  }
  fit <- lm(x ~ z)
  r <- fit$residuals
  x <- sin(r) + .01
  if (sum(x >= .002) < 2) {
    stop("step 2 requires 2 observation(s) with value >= .002")
  }
  fit <- lm(x ~ z)
  r <- fit$residuals
  x <- 2 * sin(r) + .02
  if (sum(x >= .003) < 3) {
    stop("step 3 requires 3 observation(s) with value >= .003")
  }
  fit <- lm(x ~ z)
  r <- fit$residuals
  x <- 3 * sin(r) + .03
  if (sum(x >= .004) < 4) {
    stop("step 4 requires 4 observation(s) with value >= .004")
  }
  fit <- lm(x ~ z)
  r <- fit$residuals
  x <- 4 * sin(r) + .04
  return(x)
}

set.seed(1)
x <- rnorm(100)
z <- rnorm(100)
foobar0_result <- foobar0(x,z)

#Part 2
input <- function(x,threshold,step){
  if (sum(x >= threshold) < step) {
    paste("step", step, "requires", step, "observation(s) with value >= ", threshold)
  }
}
computation <- function(x,z,mul,add){
  fit <- lm(x ~ z)
  r <- fit$residuals
  x <- mul * sin(r) + add
  return(x)
}

foobar <- function(x,z){
  input(x,0.001,1)
  x <- computation(x,z,1,0.01)
  
  input(x,0.001,1)
  x <- computation(x,z,2,0.02)
  
  input(x,0.003,3)
  x <- computation(x,z,3,0.03)
  
  input(x,0.004,4)
  x <- computation(x,z,4,0.04)
  return(x)
}

set.seed(1)
x <- rnorm(100)
z <- rnorm(100)
foobar_result <- foobar(x,z)

#Part 3
all.equal(foobar0_result,foobar_result)
