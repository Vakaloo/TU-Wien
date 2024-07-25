# Task 1.1

#softmax function
softmax_R <- function(x, y){
  sign(x) * pmax(abs(x) - y, 0)
}

lasso_shoot_algorithm <- function(X,y,lambda = 10,epsilon = 1e-5, max_iter = 10000){
  
  X <- as.matrix(X)
  
  p <- ncol(X)
  beta <- numeric(p)
  
  converged <- FALSE
  iteration <- 0
  
  XX <- crossprod(X, X)
  Xy <- crossprod(X, y)
  
  while (!converged & (iteration < max_iter)){
    
    beta_prev <- beta
    for (j in 1:p){
      a <- 2 * XX[j,j]
      c <- 2 * (Xy[j] - sum(XX[j,] %*% beta) + beta[j] * XX[j,j])
      beta[j] <- softmax_R(c/a, lambda/a)
    }
    iteration <- iteration + 1
    converged <- sum(abs(beta - beta_prev)) < epsilon
  }
  # Add the intercept
  beta <- c(mean(y),beta)
  beta_coef <- list(beta = beta, n_iter = iteration, converged = converged)
  return(beta_coef)
}
# Create artificial data
set.seed(12223236)

n <- 500
p <- 20
X <- matrix(rnorm(n*p), ncol=p)
X <- scale(X)
eps <- rnorm(n, sd=5)
beta <- 3:5
y <- 2 + X[,1:3] %*% beta + eps

startTime <- Sys.time()
lasso_shoot_algorithm(X,y)
endTime <- Sys.time()
# prints recorded time
print(endTime - startTime)



# Task 1.2

lasso_shoot_algorithm_modified <- function(X,y, lambda, epsilon = 1e-5, max_iter = 10000){
  
  X <- as.matrix(X)
  
  p <- ncol(X)
  # the dimension is length(lambda) for the rows and for the columns p+1 for all the coefs plus the intercept
  beta_coef <- matrix(0, nrow = length(lambda), ncol = p+1)
  
  for (k in 1:length(lambda)){
    p <- ncol(X)
    beta <- numeric(p)
    
    converged <- FALSE
    iteration <- 0
    
    XX <- crossprod(X, X)
    Xy <- crossprod(X, y)
    
    while (!converged & (iteration < max_iter)){
      
      beta_prev <- beta
      for (j in 1:p){
        a <- 2 * XX[j,j]
        c <- 2 * (Xy[j] - sum(XX[j,] %*% beta) + beta[j] * XX[j,j])
        beta[j] <- softmax_R(c/a, lambda[k]/a)
      }
      iteration <- iteration + 1
      converged <- sum(abs(beta - beta_prev)) < epsilon
    }
    # Add the intercept
    beta <- c(mean(y),beta)
    beta_coef[k,] <- beta
  }
  return(beta_coef)
}
# lambda.grid <-  seq(0,15, by=0.2)
# result <- lasso_shoot_algorithm_modified(X,y,lambda)
# rownames(result) <- as.character(lambda) 
# result

# Task 1.3

# Own implementation (shooting algorithm)
startTime <- Sys.time()

lasso_implementation <- lasso_shoot_algorithm_modified(X,y,lambda.grid)

endTime <- Sys.time()
# prints recorded time
print(endTime - startTime)

rownames(lasso_implementation) <- as.character(lambda.grid) 
lasso_implementation

library(glmnet)
startTime <- Sys.time()
lasso_glmnet <- glmnet(x=X, y=y, alpha=1, lambda=lambda.grid)
endTime <- Sys.time()
# prints recorded time
print(endTime - startTime)

head(lasso_glmnet$beta)

# Task 1.3

microbenchmark::microbenchmark("Own Implementation of Lasso" = lasso_shoot_algorithm_modified(X,y,lambda.grid),
                               "Glmnet Lasso Implementation" = glmnet(x=X, y=y, alpha=1, lambda=lambda.grid))


# Task 1.4

cross_validation <- function(X,y,lambda.grid = c(1,10),type.measure = "mse", num_folds = 10){

  n <- length(lambda.grid)
  #Initializations
  cvm <- numeric(n)
  cvsd <- numeric(n)
  cvup <- numeric(n)
  cvlo <- numeric(n)
  nzero <- numeric(n)
  lambda.min <- 0
  lambda.1se <- 0
  index <- matrix(c(NA,NA),nrow=2)
  
  for (i in 1:n){
    fold_size <- nrow(X) / num_folds
    cv.error <- numeric(num_folds)
    betas <- matrix(0,nrow = num_folds, ncol = ncol(X))
    set.seed(12223236)
    for (fold in 1:num_folds){
      
      validation_indices <- ((fold - 1) * fold_size + 1):(fold * fold_size)
      validation_set_X <- X[validation_indices, ]
      validation_set_y <- y[validation_indices]
      
      training_indices <- setdiff(1:nrow(X), validation_indices)
      training_set_X <- X[training_indices, ]
      training_set_y <- y[training_indices]
      
      coeff <- lasso_shoot_algorithm(training_set_X, training_set_y,lambda.grid[i])
      
      
      pred <- cbind(rep(1,length(validation_indices)),validation_set_X) %*% coeff$beta
      
      
      beta.coef <- coeff$beta[-1]
      betas[fold,] <- beta.coef
      
      if (type.measure == "mse"){
        cv.error[fold] <- mean((validation_set_y-pred)^2)
        name = "Mean Squared Error"
      }
      else if (type.measure == "rmse"){
        cv.error[fold] <- sqrt(mean((validation_set_y-pred)^2))
        name = "Root Mean Squared Error"
      }
    }
    
    cvm[i] <- mean(cv.error)
    cvsd[i] <- sd(cv.error)
    nzero[i] <- sum(rowMeans(betas)!=0)
    
  }
  cvup <- cvm + cvsd
  cvlo <- cvm - cvsd
  
  df <- data.frame(cvm=cvm,cvsd=cvsd,lambda.grid = lambda.grid,id=1:n)
  index[1,]<- which.min(df$cvm)
  lambda.min <- lambda.grid[index[1,]]
  df_filter <- df[min(df$cvm)+df$cvsd[index[1,]]>=df$cvm,]
  df_filter <- dplyr::filter(df_filter,cvm==max(cvm))
  index[2,] <- df_filter$id
  lambda.1se <- lambda.grid[index[2,]]
  
  output <- list(cvm = cvm,
                 cvsd = cvsd,
                 name=name,
                 cvup = cvup,
                 cvlo = cvlo,
                 index=index,
                 lambda.1se=lambda.1se,
                 lambda.min=lambda.min,
                 lambda=lambda.grid,
                 nzero=nzero)

  attr(output,"class") <- "cv.glmnet"
  return(output)
}

lambda.grid <- 10^seq(-2,10, length=20)
res <- cross_validation(X,y,lambda.grid)

# cv.glmnet implementation
glmnet.cv <- cv.glmnet(X,y,nfolds = 10,lambda = lambda.grid)



# Task 2

install.packages("ISLR")
library(ISLR)
data("Hitters")
str(Hitters)
head(Hitters)

df_hitters <- Hitters

# Remove the NAs
df_hitters <- na.omit(df_hitters)

y <- df_hitters$Salary
X <- df_hitters[,!names(df_hitters) %in% "Salary"]

#center the integer variables
indx <- sapply(X, is.integer)
X[indx] <- lapply(X[indx], function(x) scale(x,center = TRUE, scale = FALSE))

#Convert the factor variables to integer
indx <- sapply(X, is.factor)
X[indx] <- lapply(X[indx], function(x) as.integer(x))

X <- as.matrix(X)

# Split the data into training and testing data with a ratio of 70:30
set.seed(12223236)
n <- nrow(X)
train <- sample(1:n,round(n*0.7))
test <- (1:n)[-train]

train_X <- X[train,]
train_y <- y[train]

test_X <- X[test,]
test_y <- y[test]


# Task 2.1

lambda.grid <- 10^seq(-2,10, length=20)


# Own lasso implementation
startTime <- Sys.time()
own.res <- cross_validation(train_X, train_y, lambda.grid)
endTime <- Sys.time()
# prints recorded time
print(endTime - startTime)


# Glmnet lasso implementation
# startTime <- Sys.time()
# cv.glmnet.res <- cv.glmnet(train_X,train_y,nfolds = 10,lambda = lambda.grid)
# endTime <- Sys.time()
# prints recorded time
# print(endTime - startTime)

# Start the plot for the whole path for the coefficients.
beta.df <- data.frame()

for (lambda in lambda.grid){
  beta.coeff <- lasso_shoot_algorithm(train_X, train_y,lambda)$beta
  beta.df <- rbind(beta.df,beta.coeff[-1])
}

beta_vector <- character(ncol(X))
for (i in 1:length(beta_vector)) {
  beta_vector[i] <- paste0("Beta_", i)
}

colnames(beta.df) <- beta_vector
beta.df <- cbind(beta.df, lambda.grid)
  
library(ggplot2)
library("tidyverse")

beta.df <- beta.df %>%
  gather(key = "variable", value = "value", -lambda.grid)

ggplot(beta.df, aes(x = log(lambda.grid), y = value)) + 
  geom_line(aes(color = variable), size = 0.8) + 
  ggtitle("Path for the Coefficients") +
  xlab("Log(lambda)") + 
  ylab("Beta Values") + 
  labs(color = "Beta Coefficients")
  
# Task 2.2
library(glmnet)

# GLMNET implementation

# Lasso where alpha = 1
# We are going to create the model with the train data and also fit it with the train data and not the test data
# The lambda is used from the cv.glmnet and it is the min lambda
res.glmnet.lasso <- glmnet(train_X,train_y, alpha=1, lambda = cv.glmnet.res$lambda.min)

pred.lasso <- predict(res.glmnet.lasso, newx = train_X, s = cv.glmnet.res$lambda.min)

mean((train_y-pred.lasso)^2)
sqrt(mean((train_y-pred.lasso)^2))


# OWN LASSO implementation
res.own.lasso <- lasso_shoot_algorithm(train_X, train_y,own.res$lambda.min)
pred.own <- cbind(rep(1,nrow(train_X)),train_X) %*% res.own.lasso$beta

mean((train_y-pred.own)^2)
sqrt(mean((train_y-pred.own)^2))


# Task 2.3

# ridge regression glmnet
# alpha = 0
ridge.regression <- glmnet(train_X,train_y, alpha=0)

ls.regression <- lm(train_y~.,data=data.frame(train_X))

# Task 2.4

# Lasso

pred.lasso <- predict(res.glmnet.lasso, newx = test_X, s = cv.glmnet.res$lambda.min)
mean((test_y-pred.lasso)^2)
sqrt(mean((test_y-pred.lasso)^2))


# Ridge

pred.ridge<- predict(ridge.regression, newx = test_X)
mean((test_y-pred.ridge)^2)
sqrt(mean((test_y-pred.ridge)^2))


# Least squares (best model)

pred.ls <- predict(ls.regression, newdata = data.frame(test_X))
mean((test_y-pred.ls)^2)
sqrt(mean((test_y-pred.ls)^2))






