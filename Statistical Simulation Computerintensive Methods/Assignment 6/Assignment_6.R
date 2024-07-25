# Task 1
library(ISLR)
data(Auto)
head(Auto)

df <- Auto[c("mpg", "horsepower")]

linear.model <- lm(mpg ~ horsepower, data=df)
quadratic.model <- lm(mpg ~ poly(horsepower,2), data=df)
cubic.model <- lm(mpg ~ poly(horsepower,3), data=df)

# Task 1.1

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

scatter1.1 <- ggplot(df, aes(x = horsepower, y = mpg)) +
  geom_point() +
  labs(title = "Scatterplot",
       x = "Horsepower",
       y = "Miles Per Gallon")+
  theme(plot.title = element_text(hjust = 0.5))

scatter1.1 +
  geom_smooth(method='lm', formula= y ~ x, se = F, aes(color='Linear')) +
  geom_smooth(method='lm', formula= y ~ poly(x,2), se = F, aes(color='Quadratic')) +
  geom_smooth(method='lm', formula= y ~ poly(x,3), se = F, aes(color='Cubic')) +
  scale_color_manual(name='Models',
                     breaks=c('Linear', 'Quadratic', 'Cubic'),
                     values=c('Cubic'='green', 'Quadratic'='blue', 'Linear'='red'))



# Task 1.2

performance <- function(model, data, per){
  
  set.seed(12223236)
  n <- nrow(data)
  train <- sample(1:n,round(n*per))
  test <- (1:n)[-train]
  
  if (model == "Linear Model"){
    model <- lm(mpg ~ horsepower, data=data[train,])
  }
  else if (model == "Quadratic Model"){
    model <- lm(mpg ~ poly(horsepower,2), data=data[train,])
  }
  else {
    model <- lm(mpg ~ poly(horsepower,3), data=data[train,])
  }
  
  y_test <- data$mpg[test]
  y_pred <- predict(model, newdata = data[test,])


  RMSE <- sqrt(mean((y_test-y_pred)^2))
  MSE <- mean((y_test-y_pred)^2)
  MAD <- median(abs((y_test-y_pred-median(y_test-y_pred))))
  
  result <- data.frame(RMSE = RMSE, MSE = MSE, MAD = MAD)
  return(result)
}

models <- c("Linear Model", "Quadratic Model", "Cubic Model")

# 50% train and test split
result.50 <- rbind(performance(models[1],df,0.5),
                   performance(models[2],df,0.5),
                   performance(models[3],df,0.5))

cat("The best model based on Root Mean Squared Error is: ", models[which.min(result.50$RMSE)])
cat("\nThe best model based on Mean Squared Error is: ", models[which.min(result.50$MSE)])
cat("\nThe best model based on Median Absolute Deviation is: ", models[which.min(result.50$MAD)])

# 70% train and test split
result.70 <- rbind(performance(models[1],df,0.7),
                   performance(models[2],df,0.7),
                   performance(models[3],df,0.7))

cat("The best model based on Root Mean Squared Error is: ", models[which.min(result.70$RMSE)])
cat("\nThe best model based on Mean Squared Error is: ", models[which.min(result.70$MSE)])
cat("\nThe best model based on Median Absolute Deviation is: ", models[which.min(result.70$MAD)])

# Task 1.3

mse <- function(y_test,y_pred){mean((y_test-y_pred)^2)}
rmse <- function(y_test,y_pred){sqrt(mean((y_test-y_pred)^2))}
mad <- function(y_test,y_pred){median(abs((y_test-y_pred-median(y_test-y_pred))))}

if (!require("boot")) install.packages("boot")
library(boot)

glm.linear.model <- glm(mpg ~ horsepower, data=df)
glm.quadratic.model <- glm(mpg ~ poly(horsepower,2), data=df)
glm.cubic.model <- glm(mpg ~ poly(horsepower,3), data=df)

# The first element of delta is the  raw cross-validation estimate of prediction error. The second is the adjusted.

all.models <- list(glm.linear.model, glm.quadratic.model, glm.cubic.model)
cost.functions <- list(mse,rmse)

###############################################
# LOOCV
startTime <- Sys.time()
LOOCV <- matrix(0, nrow = 3, ncol = 3)

for (i in 1:nrow(LOOCV)){
  for (j in 1:ncol(LOOCV)){
    set.seed(12223236)
    LOOCV[i,j] <- cv.glm(df, all.models[[i]], cost = cost.functions[[j]])$delta[1]
  }
}
endTime <- Sys.time()
# prints recorded time
print(endTime - startTime)
###############################################
# 5-fold CV
startTime <- Sys.time()
Fold_5_CV <- matrix(0, nrow = 3, ncol = 3)

for (i in 1:nrow(Fold_5_CV)){
  for (j in 1:ncol(Fold_5_CV)){
    set.seed(12223236)
    Fold_5_CV[i,j] <- cv.glm(df, all.models[[i]], cost = cost.functions[[j]], K = 5)$delta[1]
  }
}
endTime <- Sys.time()
# prints recorded time
print(endTime - startTime)
###############################################
# 10-fold CV
startTime <- Sys.time()
Fold_10_CV <- matrix(0, nrow = 3, ncol = 3)

for (i in 1:nrow(Fold_10_CV)){
  for (j in 1:ncol(Fold_10_CV)){
    set.seed(12223236)
    Fold_10_CV[i,j] <- cv.glm(df, all.models[[i]], cost = cost.functions[[j]], K = 10)$delta[1]
  }
}
endTime <- Sys.time()
# prints recorded time
print(endTime - startTime)
###############################################
LOOCV <- as.data.frame(LOOCV)
rownames(LOOCV) <- c("Linear_model", "Quadratic_model", "Cubic_model")
colnames(LOOCV) <- c("MSE", "RMSE", "MAD")

Fold_5_CV <- as.data.frame(Fold_5_CV)
rownames(Fold_5_CV) <- c("Linear_model", "Quadratic_model", "Cubic_model")
colnames(Fold_5_CV) <- c("MSE", "RMSE", "MAD")

Fold_10_CV <- as.data.frame(Fold_10_CV)
rownames(Fold_10_CV) <- c("Linear_model", "Quadratic_model", "Cubic_model")
colnames(Fold_10_CV) <- c("MSE", "RMSE", "MAD")

# I have 5 tables:
# train/test split of 50%/50%
# train/test split of 70%/30%
# LOOCV 
# 5-Fold
# 10-Fold

# The best model out of all models is the quadratic model. 

# Task 2

data(economics)
head(economics)
dim(economics)
summary(economics)

df_economics <- data.frame(economics[c("uempmed", "unemploy")])

# Task 2.1

# dependent = uempmed / independent = unemploy

linear <- lm(uempmed ~ unemploy, data=df_economics)
logarithmic <- lm(uempmed ~ exp(scale(unemploy)), data=df_economics)
polynomial.2 <- lm(uempmed ~ poly(unemploy,2), data=df_economics)
polynomial.3 <- lm(uempmed ~ poly(unemploy,3), data=df_economics)
polynomial.10 <- lm(uempmed ~ poly(unemploy,10), data=df_economics)

# dependent = unemploy / independent = uempmed

linear <- lm(unemploy ~ uempmed, data=df_economics)
logarithmic <- lm(unemploy ~ log(uempmed), data=df_economics)
polynomial.2 <- lm(unemploy ~ poly(uempmed,2), data=df_economics)
polynomial.3 <- lm(unemploy ~ poly(uempmed,3), data=df_economics)
polynomial.10 <- lm(unemploy ~ poly(uempmed,10), data=df_economics)

# Task 2.2

# dependent = uempmed / independent = unemploy (we have to scale because if we take the exponential)
scatter2.1 <- ggplot(df_economics, aes(x = unemploy, y = uempmed)) +
  geom_point() +
  labs(title = "Scatterplot",
       x = "Number of Unemployed in Thousands",
       y = "Median Duration of Unemployment in Weeks")+
  theme(plot.title = element_text(hjust = 0.5))

plot(scatter2.1)

# (we have to scale the unemploy variable because if we take the exponential, we have very large value and the exp is infinity)
ggplot(df_economics, aes(x = scale(unemploy), y = uempmed)) +
  geom_point() +
  labs(title = "Scatterplot",
       x = "Number of Unemployed in Thousands",
       y = "Median Duration of Unemployment in Weeks")+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method='lm', formula= y ~ x, se = F, color = "red") +
  geom_smooth(method='lm', formula= y ~ exp(x), se = F, color = "green") +
  geom_smooth(method='lm', formula= y ~ poly(x,2), se = F, color = "blue") +
  geom_smooth(method='lm', formula= y ~ poly(x,3), se = F, color = "cyan") +
  geom_smooth(method='lm', formula= y ~ poly(x,10), se = F, color = "yellow")


# dependent = unemploy / independent = uempmed

scatter2.2 <- ggplot(df_economics, aes(x = uempmed, y = unemploy)) +
  geom_point() +
  labs(title = "Scatterplot",
       x = "Median Duration of Unemployment in Weeks",
       y = "Number of Unemployed in Thousands")+
  theme(plot.title = element_text(hjust = 0.5))

plot(scatter2.2)

scatter2.2 +
  geom_smooth(method='lm', formula= y ~ x, se = F, color = "red") +
  geom_smooth(method='lm', formula= y ~ log(x), se = F, color = "green") +
  geom_smooth(method='lm', formula= y ~ poly(x,2), se = F, color = "blue") +
  geom_smooth(method='lm', formula= y ~ poly(x,3), se = F, color = "cyan") +
  geom_smooth(method='lm', formula= y ~ poly(x,10), se = F, color = "yellow")



# Task 2.3

# We are going to scale the unemploy variable

# Case 1 dependent = uempmed / independent = unemploy

glm.linear.case.1 <- glm(uempmed ~ unemploy, data=df_economics)
glm.exponential.case.1 <- glm(uempmed ~ exp(scale(unemploy)), data=df_economics)
glm.polynomial.2.case.1 <- glm(uempmed ~ poly(unemploy,2), data=df_economics)
glm.polynomial.3.case.1<- glm(uempmed ~ poly(unemploy,3), data=df_economics)
glm.polynomial.10.case.1 <- glm(uempmed ~ poly(unemploy,10), data=df_economics)


all.models.case.1 <- list(glm.linear.case.1, 
                          glm.exponential.case.1, 
                          glm.polynomial.2.case.1,
                          glm.polynomial.3.case.1,
                          glm.polynomial.10.case.1)


###############################################
# LOOCV
startTime <- Sys.time()
LOOCV <- matrix(0, nrow = 5, ncol = 2)

for (i in 1:nrow(LOOCV)){
  for (j in 1:ncol(LOOCV)){
    set.seed(12223236)
    LOOCV[i,j] <- cv.glm(df_economics, all.models.case.1[[i]], cost = cost.functions[[j]])$delta[1]
  }
}
endTime <- Sys.time()
# prints recorded time 
print(endTime - startTime)
###############################################
# 5-fold CV
startTime <- Sys.time()
Fold_5_CV <- matrix(0, nrow = 5, ncol = 2)

for (i in 1:nrow(Fold_5_CV)){
  for (j in 1:ncol(Fold_5_CV)){
    set.seed(12223236)
    Fold_5_CV[i,j] <- cv.glm(df_economics, all.models.case.1[[i]], cost = cost.functions[[j]], K = 5)$delta[1]
  }
}
endTime <- Sys.time()
# prints recorded time
print(endTime - startTime)
###############################################
# 10-fold CV
startTime <- Sys.time()
Fold_10_CV <- matrix(0, nrow = 5, ncol = 2)

for (i in 1:nrow(Fold_10_CV)){
  for (j in 1:ncol(Fold_10_CV)){
    set.seed(12223236)
    Fold_10_CV[i,j] <- cv.glm(df_economics, all.models.case.1[[i]], cost = cost.functions[[j]], K = 10)$delta[1]
  }
}
endTime <- Sys.time()
# prints recorded time
print(endTime - startTime)
###############################################

LOOCV <- as.data.frame(LOOCV)
rownames(LOOCV) <- c("Linear_model", "Exponential_model", "Quadratic_model", "Cubic_model", "Polynomial_Degree_10")
colnames(LOOCV) <- c("MSE", "RMSE")

Fold_5_CV <- as.data.frame(Fold_5_CV)
rownames(Fold_5_CV) <- c("Linear_model", "Exponential_model", "Quadratic_model", "Cubic_model", "Polynomial_Degree_10")
colnames(Fold_5_CV) <- c("MSE", "RMSE")

Fold_10_CV <- as.data.frame(Fold_10_CV)
rownames(Fold_10_CV) <- c("Linear_model", "Exponential_model", "Quadratic_model", "Cubic_model", "Polynomial_Degree_10")
colnames(Fold_10_CV) <- c("MSE", "RMSE")



########################################################################################

# Case 2 dependent = unemploy / independent = uempmed

glm.linear.case.2 <- glm(unemploy ~ uempmed, data=df_economics)
glm.logarithmic.case.2 <- glm(unemploy ~ log(uempmed), data=df_economics)
glm.polynomial.2.case.2 <- glm(unemploy ~ poly(uempmed,2), data=df_economics)
glm.polynomial.3.case.2 <- glm(unemploy ~ poly(uempmed,3), data=df_economics)
glm.polynomial.10.case.2 <- glm(unemploy ~ poly(uempmed,10), data=df_economics)


all.models.case.2 <- list(glm.linear.case.2, 
                          glm.logarithmic.case.2, 
                          glm.polynomial.2.case.2,
                          glm.polynomial.3.case.2,
                          glm.polynomial.10.case.2)


###############################################
# LOOCV
startTime <- Sys.time()
LOOCV <- matrix(0, nrow = 5, ncol = 2)

for (i in 1:nrow(LOOCV)){
  for (j in 1:ncol(LOOCV)){
    set.seed(12223236)
    LOOCV[i,j] <- cv.glm(df_economics, all.models.case.2[[i]], cost = cost.functions[[j]])$delta[1]
  }
}
endTime <- Sys.time()
# prints recorded time 
print(endTime - startTime)
###############################################
# 5-fold CV
startTime <- Sys.time()
Fold_5_CV <- matrix(0, nrow = 5, ncol = 2)

for (i in 1:nrow(Fold_5_CV)){
  for (j in 1:ncol(Fold_5_CV)){
    set.seed(12223236)
    Fold_5_CV[i,j] <- cv.glm(df_economics, all.models.case.2[[i]], cost = cost.functions[[j]], K = 5)$delta[1]
  }
}
endTime <- Sys.time()
# prints recorded time
print(endTime - startTime)
###############################################
# 10-fold CV
startTime <- Sys.time()
Fold_10_CV <- matrix(0, nrow = 5, ncol = 2)

for (i in 1:nrow(Fold_10_CV)){
  for (j in 1:ncol(Fold_10_CV)){
    set.seed(12223236)
    Fold_10_CV[i,j] <- cv.glm(df_economics, all.models.case.2[[i]], cost = cost.functions[[j]], K = 10)$delta[1]
  }
}
endTime <- Sys.time()
# prints recorded time
print(endTime - startTime)
###############################################

LOOCV <- as.data.frame(LOOCV)
rownames(LOOCV) <- c("Linear_model", "Logarithmic_model", "Quadratic_model", "Cubic_model", "Polynomial_Degree_10")
colnames(LOOCV) <- c("MSE", "RMSE")

Fold_5_CV <- as.data.frame(Fold_5_CV)
rownames(Fold_5_CV) <- c("Linear_model", "Logarithmic_model", "Quadratic_model", "Cubic_model", "Polynomial_Degree_10")
colnames(Fold_5_CV) <- c("MSE", "RMSE")

Fold_10_CV <- as.data.frame(Fold_10_CV)
rownames(Fold_10_CV) <- c("Linear_model", "Logarithmic_model", "Quadratic_model", "Cubic_model", "Polynomial_Degree_10")
colnames(Fold_10_CV) <- c("MSE", "RMSE")





