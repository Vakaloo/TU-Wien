library("ISLR")
data(College,package="ISLR")

?College
str(College)

College <- na.omit(College) #Check for NA observations
College <- College[,-c(3,4)] #Remove the Accept and Enroll variables
College$Apps <- log(College$Apps) #log transformation

set.seed(12223236)
n <- nrow(College)
train <- sample(1:n,round(n*2/3))
test <- (1:n)[-train]

#Question 1(a)

library(MASS)

res.ridge <- lm.ridge(Apps~., data=College, lambda=seq(0,50, by=0.05), subset=train)
plot(res.ridge$lambda,res.ridge$GCV,type="l")

select(res.ridge)
lambda.opt <- res.ridge$lambda[which.min(res.ridge$GCV)]


#Question 1(b)

res.ridge.opt <- lm.ridge(Apps~., data=College, lambda = lambda.opt, subset=train) #scales variables
res.ridge.opt$coef #coefficients for scaled x

ridge.coef <- coef(res.ridge.opt) #unscaled with the intercepet

#Question 1(c)
test.data <- cbind(rep(1,length(test)),College[test,-2])
test.data$Private <- ifelse(test.data$Private=="Yes",1,0)
pred.ridge <- data.matrix(test.data)%*%ridge.coef
plot(College[test,"Apps"],pred.ridge)
abline(c(0,1))

mean((College[test,"Apps"]-pred.ridge)^2) # MSE_test

#Question 2(a)
library(glmnet)
data <- College
data$Private <- ifelse(data$Private=="Yes",1,0)
res.lasso <- glmnet(data.matrix(data[train,-2]),data[train,2], alpha=1)
plot(res.lasso)


"
The alpha parameter is mixing parameter, which gets values between [0,1]. Alpha parameter 
is calulated using the folmula (see ?glmnet) and it is actually combine the L1 and L2 penalties/
For alpha=1 we get the lasso penalty, and alpha=0 we get ridge penalty.
"


#Question 2(b)

res.cv <- cv.glmnet(data.matrix(data[train,-2]),data[train,2])
plot(res.cv)



#optimal tuning parameter
res.cv$lambda.1se

#regression coefficients
coef(res.cv,s="lambda.1se")


#Question 2(c)
pred.lasso <- predict(res.cv,newx=data.matrix(data[test,-2]),s="lambda.1se")

mean((data[test,"Apps"]-pred.lasso)^2)
plot(data[test,"Apps"],pred.lasso)
abline(c(0,1))








