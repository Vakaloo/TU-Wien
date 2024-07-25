#Preliminary tasks
library(ISLR)


data(College,package="ISLR")
#?College
#str(College)


identical(College, na.omit(College)) #Check for NA observations
College = College[,-c(3,4)] #Remove the Accept and Enroll variables



hist(College$Apps, breaks = 10) 
College$Apps <- log(College$Apps)

hist(College$Apps, breaks = 10)

#Split the data into train and test
n <- nrow(College)
train <- sample(1:n,round(n*2/3))
test <- (1:n)[-train]

#Question 1(a)
res <- lm(Apps~., data = College, subset = train)
summary(res)
par(mfrow = c(2, 2))
plot(res)

#Question 1(b)
res <- model.matrix(Apps~., data = College)
betaHat = solve(t(res[train,])%*%res[train,])%*%t(res[train,])%*%College$Apps[train]
contrasts(College$Private) #R creates a dummy variable for the Private variable which takes Yes and No values

#Question 1(c)

res <- lm(Apps~., data = College, subset = train)
pred.train <- predict(res,newdata = College[train,])
pred.test <- predict(res,newdata = College[test,])

plot(College[train,"Apps"],pred.train,xlab="y measured",ylab="y predicted",cex.lab=1.3
     ,xlim=c(4,13),ylim=c(4,13))
plot(College[test,"Apps"],pred.test,xlab="y measured",ylab="y predicted",cex.lab=1.3,
     xlim=c(4,13),ylim=c(4,13))

#Question 1(d)

RMSE_train = sqrt(sum((College$Apps[train]-pred.train)^2)/length(train))
RMSE_test = sqrt(sum((College$Apps[test]-pred.test)^2)/length(test))

#Question 2
res <- lm(Apps~., data = College, subset = train)
pvalues = summary(res)$coefficients[,4]
pvalues = pvalues[-c(1)]                    #Remove the intercept Pvalue
new_variables = names(which(pvalues<=0.05)) #Select the variables with Pvalue<= 0.05
new_variables = c(new_variables, "Apps")    #Add the Apps variable to the new dataset

new_College = College[, (names(College) %in% new_variables)] #Create the new data

new_model = lm(Apps~., data = new_College, subset = train)
summary(new_model)


new_model <- model.matrix(Apps~., data = new_College)
new_betaHat = solve(t(new_model[train,])%*%new_model[train,])%*%t(new_model[train,])%*%new_College$Apps[train]


#Question 2b
new_model <- lm(Apps~., data = new_College, subset = train)
pred.train <- predict(new_model,newdata = new_College[train,])
pred.test <- predict(new_model,newdata = new_College[test,])

plot(new_College[train,"Apps"],pred.train,xlab="y measured",ylab="y predicted",cex.lab=1.3)
abline(c(0,1))
plot(new_College[test,"Apps"],pred.test,xlab="y measured",ylab="y predicted",cex.lab=1.3)
abline(c(0,1))


#Question 2c
RMSE_train_new = sqrt(sum((new_College$Apps[train]-pred.train)^2)/length(train))
RMSE_test_new = sqrt(sum((new_College$Apps[test]-pred.test)^2)/length(test))

#Question 2d
anova(res,new_model)



#Question 3

# backward selection:
model.backward <- step(res,direction="backward")
#summary(model_back)

# forward selection
model.empty <- lm(Apps~1,data=College, subset=train)
model.forward <- step(model.empty,scope=formula(res),direction="forward")
#summary(model_forward)

#Print the AIC value
AIC(model.backward)
AIC(model.forward)

#Predict
pred_back <- predict(model_back,College[test,])
pred_forward <- predict(model_forward,College[test,])

#Resulting models with RMSE
sqrt(mean((College$Apps[test]-pred_back))^2)
sqrt(mean((College$Apps[test]-pred_forward))^2)

#Plot y measures vs y prediction
plot(College[test,"Apps"],pred_back,xlab="y measured",ylab="y predicted",cex.lab=1.3)
abline(c(0,1))

plot(College[test,"Apps"],pred_forward,xlab="y measured",ylab="y predicted",cex.lab=1.3)
abline(c(0,1))

