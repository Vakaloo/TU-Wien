library(ISLR)
library(dplyr)
library(Hmisc)


data("Auto")
data <- Auto
data <- data[,-9]
data$cylinders <- as.factor(data$cylinders)
data$origin <- as.factor(data$origin)

data$mpg <- log(data$mpg)
data$weight <- log(data$weight)
data$horsepower <- log(data$horsepower)
hist.data.frame(data)

#Question 1

newdata <- subset(data, select=c("mpg", "acceleration"))

American <- newdata[which(data$origin==1),]
European <- newdata[which(data$origin==2),]
Japanese <- newdata[which(data$origin==3),]

library(splines)

#Calculate the degrees of freedom using the smoothing splines

American.df <-  smooth.spline(American$acceleration, American$mpg, cv=TRUE)$df
European.df <-  smooth.spline(European$acceleration, European$mpg, cv=TRUE)$df
Japanese.df <-  smooth.spline(Japanese$acceleration, Japanese$mpg, cv=TRUE)$df

American.df <- round(American.df, digits = 0)
European.df <- round(European.df, digits = 0)
Japanese.df <- round(Japanese.df, digits = 0)

# Question 1(a)

#Sort the data based on acceleration
American <- American[order(American$acceleration),]
European <- European[order(European$acceleration),]
Japanese <- Japanese[order(Japanese$acceleration),]


lm.American <- lm(mpg ~ bs(acceleration, df=5), data = American)
lm.European <- lm(mpg ~ bs(acceleration, df=5), data = European)
lm.Japanese <- lm(mpg ~ bs(acceleration, df=5), data = Japanese)

par(mfrow = c(2, 2))
plot(American$acceleration, American$mpg, xlab="Acceleration", ylab="MPG", main = "American")
lines(American$acceleration, fitted(lm.American), col="blue")

plot(European$acceleration, European$mpg, xlab="Acceleration", ylab="MPG", main = "European")
lines(European$acceleration, fitted(lm.European), col="blue")

plot(Japanese$acceleration, Japanese$mpg, xlab="Acceleration", ylab="MPG", main = "Japanese")
lines(Japanese$acceleration, fitted(lm.Japanese), col="blue")

# Question 1(b)

lm.American <- lm(mpg ~ ns(acceleration, df=American.df), data = American)
lm.European <- lm(mpg ~ ns(acceleration, df=European.df), data = European)
lm.Japanese <- lm(mpg ~ ns(acceleration, df=Japanese.df), data = Japanese)

par(mfrow = c(2, 2))
plot(American$acceleration, American$mpg, xlab="Acceleration", ylab="MPG", main = "American")
lines(American$acceleration, fitted(lm.American), col="blue")

plot(European$acceleration, European$mpg, xlab="Acceleration", ylab="MPG", main = "European")
lines(European$acceleration, fitted(lm.European), col="blue")

plot(Japanese$acceleration, Japanese$mpg, xlab="Acceleration", ylab="MPG", main = "Japanese")
lines(Japanese$acceleration, fitted(lm.Japanese), col="blue")

# Question 1(c)

American <- newdata[which(data$origin==1),]
European <- newdata[which(data$origin==2),]
Japanese <- newdata[which(data$origin==3),]

American.smooth <-  smooth.spline(American$acceleration, American$mpg, cv=TRUE)
European.smooth <-  smooth.spline(European$acceleration, European$mpg, cv=TRUE)
Japanese.smooth <-  smooth.spline(Japanese$acceleration, Japanese$mpg, cv=TRUE)

par(mfrow = c(2, 2))
plot(American$acceleration, American$mpg, xlab="Acceleration", ylab="MPG", main = "American")
lines(American.smooth, col="blue")

plot(European$acceleration, European$mpg, xlab="Acceleration", ylab="MPG", main = "European")
lines(European.smooth, col="blue")

plot(Japanese$acceleration, Japanese$mpg, xlab="Acceleration", ylab="MPG", main = "Japanese")
lines(Japanese.smooth, col="blue")


#Question 2

set.seed(12223236)
n <- nrow(data)
train <- sample(1:n,round(n*2/3))
test <- (1:n)[-train]


model <- lm(mpg~cylinders+ns(displacement,4)+ns(horsepower,4)
            +ns(weight,4)+ns(acceleration,4)+ns(year,4)+origin, data=data,subset=train)

#Question 2(a)

pred <- predict(model,newdata=data[test,])
sqrt(mean((data$mpg[test]-pred)^2))

#Question 2(b)
model.step <-  step(model,direction="both")

pred <- predict(model.step,newdata=data[test,])
sqrt(mean((data$mpg[test]-pred)^2))


#Question 2(c)
X <- model.matrix(model.step)

plot(data[train,]$displacement,X[,6:9]%*%model.step$coefficients[6:9])

plot(data[train,]$horsepower,X[,10:13]%*%model.step$coefficients[10:13])

plot(data[train,]$weight,X[,14:17]%*%model.step$coefficients[14:17])

plot(data[train,]$acceleration,X[,18:21]%*%model.step$coefficients[18:21])

plot(data[train,]$year,X[,22:25]%*%model.step$coefficients[22:25])





