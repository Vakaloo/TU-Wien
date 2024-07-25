library(rpart)
library(rpart.plot)

bank <- read.csv2("C:/Users/vaka1/Desktop/bank.csv")
bank <- na.omit(bank)

bank$job <- as.factor(bank$job)
bank$marital <- as.factor(bank$marital)
bank$education <- as.factor(bank$education)
bank$default <- as.factor(bank$default)
bank$housing <- as.factor(bank$housing)
bank$loan <- as.factor(bank$loan)
bank$contact <- as.factor(bank$contact)
bank$month <- as.factor(bank$month)
bank$poutcome <- as.factor(bank$poutcome)


bank$y <- ifelse(bank$y=="yes",1,0)
bank$y <- as.factor(bank$y)

bank$age <- as.numeric(bank$age)
bank$balance <- as.numeric(bank$balance)
bank$day <- as.numeric(bank$day)
bank$duration <- as.numeric(bank$duration)
bank$campaign <- as.numeric(bank$campaign)
bank$pdays <- as.numeric(bank$pdays)
bank$previous <- as.numeric(bank$previous)

set.seed(12223236)
n <- nrow(bank)
train <- sample(1:n,round(n*2/3))
test <- (1:n)[-train]

#Question 1(a)

model.tree <- rpart(y~.,data=bank, subset=train)

#Question 1(b)

plot(model.tree)
text(model.tree)
prp(model.tree)
#rpart.plot::rpart.plot(model.tree)

#Question 1(c)

predicted.classes <- predict(model.tree,newdata=bank[test,], type="class")
actual.classes <- bank[test,]$y
TAB <- table(actual.classes,predicted.classes)
TAB

misclassification.error <- 1-sum(diag(TAB))/sum(TAB)
misclassification.error

#Question 1(d)

printcp(model.tree)
plotcp(model.tree)

#Question 1(e)

model.tree.pruned <- prune(model.tree,cp=0.021)
plot(model.tree.pruned)
text(model.tree.pruned)
prp(model.tree.pruned)
#Question 1(f)

predicted.classes <- predict(model.tree.pruned,newdata=bank[test,], type="class")
actual.classes <- bank[test,]$y
TAB <- table(actual.classes,predicted.classes)
TAB

misclassification.error <- 1-sum(diag(TAB))/sum(TAB)
misclassification.error

#Question 1(g)


#Question 2(a)
library(randomForest)

model.random.forest <- randomForest(y~.,data=bank, subset=train)


predictions <- predict(model.random.forest,newdata=bank[test,], type="class")
actual <- bank[test,]$y
TAB.forest <- table(actual,predictions)
TAB.forest

misclassification.error.forest <- 1-sum(diag(TAB.forest))/sum(TAB.forest)
misclassification.error.forest


#Question 2(b)

model.random.forest <- randomForest(y~.,data=bank, subset=train,importance = TRUE)
plot(model.random.forest)
varImpPlot(model.random.forest)

#Question 2(c)

#Sampsize
model.random.forest <- randomForest(y~.,data=bank, subset=train,sampsize = c(160,160))

predictions <- predict(model.random.forest,newdata=bank[test,], type="class")
actual <- bank[test,]$y
TAB.forest <- table(actual,predictions)
TAB.forest

misclassification.error.forest <- 1-sum(diag(TAB.forest))/sum(TAB.forest)
misclassification.error.forest


#Classwt

w0 <- length(train)/(2*table(bank[train,]$y)[1])
w1 <- length(train)/(2*table(bank[train,]$y)[2])

model.random.forest <- randomForest(y~.,data=bank, subset=train,classwt = c(w0,w1))

predictions <- predict(model.random.forest,newdata=bank[test,], type="class")
actual <- bank[test,]$y
TAB.forest <- table(actual,predictions)
TAB.forest

misclassification.error.forest <- 1-sum(diag(TAB.forest))/sum(TAB.forest)
misclassification.error.forest

#cutoff

model.random.forest <- randomForest(y~.,data=bank, subset=train,cutoff = c(0.3,0.7))

predictions <- predict(model.random.forest,newdata=bank[test,], type="class")
actual <- bank[test,]$y
TAB.forest <- table(actual,predictions)
TAB.forest

misclassification.error.forest <- 1-sum(diag(TAB.forest))/sum(TAB.forest)
misclassification.error.forest

#strata

model.random.forest <- randomForest(y~.,data=bank, subset=train,strata = bank$y)

predictions <- predict(model.random.forest,newdata=bank[test,], type="class")
actual <- bank[test,]$y
TAB.forest <- table(actual,predictions)
TAB.forest

misclassification.error.forest <- 1-sum(diag(TAB.forest))/sum(TAB.forest)
misclassification.error.forest










