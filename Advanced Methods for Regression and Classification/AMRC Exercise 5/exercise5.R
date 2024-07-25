

bank <- read.csv2("C:/Users/vaka1/Desktop/bank.csv")
bank <- na.omit(bank)

library(ggplot2)

ggplot(bank, aes(x=y)) +
  geom_histogram(stat="count")


library(Hmisc)
hist.data.frame(bank[,which(sapply(bank, is.numeric)==TRUE)])
hist.data.frame(bank[,which(sapply(bank, is.numeric)==FALSE)])



#Question 1(a)

set.seed(12223236)
train <- sample(1:nrow(bank), 3000)
test <- (1:nrow(bank))[-train]

bank$y <- ifelse(bank$y=="yes",1,0)
model.lr <- glm(y~., data=bank, subset=train, family="binomial")
summary(model.lr)


# Question 1(b)

pred <- predict(model.lr,bank[test,])

prob <- predict(model.lr,bank[test,], type="response")
predicted.classes <- ifelse(prob > 0.5, 1, 0)
actual.classes <- bank[test,]$y
TAB <- table(actual.classes,predicted.classes)

error.no <- 1 - TAB[1,1]/(TAB[1,2]+TAB[2,1]+TAB[1,1])
error.yes <- 1 - TAB[2,2]/(TAB[1,2]+TAB[2,1]+TAB[2,2])

# Question 1(c)


w0 <- length(train)/(2*table(bank[train,]$y)[1])
w1 <- length(train)/(2*table(bank[train,]$y)[2])

w <- ifelse(bank[train,]$y == 1, w1, w0)
model.lr.weight <- glm(y~., data=bank[train,], family="binomial",
                       weight = w)

prob <- predict(model.lr.weight,bank[test,], type="response")
predicted.classes <- ifelse(prob > 0.5, 1, 0)
actual.classes <- bank[test,]$y
TAB.weight <- table(actual.classes,predicted.classes)

error.no.weight <- 1 - TAB.weight[1,1]/(TAB.weight[1,2]+TAB.weight[2,1]+TAB.weight[1,1])
error.yes.weight <- 1 - TAB.weight[2,2]/(TAB.weight[1,2]+TAB.weight[2,1]+TAB.weight[2,2])

# Question 1(d)

model.lr.step <-  step(model.lr.weight,direction="both")

prob <- predict(model.lr.step,bank[test,], type="response")
predicted.classes <- ifelse(prob > 0.5, 1, 0)
actual.classes <- bank[test,]$y

TAB.step <- table(actual.classes,predicted.classes)

error.no.step<- 1 - TAB.step[1,1]/(TAB.step[1,2]+TAB.step[2,1]+TAB.step[1,1])
error.yes.step <- 1 - TAB.step[2,2]/(TAB.step[1,2]+TAB.step[2,1]+TAB.step[2,2])


#Question 2

library(ISLR)
data(Khan)


khan_train = data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
khan_test = data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))

kakka = data.frame(x = Khan$xtrain)

#Question 2(b)

library(glmnet)

res.cv <-  cv.glmnet(Khan$xtrain,as.factor(Khan$ytrain),family="multinomial",
                     type.measure = "class")
plot(res.cv)

#Question 2(c)

coef <- coef(res.cv,s="lambda.1se")

which(coef$`1`!=0)
which(coef$`2`!=0)
which(coef$`3`!=0)
which(coef$`4`!=0)

#Question 2(d)
plot(khan_train$x.175,khan_train$y)

#Question 2(e)

pred <- as.numeric(predict(res.cv,newx=Khan$xtest,s="lambda.1se",type="class"))
actual.classes <- Khan$ytest
TAB <- table(actual.classes,pred)
TAB
class.error<- 1-sum(diag(TAB))/sum(TAB)


