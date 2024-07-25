d <- read.csv("C:/Users/vaka1/Desktop/hcvdat1.csv")

d <- na.omit(d)

#Question 1

d$Sex <- ifelse(d$Sex=="m",1,0) #replace the males with 1 and females with 0

X <- d[,-1]
X <- data.frame(scale(X))
pca.data <- princomp(X, scores = TRUE)
scores <- pca.data$scores[,1:2]
plot(scores, pch=18, col=as.factor(d$Category))
legend("topleft",pch = 19, col = 1:3,
       legend = levels(factor(d$Category)))


#Question 2(a) (LDA)

#split the data into train and test
set.seed(12223236)
n <- nrow(d)
train <- sample(1:n,round(n*2/3))
test <- (1:n)[-train]

#Test set error
library(MASS)
model.lda <- lda(Category~., data=d[train,])
pred <- predict(model.lda,d[test,])
TAB <- table(d[test,]$Category,pred$class)
error.test.lda <- 1-sum(diag(TAB))/sum(TAB)
error.test.lda

confusionMatrix(factor(pred$class),factor(d[test,]$Category),mode = "everything")



#Cross validation Error
data <- d
mypredict.lda <- function(object, newdata) {predict(object, newdata = newdata)$class}
library(ipred)
control <- control.errorest(k = 5)

data$Category <- factor(data$Category)

model.ldacv <- errorest(Category~., data=data[train,],predict= mypredict.lda,
                     model=lda,est.para=control)
model.ldacv


#Question 2(b) (QDA)

model.qda <- qda(Category~., data=d[train,])
pred <- predict(model.qda,d[test,])
TAB <- table(d[test,]$Category,pred$class)
error.test.qda <- 1-sum(diag(TAB))/sum(TAB)
error.test.qda


mypredict.qda <- function(object, newdata) {predict(object, newdata = newdata)$class}

control <- control.errorest(k = 28)

data$Category <- factor(data$Category)

model.ldacv <- errorest(Category~., data=data[train,],predict= mypredict.qda,
                        model=qda,est.para=control)
model.ldacv

#Question 3(a)
library(caret)
set.seed(12223236)
train_str <- createDataPartition(d$Category,times = 1,p=2/3, list=F) #stratified separation
test_str <- (1:n)[-train_str]

model.lda <- lda(Category~., data=d[train_str,])
pred <- predict(model.lda,d[test_str,])
TAB <- table(d[test_str,]$Category,pred$class)
error.test.lda <- 1-sum(diag(TAB))/sum(TAB)
error.test.lda

confusionMatrix(pred$class,factor(d[test_str,]$Category),mode = "everything")
#F1 score


#test with sampling
library(caret)
train.data <- d[,-1]
class <- d$Category 

own_train <- downSample(x = train.data,
                        y = factor(class))


n <- nrow(own_train)
train_str <- createDataPartition(own_train$Class,p=2/3, list=F) #stratified separation
test_str <- (1:n)[-train_str]

library(MASS)
model.lda <- lda(Class~., data=own_train[train_str,])
pred <- predict(model.lda,own_train[test_str,])
TAB <- table(own_train[test_str,]$Class,pred$class)
error.test.lda <- 1-sum(diag(TAB))/sum(TAB)
error.test.lda

confusionMatrix(pred$class,own_train[test_str,]$Class,mode = "everything")



#Question 3(b)

#split the data into train and test
set.seed(12223236)
n <- nrow(d)
train <- sample(1:n,round(n*2/3))
test <- (1:n)[-train]


#train <- createDataPartition(d$Category,times = 1,p=2/3, list=F) #stratified separation
#test <- (1:n)[-train]

scores <- data.frame(pca.data$scores[,1:2])
pca.qda <- cbind(scores, Category=d$Category)


model.qda <- qda(Category~., data=pca.qda[train,])
pred <- predict(model.qda,pca.qda[test,])
TAB <- table(pca.qda[test,]$Category,pred$class)
error.test.qda <- 1-sum(diag(TAB))/sum(TAB)
error.test.qda



mypredict.qda <- function(object, newdata) {predict(object, newdata = newdata)$class}

control <- control.errorest(k = 5,random=FALSE)

pca.qda$Category <- factor(pca.qda$Category)
model.ldacv <- errorest(Category~., data=pca.qda[train,],predict= mypredict.qda,
                        model=qda,est.para=control)
model.ldacv$error






## Question 3(c)

library(rJava)
library(FSelector)
data.rf <- random.forest.importance(Category~., d, importance.type = 1)
data.rf















