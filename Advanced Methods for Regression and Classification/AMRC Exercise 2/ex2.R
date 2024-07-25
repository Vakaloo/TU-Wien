#Preliminary tasks
library(ISLR)
library(cvTools)


data(College,package="ISLR")
#?College
#str(College)


identical(College, na.omit(College)) #Check for NA observations
College = College[,-c(3,4)] #Remove the Accept and Enroll variables

College$Apps <- log(College$Apps)


#Split the data into train and test
set.seed(12223236)
n <- nrow(College)
train <- sample(1:n,round(n*2/3))
test <- (1:n)[-train]

#Question 1
fit <- lm(Apps~., data = College, subset = train)
cross.validation.fit <- cvFit(fit, data=College[train,], 
                        y=College[train,]$Apps, cost=rmspe, 
                        K=5,R=100,seed=12223236)

plot(cross.validation.fit, method = "bw")
plot(cross.validation.fit, method = "density")

###############################################################################################
#Question 2a
library(leaps)
sub.reg <- regsubsets(College$Apps~., data=College,  subset=train,
                         nbest=3,nvmax=10)

#Question 2b
plot(sub.reg)

#Question 2c
sub.summary <- summary(sub.reg)
str(sub.summary)
BIC <- summary(sub.reg)$bic
plot(BIC)

###############################################################################################

#Question 3a
library(pls)

#College$Private <- as.numeric(as.factor(College$Private))-1

pcr_model <- pcr(College$Apps~., data=College, scale=TRUE, subset=train,
                  validation="CV", segments=10, segment.type="random")
summary(pcr_model)

#Question 3b
plot(pcr_model, plottype = "validation", val.type = "RMSEP", legend = "topright")
pred_pcr <- predict(pcr_model,newdata=College[train,],ncomp=10)
sqrt(mean((College$Apps[train]-pred_pcr)^2))


#Question 3c
predplot(pcr_model,ncomp=10,asp=1, line=TRUE)

#Question 3d

pred_pcr <- predict(pcr_model,newdata = College[test,],ncomp=10)
plot(College[test,]$Apps, pred_pcr, xlab="measured", ylab="predicted",)
abline(0,1)
sqrt(mean((College$Apps[test]-pred_pcr)^2))

###############################################################################################

#Question 4a
pls_model <- plsr(College$Apps~., data=College, scale=TRUE, subset=train,
                  validation="CV", segments=10, segment.type="random")
summary(pls_model)

#Question 4b
plot(pls_model, plottype = "validation", val.type = "RMSEP", legend = "topright")
pred_pls <- predict(pls_model,newdata=College[train,],ncomp=4)
sqrt(mean((College$Apps[train]-pred_pls)^2))

#Question 4c
predplot(pls_model,ncomp=3,asp=1, line=TRUE)


#Question 4d

pred_pls <- predict(pls_model,newdata=College[test,],ncomp=3)
plot(College[test,]$Apps, pred_pls, xlab="measured", ylab="predicted",)
abline(0,1)
sqrt(mean((College$Apps[test]-pred_pls)^2))


#Question 5
X <- College[,-c(2)]
#X <- College
X$Private <- as.numeric(as.factor(X$Private))-1

X <- data.frame(scale(X))
pca <- princomp(~.,data=X,subset=train,scores=TRUE)
scores <- data.frame(pca$scores)[1:10]
loadings <- data.frame(unclass(pca$loadings))[1:10]

model <- lm(College$Apps[train]~., data = scores)

Z <- data.frame(as.matrix(X[test,]) %*% as.matrix(loadings))

pred <- predict(model,Z)
sqrt(mean((College$Apps[test]-pred)^2))
