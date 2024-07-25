### Linear Methods for Classification

###Linear Regression based on an Indicator Matrix

set.seed(1234)
grp <- c(rep(1,100),rep(2,100))
g1 <- matrix(rnorm(2*100,0,1), ncol = 2)
g2 <- matrix(rnorm(2*100,4,1), ncol = 2)
x <- rbind(g1,g2)
plot(x,pch=grp,col=grp+2,xlab="X1", ylab="X2")

# Separating line:
y <- c(rep(-1,100), rep(1,100))
res <- lm(y~x)
cf <- coef(res)
# (Intercept)          x1          x2 
#   0.8645231  -0.2375487  -0.1932401
# b0+b1*x1+b2*x2=0
# => x1 = -b0/b1 - b2/b1 * x2 = A + B*x2
A <- -cf[1]/cf[2]
B <- -cf[3]/cf[2]
x2 <- seq(from=-10,to=10,by=0.01)
x1 <- A+B*x2
lines(x1,x2,col=2)



y1 <- c(rep(1,100), rep(0,100))
y2 <- c(rep(0,100), rep(1,100))
y <- cbind(y1,y2)

df <- data.frame(y=y,x=x)
res.lm <- lm(y~x)
res.lm

apply(predict(res.lm,df),1,which.max)

showsep.lm <- function(lm.object,dat,resol=100)
{
# Show separation by linear regression
# lm.object ... reulting lm object
# dat ... data which have been used for lm
# resol ... resolution of separating points
xmi <- min(x[,1])
xma <- max(x[,1])
xr <- xma-xmi
ymi <- min(x[,2])
yma <- max(x[,2])
yr <- yma-ymi

x1.all <- seq(from=xmi-xr/10,to=xma+xr/10,length=100)
x2.all <- seq(from=ymi-yr/10,to=yma+yr/10,length=100)
x1all <- rep(x1.all,length(x2.all))
x2all <- sort(rep(x2.all,length(x1.all)))
xall <- cbind(x1all,x2all)
xall.pred <- xall%*%lm.object$coef[-1,]+rep(1,nrow(xall))%*%t(lm.object$coef[1,])
ind1 <- seq(1,nrow(xall))[xall.pred[,1]>xall.pred[,2]]
ind2 <- seq(1,nrow(xall))[xall.pred[,1]<=xall.pred[,2]]
plot(x,pch=grp,col=grp+2,xlab="X1",ylab="X2",type="n",
     xlim=c(xmi,xma),ylim=c(ymi,yma))
points(xall[ind1,],col="lightgreen",pch=20,cex=0.1)
points(xall[ind2,],col="steelblue",pch=20,cex=0.1)
points(x,pch=grp,col=grp+2)
}
showsep.lm(res.lm,x,resol=100)


##############################################################
# other example:
set.seed(1234)
library(mvtnorm)
grp <- c(rep(1,100),rep(2,100))
g1 <- rmvnorm(100,mean=c(0,0),sigma = matrix(c(1.2,1,1,1.2), ncol = 2))
g2 <- rmvnorm(100,mean=c(2,2),sigma = matrix(c(1,-1,-1,2), ncol = 2))
x <- rbind(g1,g2)
plot(x,pch=grp,col=grp+2,xlab="X1", ylab="X2")

y1 <- c(rep(1,100), rep(0,100))
y2 <- c(rep(0,100), rep(1,100))
y <- cbind(y1,y2)

res.lm <- lm(y~x)
res.lm
showsep.lm(res.lm,x,resol=100)


#############################################
# 3 groups:

set.seed(1234)
grp <- c(rep(1,100),rep(2,100),rep(3,100))
g1 <- matrix(rnorm(2*100,0,1), ncol = 2)
g2 <- matrix(rnorm(2*100,8,1), ncol = 2)
g3 <- cbind(rnorm(100,2,1),rnorm(100,6,1))
x <- rbind(g1,g2,g3)
plot(x,pch=grp,col=grp+4,xlab="X1", ylab="X2")

y1 <- c(rep(1,100), rep(0,100), rep(0,100))
y2 <- c(rep(0,100), rep(1,100), rep(0,100))
y3 <- c(rep(0,100),  rep(0,100),rep(1,100))
y <- cbind(y1,y2,y3)

res.lm <- lm(y~x)
res.lm
x.pred <- predict(res.lm,data.frame(x))
grp.pred <- apply(x.pred,1,which.max)
table(grp.pred,grp)

showsep3.lm <- function(lm.object,dat,resol=100)
{
  # Show separation by linear regression
  # lm.object ... reulting lm object
  # dat ... data which have been used for lm
  # resol ... resolution of separating points
  xmi <- min(x[,1])
  xma <- max(x[,1])
  xr <- xma-xmi
  ymi <- min(x[,2])
  yma <- max(x[,2])
  yr <- yma-ymi
  
  x1.all <- seq(from=xmi-xr/10,to=xma+xr/10,length=100)
  x2.all <- seq(from=ymi-yr/10,to=yma+yr/10,length=100)
  x1all <- rep(x1.all,length(x2.all))
  x2all <- sort(rep(x2.all,length(x1.all)))
  xall <- cbind(x1all,x2all)
  #xall.pred <- predict(lm.object,data.frame(xall))
  xall.pred <- xall%*%lm.object$coef[-1,]+rep(1,nrow(xall))%*%t(lm.object$coef[1,])
  pred.grp <- apply(xall.pred,1,which.max)
  plot(x,pch=grp,col=grp+2,xlab="X1",ylab="X2",type="n",
       xlim=c(xmi,xma),ylim=c(ymi,yma))
  points(xall,col=pred.grp+2,pch=20,cex=0.1)
  points(x,pch=grp,col=grp+2)
}

showsep3.lm(res.lm, x, resol=100)



######################################
# Pima Indian data:
library(mlbench)
data(PimaIndiansDiabetes2)
plot(PimaIndiansDiabetes2)
pid <- na.omit(PimaIndiansDiabetes2)

plot(pid, cex=0.8, cex.labels=0.7)

pidind <- pid
pid$diabetes <- ifelse(pid$diabetes=="neg",-1,1)

set.seed(101)
train <- sample(1:nrow(pid), 300)


# Regression with indicator matrix:
mod.ind <- lm(diabetes~., data=pid[train,])
mod.ind

mod.pred <- predict(mod.ind, newdata=pidind[-train,])
(TAB <- table(pid$diabetes[-train],mod.pred>0))
(mclrate <- 1-sum(diag(TAB))/sum(TAB))

library(cvTools)
res <- cvFit(lm,formula=diabetes~.,data=pid[train,],K=5,R=100,cost=mspe)
plot(res)



#############################################################################
# Example Pima Indian Diabetes Data:

library(mlbench)
data(PimaIndiansDiabetes2)
plot(PimaIndiansDiabetes2)
pid <- na.omit(PimaIndiansDiabetes2)

# LDA for train and test:
library(MASS)
set.seed(101)
train <- sample(1:nrow(pid), 300)
mod.lda <- lda(diabetes~.,data=pid[train,])
pred.lda <- predict(mod.lda,pid[-train,])$class
(TAB <- table(pid[-train,]$diabetes,pred.lda))
mclrate <- 1-sum(diag(TAB))/sum(TAB)
mclrate

# LDA with leave-one-out CV:
res <- lda(diabetes~.,data=pid[train,],CV=TRUE)
(TAB <- table(res$class,pid[train,"diabetes"]))
1-sum(diag(TAB))/sum(TAB)


# LDA with CV or bootstrap:
mypred <- function(object, newdata) UseMethod("mypred", object)
mypred.lda <- function(object, newdata){
  predict(object, newdata = newdata)$class
}   
library(ipred)
CEE <- control.errorest(k = 5, nboot=100)
ldacvest <- errorest(diabetes~., data=pid, model=lda, predict=mypred,
                     estimator="cv",est.para=CEE)
ldacvest
ldabest <- errorest(diabetes~., data=pid, model=lda, predict=mypred,
                    estimator="boot", est.para=CEE)
ldabest

# QDA:
mypred.qda <- function(object, newdata){
  predict(object, newdata = newdata)$class
}  
qdacvest <- errorest(diabetes~., data=pid, model=qda, predict=mypred,
                     estimator="cv",est.para=CEE)
qdacvest


# RDA for diabetes data:
library(klaR)
mod.rda<-rda(diabetes~.,data=pid[train,])
predictrda<-predict(mod.rda, pid[-train,])
(TAB<-table(pid$diabetes[-train], predictrda$class))
mcrrda<-1-sum(diag(TAB))/sum(TAB)
mcrrda



############################################################
# Logistic Regression:
library(mlbench)
data(PimaIndiansDiabetes2)
plot(PimaIndiansDiabetes2)
pid <- na.omit(PimaIndiansDiabetes2)

set.seed(101)
train<-sample(1:nrow(pid), 300)

modelglm <- glm(diabetes~.,data=pid, family=binomial,subset=train)
summary(modelglm) # tests based on Pearson residuals
anova(modelglm,test="Chisq") # test in sequence of predictors

mod.glm <- step(modelglm,direction="both")
summary(mod.glm)
anova(mod.glm,modelglm,test="Chisq") # compare two models


pred.glmstep <- predict(mod.glm, pid[-train,],type="link") # in the scale of lin. predictor
range(pred.glmstep)

range(predict(mod.glm, pid[-train,],type="response")) # scale of response var.

plot(pred.glmstep,col= as.numeric(pid$diabetes[-train])+2)
abline(h=0)

TAB <- table(pid$diabetes[-train],pred.glmstep>0)
sum(diag(TAB))/sum(TAB)

TAB <- table(pid$diabetes[-train],predict(modelglm, pid[-train,])<0)
1-sum(diag(TAB))/sum(TAB)

library(MASS)
modLDA <- lda(diabetes~., data = pid,subset=train)
plot(pred.glmstep, col=as.numeric(pid$diabetes[-train])+2,
     pch=as.numeric(predict(modLDA,pid[-train,])$class))
abline(h=0)


library(ipred)
mypred = function(object, newdata) UseMethod("mypred", object)
mypred.glm = function(object, newdata){
    LEV = levels(object$model[,1])
    as.factor(LEV[(predict(object, newdata=newdata, type="response")>0.5)+1])
}
CEE = control.errorest(k = 5, nboot=10)
logcvest <- errorest(diabetes~., data=pid, model=glm, family=binomial(),  predict=mypred,
         est.para=CEE)
logcvest
logbest<-errorest(diabetes~., data=pid, model=glm, family=binomial(),  predict=mypred,
         estimator="boot", est.para=CEE)
logbest
TAB <- table(pid$diabetes[-train],mypred(mod.glm, pid[-train,])) 
mcrlog <- 1-sum(diag(TAB))/sum(TAB)
mcrlog



#######################################################################
# multi-group case:
library(classifly)
data("olives")
n <- nrow(olives)
set.seed(123)
train <- sample(1:n,round(2/3*n))
test <- (1:n)[-train]

library(nnet)
res <- multinom(class.ind(Region)~.-Area-eicosenoic,data=olives,subset=train)
summary(res)
table(olives$Region[test],predict(res,olives[test,]))

B <- coef(res)
X <- model.matrix(res)
z1 <- X%*%B[1,]
z2 <- X%*%B[2,]
plot(z1,z2,col=olives$Region[train])
z1 <- scale(X%*%B[1,])
z2 <- scale(X%*%B[2,])
p1 <- exp(z1)/(1+exp(z1)+exp(z2))
p2 <- exp(z2)/(1+exp(z1)+exp(z2))
p3 <- 1/(1+exp(z1)+exp(z2))
library(StatDA)
ternary(cbind(p1,p2,p3),col=olives$Region[train])

res.step <- step(res)
summary(res.step)
table(olives$Region[test],predict(res.step,olives[test,]))

library(VGAM)
res1 <- vglm(Region~.-Area-eicosenoic,family=multinomial,data=olives,subset=train)
summary(res1)
res1.pred <- predictvglm(res1,type="link",olives[test,])
str(res1.pred)
plot(res1.pred,col=olives$Region[test])
res1.pred1 <- predictvglm(res1,type="response",olives[test,])
str(res1.pred1)
ternary(res1.pred1,col=olives$Region[test])

table(olives$Region[test],apply(res1.pred1,1,which.max))

step(res1)


# glmnet:
library(glmnet)
res <- cv.glmnet(as.matrix(olives[train,3:9]),olives$Region[train],family="multinomial")
plot(res)
coef(res)
pred <- predict(res,newx=as.matrix(olives[-train,3:9]),type="response")
ternary(drop(pred),col=olives$Region[-train])

pred2 <- drop(predict(res,newx=as.matrix(olives[-train,3:9]),type="link"))
plot(pred2[,1:2],col=olives$Region[-train])

pred3 <- drop(predict(res,newx=as.matrix(olives[-train,3:9]),type="class"))
table(olives$Region[-train],pred3)


# 9 classes
res <- cv.glmnet(as.matrix(olives[train,3:9]),olives$Area[train],family="multinomial")
plot(res)
coef(res)
pred <- predict(res,newx=as.matrix(olives[-train,3:9]),type="response")

pred2 <- drop(predict(res,newx=as.matrix(olives[-train,3:9]),type="link"))
plot(pred2[,1:2],col=olives$Area[-train])

pred3 <- drop(predict(res,newx=as.matrix(olives[-train,3:9]),type="class"))
(TAB <- table(olives$Area[-train],pred3))
sum(diag(TAB))/sum(TAB)

