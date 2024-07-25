library(ISLR)
library(tidyverse)
library(ggplot2)
library(mgcv)
data(OJ,package="ISLR")
data <- OJ
str(data)

#convert some numeric variables to factors
catVars = c("STORE","StoreID","SpecialCH","SpecialMM")
data[catVars] <- lapply(data[catVars], as.factor)

#plot the factor variables
data %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value, fill=value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar() +
  theme(legend.position="none")

#I will probably do not add it to the exercise (just for me)
#plot the numeric variables
data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value,fill=key)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins=sqrt(nrow(data))) +
  theme(legend.position="none")


#Question 1(a)

set.seed(12223236)
n <- nrow(data)
train <- sample(1:n,round(n*2/3))
test <- (1:n)[-train]



mod.gam <-gam(Purchase ~ s(PriceMM, k=3)+s(WeekofPurchase)+s(PriceCH)+s(DiscCH)+s(DiscMM)+
                s(LoyalCH)+s(SalePriceMM)+s(SalePriceCH)+s(PriceDiff)+s(PctDiscMM)+s(PctDiscCH)
              +s(ListPriceDiff)+StoreID+SpecialCH+SpecialMM+Store7+STORE
              , data=data, family="binomial", subset=train)

summary(mod.gam)

plot(mod.gam,page=1,shade=TRUE,shade.col="yellow")

gam.res <- predict(mod.gam, data[test,])>0
gam.TAB <- table(data$Purchase[test],as.numeric(gam.res))
gam.TAB
mkrgam<-1-sum(diag(gam.TAB))/sum(gam.TAB)
mkrgam


##################################

reduced.mod.gam <- gam(Purchase ~ s(LoyalCH)+s(SalePriceMM)+s(PctDiscCH)+s(ListPriceDiff)+SpecialCH+SpecialMM+Store7
                       , data=data, family="binomial", subset=train)

gam.res <- predict(reduced.mod.gam, data[test,],type="response")>0.5
gam.TAB <- table(data$Purchase[test],as.numeric(gam.res))
gam.TAB
mkrgam<-1-sum(diag(gam.TAB))/sum(gam.TAB)
mkrgam

