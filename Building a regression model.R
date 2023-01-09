#load the data
setwd("E:/School/STAT 5120 - Spring 2018/Week 11")
bf <- read.csv("Bodyfat.csv")
head(bf)
#let's build the model
require(leaps)
regfit.full=regsubsets(Bodyfat~.,bf)
summary(regfit.full)
#An asterisk indicates that a given variable is included in the corresponding model

#let's build a full model and look at the summary information to decides what variables and how many variables create the best model
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2, xlab="Number of Variables",ylab="Adjusted R Sq",type="l")
plot(reg.summary$cp, xlab="Number of Variables",ylab="Cp",type="l")
plot(reg.summary$bic, xlab="Number of Variables",ylab="BIC",type="l")
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)
plot(regfit.full,scale="bic")

#the model with the lowest BIC is the two-variable model that contains only Neck and Abdomen 
coef(regfit.full,2)