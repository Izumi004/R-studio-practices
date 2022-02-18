squid = read.table("squid.txt", header = T)
install.packages("leaps")
install.packages("MASS")
library("leaps")

#part I
#a)
squid.best.subsets <- regsubsets(y ~ x1+x2+x3+x4+x5, data = squid )
summary(squid.best.subsets)
#brief coment The best model obtained with four predictors is consists of x1, x2, x4 and x5.　

#b)
#table for adjusted R2 and Cp
library('leaps')
cp = leaps(x=squid[,1:5], y=squid$y, names=names(squid)[1:5],method = "Cp")
cbind(cp$which, Cp=cp$Cp)
adjr2 = leaps(x=squid[,1:5], y=squid$y, names=names(squid)[1:5],method = "adjr2")
cbind(adjr2$which, adjr2=adjr2$adjr2)
cp_0 = leaps(x=squid, y=squid$y, names=names(squid)[1:5],method = "Cp")


#PRESS
PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  sum(pr^2)
}
PRESS(lm(y ~ ., data=squid))
PRESS(lm(y ~ x5, data=squid))
PRESS(lm(y ~ x4+x5, data=squid))
PRESS(lm(y ~ x2+x4+x5, data=squid))
PRESS(lm(y ~ x1+x2+x4+x5, data=squid))
PRESS(lm(y ~ x1+x2+x4+x5+x3, data=squid))


PRESS = leaps(x=squid[,1:5], y=squid$y, names=names(squid)[1:5],method = "press")


#Part II
library('MASS')
#a) 1)
squid_mod = lm(y~1,data=squid)
step <- stepAIC(squid_mod, scope=~x1+x2+x3+x4+x5, direction = "forward")
step$coefficients

#b) 
squid_mod = lm(y ~ x1+x2+x3+x4+x5, data = squid)
step <- stepAIC(squid_mod, direction = "backward")
step$coefficients

#c)
squid_mod_x1 = lm(y ~ x1, data = squid)
step <- stepAIC(squid_mod_x1,scope=~x1+x2+x3+x4+x5, direction = "both")
step$coefficients
 

# III
squid_mod = lm(y~x1+x2+x3+x4+x5, data = squid)
par(mfrow = c(2,2))
plot(squid_mod)
par(mfrow=c(1,1))

squid_mod_s = lm(y~x4+x5, data = squid)
par(mfrow = c(2,2))
plot(squid_mod_s)
par(mfrow=c(1,1))

as
f1 <- lm(y~x1+x2+x3+x4+x5,data=squid)
f2 <- lm(y~x4+x5, data = squid)
anova(f1,f2)

summary(f2)
Call:
  lm(y~x1+x2+x3+x4+x5, data = squid)

