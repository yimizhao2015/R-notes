### example of Poisson regression in R###
## including: normal Poisson regression model, Rate Poisson model and Negative binomial

## 1) normal Poisson regression
library(faraway)
data(gala)
gala<-gala[,-2]
head(gala)
# fit linear model
modl <- lm(Species ~ ., gala)
modt <- lm(sqrt(Species) ~ ., gala) 
summary(modt)

# poisson regression model
modp<-glm(Species~., family=poisson,gala)
summary(modp)

# goodness of fit measures
# check outlier
halfnorm(residuals(modp))
# compute dispersion 
(dp<-sum(residuals(modp,type="pearson")^2)/modp$df.res)
# or in this way
summary(modp,dispersion=dp)$dispersion
drop1(modp,test="F")

## 2)Rate Poisson model
data(dicentric)
round(xtabs(ca/cells~doseamt+doserate,dicentric),2)
with(dicentric,interaction.plot(doseamt,doserate,ca/cells))
# rate model fitting
dicentric$dosef <-factor(dicentric$doseamt)
pmod<-glm(ca~log(cells)+log(doserate)*dosef,family=poisson,dicentric) 
summary(pmod)

# offset 
rmod<-glm(ca~offset(log(cells))+log(doserate)*dosef,family=poisson,dicentric) 
summary(rmod)

## 3)Negative binomial model
data(solder)
mod_p<-glm(skips~.,family=poisson,data=solder)

deviance(mod_p)
pchisq(deviance(mod_p),df.residual(mod_p),lower=F)
# including 2nd order interactions
mod_p2<-glm(skips~.^2,family=poisson,data=solder)
deviance(mod_p2)
pchisq(deviance(mod_p2),df.residual(mod_p2),lower=F)

# fit negative binomial model(k can be varied to get the best fit)
library(MASS)
(mod_n<-glm(skips~.,family=negative.binomial(1),data=solder))
summary(mod_n)
# another way:considers k as a variable and estimates it using maximum likelihood
mod_n2<-glm.nb(skips~.,data=solder)
summary(mod_n2)
