### Box-Cox Transformations for Linear Models using R###
# Problem 3: Using the ozone data, fit a model with O3 as the response and temp, humidity and ibh
# as predictors. Use the Box–Cox method to determine the best transformation on the response.
library(faraway)
data(ozone)
head(ozone)

# model fit
md <-lm(O3~temp+humidity+ibh,data=ozone)
summary(md)
#Plot only the fitted vs residuals to check the assumptions
plot(md,which=1)

#box-cox power transformation
library(MASS)
bc<-boxcox(md,plotit=T)
# or further specify the range of lambda to make the plot more clear
bc<-boxcox(md,plotit=T,lambda=seq(0,0.8,by=0.1))
# find the best value of lambda
which.max(bc$y)
(lambda<-bc$x[which.max(bc$y)])

# new model fitting use transformed response(O3^lambda)
md_best<-lm(I(O3^lambda)~temp+humidity+ibh,data=ozone)
summary(md_best)
plot(md_best,which=1)
