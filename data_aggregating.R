# data aggregating 
data(mtcars)
## group by one variable, compute values on one function
tapply(mtcars$mpg,mtcars$cyl,mean) # or
with(mtcars,tapply(mpg,cyl,mean))
#    4        6        8 
# 26.66364 19.74286 15.10000 
---------------------------------------------------
## group by one variable, compute values on customized function
myFun<- function(x){
  c(Average=mean(x),
    Median=median(x),
    SD=sd(x))
} 
tapply(mtcars$mpg,mtcars$cyl,myFun) #or
with(mtcars,tapply(mpg,cyl,myFun))
# unfortunately, the output is an array, and it is not convenient to read. 
# $`4`
# Average    Median        SD 
# 26.663636 26.000000  4.509828 
# 
# $`6`
# Average    Median        SD 
# 19.742857 19.700000  1.453567 
# 
# $`8`
# Average    Median        SD 
# 15.100000 15.200000  2.560048
-----------------------------------------
## to organize the result
do.call(rbind,tapply(mtcars$mpg,mtcars$cyl,myFun)) #or
with(mtcars,do.call(rbind,tapply(mpg,cyl,myFun)))
#   Average   Median   SD
# 4 26.66364   26.0 4.509828
# 6 19.74286   19.7 1.453567
# 8 15.10000   15.2 2.560048