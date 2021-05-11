# Question 2
#  Simulate one sample from this model letting T = 100, 500, 1000. Plot the ACF and PACF of
# this model, and explain whether they make sense.
# model - y_t = beta*eps_t-1 + eps_t , beta = 0.7, eps_t ~ N(0,1)

install.packages("sandwich")
library(sandwich)
install.packages("lmtest")
library(lmtest)
y = matrix(0,nrow = Ti,ncol = 1)
eps = rnorm(Ti+1)
y = 0.7*eps[1:Ti] + eps[2:(Ti+1)]

acf(y)
pacf(y)

# b)

test1= matrix(0, nrow = 500, ncol = 1)
for( i in 1: 500) {

y = matrix(0,nrow = Ti,ncol = 1)
eps = rnorm(Ti+1)
y = 0.7*eps[1:Ti] + eps[2:(Ti+1)]


fm = lm(y ~ 1)
HAC1 = kernHAC(fm, bw = bwNeweyWest, prewhite = 0)


test1[i,1] = (coeftest(fm, vcov = HAC1)[4] <0.05)
}
mean(test1)
# 18.8 times we are rejecting. we are suppose to reject only 5 percent of the time. Empirical type 1 error is hig because HAC
# is downward biased

# c)


Ti = 200
y = matrix(0,nrow = Ti,ncol = 1)


  for (i in 2:Ti){
    y[i,1] =  0.9*y[i-1,1] + rnorm(1,sd = 1)
  }
    y_afterburnin = y[101:length(y),1]
 
# burn in is to ensure the stantionarity of the process
    
    
    
# d)
    
    acf(y_afterburnin)
    pacf(y_afterburnin)
    
# e)
    
    Ti = 200
    y = matrix(0,nrow = Ti,ncol = 1)
    test1= matrix(0, nrow = 500, ncol = 1)
    M = 500
    
    for (j in 1:M){
       
    for (i in 2:Ti){
      y[i,1] =  0.9*y[i-1,1] + rnorm(1,sd = 1)
    }
    y_afterburnin = y[101:length(y),1]
    fm = lm(y_afterburnin ~ 1)
    HAC1 = kernHAC(fm, bw = bwNeweyWest, prewhite = 0)
    
    
    test1[j,1] = (coeftest(fm, vcov = HAC1)[4] <0.05)
    
    
    } # second for end
    
    mean(test1)
    
    # 28 percent chance of making type 1 error
    
   #  for Ti = 500
    
    Ti = 500
    y = matrix(0,nrow = Ti,ncol = 1)
    test1= matrix(0, nrow = 500, ncol = 1)
    M = 500
    
    for (j in 1:M){
      
      for (i in 2:Ti){
        y[i,1] =  0.9*y[i-1,1] + rnorm(1,sd = 1)
      }
      y_afterburnin = y[101:length(y),1]
      fm = lm(y_afterburnin ~ 1)
      HAC1 = kernHAC(fm, bw = bwNeweyWest, prewhite = 0)
      
      
      test1[j,1] = (coeftest(fm, vcov = HAC1)[4] <0.05)
      
      
    } # second for end
    
    mean(test1)
    
    # 23 percent chance of making type 1 error
    
    # for Ti = 1000
    Ti = 1100
    y = matrix(0,nrow = Ti,ncol = 1)
    test1= matrix(0, nrow = 500, ncol = 1)
    M = 500
    
    for (j in 1:M){
      
      for (i in 2:Ti){
        y[i,1] =  0.9*y[i-1,1] + rnorm(1,sd = 1)
      }
      y_afterburnin = y[101:length(y),1]
      fm = lm(y_afterburnin ~ 1)
      HAC1 = kernHAC(fm, bw = bwNeweyWest, prewhite = 0)
      
      
      test1[j,1] = (coeftest(fm, vcov = HAC1)[4] <0.05)
      
      
    } # second for end
    
    mean(test1)
    
    # finite sample bias plus HAC could be downward biased