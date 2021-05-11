# Import data Income10-data.txt; File > Import Dataset > From text

# The data set 'income10-data.txt' contains quarterly data for the Total Per-
# sonal Disposable Income (variable "income") and Consumer Expenditures
# (variable "consum") in the UK for the years 1971 (from the first quarter) to
# 1980 (to the first quarter).

# Question 1:
# Load the data set and check that the two variables are present.

Income10 <- read.delim("C:/Econometrics 3/Pavel's Part/send/Income10-data.txt")
View(Income10)

# Question 2
# The analysis concerns only the (logarithm of) consumer expenditures.
# Create the time series "lnc" equal to the logarithm of variable "con-
#  sum" by taking the logarithm of "consum" and creating a time series
# object by specifying the dates of the first and last observation and the
# frequency of the time series (function ts).

# Use ts function to convert the vector into an R time series object.

lnc = ts(log(Income10$CONSUM), start = 1971, end = 1980, frequency = 4)

# Question 3
# Plot the time series "lnc". Are there trend or seasonal effects in your
# opinion?
plot(lnc)

# Question 4
# Estimate the linear trend and seasonal (quarterly) effects (e.g., using
# function tslm).

install.packages("forecast")
library(forecast)
lnconsum_fit = tslm(lnc ~ trend + season)
# type ?tslm for details.
summary(lnconsum_fit)

# Question 5
# Are there significant seasonal effects? Test the joint hypothesis using
# the Wald test or likelihood ratio test.

# get the least squares object of the restricted regression without season dummies.
lnconsum_fit_trend = tslm(lnc ~ trend)
# perform lm test, for that we need to install "lmtest"package
install.packages("lmtest")
library(lmtest)
lrtest(lnconsum_fit_trend, lnconsum_fit)

# Question 6
# Plot the trend and seasonal-effect fit of the series as well as the corre-
# sponding residuals and save the residuals under the name "rlnc". Do
# you observe some pattern in the residuals or do they resemble white
# noise?
rlnc = residuals(lnconsum_fit)
trend_seasonal_fit = fitted.values(lnconsum_fit)
ts.plot(lnc, trend_seasonal_fit, gpars = list(col = c("black", "red")))
plot(rlnc)


# Question 7
# Plot the autocorrelation and partial autocorrelation functions of the
# residual series "rlnc" (e.g., using functions such as acf, pacf, and auto-plot ). 
# Is there a significant time dependence in the residuals? What
# ARMA model would you use for this series?
acf(rlnc)
pacf(rlnc)
# The residuls seem to follow an AR(1) process

# Question 8
# Estimate AR(1) model for the series "rlnc" (e.g., using a function arima
# or Arima). Is the autoregressive coefficient significant? The lagged
# value of the variable can be denoted "rlnc(-1)".
AR1_rlnc = arima(x = rlnc, order = c(1, 0, 0))
# order = c(AR order, degree of differencing, MA order)
AR1_rlnc
coeftest(AR1_rlnc)

# Question 9
# To check against a wrong (too small) selected model, estimate also
# AR(2) and ARMA(1,1) (e.g., using lrtest or waldtest ). Are additional
# AR or MA terms significant? The moving average terms can be de-
#  noted "MA(1)" and so on.

AR2_rlnc = arima(x = rlnc, order = c(2,0,0))
AR2_rlnc
coeftest(AR2_rlnc)
ARMA11_rlnc = arima(x = rlnc, order = c(1,0,1))
coeftest(ARMA11_rlnc)

# Question 10:
# To check that AR(1) captures all time dependence and the residu-
# als from this model are white noise now, plot the correlogram of the
# residuals from this AR(1) model and perform the Q-test of serial au-
# tocorrelation in the regression residuals (e.g., using function Box.test ).

residuals_ar1 = residuals(AR1_rlnc)
acf(residuals_ar1)
Box.test(residuals_ar1, lag = 8)


# no autocorrelation across previous lags

# Question 11
# The full model for the logarithm of "consum" thus contains the trend,
# seasonal effects, and an autoregressive term. Estimate the complete
# model jointly. Do you get the same value of the autoregressive coeffi-
# cient?

lnconsum_AR1 = Arima(lnc, order = c(1,0,0), seasonal = c(1,0,0), xreg = c(1:length(lnc)))
lnconsum_AR1

# Question 12
# Estimate the full model in point 11 without the seasonal effect. What
# happens to the autoregressive coefficient? What results do you obtain
# from the Q-test applied to the regression residuals now?

lnconsum_AR1_Noseason = Arima(lnc, order = c(1,0,0), xreg = c(1:length(lnc)))
Box.test(residuals(lnconsum_AR1_Noseason), lag = 8)

########################################################################

# Section 3
# Question 1
# Open the data set, create a time series and plot from the variable "dax".
# Does it look stationary?
Dax30.data <- read.csv("C:/Econometrics 3/Pavel's Part/send/Dax30-data.txt", sep="")
View(Dax30.data)
plot(Dax30.data)
plot(Dax30.data$DAX)

# Question 2
# Plot the autocorrelation function of the series "dax". Does it resemble
# the autocorrelation structure of any ARMA process?
acf(Dax30.data$DAX)
pacf(Dax30.data$DAX)
# Unit Root

# Question 3
# In an attempt to create a stationary series, create the first differences
# of "dax" (e.g., using diff ) and denoted them "ddax" (the lagged value
# of the variable "dax" can be also be computed using lag). Plot again
# the series itself and its autocorrelation function. How do they differ
# from the one in point 2? Could "ddax" be stationary or not?

ddax = diff(Dax30.data$DAX)
plot(ddax)

# Question 4
# Compute the Ljung-Box statistics for series "ddax". What do you ob-
# serve?

Box.test(ddax, lag = 8)

# Question 5
# Believing that small p-values of the Q-statistics indicate a significant
# autocorrelation in the data, try to fit AR(3) or AR(5) or AR(7). Are
# there any significant coefficients?

Dax30_Ar3 = arima(ddax, order = c(3,0,0))
coeftest(Dax30_Ar3)
Dax30_Ar5 = arima(ddax, order = c(5,0,0))
coeftest(Dax30_Ar5)
Dax30_Ar7 = arima(ddax, order = c(7,0,0))
coeftest(Dax30_Ar7)

# Question 6
# Recalling the plot of "ddax" in point 3 and thoughts about station-
# arity, reestimate the AR(7) model using only observations 1 to 1500.
# Do the estimates and their significance change? How? Support you
# observations by an application of the Ljung-Box test.
ddax1 = ddax[1:1500]
Dax301_Ar7 = arima(ddax1, order = c(7,0,0))
coeftest(Dax301_Ar7)



  
