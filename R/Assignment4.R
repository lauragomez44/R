library(readxl)
macro6_Ass4 <- read_excel("C:/Econometrics 3/macro6_Ass4.xlsx")
View(macro6_Ass4)

y = ts(macro6_Ass4[,c(2,3,4,5)], start = c(1959,1), frequency = 4)


# Q1)
install.packages("tseries")
library(tseries)

summary(ur.pp(y[,2], model = "constant"))
summary(ur.pp(y[,3], model = "constant"))
summary(ur.pp(y[,1], model = "constant"))
summary(ur.pp(y[,4], model = "constant"))

# all have  unit root

# Q2)
install.packages("urca")
library(urca)
vecm_trace = ca.jo(y,type = "trace", ecdet = "const", K = 3, spec = "transitory")
summary(vecm_trace)

# 3 cointegrating relationships

#Q3)
vecm_eigen = ca.jo(y,type = "eigen", ecdet = "const", K = 3, spec = "transitory")
summary(vecm_eigen)

# same as above, 3 cointegrating relationships

#Q4)
# broadm_t + 0.011 *ffr_t - 0.41 *gdp_t + 1.4809 = u_t

# negative relationship between money supply and interest rate,, positive relationship between gdp growth 
# and money supply

#Q5)
install.packages("tsDyn")
library(tsDyn)

vecm_y = VECM(y, lag = 3, r = 1, include = "const")
summary(vecm_y)
