setwd("C:/Econometrics 3/E3/Lab 2")
library(readxl)
macro3_svar <- read_excel("C:/Econometrics 3/E3/Lab 3/macro3_svar.xlsx")
View(macro3_svar)
# Question 1 - Create new variables - dy, dpi, dr - as the first differences in lgdp, infl and ffr, annualized
# and expressed in percentage points. To annualize quarterly data expressed in percentage points, multiply the
# data by 4.)
loggdp = ts(macro3_svar[,2], start = c(1955,1), frequency = 4)
infl = ts(macro3_svar[,3], start = c(1955,1), frequency = 4)
FFR = ts(macro3_svar[,4], start = c(1955,1), frequency = 4)
dy = diff(loggdp) * 4
dp = diff(infl) * 4
dr = diff(FFR) * 4

y = ts.union(dy, dp, dr)
# Question 2 - Estimate an unrestricted VAR(1) model for dy, dr and dpi. Look at the coefficients on dy,
# dr and dpi - do they make sense in terms of signs?

install.packages("vars")
library(vars)
VAR1_Q2 = VAR(y, p = 1, type = c("const"))
summary(VAR1_Q2)

# Question 3 - set B to be the identity matrix, Are these restrictions enough to identify the SVAR?

SVAR_Q3 = SVAR(VAR1_Q2, Bmat = NULL)

# Question 4
# Set the elements of B to one, and set A so that the recursive ordering is as follows: 
# output and inflation do not affect interest rates contemporaneously; output does not affect 
# inflation contemporaneusly
amat = diag(3)
amat[1,2] = NA
amat[1,3] = NA
amat[2,3] = NA
amat[1,1] = NA
amat[2,2] = NA
amat[3,3] = NA
SVAR_Q4 = SVAR(VAR1_Q2,estmethod = "scoring", Bmat = NULL, Amat = amat, lrtest = FALSE, max.iter = 1000)

# Question 5
# plot the impulse responses of Question 4
irf(SVAR_Q4)
plot(irf(SVAR_Q4))

# Question 6
# Suppose you want to do a causal analysis of the implied simultaneous system of equations
# AYt = CYt-1 + eps_t, where Var(eps_t) = B is diagonal. Modify the normalization in d) on A and
# interpret the contemporaneous multipliers of interest rates on inflation. Does it make sense?
# Explain why or why not.

amat = diag(3)
amat[1,2] = NA
amat[1,3] = NA
amat[2,3] = NA
bmat = diag(3)
bmat[1,1] = NA
bmat[2,2] = NA
bmat[3,3] = NA
SVAR_Q6 = SVAR(VAR1_Q2,estmethod = "scoring", Bmat = bmat, Amat = amat, lrtest = FALSE, max.iter = 1000)

# Question 7
# Suppose you decide to normalize using long-run restrictions instead. Estimate the model
# assuming that the interest rates shocks are neutral to output and inflation in the long-run, and
# inflation shocks are neutral to output in the long-run. Can you recover somehow correlated
# reduced form errors rather than correlated structural shocks? Explain.
lrmat = diag(3)
lrmat[1,1] = NA
lrmat[2,2] = NA
lrmat[3,3] = NA
lrmat[2,1] = NA
lrmat[3,1] = NA
lrmat[3,2] = NA

bmat = diag(3)
bmat[1,1] = NA
bmat[2,2] = NA
bmat[3,3] = NA
SVAR_Q7 = SVAR(VAR1_Q2, estmethod = "scoring", Amat = NULL, Bmat = bmat, LRIM = lrmat,lrtest = FALSE, max.iter = 1000)