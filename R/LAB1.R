males <- read_dta("males.dta")
View(males)
# Use Import Dataset tab under File section to Import the data.
# Exercise 1.1 : 
# Tabulate the variable YEAR to identify the dimensions of panel data at hand

table(males$YEAR)

# Exercise 1.2:
# Present summary statistics of all variables. Check whether means of
# dummies add up to one where you think they should.

summary(males)

varnames = names(males)
# extract all the var names
dummy = rowSums(males[grep("IYEAR*",varnames)])
check_dummy = cbind(males$YEAR, dummy)
# or simply
dummy = rowSums(males[males$YEAR > 1980, grep("IYEAR*",varnames)])
# grep searches for columns matching with the given pattern "*IYEAR"

# Exercise 1.3:
# Tabulate the workers labor market experience and schooling. In which
# ranges does the majority of observations fall?
  
table(males$EXPER)
table(males$SCHOOL)
hist(males$EXPER)
hist(males$SCHOOL)

# Exercise 1.4:
#  Produce histograms of the log wage distribution in year 1981 and 1987.
# Are there some substantial differences?

logwage_1981 = males$WAGE[males$YEAR == 1981]
logwage_1987 = males$WAGE[males$YEAR == 1987]
# Select only those relevant year rows

par(mfrow = c(1,2)) # graphical setting to plot two graphs in the same window.
hist(logwage_1981, type = "1", col = "green")
hist(logwage_1987, type = "1", col = "red")


# Exercise 2.1:
#  Estimate a static pooled regression model explaining the log hourly
# wage rate by education, experience and its square, dummies black,
# hispanic, married, and union, regional dummies, and time dummies.

# Read https://cran.r-project.org/web/packages/plm/vignettes/plmPackage.html for information on how to estimate panel data models on R
# install plm package. 
install.packages("plm")
library("plm")
pdata2_1 = pdata.frame(males,index = c("NR","YEAR"))

xnames = c("SCHOOL", "EXPER", "EXPER2", "BLACK", "HISP", "MAR","UNION", "RUR","NE","NC","S")
xnames = c(xnames, varnames[38:44])
model2_1 = as.formula(paste("WAGE ~", paste(xnames, collapse = "+")))
pooling = plm(model2_1, data = pdata2_1, model = "pooling")
summary(pooling)

# Exercise 2.2
# Estimate a static random-effects (RE) model using the same specification as in part 2-1. Interpret the results.
random2_2 = plm(model2_1, data = pdata2_1, model = "random")
summary(random2_2)

# Exercise 2.3
# The RE estimator corresponds to the least squares method applied on
# lambda - demeaned data. Is lambda close to 1?

summary(random)$ercomp$sigma2
# Computed lambda is approximately 0.87.

# Exercise 2.4
# Test the significance of time dummies in the random-effect model at
# 5% significance level and omit them from the model if they are not
# useful.

# using plmtest from plm package. the test for time effects is done based on the results from pooled regression.
xnames2_4 = c("SCHOOL", "EXPER", "EXPER2", "BLACK", "HISP", "MAR","UNION", "RUR","NE","NC","S")
model2_4 = as.formula(paste("WAGE ~", paste(xnames2_4, collapse = "+")))
pooling2_4 = plm(model2_4, data = pdata2_1,model = "pooling" )
plmtest(pooling2_4,"time")

# Exercise 2.5
# Check whether there are significant differences between regions in the
# model of part 2-2. Which regions stand out?

# install "car" package to test multiple restrictions
install.package("car")
library("car")
linearHypothesis(random2_2, c("NE = 0", "NC = 0", "S = 0"))

# Exercise 2.6
# Add occupational dummies and estimate the new RE model. Compare
# the estimated coefficients of the schooling and experience with the
# outcome of model 2-3.
xnames2_6 = c(xnames, varnames[grep("OCC",varnames)])
model2_6 = as.formula(paste("WAGE ~", paste(xnames2_6, collapse = "+")))

random2_6 = plm(model2_6, data = pdata2_1, model = "random")
summary(random2_6)
# case of omitted variable bias, coefficients values are smaller in the new model.


# Exercise 2.7
# Do the managerial jobs (OCC2==1) lead to higher wages compared
# to the service jobs?
summary(random2_6)
# Yes

# Exercise 2.8
# Can you analyze the wage differentials between managerial and service jobs in the fixed-effects (FE) framework?
# If yes, estimate the FE model and specify for which variables one cannot identify the variable coeffcients in the FE model.

# We can, as the occupational dummy variables are changing with time for each individual.

fixed2_8 = plm(model2_6, data = pdata2_1, model = "within")
summary(fixed2_8)

# Exercise 2.9
# Test whether FE is a better strategy than RE.

phtest(fixed2_8,random2_6)
# Hausman test rejects null hypothesis that there is no correlation between the individual effects and regressors.
# Under the null, FE and RE are consistent and RE is efficient and under the alternative, FE is consistent.
# do not reject the null, choose RE

# Exercise 2.10
# Based on the model chosen in 2-8, do you conclude that the managerial
# jobs lead to higher wages compared to the service jobs?

summary(random2_8)
# Yes, the coefficient of OCC2 is positive in the chosen random effects model.



# Exercise 3.1
#  Let us go back to a simple static regression model explaining the log hourly wage rate by education, 
#  experience and its square, dummies married, and union. Estimate this model by the standard random effect model.

xnames3_1 = c("SCHOOL", "EXPER", " EXPER2", "MAR", "UNION")
model3_1 = as.formula(paste("WAGE ~", paste(xnames3_1, collapse = "+")))
random3_1 = plm(model3_1, data = pdata2_1, model = "random")
summary(random3_1)

# Exercise 3.2
# Next, the schooling variable is often considered endogenous. Are there
# some suitable instruments in the data for variable SCHOOL? What
# about RUR, BLACK, HISP, HLTH?


# Yes, participation in school could depend on an individual's ethnic and health status and could be a cause of endogenity in the above model.
# For this data RUR, BLACK, HISP, HLTH could be good instruments in explaining the variation of logwage.

# Exercise 3.3
# Estimate the instrumental variable RE model. Are there substantial differences in the estimated coefficients?

ivnames = c( "RUR", "BLACK", "HISP", "HLTH", "EXPER", "EXPER2", "MAR", "UNION")
# pipe symbol = alt gr + shift + back slash"
model3_3 = as.formula(paste("WAGE ~", paste(xnames3_1, collapse = "+"), "|", paste(ivnames, collapse = "+")))
random3_3 = plm(model3_3, data = pdata2_1, model = "random")
summary(random3_3)

# Not much change in the estimate but the variance of the IV RE regression is higher than the standard RE regression as expected.

# Exercise 3.4
# Perform the Hausman test of endogeneity of the years of schooling. What do you conclude?
phtest(random3_3,random3_1)
# reject the null hypothesis that SCHOOL is exogeneous. Choose IV

# Exercise 3.5
#  Can this instrumental variable estimation for the variable SCHOOL be performed in the context of FE estimation?
# Is it needed in the context of FE estimation? 
# Do the instrumental variable RE estimates of the above model substantially di???er from the standard FE estimates of the same model?

fixed3_4 = plm(model3_3, data = pdata2_1, model = "within")
summary(fixed3_4)

# No, we cannot use FE estimation and instrument SCHOOL with time invariant regressors RUR, BLACK, HISP, HLTH.

####################################################################################################################

# Section 3

####################################################################################################################

# Exercise 1.1
# Present summary statistics of all variables. 
# Check whether means of dummies add up to one where you think they should.
# Load the data

mex92 <- read_dta("Pavel's Part/send/mex92.dta")
View(mex92)
summary(mex92)

# Exercise 1.2
# How do the fractions of formal and informal sector workers change over time?
summary(mex92$formm)
summary(mex92$informm)
# use tapply function to get the quarter wise fractions of formal and informal fractions.
frac_for = tapply(mex92$formm, INDEX = mex92$quarter, function(i) mean(i))
# fraction of workers working in formal sector has decreased in quarter 5 compared to quarter 1.
frac_inf = tapply(mex92$informm, INDEX = mex92$quarter,function(i) mean(i))
# fraction of workers working in informal sector has increased in qarter 5 compared to quarter 1.

# Exercise 1.3
# Compute for each quarter the mean log wage for formal and informal sector workers separately. 
# What is the (raw) wage differential in each quarter? And if you pool both quarters?
meanlogwage_formal = mean(mex92$lwage[mex92$formm == 1])
# meanlogwage_formal = 1.835921
meanlogwage_informal = mean(mex92$lwage[mex92$informm == 1])
# meanlogwage_informal = 1.817549
# wage rate of formal workers grows at a higher rate than that of informal workers on average.

# Raw wage differential in each quarter:
form_quartwise = tapply(mex92$lwage*mex92$formm, INDEX = mex92$quarter, function(i) mean(i))
inform_quartwise = tapply(mex92$lwage*mex92$informm, INDEX = mex92$quarter, function(i) mean(i))
wage_diff = form_quartwise - inform_quartwise

wagediff_pool = mean(mex92$lwage*mex92$formm) - mean(mex92$lwage*mex92$informm)

# Exercise 1.4
# Produce histograms of the log wage distribution in each quarter and 
# check whether it is necessary to remove outliers. 
# If so, repeat part c without the outlier

logwage_quarter1 = mex92$lwage[mex92$quarter == 1]
logwage_quarter2 = mex92$lwage[mex92$quarter == 5]
par(mfrow = c(1,2)) # graphical setting to plot two graphs in the same window.
hist(logwage_quarter1, type = "1", col = "green")
hist(logwage_quarter2, type = "1", col = "red")



# Exercise 2.1
# Estimate a static pooled regression model explaining the log hourly wage rate from educational dummies,
# age, city dummies, and time dummies.

pdata3_2_1 = pdata.frame(mex92,index = c("pn","quarter"))
pooling3_2_1 = plm(lwage ~  mided + higed + age + mexio + juare + tijua + monte + factor(quarter), 
                   data = pdata3_2_1, model = "pooling")
summary(pooling3_2_1)

# Exercise 2.2
#  Estimate a static random-effects (RE) effects model using the same
# specification as in part 2-1. Interpret the results.

random3_2_2 = plm(lwage ~  mided + higed + age + juare + tijua + monte + factor(quarter), 
                  data = pdata3_2_1, model = "random")
summary(random3_2_2)

# Exercise 2.3
# Test for signifcant differences between cities in the model of part 2-2.

install.packages("car")
library("car")
linearHypothesis(random3_2_2, c("juare = 0", "tijua = 0", "monte = 0"))

# Exercise 2.4
# Add a dummy for working in the formal sector and estimate the new RE model. 
# Compare the estimated coefficient on the dummy to the raw log wage differential in exercise 1-3.
random3_2_4 = plm(lwage ~  mided + higed + age + juare + tijua + monte + formm + factor(quarter), 
                  data = pdata3_2_1, model = "random")
summary(random3_2_4)



# Exercise 2.5
# Test the model in part 2-4 against the static RE model with different coefficients for formal and informal sector
# wages. Which model do you prefer? (Hint: you can refer to multiple variables for example by "age*", meaning all
# variables in the data set with names beginning by "age".)


Xformm = mex92[,c("mided", "higed", "age", "juare", "tijua", "monte")]*mex92$formm
names(Xformm) = c("midedformm", "higedformm", "ageformm", "juareformm", "tijuaformm", "monteformm")
Xinformm = mex92[,c("mided", "higed", "age", "juare", "tijua", "monte")]*mex92$informm
names(xinformm) = names(Xformm) = c("midedinformm", "higedinformm", "ageinformm", "juareinformm", "tijuainformm", "monteinformm")

model3_2_5 = as.formula(paste("lwage ~", paste(names(Xformm), collapse = "+"), "+", paste(names(Xinformm), collapse = "+"), "+ factor(quarter)" ))
pdata3_2_5 = cbind(pdata3_2_1, Xformm, Xinformm)
random3_2_5 = plm(model3_2_5, data = pdata3_2_5, model = "random")
summary(random3_2_5)

linearHypothesis(random3_2_5, c("midedformm = 0", "higedformm = 0", "ageformm = 0", "juareformm = 0", "tijuaformm = 0", "monteformm =0", "midedinformm = 0", 
                                "higedinformm = 0", "ageinformm = 0", "juareinformm = 0", "tijuainformm = 0", "monteinformm = 0"))

# dummy specific coefficients don't have any joint significance.

# Exercise 2.6
# Use the static RE framework to estimate wage di???erentials between formal and informal sector jobs
# for different education levels. (Hint: you can predict the wages in each sector and then summarize them 
# by education levels.)
wage_predict = mex92$lwage - residuals(random3_2_4)
mex92 = cbind(mex92,wage_predict)
wage_diff_formal = tapply(wage_predict, INDEX = mex92$formm, function(i) mean(i))

mean(mex92$wage_predict[mex92$formm == 1 & mex92$mided == 1]) - mean(mex92$wage_predict[mex92$informm == 1 & mex92$mided == 1])
mean(mex92$wage_predict[mex92$formm == 1 & mex92$higed == 1]) - mean(mex92$wage_predict[mex92$informm == 1 & mex92$higed == 1])
mean(mex92$wage_predict[mex92$formm == 1 & mex92$based == 1]) - mean(mex92$wage_predict[mex92$informm == 1 & mex92$based == 1])
# mean wage of workers havinng intermediate education is higher is informal sector and mean wage of workers having higher
# education is higher in formal sector.

# Exercise 2.7
#  Can you analyze the wage di???erentials between formal and informal sector jobs in the fixed-effects (FE) framework?
# If so, also test whether FE is a better strategy than RE

random3_2_7 = plm(lwage ~  mided + higed + age + juare + tijua + monte + formm + factor(quarter), 
                  data = pdata3_2_1, model = "within")
summary(random3_2_7)
# No, many of the regressors are time invariant.

# Exercise 3.1
# Use your favorite RE model from Exercise 2 to construct the predicted wage differential between the formal and 
# informal sector log wage rate for every worker in every time period.
wage_diff1 = matrix(0,nrow = nrow(mex92), ncol = 1)
mex92 = cbind(mex92, wage_diff1)
mex92$wage_diff1[mex92$quarter == 1] = coefficients(random3_2_4)[8]

mex92$wage_diff1[mex92$quarter == 5] = coefficients(random3_2_4)[8] + coefficients(random3_2_4)[9]

# Exercise 3.2
# Estimate the RE binary probit model explaining the choice of formal or informal sector job, 
# using the predicted wage differential (and an intercept) as the only explanatory variable. Interpret the results
install.packages("pglm")
library("pglm")
pdata3_3_2 = pdata.frame(mex92,index = c("pn","quarter"))
random3_3_2 = pglm(formm ~ wage_diff1, family = binomial('probit'),data = pdata3_3_2, model = "random")

# Exercise 3.3
# Do the same as in part 3-2, but using the RE logit model. Compare the results. Which one do you prefer?
random3_3_3 = pglm(formm ~ wage_diff1, family = binomial('logit'),data = pdata3_3_2, model = "random")
# both models give more or less the same choice predictions.

# Exercise 3.4
# Compute the average marginal effects of a change in the log wage differential, both for probit and logit specifications. 
# Compare the results
Marginal_Effects = function(ex,beta,sigma1){
   integrand = function(x){ (1/(2*pi))*exp(-(beta[1]+beta[2]*ex+x)^2)*exp(-(x/sigma1)^2)}
  Integral_ME = integrate(integrand, -Inf, Inf)
  return(Integral_ME)
   }
 
MarginalEffects = sapply(1:length(mex92$wage_diff1),function(row){Marginal_Effects(mex92$wage_diff1[row],c(coefficients(random3_3_2)[1],coefficients(random3_3_2)[2]),coefficients(random3_3_2)[3])$value})
Avg_ME = mean(MarginalEffects)*coefficients(random3_3_2)[2]
# Exercise 3.5
# Test whether it is worthwhile including city dummies in the model of part 3-2 or 3-3 
# whichever you think it is most suitable)
random3_3_5 = pglm(formm ~ wage_diff1 + juare + tijua + monte , family = binomial('probit'),data = pdata3_3_2, model = "random")
summary(random3_3_5)
linearHypothesis(random3_3_5, c("juare = 0", "tijua = 0", "monte = 0"))

# Exercise 3.6
# Could you also use the FE model in part 3-3? If so, compare the results with part 3-3.
# Not possible.

# Exercise 3.7
# Could you also include education level in the binary choice model? Explain why or why not.
random3_3_7 = pglm(formm ~ wage_diff1 + juare + tijua + monte + mided + higed, family = binomial('probit'),data = pdata3_3_2, model = "random")
summary(random3_3_7)
# According to the coefficient signs , intermediate and high level education increases the likelihood of choosing a job in formal sector job in a random effects specification.
# so education can also be included in the binary choice model.

