rm(list=ls())
Conway <- read.csv("Conway-House.csv",header =T)
print(summary(Conway))

require(psych)
print(describe(Conway))

#Checking Linear terms
pairs(House.Price_thousand ~ Beds + Baths + Area_thousand + Address + Year.built  + Parking + Yard + Church, data = Conway, 
      main = "scattered plot matrix")
#Assess the model
model <- lm(House.Price_thousand ~ Beds + Baths +  Area_thousand + Year.built  + Parking + Yard + Church , data = Conway)
print(summary(model))

# Stepwise
fullmodel <-lm(House.Price_thousand ~ Beds + Baths + Area_thousand + Year.built + Parking + Yard + Church , data = Conway)
model <- step(fullmodel, direction = "both")
print(summary(model))
#lm(formula = House.Price_thousand ~ Baths + Area_thousand + Year.built, data = Conway)

#VIF
require(car)
Result = vif(model)
print(Result) #VIF < 10, no multicolinearity for all. If the VIF goes above 10, 
#you can assume that the regression coefficients are poorly estimated due to multicollinearity.
 #    Baths Area_thousand    Year.built       Parking       
#    4.06          3.90          1.18          1.49      
####Baths Area_thousand    Year.built       Parking 
#####3.96          3.87          1.18          1.48 
###Baths Area_thousand    Year.built #############
####3.94          3.80          1.08 ################

#Finding CV
s <- 32.4
y <- Conway$House.Price_thousand
CV <- s/mean(y)*100
print(CV) #CV = 16.4 >10; This means that it is a BAD model
                #16
res.model = residuals(model)
hist(res.model)
qqPlot(model, main = "Q-Q Plot")
##### Identifying observations that are outliers
# Finding standardized residuals
stdres <- rstandard(model)
obs.outliers <- Conway[abs(stdres)> 3,]
print (obs.outliers)
#   House.Price_thousand Beds Baths Area_thousand                            Address Year.built Parking Yard Church
#40                  235    4     3          2.36 1025 Mitchell St, Conway, AR 72034       1917       2   No    0.3
# If you find any outliers, you remove that data then rebuild the model.
#### Checking the normality of residuals
res <- residuals(model)
# Subjective decision
hist(res)
qqPlot(model, main="QQ Plot")

# Objective decision
result1 <- shapiro.test(res)
print(result1) # p-value = 0.1048 is greater than 0.05 the sample comes from a normal distribution
                #p-value = 0.1247
result2 <- ks.test(res, "pnorm")
print(result2)# p-value = < 2.2e-16 is lower than 0.05 the sample deviates from normality
#This p-value tells you what the chances are that the sample comes from a normal distribution. 
#The lower this value, the smaller the chance. Statisticians typically use a value of 0.05 as a cutoff, 
#so when the p-value is lower than 0.05, you can conclude that the sample deviates from normality.
##### Checking for constant variance of residuals (homoscedasticity) ###########
# Subjective decision
predicted.y <- predict(model)
plot(predicted.y, res)
abline(0,0)
# Objective decision using Breusch-Pagan test
# H0: Variances of residuals are constant (homoscedasticity)
require(lmtest)
bp.result <- bptest(model)
print(bp.result) #p-value = 0.07228 > 0.05 residuals are constant (homooscedasticy)
                 # p-value = 0.04743
##### Checking for independence of residuals (Autocorrelation) ##########
# H0: Not autocorrelated
result <- dwtest(model, alternative = "two.sided")
print(result)
# DW = 1.96 close to two,  p-value = 0.8233 not autocorrelated they are independent.
# DW = 1.97, p-value = 0.844
##### Influential Observations ########
##### Checking for influential observations using leverage (number between 0 and 1)
# Influential observation = The one that changes the fit drastically 
# if removed or it is an outlier
# Leverage finds influential observations using 
# unusual values of (only) DEPENDENT variables
# If h > 2(k+1)/n, where k+1 is the number of parameters, 
# then it is an influential observation
# Finding leverages
lev <- hatvalues(model)
plot(lev)
# Printing the observation with the largest leverage
obs.influential <- Conway[lev > 2*9/91,]
print(obs.influential)

#House.Price_thousand Beds Baths Area_thousand                             Address Year.built Parking Yard Church
#51                  190    4     2          2.81 2033 Robinson Ave, Conway, AR 72034       1886       0  Yes    0.4
#54                  310    4     3          2.58   122 Stanford Rd, Conway, AR 72032       1976       6  Yes    1.4



##### Checking for an influential observation using Cook's distance
# If Cook's distance of an observation is more than 1 (or 4/n), 
# then it is an influential outlier
# Cook's distance finds influential observations 
# using unusual values of (both) INDEPENDENT and/or DEPENDENT variables 
cook <- cooks.distance(model)
plot(cook, ylab = "Cook's distances")
# Printing the observation with the largest Cook's distance
obs.influential.cook <- Conway[cook > 1,]
print(obs.influential.cook)

##### Combining outliers, leverages, and Cook's distances in one plot #####
plot(lev, stdres, ylim = c(-6,6), type = 'n',
     xlab = "Leverage", ylab = "Standardized residuals")
points(lev, stdres, cex=10*cook/max(cook))
abline(v=2*4/25, lty=2)
abline(h=c(-3,0,3, ltyp=2))
identify(lev, stdres, row.names((Conway)))

