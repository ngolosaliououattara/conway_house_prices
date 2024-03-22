rm(list=ls())
Conway <- read.csv("ConwayHP(initial).csv",header =T)

print(summary(Conway))

require(psych)
print(describe(Conway))


# Finding parwise correlation of independent variables (I needed to create a new
# data with no qualitative variables)
Conway1 <- read.csv("Conway house(initial)1.csv",header =T)
result = cor(Conway1[,1:7])
print(result)

#Checking Linear terms
pairs(House.Price_thousand ~ Beds + Baths + Area_thousand + Address + Year.built  
      + Parking + Yard + Church , data = Conway, 
      main = "scattered plot matrix")

# fitting the initial model
Initialmodel <- lm(House.Price_thousand ~ Beds + Baths +  Area_thousand + Year.built 
                   + Parking + Yard + Church, data = Conway)
print(summary(Initialmodel))



# Stepwise
fullmodel <-lm(House.Price_thousand ~ Beds + Baths +  Area_thousand + Year.built 
               + Parking + Yard + Church , data = Conway)
model <- step(fullmodel, direction = "both")
print(summary(model))

#Finding the standard error(s)
s<- summary(model)$sigma
print(s)
#Finding CV
y <- Conway$House.Price_thousand
CV <- s/mean(y)*100
print(CV) 


##Identifying observations that are outliers
# Finding standardized residuals
stdres <- rstandard(model)
obs.outliers <- Conway[abs(stdres)> 3,]
print (obs.outliers)


##### Influential Observations ########
# Finding leverages
lev <- hatvalues(model)
plot(lev)
# Printing the observation with the largest leverage
obs.influential <- Conway[lev > 2*7/80,]
print(obs.influential)

##### Checking for an influential observation using Cook's distance
cook <- cooks.distance(model)
plot(cook, ylab = "Cook's distances")
# Printing the observation with the largest Cook's distance
obs.influential.cook <- Conway[cook > 1,]
print(obs.influential.cook)



#Standardized Regression Coefficients
stdModel <- lm(scale(House.Price_thousand) ~ scale(Baths) + scale(Area_thousand)
               + scale(Year.built), data = Conway)
print(summary(stdModel))

#### Checking the normality of residuals
res <- residuals(model)
# Subjective decision
hist(res)
qqPlot(model, main="QQ Plot")

# Objective decision
result1 <- shapiro.test(res)
print(result1) # p-value = 0.1756 is greater than 0.05 the sample comes from a normal distribution

##### Checking for constant variance of residuals (homoscedasticity)
# Objective decision using Breusch-Pagan test
# H0: Variances of residuals are constant (homoscedasticity)
require(lmtest)
bp.result <- bptest(model)
print(bp.result) #p-value = 0.1106 > 0.05 residuals are constant (homooscedasticy)

##### Checking for independence of residuals (Autocorrelation)
# H0: Not autocorrelated
result <- dwtest(model, alternative = "two.sided")
print(result)
#DW = 1.94 close to two, p-value = 0.74773 not autocorrelated they are independent.


require(car)
crPlots(model)

