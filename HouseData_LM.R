
#  car package installation for Levene Tests
# install.packages("car")

# # readxl package installation to load and read excel Data
# install.packages("readxl")

# # readxl package installation to conduct probability tests
# install.packages("ggpubr")

# # readxl package installation to  plot the graphs
# install.packages("ggplot2")

library(car) # Loading the car library for Levene Test
library("readxl") # Loading the readxl library to read excel data
library(dplyr)      # for data manipulation functions
library(tidyr)      # for data manipulation functions

# Reading the data from excel and storing to a data frame
data <- readxl::read_excel("C:\\Users\\Srinivas\\Documents\\DataSets\\LM_HouseData-6.xlsx")

data # prints the data stored in the variable  named "df"


#_________ QUESTION: 1 _________

#Shapiro-Wilk test to check normality of response variable.
# If p-value > 0.05 then Null Hypothesis and it is Normally Distributed.


# Plotting the HousePrice2021 to analyse
boxplot(HousePrice2021  ~ Location, data = dt, main = "House Prices of 2021 per Location", col = c(2,3,4,5))


# Plotting the HousePrice2020 to analyse
boxplot(HousePrice2020  ~ Location, data = dt, main = "House Prices of 2020 per Location", col = c(2,3,4,5))

### Method 1:

# Peroforming Shapiro wilk test to check Normality for House Prices in 2021
shapiro.test(subset(dt, Location == "East")$HousePrice2021)
shapiro.test(subset(dt, Location == "West")$HousePrice2021)
shapiro.test(subset(dt, Location == "North")$HousePrice2021)
shapiro.test(subset(dt, Location == "South")$HousePrice2021)

# Peroforming Shapiro wilk test to check Normality for House Prices in 2020
shapiro.test(subset(dt, Location == "East")$HousePrice2020)
shapiro.test(subset(dt, Location == "West")$HousePrice2020)
shapiro.test(subset(dt, Location == "North")$HousePrice2020)
shapiro.test(subset(dt, Location == "South")$HousePrice2020)



# Method 2:

# select specific rows and coloumns for East Location
df_est <- data[1:5, c(1:3)]

# checking whether the data is normally distributed using "Shapiro-Wilk test" for East Location

shapiro.test(df_est$HousePrice2021) 
shapiro.test(df_est$HousePrice2020)

# select specific rows and coloumns for West Location
df_wst <- data[6:10, c(1:3)]

# checking whether the data is normally distributed using "Shapiro-Wilk test" for West Location

shapiro.test(df_wst$HousePrice2021) 
shapiro.test(df_wst$HousePrice2020)

# select specific rows and coloumns for North Location
df_nth <- data[11:15, c(1:3)]

# checking whether the data is normally distributed using "Shapiro-Wilk test" for North Location
shapiro.test(df_nth$HousePrice2021) # Shapiro-wilk normality test failed p < 0.05
shapiro.test(df_nth$HousePrice2020)

# select specific rows and coloumns for South Location
df_sth <- data[16:20, c(1:3)]

# checking whether the data is normally distributed using "Shapiro-Wilk test" for South Location
shapiro.test(df_sth$HousePrice2021) # Shapiro-wilk normality test syntax
shapiro.test(df_sth$HousePrice2020)



#_________ QUESTION: 2 _________

# Levene's test to check homogeneity of variances.

# Using the Levene's test in
# R, assessing whether the assumption of homogeneity of variances is met and commenting on results'''


# Levene Test for House Price 2021 and location
leveneTest(df$HousePrice2021, df$Location, center = mean) 

# Levene Test for House Price 2020 and location
leveneTest(df$HousePrice2020, df$Location, center = mean)

# The assumption of homogeneity of variances is not violated for neither 
# HousePrice2021 nor HousePrice2021 respectively F value = 0.582, p= .0.6354 and
# F value = 1.8452, p = 0.1796.


# _________ QUESTION: 3 _________

# One-way ANOVA test
est21 <- aov(HousePrice2020 ~ Location, data = data)

summary(est21)

# To find the means of the table
model.tables(est21, "means") 

# Tukey tests to find the critical values and probability at 95% confidence level
comparison <- TukeyHSD(est21) 

comparison

# Plotting the graph of one-way ANOVA test
plot(comparison, col = c(1,2,3,4,5,6))

ls <- par(cex = 0.6) # setting legend size

legend("topleft", c("North-East","South-East", "West-East","South-North","West-North","West-South"),
       fill=c(1,2,3,4,5,6))

par(ls) 

#_________ QUESTION: 4 _________

# Performing Analysis of Covariance to test if there is a difference between selling prices 
# in 2021 across four different locations.

is.factor(data$Location)

# Checking ANOVA test for the House Prices of 2021
base_mdl <- aov(HousePrice2021 ~ Location, data = data)
summary(base_mdl)

# Creating a Linear model
mdl <- lm(HousePrice2021 ~ Location, data = data)
anova(mdl)

# Checking summary of the created  linear model
summary(mdl)

# ANCOVA tests
Anova(mdl, type = "III")


# Checking ANCOVA test for the House Prices of 2021 in another method
ancova <- aov(HousePrice2021 ~ Location, data = data)

Anova(ancova, type = "III")


# linear model 1 to find and plot Residual vs fitted values and Normal Q-Q vs Theoretical Quantiles
model1 <- lm(HousePrice2021 ~ HousePrice2020 + factor(Location), data = data)
summary(model1)
anova(model1)

model1$coefficients
Anova(model1, type = "III") # Anova type 3 tests

par(mfrow = c(1,2))

# Plotting the graphs for Residuals and Theoretical Quantiles
plot(model1, which = 1)
plot(model1, which = 2)


# linear model 2 to find and plot Residual vs fitted values and Normal Q-Q vs Theoretical Quantiles

model2 <- lm(HousePrice2021 ~ HousePrice2020 * factor(Location), data = data)
summary(model2)
anova(model2)

model2$coefficients

Anova(model2, type = "III") # Anova type 3 tests

par(mfrow = c(1,2))

# Plotting the graphs for Residuals and Theoretical Quantiles
plot(model2, which = 1)
plot(model2, which = 2)
mdl_ancv <- summary(aov(HousePrice2021 ~ Location+HousePrice2020, data = data))
mdl_ancv



#_________ QUESTION: 5 _________
# ANCOVA (Analysis of Covariance) for House price 2021

#Applying the linear regression model
mdl_lm <- lm(HousePrice2021 ~ Location+HousePrice2020, data = data)

resl <- summary(mdl_lm) # checking summary of the linear model

# Finding the Co-efficients of ANCOVA model
coeffs<- coef(resl)

east_intercept <- coeffs[1,1]
west_intercept <- coeffs[2,1] + east_intercept
north_intercept <- coeffs[3,1] + west_intercept
south_intercept <- coeffs[4,1] + north_intercept

slope <- coeffs[5,1]
coeffs

#creating a scatter plot between house Price2021 and House Price 2020 for differences
plot(HousePrice2021 ~ HousePrice2020, data=data,
     pch = rep(c(3,17,19,25),c(10,10,10,10)), col = rep(c("green","red","blue","black"), c(10,10,10,10)),
     main = "ANCOVA Regression Lines", cex.lab = 1.1, cex.axis = 1)

op <- par(cex = 0.8) # setting legend size


# Adding legend to the above plot
legend("bottomright", legend = levels(house_data$Location), pch = c(3,17,19), col = c("green", "red","blue","black"),
       border = "black", plot = TRUE)

par(op) # calling op variable


#creating regression lines for east,west,north,south location using coefficients of linear regression
abline(east_intercept,slope, col = "green")
abline(west_intercept,slope, col = "red")
abline(north_intercept,slope, col = "blue")
abline(south_intercept,slope, col = "black")



