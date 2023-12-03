# install.packages('lme4')
library(lme4) 
library(MASS)
library(broom)
library(tidyverse)

#==================================
# 1. Data Exploration.
#==================================

# Uploading the data
df <- sleepstudy

# b. Explore the structure of the dataset.
str(df)
names(df)

summary(df$Reaction)
summary(df$Days)
range(df$Days)


# c. Visualize the data using appropriate plots to understand the distribution and relationships.
require(lattice)

# Ploting the relation between the reaction time and 
# days of sleep deprivation for each subject
xyplot(Reaction ~ Days | Subject, df, type = c("g","p","r"),
       index = function(x,y) coef(lm(y ~ x))[1],
       xlab = "Days of sleep deprivation for each Subject",
       ylab = "Average reaction time (ms)", aspect = "xy")

###########################
# 2. Descriptive Statistics.
###########################

# a. Compute and report summary statistics for the key variables.
summary(df$Reaction)
summary(df$Days)
range(df$Days)
sd(df$Reaction)
median(df$Reaction)

# b. Create visualization
# Ploting the boxplot of reaction by days
boxplot(Reaction ~ Days, df, 
        xlab = "Days of sleep deprivation",
        ylab = "Average reaction time (ms)")

# Check for normality graphic
hist(df$Reaction, main="Histogram")
qqnorm(df$Reaction)
qqline(df$Reaction)

# Test for normality
shapiro.test(df$Reaction) # Not normally distributed

# Log transformation
df <- mutate(df, log_Reaction = log(Reaction))
shapiro.test(log_Reaction) # Normal now after a non-linear transformation


# 3. Fit an adequate Model(s).
# install.packages("Matrix", dependencies=TRUE)

# Modelling 
# Days 0-1 were adaptation and training (T1/T2), day 2 was baseline (B); 
# sleep deprivation started after day 2. that is why subset=Days>=2
model1 <- lmer(log_Reaction ~ Days + (1|Subject), df, subset=Days>=2)
summary(model1)



# 4. Residual Analysis

# Fit the mixed-effects model

# Obtain model residuals
residuals <- resid(model1)

# Check for normality of residuals with a Q-Q plot
qqnorm(residuals)
qqline(residuals)

# Check for homoscedasticity with a scale-location plot
plot(fitted(model1), sqrt(abs(residuals)), 
     main = "Scale-Location Plot", xlab = "Fitted Values", 
     ylab = "sqrt(|Residuals|)")
