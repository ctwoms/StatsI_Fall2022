#load libraries and data
library(car)
library(tidyverse)
library(stargazer)
data("Prestige")
help(Prestige)

#Question 1 ----
#1a create new variable 
Prestige <- Prestige %>% 
  mutate(professional = ifelse(
    type == "prof", "1", 
    ifelse(type == "bc", "0", 
           ifelse(type == "wc", "0", 
                  ifelse(is.na(type), NA, "Other")))))

#1b Linear model 
lm1 <- lm(prestige ~ income + professional + income:professional, data = Prestige) 
lm1
stargazer(lm1)

#1c Write prediction equation
#yhat = b0 + b1x + b2d + b3xd
#yhat = 21.142 + .003x + 37.781d + -.002xd
#d = 0 / yhat = b0 + b1x
#d = 1 / yhat = (b0 + b2) + (b1 + b3)x

#1d interpret income coefficient
# For a one dollar increase, controlling for the type of work and the 
# interaction effect between income and type of work, there is a .003171
# increase in prestige

#1e interpret professional coefficient
# Being a white collar worker, controlling for the type of work and the 
# interaction effect between income and type of work, is associated with 
# a 37.781 increase in prestige

# 1f effect of $1,000 increase in income on prestige 
# d = 1 / yhat = (b0 + b2) + (b1 + b3)x
# yhat = 21.142 + .003x + 37.781d + -.002xd
# yhat = 21.142 + .003x + 37.781 (1) + -.002x(1)
# yhat = 21.142 + .003x + 37.781 - .002x 
# yhat = (21.142 + 37.781) + (.003x - .002x)
# yhat = 58.923 + .001x 
# yhat = 58.923 + .001(1000)
# yhat = 58.923 + 1
# yhat = 59.923

# 1g effect of changing professional type when income $6,000
# when d = 0
# yhat = b0 + b1x + b2d + b3xd
#yhat = 21.142 + .003x + 37.781d + -.002xd
# yhat = 21.142 + .003x + 37.781(0) +- .002x(0)
# yhat = 21.142 + .003x
# yhat = 21.142 + .003(6000)
# yhat = 21.142 + 18
# yhat = 39.142

# when d = 1
# yhat = 58.923 + .001(6000)
# yhat = 58.923 + 6
# yhat = 64.923

# 64.923 - 39.142
# 25.781

# the marginal effect of changing from non-professional to professional when
# income is $6,000 is 25.781 increase in prestige 

# Question 2----
#2a how signs in precinct affects vote share
#hypothesis testing 

# Step 1 : 
# I am assuming the distribution is normal 

# Step 2 :
# The null hypothesis is vote share is not effected by lawn signs
# The alternative hypothesis is that vote share is effected by lawn signs

# Step 3: 
# Calculate test statistic
# t.score = ybar - mu/ se 
ts <- (.042-0)/.016
ts
#2.625

#Step 4:
# Calculate p-value for two-tailed test
# p_value <- 2 x pt(ts, df=length(y)-parameters, lower.tail=FALSE)
pvalue <- 2*(pt(ts, 131-3, lower.tail = F))
pvalue
#0.00972002

#Step 5: 
# Conclusion
#Because the p value is less than alpha (.05), we can reject
# the null hypothesis

#2b how signs in adjacent affects vote share
#hypothesis testing 

# Step 1 : 
# I am assuming the distribution is normal 

# Step 2 :
# The null hypothesis is vote share is not effected by lawn signs
# The alternative hypothesis is that vote share is effected by lawn signs

# Step 3: 
# Calculate test statistic
# t.score = ybar - mu/ se 
ts1 <- (.042-0)/.013
ts1
#3.230769

#Step 4:
# Calculate p-value for two-tailed test
# p_value <- 2 x pt(ts, df=length(y)-parameters, lower.tail=FALSE)
pvalue1<- 2*(pt(ts1, 131-3, lower.tail = F))
pvalue1
#0.00156946

#Step 5: 
# Conclusion
# Because the p value is less than alpha (.05), we can reject
# the null hypothesis

#2c interpret coefficient for constant
# When the all other covariates are zero, the constant 
# represents the predicted value for the outcome 
# variable. So in this case, this is the predicted proportion
# of the vote share that went to McAuliff's opponent
# Ken Cuccinelli if there were no lawn signs at all. 

# 2d Evaluate the model fit for the regression 
# The model has an Rsquared value of .092 which is very low.
# This means that the model does not account for much 
# of the variance which, unsurprisingly, indicates that other factors may be
# at play for the swing to Cuccinelli other than one's proximity
# to a lawn sign






