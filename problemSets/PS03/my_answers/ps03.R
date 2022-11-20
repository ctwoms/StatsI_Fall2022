#load libraries 
library(readr)
library(stargazer)
library(ggplot2)

#load data
dat <- read_csv("datasets/incumbents_subset.csv")

###question1----
#1.1 run a regression
lm1 <- lm(voteshare ~ difflog, data = dat )
stargazer(lm1)

#1.2 create scatter plot and add regression line 
lm1_plot <- ggplot(dat, aes(difflog, voteshare))+
            geom_point(alpha = .2)+
            geom_smooth(method = lm)
lm1_plot
ggsave("lm1_plot.png")

#1.3 saving residuals
rlm1 <- lm1$residuals

#1.4 writing prediction equation
# yhat = b0 + B1x
# yhat = 0.579 + 0.42x

###question 2 ----
#2.1 run regression model 
lm2 <- lm(presvote ~ difflog, dat)
stargazer(lm2)

#2.2 create scatter plot and add regression line 
lm2_plot <- ggplot(dat, aes(difflog, presvote))+
  geom_point(alpha = .2)+
  geom_smooth(method = lm)
lm2_plot
ggsave("lm2_plot.png")

#2.3 save residuals
rlm2 <- lm2$residuals

#2.4 writing prediction equation
# yhat = b0 + b1x
# yhat = 0.508 + 0.024x

###question 3 ----
#3.1 run regression model
lm3 <- lm(voteshare ~ presvote, dat)
stargazer(lm3)

#3.2 create scatterplot and add regression line
lm3_plot <- ggplot(dat, aes(voteshare, presvote))+
  geom_point(alpha = .2)+
  geom_smooth(method = lm)
lm3_plot
ggsave("lm3_plot.png")

#3.3 write prediction equation
# yhat = b0 + b1x
# yhat = 0.441 + 0.338x

###question 4----
#4.1 run regression model 
lm4 <- lm(rlm1 ~ rlm2)
stargazer(lm4)

#4.2 residuals
rlm4 <- lm4$residuals

# 4.3 scatterplot and regression line
####what do I do here?? 
lm4_plot <- ggplot(aes(rlm2, rlm1), data = dat)+
  geom_point(alpha = .2)+
  geom_smooth(method = lm)
lm4_plot
ggsave("lm4_plot.png")

#4.4 prediction equation
#yhat = b0 + b1x 
#yhat = .000 + 0.257x

###Question 5 ----
# 5.1 run regression 
lm5 <- lm(voteshare ~ difflog + presvote, dat)
stargazer(lm5)

# 5.2 prediction equation
# yhat = b0 + b1x + b2x 
# yhat = 0.449 + 0.36x + 0.257x 

# 5.3 
#

