#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("ggplot2", "tidyverse"),  pkgTest)

<<<<<<< HEAD
# set working directory
#setwd("~/Documents/GitHub/QTM200Spring2021/problem_sets/PS1")


=======
>>>>>>> af2a91eef73f36234332980a3a4c406b0f477d9e
#####################
# Problem 1
#####################

#load data
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#calculate mean
ybar <- mean(y)

#visualize y
hist(y, xlab = "IQ scores")

# get confidence intervals 
CI_lower <- qnorm(0.05, 
                  mean = mean(y), 
                  sd = (sd(y)/sqrt(length(y))) # the equation for the standard error of the mean
)

CI_upper <- qnorm(0.95,
                  mean = mean(y),
                  sd = (sd(y)/sqrt(length(y)))
)

matrix(c(CI_lower, CI_upper), ncol = 2,
       dimnames = list("",c("Lower", "Upper")))

#get confidence intervals using t distribution
se <- sd(y)/sqrt(length(y))
se
t_score <- qt(.05, df = length(y)-1, lower.tail = FALSE)
t_score
CI_lower_t <- mean(y) - (se * t_score)
CI_lower_t
CI_upper_t <- mean(y) + (se * t_score)
CI_upper_t

# Test our hypothesis
# get assumptions Week 2 slides
# null hypothesis ybar less than/equal to 100
# alternative hypothesis ybar greater than 100 

# Step 1 : 
# I am assuming the distribution is normal 

# Step 2 :
# The null hypothesis is that ybar is less than or equal to 100
# The alternative hypothesis is that ybar is greater than 100

# Step 3: 
# Calculate test statistic
# t.score = ybar - mu/ se 
ts <- (ybar - 100)/se
ts

#check t.score result with R t.test() 
t.test(y,
       mu = 100, 
       alternative = "greater",
       conf.level = .95)

#Step 4:
# Calculate p-value for a right-tailed test
p_value=pt(q=-0.59574, df=length(y)-1, lower.tail=FALSE)

#Step 5: 
# Conclusion
#Because the p value is greater than alpha, we cannot reject
# the null hypothese

#####################
# Problem 2
#####################

<<<<<<< HEAD
#load data
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/expenditure.txt", header=T)

##Question 1 

# plot the relationship between Y and X1
ggplot(expenditure, aes(x=Y, y=X1)) + geom_point() +
labs(x="Per capita expenditure on shelters / housing \n assistance",
     y = "Per capita personal income")
ggsave("plot1.png")
#This relationship between these variables is broadly linear however
# there are a few outliers

# plot the relationship between Y and X2
ggplot(expenditure, aes(x=Y, y=X2)) + geom_point()+
  labs(x="Per capita expenditure on shelters / housing \n assistance",
       y = "No. per 100,000 who\nare 'financially insecure'")
ggsave("plot2.png")
# There is not a linear relationship between these variables

#plot the relationship between Y and X3
ggplot(expenditure, aes(x=Y, y=X3)) + geom_point() +
  labs(x="Per capita expenditure on shelters / housing \n assistance",
       y = 'No. per thousand residing\n in urban areas')
ggsave("plot3.png")
#There is a somewhat linear relationship between these variables

#plot the relationship between X1 and X2
ggplot(expenditure, aes(x=X1, y=X2)) + geom_point() +
  labs(x= "Per capita personal income",
       y = "No. per 100,000 who\nare 'financially insecure'")
ggsave("plot4.png")
#There is not a linear relationship between these variables

#plot the relationship between X1 and X3 
ggplot(expenditure, aes(x=X1, y=X3)) + geom_point() +
  labs(x= "Per capita personal income",
       y = 'No. per thousand residing\n in urban areas')
ggsave("plot5.png")
#There is a linear relationship between these variables

#plot the relationship between X2 and X3
ggplot(expenditure, aes(x=X2, y=X3)) + geom_point() + 
  labs(x = "No. per 100,000 who\nare 'financially insecure'",
       y = 'No. per thousand residing\n in urban areas')
ggsave("plot6.png")
#There is no relationship between these variables 

##Question 2 
#plot the relationship between Y and Region
ggplot(expenditure, aes(x=Y, y = Region, group = Region)) +
  geom_boxplot() +
  labs(x = "Per cap. expenditure on \nshelters/housing assistance") +
  coord_flip()
ggsave("plot7.png")

# From this plot we can see that Region 4 (West) spends the most 
# on average per capita on shelters/housing assistance

# Question 3
yx1 + geom_point(aes(shape = factor(Region)))

ggplot(expenditure, aes(x=Y, y=X1)) +
  geom_point(aes(shape = factor(Region))) +
  geom_point(aes(color = factor(Region)))+
  labs(x="Per capita expenditure on shelters / housing \n assistance",
       y = "Per capita personal income")
ggsave("plot8.png")
 


=======
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/expenditure.txt", header=T)
>>>>>>> af2a91eef73f36234332980a3a4c406b0f477d9e
