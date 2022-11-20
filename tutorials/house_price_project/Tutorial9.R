#load libraries 
library(stargazer)
library(tidyverse)
library(ggthemes)
library(texreg)

#laod data
dat <- readRDS("data/train.rds")

#histogram
hist(dat$BldgGrade)

with(dat, boxplot(AdjSalePrice ~ BldgGrade))

mod1 <- lm(dat$AdjSalePrice ~ as.ordered(dat$BldgGrade))
stargazer(mod1, type = "html")

#plot zipcodes
dat %>%
  group_by(ZipCode) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(as.factor(reorder(ZipCode, n)), n)) +
  geom_col() +
  coord_flip() +
  xlab("Zip Code")

#create zip_group
Zip_group <- dat %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(AdjSalePrice),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

dat <- dat %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")

mod2 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup, data = dat)

stargazer(mod2, type = "html")

###Property type
dat <- dat %>%
  mutate(propertytype = as.factor(PropertyType))

#plot property type
dat %>%
  group_by(propertytype) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes((reorder(propertytype, n)), n)) +
  geom_col()+
  ylab("") +
  xlab("") +
  theme_economist()
  
#create propertytype group
PT_group <- dat %>%
  group_by(propertytype) %>%
  summarise(med_price = median(AdjSalePrice),
            count = n()) %>%
  arrange(med_price)

dat <- dat %>%
  left_join(select(PT_group, propertytype), by = "propertytype")

mod3 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + propertytype, data = dat)

stargazer(mod3, type = "text")

texreg(mod3)



