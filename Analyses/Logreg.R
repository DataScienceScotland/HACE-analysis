##Logistic regression analyses to assess which protected characteristics are most closely associated with unmet caring need
library(tidyverse)

#Base dataset
View(hace2122)


##Preparing regression dataset + variable selection
regression_dataset <- hace2122 %>% 
  mutate(unmetneed = case_when(q29g == "1" ~ "1",
                               q29g == "0" ~ "0",
                               T~q29g),                  
         ethnicity = case_when(q43 == "1" ~"White",
                               q43 %in% c("2","3","4","5","6") ~ "Black/Minority/Ethnic",
                               T~q43)) %>% 
  filter(!is.na(unmetneed)) %>%                   #2843 unmet need, 114,538 not unmet need, 12,971 NAs - THIS STEP REMOVES NAs to unmet need question 
  replace(is.na(.), "UNANSWERED") %>%             #creating a new category for those who did not answer
##turning explanatory variables into factors
  mutate(SIMD = as.factor(simd2020v2_sc_quintile),
         instensity = as.factor(q35),
         disability = as.factor(q41),
         ethnicity = as.factor(ethnicity),
         sexorientation = as.factor(q44),
         unmetneed = as.numeric(unmetneed)) %>%                
  select(unmetneed, SIMD, instensity, disability, ethnicity, sexorientation) 



#reordering factors to toggle reference variable
regression_dataset$instensity <- relevel(regression_dataset$instensity, "6")
regression_dataset$disability <- relevel(regression_dataset$disability, "3")

##checking variable order
contrasts(regression_dataset$disability)


##missing data?
library(Amelia)

missmap(regression_dataset, col=c('blue', 'red'))


##let's create a model, shall we?

#removed sexual orientation to improve model because of low deviance
model <- glm(formula = unmetneed ~ SIMD+instensity+disability+ethnicity, family = binomial, data = regression_dataset)
summary(model) ## holy smokes, these are nearly all significant compared to the reference variable!

#calculate odds with exp(logit)
#calculate probability with exp(logit)/(1+exp(logit))


##table of deviance
##sexual orientation is pretty weak
anova(model, test = 'Chisq')

