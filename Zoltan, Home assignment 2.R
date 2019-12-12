library(psych) # for describe	
library(lm.beta) # for lm.beta	
library(tidyverse) # for tidy format	
library(gridExtra) # for grid.arrange	
library(ggplot2)
library(car)
library(lmtest)


#imort dataset. Home_sample_1
#look at the data#

describe(home_sample_1)	
summary(home_sample_1)	
str(home_sample_1)


###  We plotted the data and found an outlier in the STA variable, witch is probably due to a wrong readning, this because the scale ranges between 20 and 80, and the reading is 3,5. You could assume that the pont margin is placed on the wrong side of the digit (5) in this case, but rather than taking the chance of this and contribute to falsification of tha data i proceded with taking away the patricipant from the analysis. 
#Since the data is exactly det same as in ass.1 we settle with removing the outlier in this dataset. 
#finding and removing the outlier

na.omit(home_sample_1)
pain.1 <- home_sample_1 [-c(18), ]

describe(pain.1)	
summary(pain.1)

# and we build the model as in ass. 1. 

pain.mod.theory <- lm(pain ~ STAI_trait + cortisol_serum + mindfulness + pain_cat + age + sex, data = pain.1)	
pain.mod.theory
summary(pain.mod.theory)

#the primary model is now finnished to use. 

#############Time to build the backward regression model#################

#First rechecking the data since this model will use new varaibles. 

pain.1 %>% 	
  ggplot() 	+
  aes(x = age)	+
  geom_histogram( bins = 50)	

pain.1 %>% 	
  ggplot() 	+
  aes(x = age)	+
  geom_histogram( bins = 50)	

pain.1 %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram( bins = 50)	

pain.1 %>% 	
  ggplot() +	
  aes(x = STAI_trait) +
  geom_histogram( bins = 50)

pain.1 %>% 	
  ggplot() +	
  aes(x = mindfulness) +	
  geom_histogram( bins = 50)	

pain.1 %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram( bins = 50)	

pain.1 %>% 	
  ggplot() +	
  aes(x = household_income) +	
  geom_histogram( bins = 50)	

pain.1 %>% 	
  ggplot() +	
  aes(x = IQ) +	
  geom_histogram( bins = 50)	

# scatterplot	

pain.1 %>% 	
  ggplot() +	
  aes(x = age, y = pain) +	
  geom_point()	

pain.1 %>% 	
  ggplot() +	
  aes(x = pain_cat, y = pain) +	
  geom_point()	

pain.1 %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = pain) +	
  geom_point()	

pain.1 %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain) +	
  geom_point()	

pain.1 %>% 	
  ggplot() +	
  aes(x = cortisol_serum, y = pain) +	
  geom_point()	

pain.1 %>% 	
  ggplot() +	
  aes(x = IQ, y = pain) +	
  geom_point()	

pain.1 %>% 	
  ggplot() +	
  aes(x = household_income, y = pain) +	
  geom_point()	

pain.1 %>% 	
  ggplot() +	
  aes(x = weight, y = pain) +	
  geom_point()	

##First fitting the model. as the collegue priviusly did, using the same datafram as before ##

pain.mod.changed <- lm(pain ~ sex + STAI_trait + cortisol_serum + IQ + weight + household_income + mindfulness + pain_cat + age, data = pain.1)	
pain.mod.changed
summary(pain.mod.changed)

#now running the backward regression. 

pain.mod.backward <-  step(pain.mod.changed, direction = "backward")
summary(pain.mod.backward)

### tests of the model. 

# checking for inconsistensies in the residuals of tha data in the model. 

##Checking for normality in resuduals

win.graph()
pain.mod.backward %>% 	#Cooks distance
  plot(which =5)	

pain.mod.backward %>% 	#Cooks distance 
  plot(which = 4)	

pain.mod.backward %>% 	#following the axis
  plot(which = 2)	

win.graph()
hist( x = residuals( pain.mod.backward ), # data are the residuals
      xlab = "Value of residual", # x-axis label
      main = "", # no title
      breaks = 20 # lots of breaks
)

##Checking for liniarity in resuduals
win.graph()
pain.mod.backward %>% 	#following the axis
  plot(which = 1)	

pain.mod.backward %>% residualPlots() #OBS!!! pain_cat is significantly non linear 

##Checking for Homodestacity in resuduals

pain.mod.backward %>% 	#look for straight horisontal line in the middle
  plot(which = 3)	

pain.mod.backward %>% 	#you want a non sig. P-value
  ncvTest() # 

pain.mod.backward %>% 	
  bptest() # Breush-Pagan test, 

##Checking for Homodestacity in resuduals

pain.mod.backward %>% vif() #values below 1 are "redflagged"

#### now compoaring the models. 

summary(pain.mod.theory)$adj.r.squared	
summary(pain.mod.backward)$adj.r.squared	

lm.beta(pain.mod.theory)
lm.beta(pain.mod.backward)

AIC(pain.mod.theory)	
AIC(pain.mod.backward)

confint(pain.mod.backward)
confint(pain.mod.theory)

anova(pain.mod.theory, pain.mod.backward)	

summary(pain.mod.theory)
summary(pain.mod.backward) 
pain.mod.backward

##### Her model is stronger in AIC. 

####Dataset 2###  overfitting 
#load dataset home_sample_2
##Checking the data for inconsistencies. 

pain.2 <- home_sample_2

describe(pain.2)	
summary(pain.2)	
str(pain.2)


pain.2 %>% 	
  ggplot() 	+
  aes(x = age)	+
  geom_histogram( bins = 50)	

pain.2 %>% 	
  ggplot() 	+
  aes(x = age)	+
  geom_histogram( bins = 50)	

pain.2 %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram( bins = 50)	

pain.2 %>% 	
  ggplot() +	
  aes(x = STAI_trait) +
  geom_histogram( bins = 50)

pain.2 %>% 	
  ggplot() +	
  aes(x = mindfulness) +	
  geom_histogram( bins = 50)	

pain.2 %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram( bins = 50)	

pain.2 %>% 	
  ggplot() +	
  aes(x = household_income) +	
  geom_histogram( bins = 50)	

pain.2 %>% 	
  ggplot() +	
  aes(x = IQ) +	
  geom_histogram( bins = 50)	

# scatterplot	

pain.2 %>% 	
  ggplot() +	
  aes(x = age, y = pain) +	
  geom_point()	

pain.2 %>% 	
  ggplot() +	
  aes(x = pain_cat, y = pain) +	
  geom_point()	

pain.2 %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = pain) +	
  geom_point()	

pain.2 %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain) +	
  geom_point()	

pain.2 %>% 	
  ggplot() +	
  aes(x = cortisol_serum, y = pain) +	
  geom_point()	

pain.2 %>% 	
  ggplot() +	
  aes(x = IQ, y = pain) +	
  geom_point()	

pain.2 %>% 	
  ggplot() +	
  aes(x = household_income, y = pain) +	
  geom_point()	

pain.2 %>% 	
  ggplot() +	
  aes(x = weight, y = pain) +	
  geom_point()	

# Seems reasonable. 
# Now we can compare the model performance.	

pain.testset <- home_sample_2

pred.test <- predict(pain.mod.theory, pain.testset) 
pred.test.back <- predict(pain.mod.backward, pain.testset)

RSS.test = sum((pain.testset[, "pain"] - pred.test)^2) 
RSS.test.back = sum((pain.testset[, "pain"] - pred.test.back)^2) 

RSS.test
RSS.test.back



################### THE END ##########################


