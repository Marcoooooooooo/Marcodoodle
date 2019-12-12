library(psych) # for describe	
library(lm.beta) # for lm.beta	
library(tidyverse) # for tidy format	
library(gridExtra) # for grid.arrange	
library(ggplot2)
library(car)
library(lmtest)
library(ppcor)



#imort dataset. Home_sample_1
#look at the data#

describe(home_sample_1)	
summary(home_sample_1)	
str(home_sample_1)

# histogram	
home_sample_1 %>% 	
  ggplot() 	+
  aes(x = age)	+
  geom_histogram( bins = 50)	

home_sample_1 %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram( bins = 50)	

home_sample_1 %>% 	
  ggplot() +	
  aes(x = STAI_trait) +
  geom_histogram( bins = 50)	# WARNING OUTLIER (measures from 20 to 80, therfore it shall be taken away)

home_sample_1 %>% 	
  ggplot() +	
  aes(x = mindfulness) +	
  geom_histogram( bins = 50)	

home_sample_1 %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram( bins = 50)	

home_sample_1 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva) +	
  geom_histogram( bins = 50)	

# scatterplot	
home_sample_1 %>% 	
  ggplot() +	
  aes(x = age, y = pain) +	
  geom_point()	

home_sample_1 %>% 	
  ggplot() +	
  aes(x = pain_cat, y = pain) +	
  geom_point()	

home_sample_1 %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = pain) +	#WARNING Outlier, (measures from 20 to 80, therfore it shall be taken away)
  geom_point()	

home_sample_1 %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain) +	
  geom_point()	

home_sample_1 %>% 	
  ggplot() +	
  aes(x = cortisol_serum, y = pain) +	
  geom_point()	

home_sample_1 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva, y = pain) +	
  geom_point()	


###  We plotted the data and found an outlier in the STA variable, witch is probably due to a wrong readning, this because the scale ranges between 20 and 80, and the reading is 3,5. You could assume that the pont margin is placed on the wrong side of the digit (5) in this case, but rather than taking the chance of this and contribute to falsification of tha data i proceded with taking away the patricipant from the analysis. 
#finding and removing the outlier

na.omit(home_sample_1)
pain.1 <- home_sample_1 [-c(18), ]

describe(pain.1)	
summary(pain.1)

# plot again#

# histogram	
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
  aes(x = cortisol_saliva) +	
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
  aes(x = cortisol_saliva, y = pain) +	
  geom_point()	

#As you can se the STI trait variable is now normally distributed and the scatterplot indicates that there is a decent liniarity betwen the variables. 
# Now, continue with the fitting of a regression model. 	

pain.mod.1 <- lm(pain ~ STAI_trait + cortisol_serum + cortisol_saliva + mindfulness + pain_cat + age + sex, data = pain.1)	
pain.mod.1
summary(pain.mod.1)

#run mod 2 after checking mod 1, for multcoliniarity. 
pain.mod.2 <- lm(pain ~ STAI_trait + cortisol_serum + mindfulness + pain_cat + age + sex, data = pain.1)	
pain.mod.2
summary(pain.mod.2)


# checking for inconsistensies in the residuals of tha data in the model. 
#Since there is a high probability of multicoliniarity between the tyo types of cortisolmeasurement we start by testing for this right away. 
##Checking for multi-coliniarity in resuduals

pain.mod.2 %>% vif() 
#When running the test of mod.1, its obvius that the cortisol measurements are to intercorrelated. When we are eliminating the cortisol saliva. and run it again. 
 
pain.mod.2 %>% vif() 

##Checking for normality in resuduals

pain.mod.2 %>% 	#Cooks distance: The graph shows an outlier with a high leverage.
  plot(which =5)	

pain.mod.2 %>% 	#Cooks distance 
  plot(which = 4)	

pain.mod.2 %>% 	#
  plot(which = 2)	

win.graph()
hist( x = residuals( pain.mod.1 ), 
      xlab = "Value of residual", 
      main = "", 
      breaks = 20)

##Checking for liniarity in resuduals
win.graph()
pain.mod.2 %>% 	#following the axis
  plot(which = 1)	

pain.mod.2 %>% residualPlots() 

##Checking for Homodestacity in resuduals
win.graph()
pain.mod.2 %>% 	#look for straight horisontal line in the middle
  plot(which = 3)	

pain.mod.2 %>% 	#you want a non sig. P-value
  ncvTest() # 

pain.mod.2 %>% 	
  bptest() # Breush-Pagan test, Again non sig. values.
#again all values indicates that the model has no problem with homodestacity#


#After running the tests they all indicates there were no significant outliers in the test. 





#############################################


#### NOW MAKING MODEL 2   #################

pain.mod.3 <- lm(pain ~ age + sex, data = pain.1)	
pain.mod.3
summary(pain.mod.3)

#COOkS DISTANCE. In this case Cooks distance is either assumtion 1, vitch gives values below 1 a gho-ahead. In assumtion 2 tjo its 4/n in this case 0,025. (in this case we are probably having problems with cooks assumtion 2 (res. över 2,5)), ) 

# checking for inconsistensies in the residuals of tha data in the model. 

##Checking for normality in resuduals
win.graph()
pain.mod.3 %>% 	#Cooks distance: The graph shows outlier with leverage, we could be experiencing a problem with this one.CONSIDER BOOTSTRAPPING
  plot(which =5)	

pain.mod.3 %>% 	#Cooks distance 
  plot(which = 4)	

pain.mod.3 %>% 	#
  plot(which = 2)	
summary(pain.1)

#check the individual outliers to see if there can be something wrong. 

win.graph()
hist( x = residuals( pain.mod.3 ),
      xlab = "Value of residual", 
      main = "", 
      breaks = 20)

##Checking for liniarity in resuduals
win.graph()
pain.mod.3 %>% 	#following the axis
  plot(which = 1)	

pain.mod.3 %>% residualPlots() #As we can se, there is no significant values witch indicates that we have no problem with liniarity. 

##Checking for Homodestacity in resuduals

pain.mod.3 %>% 	#look for straight horisontal line in the middle
  plot(which = 3)	

pain.mod.3 %>% 	#you want a non sig. P-value
  ncvTest() # 

pain.mod.3 %>% 	
  bptest() # again, a non sig value.  
#again all values indicates that the model has no problem with homodestacity#

##Checking for multi-coliniarity in resuduals

pain.mod.3 %>% vif() #values below 1 are "redflagged", again, no problkems with multicoliniarity. 



#############TIME FOR MODEL COMPARRISON###################################

pain.mod.2
pain.mod.3

summary(pain.mod.3)
summary(pain.mod.2)	

summary(pain.mod.3)$adj.r.squared	
summary(pain.mod.2)$adj.r.squared	

lm.beta(pain.mod.3)
lm.beta(pain.mod.2)

confint(pain.mod.2)
confint(pain.mod.3)

AIC(pain.mod.3)	
AIC(pain.mod.2) 


anova(pain.mod.3, pain.mod.2)	



##################   This is the end!!!   #######################





