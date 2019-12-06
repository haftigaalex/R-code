# R-code
R-code for Zoltan's assignment




#ASSIGNMENT 1#


library(broom)
library(psych)
library(gsheet)
library(tidyverse)
library(car)
library(plyr)
library(QuantPsyc)
mydata = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv ") 

mydata_cleaned <- mydata %>%mutate(STAI_trait =replace(STAI_trait,STAI_trait == "3.5", 35))

mydata_cleaned<- mydata_cleaned %>%mutate(household_income =replace(household_income,household_income == "-4562", 4562))

mydata_cleaned$ID=NULL

summary(mydata_cleaned)

painmodel <- lm(pain ~ age + sex, data = mydata_cleaned)
summary(painmodel)   #Adjusted R-squared=0.13

imp_painmodel <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum+cortisol_saliva, data=mydata_cleaned)
summary(imp_painmodel)   #Adjusted R-squared=0.44

AIC(painmodel)        #576.34

AIC(imp_painmodel)    #511.19, the difference is greater than 2. result is significant

anova(painmodel, imp_painmodel)  #significant


par(mfrow = c(2, 2))
plot(imp_painmodel)        #Residuals vs Fitted worries me. An S shape can almost be hinted
#Check assumptions of linearity
imp_painmodel %>% residualPlots()
#Check assumptions of homoscedasticty
imp_painmodel %>% ncvTest()   
#Multicollinearity
vif(imp_painmodel)      # cortisol_saliva, serum displays high multicollinearity.
#Normality
describe(residuals(imp_painmodel))	
# skew and kurtosis<1

imp_painmodel1 <- lm(pain ~ sex+age+pain_cat+mindfulness+cortisol_serum, data=mydata_cleaned)
AIC(imp_painmodel1)     #516.75.
summary(imp_painmodel1) # Adjusted R-squared .41, a bit lower but with improvement to stability
par(mfrow = c(2, 2))
plot(imp_painmodel1)
#Multicollinearity
vif(imp_painmodel1)     #With cortisol_saliva removed, multicollinearity on cortisol_serum drops from 4.9 to 1.5. Much better value. Linearity also looks better
#Check assumptions of linearity
imp_painmodel1 %>% residualPlots()    #it would seem that the model would benefit from using higher order terms.
#Check assumptions of homoscedasticty
imp_painmodel %>% ncvTest()    #Not significant
#normality
describe(residuals(imp_painmodel1))	# skew and kurtosis<1


imp_painmodel2 <- lm(pain ~ sex+age+STAI_trait+pain_cat+mindfulness+cortisol_saliva, data=mydata_cleaned)
AIC(imp_painmodel2)     #509 SUPERIOR
summary(imp_painmodel2) # Adjusted R-squared .44 SUPERIOR
par(mfrow = c(2, 2))
plot(imp_painmodel2)     #Everything looks a lot better
#Multicollinearity
vif(imp_painmodel2)     #Fine
#Check assumptions of linearity
imp_painmodel2 %>% residualPlots()    #Fine
#Check assumptions of homoscedasticty
imp_painmodel2 %>% ncvTest()    #Fine
#normality
describe(residuals(imp_painmodel2))	# skew and kurtosis<1

#It wouldn't make sense theoretically to discard serum over saliva, since serum is considered the more valid measure.
#Superiority might be a case of overfitting.
#Returning to imp_painmodel1.

summary(imp_painmodel1)





#assignment 2################################################################################################################

library(broom)
library(psych)
library(gsheet)
library(tidyverse)
library(car)


mydata = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv ") 
mydata_cleaned <- mydata %>%mutate(STAI_trait =replace(STAI_trait,STAI_trait == "3.5", 35))

mydata_cleaned<- mydata_cleaned %>%mutate(household_income =replace(household_income,household_income == "-4562", 4562))

mydata_cleaned$ID=NULL

summary(mydata_cleaned)   #Everything looks in order now

allvar_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva + weight + household_income + IQ, data=mydata_cleaned)
summary(allvar_model)     #adjusted R-squared = 0.448 
AIC(allvar_model)      #510.74

par(mfrow = c(2, 2))
plot(allvar_model)     # The plots look ok.
vif(allvar_model)      ##Multicollinearity. Corticol serum and saliva are 4.963 and 5.648 respectively
allvar_model %>% ncvTest() #homoscedasticty. No violation
describe(residuals(allvar_model))	#Skew=-0.06. This falls within -0.5 and 0.5. Distribution is approximately symmetric
allvar_model %>% residualPlots()  #Linearity. No assumption violated

her_model <-lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+weight+IQ+household_income,data=mydata_cleaned)
summary(her_model)  #adjusted R-squared: 0.418 Insignificant

AIC(her_model)      #518.47 Insignificant

par(mfrow = c(2, 2))
plot(her_model)     # The plots look ok.
vif(her_model)      ##Multicollinearity. No violation
her_model %>% ncvTest() #Homoscedasticty. No violation
describe(residuals(her_model))	#Normality. Skew=-0.16. No violation
her_model %>% residualPlots()  #Assumption of linearity violated. p<0.032

her_model2 <-lm(pain~age+sex+pain_cat+cortisol_serum+mindfulness+weight,data=mydata_cleaned)
summary(her_model2) # Adjusted R-squared 0.418
AIC(her_model2) #515.580

par(mfrow = c(2, 2))
plot(her_model2)     # The plots look ok.
vif(her_model2)      ##Multicollinearity. No violation
her_model2 %>% ncvTest() #Homoscedasticty. No violation
describe(residuals(her_model2))	#Normality. Skew=-0.16. No violation
her_model2 %>% residualPlots()  #p<0.032 for pain_cat


#WINNER BELOW. No more significant results can be obtained by removing any more predictor

her_model2 <-lm(pain~age+sex+pain_cat+cortisol_serum+mindfulness+weight,data=mydata_cleaned)
summary(her_model2) # Adjusted R-squared 0.418
AIC(her_model2) #515.580

coef_table(her_model2)



#assignment 3 ##############################################################3

library(broom)
library(psych)
library(gsheet)
library(tidyverse)
library(car)
library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn)

# load custom functions

coef_table = function(model){
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")
  mod_sum_table["(Intercept)","Std.Beta"] = "0"
  return(mod_sum_table)
}


stdCoef.merMod <- function(object) {	                     #####standardised coefficient
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}


mydata = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_3.csv")  # First set
mydata2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_4.csv")  # Second set

summary(mydata2)

summary(mydata)

mydata_cleaned<-mydata

mydata_cleaned2<-mydata2

mydata_cleaned$ID=NULL    # For some reason, R refuses to run this line when you run the entire script. You have to individually
                          #run this line in order for it to work

mydata_cleaned2$ID=NULL

mydata_cleaned <- mydata %>% mutate(sex =droplevels(replace(sex,sex == "Female", "female")))

mydata_cleaned2<- mydata_cleaned2 %>%mutate(household_income =replace(household_income,household_income == "-23346", 23346))

mydata_cleaned2<- mydata_cleaned2 %>%mutate(household_income =replace(household_income,household_income == "-9250", 9250))

mean<-mean(mydata_cleaned$mindfulness[-which.max(mydata_cleaned$mindfulness)])

mydata_cleaned$mindfulness[which.max(mydata_cleaned$mindfulness)]= mean


summary(mydata_cleaned)
summary(mydata_cleaned2)


fixedmodel<-lm(pain~sex+age+pain_cat+mindfulness+cortisol_serum, data=mydata_cleaned)

RImodel<-lmer(pain ~ sex+age+pain_cat+mindfulness+cortisol_serum + (1|hospital),data = mydata_cleaned) # Random intercept model

summary(fixedmodel)
summary(RImodel)



coef_table(fixedmodel)
stdCoef.merMod(RImodel)

sum(residuals(fixedmodel)^2) #292.36
sum(residuals(RImodel)^2)  #246.23

plot(RImodel)

AIC(fixedmodel) #657.51
cAIC(RImodel)$caic #638.25

Fixedprediction<-predict(fixedmodel, allow.new.levels=T, newdata = mydata_cleaned2)  
RIprediction<-predict(RImodel, allow.new.levels=T, newdata = mydata_cleaned2)  
summary(Fixedprediction)
summary(RIprediction)

#  Now compute the variance explained by the model on data file 4. You can do this by using the formula we learned in class: 1-(RSS/TSS).
# Compare this R2 to the marginal and conditional R2 values computed for the model on data file 3. 

r2beta(RImodel, method = "nsj", data = mydata_cleaned)
r.squaredGLMM(RImodel)

actualpain<-mydata_cleaned2$pain


TSS = sum((actualpain - mean(actualpain))^2)
RSS = sum((actualpain - RIprediction)^2)
RIVariance<-1-(RSS/TSS)
RIVariance  # Computed variance for RI model =0.31
RSS1 = sum((actualpain - Fixedprediction)^2)
Fixedvariance<-1-(RSS1/TSS)
Fixedvariance # Computed variance for fixed model = 0.30

##################################################################################################################
RSmodel<-lmer(pain~cortisol_serum + (cortisol_serum|hospital),data=mydata_cleaned)

RSprediction <- predict(RSmodel, mydata_cleaned)

RSmodel %>%ggplot() +aes(y = pain, x = cortisol_serum,group = hospital) +geom_point(aes(color = hospital), size = 2) +geom_line(color = "black",aes(y = RSprediction, x = cortisol_serum)) +facet_wrap(~hospital, ncol = 2)

r2beta(RImodel, method = "nsj", data = mydata_cleaned) #Rsq=0.36

r.squaredGLMM(RImodel)   #Marginal Rsq=0.36, Conditional Rsq=0.45




