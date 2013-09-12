# required packages -----------------------------------------------------------
require(MASS)
require(Hmisc)
require(car)
require(effects)
require(xlsx)

# Import and tidiy up data ------------------------------------------------

#set the working directory to where the txt data is:
setwd("C:/HY-Data/BURGASRI/Dropbox/Recodepository/data/Birds")

####   read the data, enter the txt file name and check which decimals are in the file:
# RL <- read.table(file = "C:/HY-Data/BURGASRI/Dropbox/Recodepository/data/Birds/RL237sp_120524_plain.txt", 
#                  header = T, dec=".")

RL <- read.xlsx("C:/HY-Data/BURGASRI/Dropbox/Recodepository/data/Birds/RL237sp_120524_plain.xlsx",
                1) # pick sheet n.1
str(RL)
names(RL)

# import investment ha & costs
HabInvest <- read.xlsx("C:/HY-Data/BURGASRI/Dropbox/Recodepository/data/Habitat/funding_by_habitat_preliminary.xlsx",
                "funding_by_habitat_preliminary")
names(HabInvest)


RL$BodyMass <- as.numeric(RL$BodyMass)
RL$ClutchesPerYear <- as.numeric(RL$ClutchesPerYear)
RL$ClutchSize <- as.numeric(RL$ClutchSize)
RL$EggsYear <- RL$ClutchSize * RL$ClutchesPerYear


RL$Response <- factor(RL$Backcast2000 - RL$Cat2010 + 3)


#convert Cat2000, Cat2010 and Biotope (ELymp) from numerical variables to factors:
RL$Cat2000 <- factor(RL$Cat2000)
RL$Cat2010 <- factor(RL$Cat2010)
# RL$proposedCl2000 <- factor(RL$proposedCl2000)
RL$LOGBodyMass <- log(RL$BodyMass) # log transform body mass, otherwise too spread values (from 10 to 1000-2000 grams)
RL$GameSpp <- factor(RL$GameSpp)


# Create habitat variable from the species data and recode it to match with that from the investment data 
RL$HabitatRL <- RL$HabitatFirst
# 'Mkh'->'Mk'
# 'Mlv'->'Ml'
# 'Mktv'->'Mkt'
# 'Mkv'->'Mk'

dput(levels(RL$HabitatRL)) #get the list of levels, modify this and  change it with the levels comand
levels(RL$HabitatRL)<-c("I", "Ij", "In", "Io", "Ip", "Ir", "It", "Iv", "K", 
                        "Mk", "Mk","Mkk", "Mkt", "Mkt", "Mk", "Ml", "Ml", "Mt", 
                        "Ri", "Rik","Rim", "Rin", "Rj", "Rjn", 
                        "S", "Sk", "Sn", "Sr", 
                        "Tk", "Tl", "Ts", 
                        "Va", "Vi", "Vj", "Vk", "Vs", "Vsk", "Vsr")


# Merge conservation investment to RL data
RL <- merge(RL, HabInvest, by="HabitatRL")



# Data exploration -----------------------------------------------------------------
# using the original body mass values:
par(mfrow=c(2,2))
plot(x = RL$Response3, y = RL$BodyMass, xlab="Response3",
     ylab = "BodyMass", ) 
plot(x = RL$MigrationEcology, y = RL$BodyMass, xlab="MigrationEcology",
     ylab = "BodyMass") 
plot(x = RL$Distribution, y = RL$BodyMass, xlab="Dsitribution",
     ylab = "BodyMass") 
plot(x = RL$ELymp, y = RL$BodyMass, xlab="ELymp",
     ylab = "BodyMass") 

# using the log transformed body mass values:
par(mfrow=c(2,2))
plot(x = RL$Response3, y = RL$LOGBodyMass, xlab="Response3",
     ylab = "LOGBodyMass") 
plot(x = RL$MigrationEcology, y = RL$LOGBodyMass, xlab="MigrationEcology",
     ylab = "LOGBodyMass") 
plot(x = RL$Distribution, y = RL$LOGBodyMass, xlab="Dsitribution",
     ylab = "LOGBodyMass") 
plot(x = RL$ELymp, y = RL$LOGBodyMass, xlab="ELymp",
     ylab = "LOGBodyMass") 


# --> we take log transformation of Body mass

# Modelling Workflow: --------------------------------------------------------------
# 1. we start from a basal model (only logBodyMass) and 
# 2. we test one by one which  biological trait variables have effect on the response
# 3. We add conservation investment to the model with thew significant biological traits 


# Null (basal) model - LOGBodyMass ------------------------------------------

Null <- polr(formula = Response3 ~ LOGBodyMass, data = RL, Hess = TRUE)


## view a summary of the model
summary(Null)

# check if it has a good fit:
1-pchisq(deviance(Null),df.residual(Null))
#5.5e-07  --> NOT quite... we have overdisperssion; violation on the assumption on the proportional odds or we are missing variables (or we have too few species...?)

## getting p-values for each predictor:

##### for OLR.2
## store table
(ctable <- coef(summary(Null)))

options(digits=3)
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, `p value` = p))

plot(allEffects(Null), ask=FALSE)


# M1.1 - MigrationEcology3 ---------------------------------------------------


M1.1 <- polr(formula = Response3 ~ LOGBodyMass +MigrationEcology3, data = RL, Hess = TRUE)


## view a summary of the model
summary(M1.1)

# check if it has a good fit:
1-pchisq(deviance(M1.1),df.residual(M1.1))
#5.5e-07  --> NOT quite... we have overdisperssion; violation on the assumption on the proportional odds or we are missing variables (or we have too few species...?)

# Getting p-values for each predictor:
## store table
(ctable <- coef(summary(M1.1)))

options(digits=3)
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## table with estimated parameters and plot effects -----------------------------------
(ctable <- cbind(ctable, `p value` = p))

plot(allEffects(M1.1), ask=FALSE)

## anova against null model ------------------------------------------------------------
anova(Null, M1.1)
# Model Resid. df Resid. Dev   Test    Df LR stat. Pr(Chi)
# 1                     LOGBodyMass       234        372                              
# 2 LOGBodyMass + MigrationEcology3       232        370 1 vs 2     2     2.71   0.257



# M1.2 - Distribution -------------------------------------------------------------


M1.2 <- polr(formula = Response3 ~ LOGBodyMass +Distribution, data = RL, Hess = TRUE)


## view a summary of the model
summary(M1.2)

# check if it has a good fit:
1-pchisq(deviance(M1.2),df.residual(M1.2))
#5.5e-07  --> NOT quite... we have overdisperssion; violation on the assumption on the proportional odds or we are missing variables (or we have too few species...?)

## getting p-values for each predictor:

## store table
(ctable <- coef(summary(M1.2)))

options(digits=3)
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## table with estimated parameters and plot effects  ------------------------
(ctable <- cbind(ctable, `p value` = p))

plot(allEffects(M1.2), ask=FALSE)

## anova against null model --------------------------------------------------
anova(Null, M1.2)



# M1.3 - Habitat -------------------------------------------------------------


M1.3 <- polr(formula = Response3 ~ LOGBodyMass + ELymp4, data = RL, Hess = TRUE)


## view a summary of the model
summary(M1.3)

# check if it has a good fit:
1-pchisq(deviance(M1.3),df.residual(M1.3))
#2.52e-08 --> NOT quite... we have overdisperssion; violation on the assumption on the proportional odds or we are missing variables (or we have too few species...?)

## getting p-values for each predictor:

## store table 
(ctable <- coef(summary(M1.3)))

options(digits=3)
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## table with estimated parameters and plot effects ------------------------
(ctable <- cbind(ctable, `p value` = p))

plot(allEffects(M1.3), ask=FALSE)

## anova against null model --------------------------------------------------
anova(Null, M1.3)
# Model Resid. df Resid. Dev   Test    Df LR stat. Pr(Chi)
# 1          LOGBodyMass       234        372                              
# 2 LOGBodyMass + ELymp4       231        368 1 vs 2     3     4.45   0.217

# M1.4 - MigrationEcology3 ---------------------------------------------------


M1.4 <- polr(formula = Response3 ~ LOGBodyMass + MigrationEcology3, data = RL, Hess = TRUE)


## view a summary of the model
summary(M1.4)

# check if it has a good fit:
1-pchisq(deviance(M1.4),df.residual(M1.4))
#2.52e-08 --> NOT quite... we have overdisperssion; violation on the assumption on the proportional odds or we are missing variables (or we have too few species...?)

## getting p-values for each predictor:

## store table 
(ctable <- coef(summary(M1.4)))

options(digits=3)
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## table with estimated parameters and plot effects ------------------------
(ctable <- cbind(ctable, `p value` = p))

plot(allEffects(M1.4), ask=FALSE)

## anova against null model --------------------------------------------------
anova(Null, M1.4)
# Model Resid. df Resid. Dev   Test    Df LR stat. Pr(Chi)
# 1          LOGBodyMass       234        372                              
# 2 LOGBodyMass + ELymp4       231        368 1 vs 2     3     4.45   0.217


# M1.5 - GameSpp -------------------------------


M1.5 <- polr(formula = Response3 ~ LOGBodyMass + GameSpp, data = RL, Hess = TRUE)


## view a summary of the model
summary(M1.5)

# check if it has a good fit:
1-pchisq(deviance(M1.5),df.residual(M1.5))
#2.52e-08 --> NOT quite... we have overdisperssion; violation on the assumption on the proportional odds or we are missing variables (or we have too few species...?)

## getting p-values for each predictor:

## store table 
(ctable <- coef(summary(M1.5)))

options(digits=3)
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## table with estimated parameters and plot effects ----------------------------------
(ctable <- cbind(ctable, `p value` = p))

plot(allEffects(M1.5), ask=FALSE)

## anova against null model ------------------------------------------------------------
anova(Null, M1.5)


# M1.6 - Total_Eur -------------------------------


M1.6 <- polr(formula = Response3 ~ LOGBodyMass + Total_Eur, data = RL, Hess = TRUE)


## view a summary of the model
summary(M1.6)

# check if it has a good fit:
1-pchisq(deviance(M1.6),df.residual(M1.6))
#2.52e-08 --> NOT quite... we have overdisperssion; violation on the assumption on the proportional odds or we are missing variables (or we have too few species...?)

## getting p-values for each predictor:

## store table 
(ctable <- coef(summary(M1.6)))

options(digits=3)
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## table with estimated parameters and plot effects ----------------------------------
(ctable <- cbind(ctable, `p value` = p))

plot(allEffects(M1.6), ask=FALSE)

## anova against null model ------------------------------------------------------------
anova(Null, M1.6)


#  independecy test --------------------------------------------------------------------

# Distribution and GameSpp seem to be the only ones that take a role
# but, are they independent from each other?

# 2-Way Frequency Table 
mytable <- table(RL$Distribution, RL$GameSpp) # A will be rows, B will be columns 
mytable # print table 
summary(mytable)

# Number of cases in table: 237 
# Number of factors: 2 
# Test for independence of all factors:
#   Chisq = 2.7, df = 2, p-value = 0.3
# Chi-squared approximation may be incorrect

#---> they are independent, therefore we can use them in the same model


# M2 - Distribution + GameSpp ---------------------------------------------------


M2 <- polr(formula = Response3 ~ LOGBodyMass + Distribution + GameSpp, data = RL, Hess = TRUE)


## view a summary of the model
summary(M2)

# check if it has a good fit:
1-pchisq(deviance(M2),df.residual(M2))
#5.5e-07  --> NOT quite... we have overdisperssion; violation on the assumption on the proportional odds or we are missing variables (or we have too few species...?)

## getting p-values for each predictor:

## store table 
(ctable <- coef(summary(M2)))

options(digits=3)
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## table with estimated parameters and plot effects ----------------------------------
(ctable <- cbind(ctable, `p value` = p))

par( mfrow = c( 1, 3 ) )
plot(allEffects(M2), ask=FALSE)
plot(effect("Distribution", M2))
plot(effect("GameSpp", M2))


# anova against null model ------------------------------------------------------------
anova(Null, M2)

# AIC table ----------------------------------------------------------------------------
#comparing the different models. Only neste models should be compared. 
# i.e. we cannot make comparision within M1 models but between Null-M1-M2
library(AICcmodavg)
aictab.polr(cand.set=list(Null, M1.1,M1.2,M1.3,M1.4,M1.5,M2), c("Null", "M1.1","M1.2","M1.3","M1.4","M1.5","M2"))


# Interactions???-----------------------------------------------------------------------
M3 <- polr(formula = Response3 ~ LOGBodyMass + Distribution + MigrationEcology3 +
  LOGBodyMass:MigrationEcology3 + LOGBodyMass:Distribution, data = RL, Hess = TRUE)

