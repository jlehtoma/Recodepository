# INDEX
# ~~~~~~~~~~~~
# INPUT DATA, ADAPTATION AND TRANSFORMATION

# DATA EXPLORATION

# MODEL WITH (PROPORTIONAL) ORDINAL LOGISTIC REGRESSION

# MODEL SELECTION

# PRESENT RESULTS FROM THE BEST MODEL

# MODEL VALIDATION PART 1 (proportional odds assumption)

# GRAPHICAL INTERPRETATION OF THE MODEL

# MODEL VALIDATION PART 2 (compare with MULTINOMIAL model)


# ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
#required packages:
require(MASS)
require(reshape2)
require(ggplot2)
require(Hmisc)
require(nnet)
require(car)
require(effects)


#set the working directory to where the txt data is:
setwd("C:/HY-Data/BURGASRI/Dropbox/Red List/Data files/Birds")

####   read the data, enter the txt file name and check which decimals are in the file:
RL <- read.table(file = "RL237sp_02_12_2012.txt", header = T, dec=",")

 
str(RL)
names(RL)
# [1] "Name_lat"          "Cat2000"           "Cat2010"           "proposedCl2000"   
# [5] "Response3"         "BodyMass"          "Clutch_size"       "Clutches_per_year"
# [9] "Maximum_longevity" "Age_1st_rep"       "Adult_survival"    "MigrationEcology" 
# [13] "MigrationEcology4" "Distribution"      "ELymp"             "ELymp4"           
# [17] "ElYmpEnsiSij"      "Global_Status"     "Global_trend"      "EU_threat_status" 
# [21] "Response"          "eggs.year"         "LOGBodyMass" 

RL$Response <- factor(RL$proposedCl2000 - RL$Cat2010 + 3)
RL$eggs.year <- RL$Clutch_size * RL$Clutches_per_year

#convert Cat2000, Cat2010 and Biotope (ELymp) from numerical variables to factors:
RL$Cat2000 <- factor(RL$Cat2000)
RL$Cat2010 <- factor(RL$Cat2010)
RL$LOGBodyMass <- log(RL$BodyMass) # log transform body mass, otherwise too spread values (from 10 to 1000-2000 grams)

# data exploration:
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
     
          
# we take log transformation of Body mass.'


#########################################################################
# DATA EXPLORATION

# Table to present the data:

mytable <- table(RL$Response3,RL$Distribution,RL$MigrationEcology4) # A will be rows, B will be columns 
ftable(mytable) # print table 
summary(mytable)
# Chisq = 6, df = 6, p-value = 0.5 
plot(mytable)

#----------------------------------------
# collinearity among explanatory variables?

plot(RL$Response3~RL$MigrationEcology4 + RL$Distribution)

# ---------------------------------------------
# Plot x~y

par(mfrow=c(2,2))
plot(Maximum_longevity~Response3, data=RL)
plot(LOGBodyMass~Response3, data=RL)
plot(eggs.year~Response3, data=RL)

par(mfrow=c(1,2))
plot(Response3~ELymp4, data=RL)
plot(Response3~MigrationEcology4, data=RL)


# Grouped Bar Plot
counts <- table(RL$Response3, RL$Distribution)
barplot(counts, main="Count of species by response and distribution ",
        xlab="Distribution", 
        legend = rownames(counts), beside=TRUE)


counts <- table(RL$Response3, RL$ELymp4)
barplot(counts, main="Count of species by response and Habitat ",
        xlab="Habitat", 
        legend = rownames(counts), beside=TRUE)

counts <- table(RL$Response3, RL$MigrationEcology4)
barplot(counts, main="Count of species by response and Migration Ecology ",
        xlab="Habitat", 
        legend = rownames(counts), beside=TRUE)


require(reshape2)
require(ggplot2)

# Figure 1.  Show differences due to Distribution and Migration ecology 
ggplot(RL, aes(x = Response3, fill = Response3)) + geom_bar()   + 
  facet_grid(Distribution ~ MigrationEcology4, scales = "free", labeller = label_both) + 
  theme_grey(base_size=18) 

# Same but stacked
# ggplot(RL, aes(x = MigrationEcology4, fill = Response3)) + geom_bar(position="fill")   + 
#   facet_grid(Distribution ~ MigrationEcology4, scales = "free", labeller = label_both)
 
###########################################################################################################
###########################################################################################################
###########################################################################################################
# MODELLING PART

# OREDERED LOGISTIC REGRESSION: 

# code mostly from: 
# http://www.ats.ucla.edu/stat/r/dae/ologit.htm

# but see:
# http://books.google.fi/books?id=YH6NotdvzF0C&printsec=frontcover#v=onepage&q&f=false
# http://isites.harvard.edu/fs/docs/icb.topic749412.files/OrdinalModels_v2.pdf
# http://stats.stackexchange.com/questions/7720/how-to-understand-output-from-rs-polr-function-ordered-logistic-regression?rq=1

str(RL)
names(RL)
# [1] "Name_lat"          "Cat2000"           "Cat2010"           "proposedCl2000"    "Response3"        
# [6] "BodyMass"          "Clutch_size"       "Clutches_per_year" "Maximum_longevity" "Age_1st_rep"      
# [11] "Adult_survival"   "MigrationEcology4" "Distribution"      "ELymp"            
# [16] "ELymp4"            "ElYmpEnsiSij"      "Global_Status"     "Global_trend"      "EU_threat_status" 
# [21] "Response"          "eggs.year"         "LOGBodyMass"     


## load the MASS package
require(MASS)

## fit ordered logit model and store results
OLR.1 <- polr(Response3 ~ LOGBodyMass + Distribution + ELymp4 + MigrationEcology4, data = RL, Hess = TRUE)

## view a summary of the model
summary(OLR.1)

1-pchisq(deviance(OLR.1),df.residual(OLR.1))


#############################################################
# MODEL SELECTION. Different procedures:

#check if interactions are relevant (adds them and test):
addterm(OLR.1, ~.^2, test = "Chisq")

# Automatized AIC model selection. Back and forward term addition
OLR.2 <- stepAIC(OLR.1, ~.^2)

OLR.2$anova
anova(OLR.1, OLR.2)
 
# ELymp not significant
# include interactions of Body size with both distribution and Migration ecology 
# --> update to model OLR.2


#############################################################
# PRESENT RESULTS FROM THE BEST MODEL

OLR.2 <- polr(formula = Response3 ~ LOGBodyMass + Distribution + MigrationEcology4 + 
    LOGBodyMass:MigrationEcology4 + LOGBodyMass:Distribution, data = RL, Hess = TRUE)

## view a summary of the model
summary(OLR.2)

# check if it has a good fit:
1-pchisq(deviance(OLR.2),df.residual(OLR.2))
#5.5e-07  --> NOT quite... we have overdisperssion; violation on the assumption on the proportional odds or we are missing variables (or we have too few species...?)

## getting p-values for each predictor:

##### for OLR.2
## store table
(ctable <- coef(summary(OLR.2)))

options(digits=3)
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, `p value` = p))

#                                 Value Std. Error t value  p value
# LOGBodyMass                    -0.058      0.238  -0.243 8.08e-01
# Distributionn                   2.198      1.341   1.639 1.01e-01  <-
# Distributions                   0.492      1.131   0.435 6.64e-01
# MigrationEcology4n             -6.562      1.601  -4.099 4.15e-05  <<---
# MigrationEcology4p             -2.037      1.262  -1.613 1.07e-01  <-
# MigrationEcology4s             -2.695      1.097  -2.456 1.41e-02  <<---
# LOGBodyMass:MigrationEcology4n  1.145      0.324   3.537 4.04e-04  <<---
# LOGBodyMass:MigrationEcology4p  0.397      0.283   1.401 1.61e-01
# LOGBodyMass:MigrationEcology4s  0.495      0.234   2.118 3.41e-02  <<---
# LOGBodyMass:Distributionn      -0.535      0.263  -2.034 4.20e-02
# LOGBodyMass:Distributions      -0.261      0.217  -1.200 2.30e-01

# improve|stable                 -3.216      1.124  -2.861 4.22e-03  
# stable|worse                    0.829      1.088   0.762 4.46e-01



OLR.3 <- polr(Response3 ~ Distribution, data = RL, Hess = TRUE)

## does de model have a good fit?
1-pchisq(deviance(OLR.3),df.residual(OLR.3))
#NO... we are missing variables or we have too few species...

###############
# confidence intervals
(ci <- confint(OLR.2))  # default method gives profiled CIs

#                                 2.5 % 97.5 %
# LOGBodyMass                    -0.526  0.407
# Distributionn                  -0.411  4.863  < tends to... 
# Distributions                  -1.705  2.739
# MigrationEcology4n             -9.817 -3.475  <-
# MigrationEcology4p             -4.532  0.416
# MigrationEcology4s             -4.875 -0.567  <-
# LOGBodyMass:MigrationEcology4n  0.521  1.798  <-
# LOGBodyMass:MigrationEcology4p -0.154  0.955
# LOGBodyMass:MigrationEcology4s  0.042  0.958  <-
# LOGBodyMass:Distributionn      -1.055 -0.023
# LOGBodyMass:Distributions      -0.691  0.163

## odds ratios
#exp(coef(OLR.2))

## OR and CI
exp(cbind(OR = coef(OLR.2), ci))
#                                     OR    2.5 %  97.5 %
# LOGBodyMass                    0.94365 5.91e-01   1.502
# Distributionn                  9.01084 6.63e-01 129.375
# Distributions                  1.63537 1.82e-01  15.470
# MigrationEcology4n             0.00141 5.45e-05   0.031
# MigrationEcology4p             0.13043 1.08e-02   1.516
# MigrationEcology4s             0.06757 7.64e-03   0.567
# LOGBodyMass:MigrationEcology4n 3.14229 1.68e+00   6.036
# LOGBodyMass:MigrationEcology4p 1.48718 8.57e-01   2.599
# LOGBodyMass:MigrationEcology4s 1.63997 1.04e+00   2.606
# LOGBodyMass:Distributionn      0.58589 3.48e-01   0.977
# LOGBodyMass:Distributions      0.77058 5.01e-01   1.178


###################################
# MODEL VALIDATION 1
# testing assumption of parallel slopes (i.e. proportional odds assumption)
# i.e. The model assumes that the regression functions for different 
# response categories are parallel on the logit scale.


require(Hmisc)
sf <- function(y) {
  c(`Y>=0` = qlogis(mean(y >= 1)), `Y>=1` = qlogis(mean(y >= 2)), `Y>=2` = qlogis(mean(y >= 
    3)))
}

(s <- with(RL, summary(as.numeric(Response3) ~ LOGBodyMass + Distribution + MigrationEcology4 + 
                          LOGBodyMass:MigrationEcology4 + LOGBodyMass:Distribution, fun = sf)))

s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]

s

plot(s, which = 1:3, pch = 1:3, xlab = "logit", main = " ", xlim = c(-5, 0))

# there are some differences (not big) in the distance between the sets of coefficients (-3.42 vs. -4.5 in Distribution) 
# It may suggest that the parallel slopes assumption does not hold for the predictor distribution
# That would indicate that the effect of being a northern species versus southern (or "all Finland") species is
# different for the transition from "worse" to "stable" and "stable" to "improve".

#still I think th differences are relatively small and we should be on the safe side



##################################################
# GRAPHICAL INTERPRETATION OF THE MODEL

require(reshape2)
require(ggplot2)

# Figure 1.  Show differences due to Distribution and Migration ecology 
ggplot(RL, aes(x = Response3, fill = Response3)) + geom_bar()   + 
  facet_grid(Distribution ~ MigrationEcology4, scales = "free", labeller = label_both) + theme_grey(base_size=18) 

# figure 2:
# set values for the explanatory variables
newdat <- data.frame(Distribution = rep(c("a","n","s"), 400), MigrationEcology4 = rep(c("l","n","p","s"), each = 300), 
                     LOGBodyMass = rep(seq(from = 1.71, to = 9.31, length.out = 100), 12))

# add predicted values by the model estimates 
newdat <- cbind(newdat, predict(OLR.2, newdat, type = "probs"))

#reorganize the data to take it by ggplots
lnewdat <- melt(newdat, id.vars = c("Distribution", "MigrationEcology4", "LOGBodyMass"), variable.name = "Response3", 
                value.name = "Probability")
## view first few rows
head(lnewdat)

# Figure 2. add the body mass
ggplot(lnewdat, aes(x = LOGBodyMass, y = Probability, colour = Response3)) + geom_line(size=1) + 
  facet_grid(Distribution ~ MigrationEcology4, scales = "free", labeller = label_both) + theme_grey(base_size=18) 

# to improve in the plots:
#
# Confidence Intervals
# Lines should only be drawn within the range of the real data
# impose same scale of y axes
# order facets
# change order of colours improve<->worse


##########################################################################
# MODEL VALIDATION PART 2 (compare ORDERED vs. MULTINOMIAL model)

### Compare with a multinomial regression model:

# The OLR is a nested model within the multinomial model 
# (i.e. we can compare them using AIC):

library(nnet)
library(car)
require(effects)

# Pg 261 Fox et al. 2010
mod.multinom <- multinom(Response3 ~ LOGBodyMass + Distribution + MigrationEcology4 + 
  LOGBodyMass:MigrationEcology4 + LOGBodyMass:Distribution,
                         data=RL)
summary(mod.multinom, Wald=TRUE) 

Anova(mod.multinom)

# AIC for the multinomial model (360) i considerably smaller than for the proportional-odds model (368.69), 
# casting doubts about the assumption of proportional odds. A rough analyses of variance yelds a p value of 0.0014,
# suggesting inadequacy of the proportional-odds model.# Pg 272 in Fox et al. 2010
pchisq(deviance(OLR.2) - deviance(mod.multinom),
       df = 24- 13, lower.tail=FALSE)
# 0.00137


# GRAPHICAL PRESENTATION OF THE MODEL:
plot(allEffects(mod.multinom), ask=FALSE)
# plot(effect("LOGBodyMass*MigrationEcology4", mod.multinom))
# plot(effect("LOGBodyMass*Distribution", mod.multinom))
# plot(effect("MigrationEcology4", mod.multinom))
# plot(effect("Distribution*MigrationEcology4", mod.multinom))

# Compare with the polr model:
plot(allEffects(OLR.2), ask=FALSE)

# and same quind of plots used previously using ggplot2: 
require(reshape2)
require(ggplot2)

# set values for the explanatory variables
newdat <- data.frame(Distribution = rep(c("a","n","s"), 400), MigrationEcology4 = rep(c("l","n","p","s"), each = 300), 
                     LOGBodyMass = rep(seq(from = 1.71, to = 9.31, length.out = 100), 12))

# add predicted values by the model estimates 
newdat <- cbind(newdat, predict(mod.multinom, newdat, type = "probs"))

#reorganize the data to take it by ggplots
lnewdat <- melt(newdat, id.vars = c("Distribution", "MigrationEcology4", "LOGBodyMass"), variable.name = "Response3", 
                value.name = "Probability")
## view first few rows
head(lnewdat)

# Figure 2. add the body mass
ggplot(lnewdat, aes(x = LOGBodyMass, y = Probability, colour = Response3)) + geom_line(size=1) + 
  facet_grid(Distribution ~ MigrationEcology4, scales = "free", labeller = label_both) + theme_grey(base_size=18) 












##################################################################################################
##################################################################################################
# EXTRA stuff:
# ~~~~~~~~~~~~~~~~

--------------------------------------------------------

# probability plots from a simplified model (if no interactions considered in model selection):

# newdat <- data.frame(Distribution = rep(c("a","n","s"), 4), MigrationEcology4 = rep(c("l","n","p","s"),3))
# newdat <- cbind(newdat, predict(OLR.2, newdat, type = "probs"))
# 
# lnewdat <- melt(newdat, id.vars = c("Distribution", "MigrationEcology4"), variable.name = "Response3", 
#                 value.name = "Probability")
# ## view first few rows
# head(lnewdat)
#---------------------------------------------------------
