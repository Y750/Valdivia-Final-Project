###################################################################################
#                      Economic Growth and Education Quality 
#                  Why Considering Measurement Error is Important?
#                                                              Montserrat Valdivia
#                                                                  mbvaldiv@iu.edu
#                                                            Final Project Y - 750 
###################################################################################

#clean global environment
rm(list=ls())

#Call package
library(dplyr)
library(MASS)
library(ggplot2)
library(psych)
library(lavaan)
library(car)
library(semPlot)


#Call all the data
setwd("C:/Users/monts/Documents/Montse/F 2018/Y 750/Economic Growth and Education Quality/Data Analysis")
EcoEdu <- read.csv("EcoEdu.csv", header = T)


# The gdp will be transformed 
EcoEdu$lgdp00<- log(EcoEdu$gdp2000)
EcoEdu$lgdp03<- log(EcoEdu$gdp2003)
EcoEdu$lgdp06<- log(EcoEdu$gdp2006)
EcoEdu$lgdp09<- log(EcoEdu$gdp2009)
EcoEdu$lgdp12<- log(EcoEdu$gdp2012)
EcoEdu$lgdp15<- log(EcoEdu$gdp2015)

# Creating Composites of Education Quality
EcoEdu$Edu06 <- (EcoEdu$Math06 + EcoEdu$Reading06 + EcoEdu$Science06)/3
EcoEdu$Edu09 <- (EcoEdu$Math09 + EcoEdu$Reading09 + EcoEdu$Science09)/3
EcoEdu$Edu12 <- (EcoEdu$Math12 + EcoEdu$Reading12 + EcoEdu$Science12)/3
EcoEdu$Edu15 <- (EcoEdu$Math15 + EcoEdu$Reading15 + EcoEdu$Science15)/3

# Data descriptive statistics
a <-describe(EcoEdu)


#==============================================================================#
#==============================================================================#
#                   Regression without Measurement Error                       #
#==============================================================================#
#==============================================================================#

#====================================#
#     Composite Education 2006       #
#====================================#
mcomp06<- lm(lgdp06 ~ Edu06 + pol2006, EcoEdu)
summary(mcomp06)
exp(coefficients(mcomp06))
plot(EcoEdu$Edu06, EcoEdu$lgdp06 )
vif(mcomp06)
exp(confint(mcomp06, "Edu06", .95))

#====================================#
#     Composite Education 2009       #
#====================================#
mcomp09<- lm(lgdp09 ~ Edu09 + pol2009, EcoEdu)
summary(mcomp09)
exp(coefficients(mcomp09))
plot(EcoEdu$Edu09, EcoEdu$lgdp09 )
vif(mcomp09)
exp(confint(mcomp09, "Edu09", .95))

#====================================#
#     Composite Education 2012       #
#====================================#
mcomp12<- lm(lgdp12 ~ Edu12 + pol2012, EcoEdu)
summary(mcomp12)
exp(coefficients(mcomp12))
plot(EcoEdu$Edu12, EcoEdu$lgdp12 )
vif(mcomp12)
exp(confint(mcomp12, "Edu12", .95))

#====================================#
#     Composite Education 2015      #
#====================================#
mcomp15<- lm(log(gdp2015) ~ Edu15 + pol2015, EcoEdu)
summary(mcomp15)
exp(coefficients(mcomp15))
plot(EcoEdu$Edu15, EcoEdu$lgdp15 )
vif(mcomp15)
exp(confint(mcomp15, "Edu15", .95))


#==============================================================================#
#     Science Simple  Regression per year, not accounting measurement error    #
#==============================================================================#

#========================#
#     Science 2006       #
#========================#
mscie06<- lm(lgdp06 ~ Science06, EcoEdu)
summary(mscie06)
exp(coefficients(mscie06))

#========================#
#     Science 2009       #
#========================#
mscie09<- lm(lgdp09 ~ Science09, EcoEdu)
summary(mscie09)
exp(coefficients(mscie09))

#========================#
#     Science 2012       #
#========================#
mscie12<- lm(lgdp12 ~ Science12, EcoEdu)
summary(mscie12)
exp(coefficients(mscie12))
#========================#
#     Science 2015       #
#========================#
mscie15<- lm(lgdp15 ~ Science15, EcoEdu)
summary(mscie15)
exp(coefficients(mscie15))

#==============================================================================#
#         Math Regression per year, not accounting measurement error        #
#==============================================================================#
#========================#
#     Math 2003          #
#========================#
mmath03<- lm(lgdp03 ~ Math03, EcoEdu)
summary(mmath03)
exp(coefficients(mmath03))
#========================#
#     Math 2006          #
#========================#
mmath06<- lm(lgdp06 ~ Math06, EcoEdu)
summary(mmath06)
exp(coefficients(mmath06))
#========================#
#     Math 2009          #
#========================#
mmath09<- lm(lgdp09 ~ Math09, EcoEdu)
summary(mmath09)
exp(coefficients(mmath09))

#========================#
#     Math 2012          #
#========================#
mmath12<- lm(lgdp12 ~ Math12, EcoEdu)
summary(mmath12)
exp(coefficients(mmath12))
#========================#
#     Math 2015          #
#========================#
mmath15<- lm(lgdp15 ~ Math15, EcoEdu)
summary(mmath15)
exp(coefficients(mmath15))


#==============================================================================#
#         Reading Regression per year, not accounting measurement error        #
#==============================================================================#
#========================#
#     Reading 2000       #
#========================#
mrea00<- lm(lgdp00 ~ Reading00, EcoEdu)
summary(mrea00)
exp(coefficients(mrea00))
#========================#
#     Reading 2003       #
#========================#
mrea03<- lm(lgdp03 ~ Reading03, EcoEdu)
summary(mrea03)
exp(coefficients(mrea03))
#========================#
#     Reading 2006       #
#========================#
mrea06<- lm(lgdp06 ~ Reading06, EcoEdu)
summary(mrea06)
exp(coefficients(mrea06))
#========================#
#     Reading 2009       #
#========================#
mrea09<- lm(lgdp09 ~ Reading09, EcoEdu)
summary(mrea09)
exp(coefficients(mrea09))

#========================#
#     Reading 2012       #
#========================#
mrea12<- lm(lgdp12 ~ Reading12, EcoEdu)
summary(mrea12)
exp(coefficients(mrea12))
#========================#
#     Reading 2015       #
#========================#
mrea15<- lm(lgdp15 ~ Reading15, EcoEdu)
summary(mrea15)
exp(coefficients(mrea15))


#==============================================================================#
#     All factors Regression per year, not accounting measurement error        #
#==============================================================================#
#===============================#
# All predictors 2003           #
# No data about science for 2003#
#===============================#

mtot03 <- lm(lgdp03 ~ Math03+Reading03+pol2003, EcoEdu)
Anova(mtot03)
summary(mtot03)
exp(mtot03$coefficients)
vif(mtot03)
#===============================#
# All predictors 2006           #
#===============================#
mtot06 <- lm(lgdp06 ~ Math06+Reading06+Science06+pol2006, EcoEdu)
Anova(mtot06)
summary(mtot06)
exp(mtot06$coefficients)
vif(mtot06)
#===============================#
# All predictors 2009           #
#===============================#
mtot09 <- lm(lgdp09 ~ Math09+Reading09+Science09+pol2009, EcoEdu)
Anova(mtot09)
summary(mtot09)
exp(mtot09$coefficients)
vif(mtot09)
#===============================#
# All predictors 2012           #
#===============================#
mtot12 <- lm(lgdp12 ~ Math12+Reading12+Science12+pol2012, EcoEdu)
Anova(mtot12)
summary(mtot12)
exp(mtot12$coefficients)
vif(mtot12)
#Controling for other variables
mtot12a <- lm(lgdp12 ~ Math12+Reading12+Science12+pol2012, EcoEdu)
Anova(mtot12a)
summary(mtot12a)
exp(mtot12a$coefficients)
vif(mtot12a)
#===============================#
# All predictors 2015           #
#===============================#
mtot15 <- lm(lgdp15 ~ Math15+Reading15+Science15, EcoEdu)
Anova(mtot15)
summary(mtot15)
exp(mtot15$coefficients)
vif(mtot15)
#controling for other variables
mtot15a <- lm(lgdp15 ~ Math15+Reading15+Science15+pol2015, EcoEdu)
Anova(mtot15a)
summary(mtot15a)
exp(mtot15a$coefficients)
vif(mtot15a)

# Plot

semPaths(mtot15a, rotation=2, style = "lisrel")

#==============================================================================#
#     All factors Regression per year, not accounting measurement error    
#                         ASSUMPTIONS
#==============================================================================#
#===============================#
# All predictors 2003           #
# No data about science for 2003#
#===============================#

mtot03 <- lm(lgdp03 ~ Math03+Reading03+pol2003, EcoEdu)
plot(mtot03)

#===============================#
# All predictors 2006           #
#===============================#
mtot06 <- lm(lgdp06 ~ Math06+Reading06+Science06+pol2006, EcoEdu)
plot(mtot06)
#===============================#
# All predictors 2009           #
#===============================#
mtot09 <- lm(lgdp09 ~ Math09+Reading09+Science09+pol2009, EcoEdu)
plot(mtot09)
#===============================#
# All predictors 2012           #
#===============================#
mtot12a <- lm(lgdp12 ~ Math12+Reading12+Science12+pol2012, EcoEdu)
plot(mtot12a)

#===============================#
# All predictors 2012           #
#===============================#

mtot15a <- lm(lgdp15 ~ Math15+Reading15+Science15+pol2015, EcoEdu)
plot(mtot15a)

# Plot
semPaths(mtot15, rotation=2)


#==============================================================================#
#==============================================================================#
#                       ACCOUNTING FOR MEASUREMENT ERROR                       #
#==============================================================================#
#==============================================================================#

#==============================================================================#
#               Year 2003 - Accounting for measurement Error                   #
#==============================================================================#

# Obtaining variance-covariance matrix
library(dplyr)

myvars03 <- c("lgdp03",  "Math03", "Reading03", "Growth2015", 
            "CPI.score.2015", "Value")
newdata <- EcoEdu[myvars03]
str(newdata)


#==============================================================================#
#               Year 2006 - Accounting for measurement Error                   #
#==============================================================================#

myvars06 <- c("lgdp06", "Science06", "Math06", "Reading06", "gdpg2006", 
              "pol2006")
newdata06 <- EcoEdu[myvars06]
lowcor06 <- lavCor(newdata06, missing = "listwise") # lavCor uses Pearson
lowcor06
describe(newdata06)
sd.ee06 <- c(1.09, 54.58, 59.27, 57.84, 4.53, 0.87)

#ee.corr<- ' 1.000  0.733  0.742 0.766  0.110  0.872  0.292                                 
#            0.733  1.000  0.973 0.972  0.040  0.810  0.268                         
#           0.742  0.973  1.000 0.950  0.067  0.781  0.181                     
#            0.766  0.972  0.950 1.000  0.128  0.801  0.258          
#            0.110  0.040  0.067 0.128  1.000  0.047  0.013           
#            0.872  0.810  0.781 0.801  0.047  1.000  0.247     
#            0.292  0.268  0.181 0.258  0.013  0.247  1.000'


ee.names06 <- c("lgdp", "edq06S", "edq06M", "edq06R",
              "growth06", "pol06")

# Filling in *all* terms in the correlation matrix
#ee.corr06 <- getCov(lowcor06, lower=T, sds= sd.ee06, names = ee.names06)
#this comand did not work
### Help from: https://groups.google.com/forum/#!topic/lavaan/WF8gxtEKuE4

ee.corr06 <- matrix(lowcor06, nrow = 6, ncol = 6,
                  dimnames = list(c("lgdp", "edq06S", "edq06M", "edq06R",
                                    "growth06", "pol06"),
                                  c("lgdp", "edq06S", "edq06M", "edq06R",
                                    "growth06", "pol06")))

round(ee.corr06, 2)

#Covariance matrix
ee.cov06<- cor2cov(ee.corr06, sd.ee06)
round(ee.cov06, 2)

# Specifying the first model

m106 <-'EQ  =~ NA*edq06S  + edq06M  + edq06R
      EG  =~ NA*lgdp + growth06   + pol06
      EQ  =~ NA*EG
      EQ ~~ 1*EQ
      EG ~~ 1*EG'
      

# Model fit and summary
fit_m06 <- lavaan::sem(m106, sample.cov = ee.cov06, sample.nobs = 40)
summary(fit_m06, fit.measures = TRUE, standardized = TRUE, ci=T)
#modification index
mod06 <- modindices(fit_m06)
mod06[order(mod06$mi, decreasing=T),]
#respecify the model

m106a <-'EQ  =~ NA*edq06S  + edq06M  + edq06R
EG  =~ NA*lgdp + growth06   + pol06
EQ  =~ NA*EG
EQ ~~ 1*EQ
EG ~~ 1*EG
edq06M ~~ growth06'
fit_m06a <- lavaan::sem(m106a, sample.cov = ee.cov06, sample.nobs = 40)
summary(fit_m06a, fit.measures = TRUE, standardized = TRUE, ci=T)


semPaths(fit_m06a, layout = "tree", rotation = 2, style = "lisrel")


#==============================================================================#
#               Year 2009 - Accounting for measurement Error                   #
#==============================================================================#

myvars09 <- c("lgdp09", "Science09", "Math09", "Reading09", "gdpg2009", 
              "pol2009", "expend2009")
newdata09 <- EcoEdu[myvars09]
lowcor09 <- lavCor(newdata09, missing = "listwise") # lavCor uses Pearson
lowcor09
describe(newdata09)
sd.ee09 <- c(1.04, 52.08, 54.56, 49.80, 4.36, 0.83, 8.15)

ee.names09 <- c("lgdp", "edq09S", "edq09M", "edq09R",
                "growth09", "pol09", "expend09")

# Filling in *all* terms in the correlation matrix
#ee.corr09 <- getCov(lowcor09, lower=T, sds= sd.ee09, names = ee.names09)
#this comand did not work
### Help from: https://groups.google.com/forum/#!topic/lavaan/WF8gxtEKuE4

ee.corr09 <- matrix(lowcor09, nrow = 7, ncol = 7,
                    dimnames = list(c("lgdp", "edq09S", "edq09M", "edq09R",
                                      "growth09", "pol09", "expend09"),
                                    c("lgdp", "edq09S", "edq09M", "edq09R",
                                      "growth09", "pol09", "expend09")))

round(ee.corr09, 2)

#Covariance matrix
ee.cov09<- cor2cov(ee.corr09, sd.ee09)
round(ee.cov09, 2)

# Specifying the first model

m109 <-'EQ  =~ NA*edq09S  + edq09M  + edq09R
EG  =~ NA*lgdp + growth09   + pol09 
EQ  =~ NA*EG
EQ ~~ 1*EQ
EG ~~ 1*EG'

# Model fit and summary
fit_m09 <- lavaan::sem(m109, sample.cov = ee.cov09, sample.nobs = 56)
summary(fit_m09, fit.measures = TRUE, standardized = TRUE, ci=T)
#modification index
mod09 <- modindices(fit_m09)
mod09[order(mod09$mi, decreasing=T),]
#respecify the model

m109a <-'EQ  =~ NA*edq09S  + edq09M  + edq09R
EG  =~ NA*lgdp + growth09   + pol09 
EQ  =~ NA*EG
EQ ~~ 1*EQ
EG ~~ 1*EG
edq09S ~~ growth09'

fit_m09a <- lavaan::sem(m109a, sample.cov = ee.cov09, sample.nobs = 56)
summary(fit_m09a, fit.measures = TRUE, standardized = TRUE, ci=T)

semPaths(fit_m09a, layout = "tree", rotation = 2, style = "lisrel")


#==============================================================================#
#               Year 2012 - Accounting for measurement Error                   #
#==============================================================================#

myvars12 <- c("lgdp12", "Science12", "Math12", "Reading12", "gdpg2012", 
              "pol2012")
newdata12 <- EcoEdu[myvars12]
lowcor12 <- lavCor(newdata12, missing = "listwise") # lavCor uses Pearson
lowcor12
describe(newdata12)
sd.ee12 <- c(1.02, 50.58, 55.49, 47.10, 2.83, .82)
ee.names12 <- c("lgdp", "edq12S", "edq12M", "edq12R",
                "growth12", "pol12")

ee.corr12 <- matrix(lowcor12, nrow = 6, ncol = 6,
                    dimnames = list(c("lgdp", "edq12S", "edq12M", "edq12R",
                                      "growth12", "pol12"),
                                    c("lgdp", "edq12S", "edq12M", "edq12R",
                                      "growth12", "pol12")))

round(ee.corr12, 2)

#Covariance matrix
ee.cov12<- cor2cov(ee.corr12, sd.ee12)
round(ee.cov12, 2)

# Specifying the first model

m112 <-'EQ  =~ NA*edq12S  + edq12M  + edq12R
EG  =~ NA*lgdp + growth12   + pol12
EQ  =~ NA*EG
EQ ~~ 1*EQ
EG ~~ 1*EG'

# Model fit and summary
fit_m12 <- lavaan::sem(m112, sample.cov = ee.cov12, sample.nobs = 65)
summary(fit_m12, fit.measures = TRUE, standardized = TRUE, ci=T)
#modification index
mod12 <- modindices(fit_m12)
mod12[order(mod12$mi, decreasing=T),]
#respecify the model

m112a <-'EQ  =~ NA*edq12S  + edq12M  + edq12R
EG  =~ NA*lgdp + growth12   + pol12
EQ  =~ NA*EG
EQ ~~ 1*EQ
EG ~~ 1*EG
edq12S ~~  lgdp '


fit_m12a <- lavaan::sem(m112a, sample.cov = ee.cov12, sample.nobs = 65)
summary(fit_m12a, fit.measures = TRUE, standardized = TRUE, ci=T)


semPaths(fit_m12, layout = "tree", rotation = 2, style = "lisrel")

#==============================================================================#
#               Year 2015 - Accounting for measurement Error                   #
#==============================================================================#

myvars15 <- c("lgdp15", "Science15", "Math15", "Reading15", "gdpg2015", 
              "pol2015")
newdata15 <- EcoEdu[myvars15]
lowcor15 <- lavCor(newdata15, missing = "listwise") # lavCor uses Pearson
lowcor15
describe(newdata15)
sd.ee15 <- c(1.00, 49.93, 55.12, 51.09, 4.50, .78)
ee.names15 <- c("lgdp", "edq15S", "edq15M", "edq15R",
                "growth15", "pol15")


#Correlation matrix
ee.corr15 <- matrix(lowcor15, nrow = 6, ncol = 6,
                    dimnames = list(c("lgdp", "edq15S", "edq15M", "edq15R",
                                      "growth15", "pol15"),
                                    c("lgdp", "edq15S", "edq15M", "edq15R",
                                      "growth15", "pol15")))

round(ee.corr15, 2)

#Covariance matrix
ee.cov15<- cor2cov(ee.corr15, sd.ee15)
round(ee.cov15, 2)

# Specifying the first model

m115 <-'EQ  =~ NA*edq15S  + edq15M  + edq15R
EG  =~ NA*lgdp + growth15   + pol15
EQ  =~ NA*EG
EQ ~~ 1*EQ
EG ~~ 1*EG'

# Model fit and summary
fit_m15 <- lavaan::sem(m115, sample.cov = ee.cov15, sample.nobs = 70)
summary(fit_m15, fit.measures = TRUE, standardized = TRUE, ci=T)
semPaths(fit_m15, layout = "tree", rotation = 2, style = "lisrel")

#modification index
mod15 <- modindices(fit_m15)
mod15[order(mod15$mi, decreasing=T),]
#respecify the model

m115a <-'EQ  =~ NA*edq15S  + edq15M  + edq15R
EG  =~ NA*lgdp  +pol15
EQ  =~ NA*EG
EQ ~~ 1*EQ
EG ~~ 1*EG
edq15S ~~ lgdp '
fit_m15a <- lavaan::sem(m115a, sample.cov = ee.cov15, sample.nobs = 70) 

summary(fit_m15a, fit.measures = TRUE, standardized = TRUE, ci=T)

semPaths(fit_m15a, layout = "tree", rotation = 2, style = "lisrel")


m115b <-'EQ  =~ NA*edq15S  + edq15M  + edq15R
EG  =~ NA*lgdp  + growth15 +pol15
EQ  =~ NA*EG
EQ ~~ 1*EQ
EG ~~ 1*EG
edq15S ~~ lgdp '
fit_m15b <- lavaan::sem(m115b, sample.cov = ee.cov15, sample.nobs = 70) 
lavTestLRT(fit_m15a, fit_m15b)


#####################################################################################
#                                  Graphs
#####################################################################################

final = read.csv("C:/Users/monts/Documents/Montse/F 2018/Y 750/Economic Growth and Education Quality/Data Analysis/New Graphs/for ci.csv", header = T)
final$Year = as.factor(final$Year)
final = na.omit(final)
str(final)

final %>% 
  ggplot(aes(x=Mea_Error, fill=Mea_Error)) +
  geom_errorbar(aes(ymax = Upper, ymin = Lower)) +
  geom_point(aes(y=Estimate), shape=1, size=3) +
  facet_wrap(~ Year)+
  ylab("Point Estimate and Confidence Intervals") +
  xlab("Accounting for Measurement Error")+ guides(fill=FALSE)





