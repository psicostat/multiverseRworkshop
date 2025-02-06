library(sjPlot)
library(tidyverse)
library(jmv)
library(ggpubr)
library(cowplot)
library(car)
library(psych)


#Read data in

mydata <- read_csv('stats.anx.merged.cleaned.june4.v10.csv')


#Participants Section Demographics (using the N = 447 final sample)

mydata.demo <- filter(mydata, math.anx != is.na(math.anx) &
                             self.efficacy != is.na(math.anx) &
                      asi != is.na(asi) &
                      frost.com != is.na(frost.com) &
                      frost.da != is.na(frost.da) &
                      stat.anx.tc != is.na(stat.anx.tc) &
                      stat.anx.i != is.na(stat.anx.i) &
                      stat.anx.ah != is.na(stat.anx.ah) &
                      stat.anx.ws != is.na(stat.anx.ws) &
                      stat.anx.fst != is.na(stat.anx.fst) &
                      stat.anx.sc != is.na(stat.anx.sc))
                      

jmv::descriptives(mydata.demo, vars = vars(gender.category, faculty, stats, stats.history, ethnicity.r, age), 
             missing = TRUE,
             sd = TRUE,
             freq = TRUE)

#Re-coding all non-binary participants as "women" functionally making the variable "men vs. not men"
#This was to avoid excluding non-binary folks on equity/diversity grounds

mydata$gender.category.r <- mydata$gender.category

mydata$gender.category.r[mydata$id ==  12377391726] <- 1
mydata$gender.category.r[mydata$id ==  12170251464] <- 1
mydata$gender.category.r[mydata$id ==  12131230442] <- 1

table(mydata$gender.category, mydata$gender.category.r, useNA = "always")

#Alternative if just delete them from dataset

mydata$gender.category[mydata$id ==  12377391726] <- NA
mydata$gender.category[mydata$id ==  12170251464] <- NA
mydata$gender.category[mydata$id ==  12131230442] <- NA

table(mydata$gender.category, mydata$gender.category.r, useNA = "always")

#Apply listwise deletion for next section
#N = 447 after listwise deletion (from 453)

#Assess missing data

table(mydata$program.type, useNA = "always")

descriptives(mydata, vars = vars(gender.category.r, faculty, program.type,
                                 self.efficacy, asi, frost.com, frost.da, stat.anx.tc), 
             missing = TRUE,
             freq = FALSE)

descriptives(mydata, vars = vars(stat.anx.i, stat.anx.ah, stat.anx.ws, stat.anx.fst, stat.anx.sc, math.anx), 
             missing = TRUE,
             freq = FALSE)

mydata2 <- select(mydata, 
                  self.efficacy, asi, frost.com, frost.da, stat.anx.tc, 
                  stat.anx.i, stat.anx.ah, stat.anx.ws, stat.anx.fst, stat.anx.sc, math.anx,
                  gender.category.r, faculty, program.type)

mydata2 <- na.omit(mydata2)


#Exploratory Factor Analysis for short-form STARS (Table 1)

jmv::efa(data = mydata, 
         vars = vars(stat.anx.1, stat.anx.2, stat.anx.3, stat.anx.4, stat.anx.5, stat.anx.6,
                     stat.anx.7, stat.anx.8, stat.anx.9, stat.anx.10, stat.anx.11, stat.anx.12,
                     stat.anx.13, stat.anx.14, stat.anx.15, stat.anx.16),
         nFactorMethod = "parallel",
         extraction = "ml",
         rotation = "oblimin",
         hideLoadings = FALSE, 
         sortLoadings = TRUE, 
         screePlot = TRUE,
         eigen = TRUE, 
         factorCor = TRUE, 
         factorSummary = TRUE,
         modelFit = TRUE,
         kmo = TRUE, 
         bartlett = TRUE)

#Exploratory Factor Analysis Models for 1-5 factors
#Supplementary exploratory analysis requested by reviewer

jmv::efa(data = mydata, 
         vars = vars(stat.anx.1, stat.anx.2, stat.anx.3, stat.anx.4, stat.anx.5, stat.anx.6,
                     stat.anx.7, stat.anx.8, stat.anx.9, stat.anx.10, stat.anx.11, stat.anx.12,
                     stat.anx.13, stat.anx.14, stat.anx.15, stat.anx.16),
         nFactorMethod = "fixed",
         nFactors = 1,
         extraction = "ml",
         rotation = "oblimin",
         hideLoadings = FALSE, 
         sortLoadings = TRUE, 
         factorCor = TRUE, 
         factorSummary = TRUE,
         modelFit = TRUE)


jmv::efa(data = mydata, 
         vars = vars(stat.anx.1, stat.anx.2, stat.anx.3, stat.anx.4, stat.anx.5, stat.anx.6,
                     stat.anx.7, stat.anx.8, stat.anx.9, stat.anx.10, stat.anx.11, stat.anx.12,
                     stat.anx.13, stat.anx.14, stat.anx.15, stat.anx.16),
         nFactorMethod = "fixed",
         nFactors = 2,
         extraction = "ml",
         rotation = "oblimin",
         hideLoadings = FALSE, 
         sortLoadings = TRUE, 
         factorCor = TRUE, 
         factorSummary = TRUE,
         modelFit = TRUE)



jmv::efa(data = mydata, 
         vars = vars(stat.anx.1, stat.anx.2, stat.anx.3, stat.anx.4, stat.anx.5, stat.anx.6,
                     stat.anx.7, stat.anx.8, stat.anx.9, stat.anx.10, stat.anx.11, stat.anx.12,
                     stat.anx.13, stat.anx.14, stat.anx.15, stat.anx.16),
         nFactorMethod = "fixed",
         nFactors = 3,
         extraction = "ml",
         rotation = "oblimin",
         hideLoadings = FALSE, 
         sortLoadings = TRUE, 
         factorCor = TRUE, 
         factorSummary = TRUE,
         modelFit = TRUE)



jmv::efa(data = mydata, 
         vars = vars(stat.anx.1, stat.anx.2, stat.anx.3, stat.anx.4, stat.anx.5, stat.anx.6,
                     stat.anx.7, stat.anx.8, stat.anx.9, stat.anx.10, stat.anx.11, stat.anx.12,
                     stat.anx.13, stat.anx.14, stat.anx.15, stat.anx.16),
         nFactorMethod = "fixed",
         nFactors = 4,
         extraction = "ml",
         rotation = "oblimin",
         hideLoadings = FALSE, 
         sortLoadings = TRUE, 
         factorCor = TRUE, 
         factorSummary = TRUE,
         modelFit = TRUE)



jmv::efa(data = mydata, 
         vars = vars(stat.anx.1, stat.anx.2, stat.anx.3, stat.anx.4, stat.anx.5, stat.anx.6,
                     stat.anx.7, stat.anx.8, stat.anx.9, stat.anx.10, stat.anx.11, stat.anx.12,
                     stat.anx.13, stat.anx.14, stat.anx.15, stat.anx.16),
         nFactorMethod = "fixed",
         nFactors = 5,
         extraction = "ml",
         rotation = "oblimin",
         hideLoadings = FALSE, 
         sortLoadings = TRUE, 
         factorCor = TRUE, 
         factorSummary = TRUE,
         modelFit = TRUE)



#Descriptive Statistics (Table 2)

descriptives(mydata2, vars = vars(self.efficacy, asi, frost.com, frost.da, stat.anx.tc, 
                                 stat.anx.i, stat.anx.ah, stat.anx.ws, stat.anx.fst, stat.anx.sc, math.anx), 
             missing = TRUE,
             sd = TRUE,
             freq = FALSE)

#Alpha Reliability (Table 2)
#Report "raw_alpha"

self.efficacy.a <- select(mydata, se.1:se.8)
alpha(self.efficacy.a)

asi.a <- select(mydata, asi.1:asi.18)
alpha(asi.a)

frost.com.a <- select(mydata, frost.com.1:frost.com.5)
alpha(frost.com.a)

frost.da.a <- select(mydata, frost.da.1:frost.da.4)
alpha(frost.da.a)

stat.anx.tc.a <- select(mydata, stat.anx.1, stat.anx.4, stat.anx.5)
alpha(stat.anx.tc.a)

stat.anx.i.a <- select(mydata, stat.anx.3, stat.anx.6, stat.anx.8)
alpha(stat.anx.i.a)

stat.anx.ah.a <- select(mydata, stat.anx.2, stat.anx.7)
alpha(stat.anx.ah.a)

stat.anx.ws.a <- select(mydata, stat.anx.9, stat.anx.13, stat.anx.16)
alpha(stat.anx.ws.a)

stat.anx.fst.a <- select(mydata, stat.anx.10, stat.anx.14, stat.anx.15)
alpha(stat.anx.fst.a)

stat.anx.sc.a <- select(mydata, stat.anx.11, stat.anx.12)
alpha(stat.anx.sc.a)

math.anx.a <- select(mydata, math.anx.1:math.anx.9)
alpha(math.anx.a)

#Bivariate Correlations (Table 3)

corrMatrix(
  data = mydata2,
  vars = vars(self.efficacy, asi, frost.com, frost.da, stat.anx.tc, stat.anx.i, stat.anx.ah, stat.anx.ws, stat.anx.fst, stat.anx.sc, math.anx),
  ci = TRUE,
  plots = FALSE)


#Partial Correlations using Multiple Regression (Table 3)

#Change faculty to a factor variable
mydata2$faculty <- factor(mydata2$faculty)

#Self-efficacy as the predictor

m1 <- lm(data = mydata2, stat.anx.tc ~ self.efficacy + gender.category.r + faculty + program.type)
m2 <- lm(data = mydata2, stat.anx.i ~ self.efficacy + gender.category.r + faculty + program.type)
m3 <- lm(data = mydata2, stat.anx.ah ~ self.efficacy + gender.category.r + faculty + program.type)
m4 <- lm(data = mydata2, stat.anx.ws ~ self.efficacy + gender.category.r + faculty + program.type)
m5 <- lm(data = mydata2, stat.anx.fst ~ self.efficacy + gender.category.r + faculty + program.type)
m6 <- lm(data = mydata2, stat.anx.sc ~ self.efficacy + gender.category.r + faculty + program.type)
m7 <- lm(data = mydata2, math.anx ~ self.efficacy + gender.category.r + faculty + program.type)

#Save Self-Efficacy Partial Correlations

tab_model(m1, m2, m3, m4, m5, m6, m7, 
          show.std = TRUE, show.est = FALSE, digits.p = 6,
          vcov.fun = "HC",
          file = "self.efficacy.html")

#Anxiety Sensitivity as the predictor

m8 <- lm(data = mydata2, stat.anx.tc ~ asi + gender.category.r + faculty + program.type)
m9 <- lm(data = mydata2, stat.anx.i ~ asi + gender.category.r + faculty + program.type)
m10 <- lm(data = mydata2, stat.anx.ah ~ asi + gender.category.r + faculty + program.type)
m11 <- lm(data = mydata2, stat.anx.ws ~ asi + gender.category.r + faculty + program.type)
m12 <- lm(data = mydata2, stat.anx.fst ~ asi + gender.category.r + faculty + program.type)
m13 <- lm(data = mydata2, stat.anx.sc ~ asi + gender.category.r + faculty + program.type)
m14 <- lm(data = mydata2, math.anx ~ asi + gender.category.r + faculty + program.type)

#Save Anxiety Sensitivity Partial Correlations
tab_model(m8, m9, m10, m11, m12, m13, m14,
          show.std = TRUE, show.est = FALSE, digits.p = 6,
          vcov.fun = "HC",
          file = "anxiety.sensitivity.html")

#Concern Over Mistakes as the predictor

m15 <- lm(data = mydata2, stat.anx.tc ~ frost.com + gender.category.r + faculty + program.type)
m16 <- lm(data = mydata2, stat.anx.i ~ frost.com + gender.category.r + faculty + program.type)
m17 <- lm(data = mydata2, stat.anx.ah ~ frost.com + gender.category.r + faculty + program.type)
m18 <- lm(data = mydata2, stat.anx.ws ~ frost.com + gender.category.r + faculty + program.type)
m19 <- lm(data = mydata2, stat.anx.fst ~ frost.com + gender.category.r + faculty + program.type)
m20 <- lm(data = mydata2, stat.anx.sc ~ frost.com + gender.category.r + faculty + program.type)
m21 <- lm(data = mydata2, math.anx ~ frost.com + gender.category.r + faculty + program.type)

#Save Concern Over Mistakes Partial Correlations
tab_model(m15, m16, m17, m18, m19, m20, m21,
          show.std = TRUE, show.est = FALSE, digits.p = 6,
          vcov.fun = "HC",
          file = "COM.html")

#Doubts About Actions as the predictor

m22 <- lm(data = mydata2, stat.anx.tc ~ frost.da + gender.category.r + faculty + program.type)
m23 <- lm(data = mydata2, stat.anx.i ~ frost.da + gender.category.r + faculty + program.type)
m24 <- lm(data = mydata2, stat.anx.ah ~ frost.da + gender.category.r + faculty + program.type)
m25 <- lm(data = mydata2, stat.anx.ws ~ frost.da + gender.category.r + faculty + program.type)
m26 <- lm(data = mydata2, stat.anx.fst ~ frost.da + gender.category.r + faculty + program.type)
m27 <- lm(data = mydata2, stat.anx.sc ~ frost.da + gender.category.r + faculty + program.type)
m28 <- lm(data = mydata2, math.anx ~ frost.da + gender.category.r + faculty + program.type)

#Save Doubts About Actions Partial Correlations
tab_model(m22, m23, m24, m25, m26, m27, m28,
          show.std = TRUE, show.est = FALSE, digits.p = 6,
          vcov.fun = "HC",
          file = "DAA.html")

#Screening Residuals
#A variety of analyses violate the normality assumption
#This might contribute to some additional uncertainty
#Thus, in the models above we use robust estimates of standard errors

plot(m1, which = 2) #Some skew
plot(m2, which = 2)
plot(m3, which = 2) 
plot(m4, which = 2) #Some skew
plot(m5, which = 2) #Some skew
plot(m6, which = 2) #Some skew
plot(m7, which = 2)

plot(m8, which = 2) #Some skew
plot(m9, which = 2)
plot(m10, which = 2)
plot(m11, which = 2) #Some skew
plot(m12, which = 2) #Some skew
plot(m13, which = 2) #Some skew
plot(m14, which = 2)

plot(m15, which = 2) #Some skew
plot(m16, which = 2)
plot(m17, which = 2)
plot(m18, which = 2) #Some skew
plot(m19, which = 2) #Some skew
plot(m20, which = 2) #Some skew
plot(m21, which = 2)

plot(m22, which = 2) #Some skew
plot(m23, which = 2)
plot(m24, which = 2)
plot(m25, which = 2) #Some skew
plot(m26, which = 2) #Some skew
plot(m27, which = 2) #Some skew
plot(m28, which = 2)

#Screening Cooks Distance values
#Absolute value of cooks distance values all quite small, unlikely that any single points are overly influential

plot(m1, which = 4)
plot(m2, which = 4)
plot(m3, which = 4)
plot(m4, which = 4) 
plot(m5, which = 4)
plot(m6, which = 4)
plot(m7, which = 4)

plot(m8, which = 4) 
plot(m9, which = 4)
plot(m10, which = 4)
plot(m11, which = 4) 
plot(m12, which = 4)
plot(m13, which = 4)
plot(m14, which = 4)

plot(m15, which = 4) 
plot(m16, which = 4)
plot(m17, which = 4)
plot(m18, which = 4) 
plot(m19, which = 4)
plot(m20, which = 4)
plot(m21, which = 4)

plot(m22, which = 4)
plot(m23, which = 4)
plot(m24, which = 4)
plot(m25, which = 4) 
plot(m26, which = 4)
plot(m27, which = 4)
plot(m28, which = 4)