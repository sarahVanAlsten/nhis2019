##################################################################
#Sarah Van Alsten
#Created: Dec 13, 2019
#Use the cleaned data from 20190928_NHIS.R to create table 1 (descriptive stats)
#Packages used: ipumsr, tidyverse, tableone, survival, survey
#Last Update: Dec 13, 2019
################################################################################
#first, create the survey design for the study, using the appropriate
#weight variables. 'PERWEIGHT' is for variables asked of everyone, whereas
#'SAMPWEIGHT' is for variables in the supplements, only asked of selected
#participants. Finally, for survival analyses, MORTWTSA is used to correct
#for eligibility issues with those not provding enough data for linkage.
#'STRATA' represents sampling stratum, 'PSU' represent primary sampling unit
#For pooled analyses, divide WT by number of included years. 
#I also will need to use subpop option because I am choosing select participants
#(eg with diabetes, CVD...)
##################################################################################
#YBARMEDS is in access to care supplement, so it should have SAMPWEIGHT.
#CRN (the 3 item responses) also asked of SAMPLE adults so should use SAMPWEIGHT
#smokev is also a sample adult so it used sampweight.
#conditions are for sample adults
#others are demographic and use PERWEIGHT

#make five survey designs: two for sampweight, one for perweight, one for
#mortality weight for 2000-2014, and one for mortality weight 2000-2010
library(survey)
library(tableone)
library(tidyverse)

#read in the data
eligible <- read.csv("data\\eligible.csv")


#the reason it's 15 and 11 is because 2000-2014 is actually 15 total cycles,
#and 2000-2010 is 11 cycles!
eligible<- eligible %>%
  mutate(sampWeight14 = SAMPWEIGHT / 15,
         sampWeight10 = SAMPWEIGHT / 11,
         sampWeight5 = SAMPWEIGHT / 5)

eligible<- eligible %>%
  mutate(perWeight14 = PERWEIGHT / 15,
         mortWeight14 = MORTWTSA / 15,
         mortWeight10 = MORTWTSA / 11,
         mortWeight5 = MORTWTSA / 5)

#sample adult weights. Need 1 for vars assesed in all years, another
#for those assessed in 2010-2014 (the more descriptive CRN items)
samp14.Svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ sampWeight14,
                        nest = TRUE, data = eligible)

samp5.Svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ sampWeight5,
                        nest = TRUE, data = eligible)

#person weights, for demographic variables
per14.Svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ perWeight14,
                       nest = TRUE, data = eligible)

#mortality weights, for COXPH
mort14.Svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight14,
                        nest = TRUE, data = eligible)

mort10.Svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight10,
                        nest = TRUE, data = eligible)

####################################################################
#per survey package guidelines, use subset() to get appropriate subpopulation estimates
diab.samp14 <- subset(samp14.Svy, DiabetesRec == 1)
diab.samp5 <- subset(samp5.Svy, DiabetesRec == 1)
cvd.samp14 <- subset(samp14.Svy, AnyCVD == 1)
cvd.samp5 <- subset(samp5.Svy, AnyCVD == 1)
cvdht.samp14 <- subset(samp14.Svy, AnyCVDHT == 1)
cvdht.samp5 <- subset(samp5.Svy, AnyCVDHT == 1)

diab.per14 <- subset(per14.Svy, DiabetesRec == 1)
cvd.per14 <- subset(per14.Svy, AnyCVD == 1)
cvdht.per14 <- subset(per14.Svy, AnyCVDHT == 1)

diab.mort14 <- subset(mort14.Svy, DiabetesRec == 1)
cvd.mort14 <- subset(mort14.Svy, AnyCVD == 1)
cvdht.mort14 <- subset(mort14.Svy, AnyCVDHT == 1)

diab.mort10 <- subset(mort10.Svy, DiabetesRec == 1)
cvd.mort10 <- subset(mort10.Svy, AnyCVD == 1)
cvdht.mort10 <- subset(mort10.Svy, AnyCVDHT == 1)
###############################################################################
#clean up environment to help things run faster
rm(eligible)
rm(per14.Svy)
rm(mort10.Svy)
rm(mort14.Svy)
rm(samp5.Svy)
rm(samp14.Svy)
######################################################################################
#now, get descriptive statistics
#FIRST, for CRN bx assessed in late years
svyCreateTableOne(vars = c("skipMed", "delayMed", "lessMed"), strata = 'CRN', data = diab.samp5, 
                  factorVars = c("skipMed", "delayMed", "lessMed"), includeNA = FALSE,
                  test = TRUE, smd = TRUE)
svyCreateTableOne(vars = c("skipMed", "delayMed", "lessMed"), strata = 'CRN', data = cvd.samp5, 
                  factorVars = c("skipMed", "delayMed", "lessMed"), includeNA = FALSE,
                  test = TRUE, smd = TRUE)
svyCreateTableOne(vars = c("skipMed", "delayMed", "lessMed"), strata = 'CRN', data = cvdht.samp5, 
                  factorVars = c("skipMed", "delayMed", "lessMed"), includeNA = FALSE,
                  test = TRUE, smd = TRUE)

#now the remaining two questions asked of sample adults
svyCreateTableOne(vars = c("BarrierMedR", "SmokeR"), strata = 'CRN', data = diab.samp14, 
                  factorVars = c("BarrierMedR", "SmokeR"), includeNA = FALSE,
                  test = TRUE, smd = TRUE)
svyCreateTableOne(vars = c("BarrierMedR", "SmokeR"), strata = 'CRN', data = cvd.samp14, 
                  factorVars = c("BarrierMedR", "SmokeR"), includeNA = FALSE,
                  test = TRUE, smd = TRUE)
svyCreateTableOne(vars = c("BarrierMedR", "SmokeR"), strata = 'CRN', data = cvdht.samp14, 
                  factorVars = c("BarrierMedR", "SmokeR"), includeNA = FALSE,
                  test = TRUE, smd = TRUE)

#now things assessed for all participants
svyCreateTableOne(vars = c("AGE", "SEX", "BMI", "REGION",
                           "RaceR", "InsType", "EduR", "IncomeR"), strata = 'CRN', data = diab.per14, 
                  factorVars = c("SEX", "REGION", "RaceR", "InsType", "EduR", "IncomeR"), includeNA = FALSE,
                  test = TRUE, smd = TRUE)
svyCreateTableOne(vars = c("AGE", "SEX", "BMI", "REGION",
                           "RaceR", "InsType", "EduR", "IncomeR"), strata = 'CRN', data = cvd.per14, 
                  factorVars = c("SEX", "REGION", "RaceR", "InsType", "EduR", "IncomeR"), includeNA = FALSE,
                  test = TRUE, smd = TRUE)
svyCreateTableOne(vars = c("AGE", "SEX", "BMI", "REGION",
                           "RaceR", "InsType", "EduR", "IncomeR"), strata = 'CRN', data = cvdht.per14, 
                  factorVars = c("SEX", "REGION", "RaceR", "InsType", "EduR", "IncomeR"), includeNA = FALSE,
                  test = TRUE, smd = TRUE)


#BMI and Age (my only 2 continuous vars) are most likely not normally distributed
#check their distributions, and change to non-normal tests as needed

ggplot(data = diab.per14$variables, aes(x = BMI, group = CRN, fill = CRN), alpha = .5)+ geom_histogram()
ggplot(data = cvd.per14$variables, aes(x = BMI, group = CRN, fill = CRN), alpha = .5)+ geom_histogram()
ggplot(data = cvdht.per14$variables, aes(x = BMI, group = CRN, fill = CRN), alpha = .5)+ geom_histogram()

#none of them are horrible, but they do all seem right skewed

ggplot(data = diab.per14$variables, aes(x = AGE, group = CRN, fill = CRN), alpha = .5)+ geom_histogram()
ggplot(data = cvd.per14$variables, aes(x = AGE, group = CRN, fill = CRN), alpha = .5)+ geom_histogram()
ggplot(data = cvdht.per14$variables, aes(x = AGE, group = CRN, fill = CRN), alpha = .5)+ geom_histogram()
#age is definitely not normal: very left skewed


bmi.diab <- svyCreateTableOne(vars = c("AGE","BMI"), strata = 'CRN', data = diab.per14,
                  argsNonNormal = c("AGE", "BMI"), includeNA = FALSE,
                  test = TRUE, smd = TRUE)
print(bmi.diab, minMax = T, nonnormal = c("AGE", "BMI"))

bmi.cvd <- svyCreateTableOne(vars = c("AGE","BMI"), strata = 'CRN', data = cvd.per14,
                              argsNonNormal = c("AGE", "BMI"), includeNA = FALSE,
                              test = TRUE, smd = TRUE)
print(bmi.cvd, minMax = T, nonnormal = c("AGE", "BMI"))

bmi.cvdht <- svyCreateTableOne(vars = c("AGE","BMI"), strata = 'CRN', data = cvdht.per14,
                             argsNonNormal = c("AGE", "BMI"), includeNA = FALSE,
                             test = TRUE, smd = TRUE)
print(bmi.cvdht, minMax = T, nonnormal = c("AGE", "BMI"))

kruskal.test(x = cvdht.per14$variables$BMI, g = cvdht.per14$variables$CRN)
kruskal.test(x = cvd.per14$variables$BMI, g = cvd.per14$variables$CRN)
kruskal.test(x = diab.per14$variables$BMI, g = diab.per14$variables$CRN)

kruskal.test(x = cvdht.per14$variables$AGE, g = cvdht.per14$variables$CRN)
kruskal.test(x = cvd.per14$variables$AGE, g = cvd.per14$variables$CRN)
kruskal.test(x = diab.per14$variables$AGE, g = diab.per14$variables$CRN)
