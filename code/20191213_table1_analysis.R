##################################################################
#Sarah Van Alsten
#Created: Dec 13, 2019
#Use the cleaned data from 20190928_NHIS.R to create table 1 (descriptive stats)
#Packages used: ipumsr, tidyverse, tableone, survival, survey
#Last Update: July 7, 2020
################################################################################
#first, create the survey design for the study, using the appropriate
#weight variables. 'PERWEIGHT' is for variables asked of everyone, whereas
#'SAMPWEIGHT' is for variables in the supplements, only asked of selected
#participants. Finally, for survival analyses, MORTWTSA is used to correct
#for eligibility issues with those not provding enough data for linkage.
#'STRATA' represents sampling stratum, 'PSU' represent primary sampling unit
#For pooled analyses, divide WT by number of included years. 
#Choose only those with finite sampling probability, which here is equivalent to
#using subpop options for those who completed the sample adult interview, which
#included questions about chronic dz (ie if sampling prob is not finite, then that
#indicates ineligibility)
##################################################################################
#YBARMEDS is in access to care supplement, so it should have SAMPWEIGHT.
#CRN (the 3 item responses) also asked of SAMPLE adults so should use SAMPWEIGHT
#smokev is also a sample adult so it used sampweight.
#conditions are for sample adults
#others are demographic and use PERWEIGHT

#make five survey designs: two for sampweight, one for perweight, one for
#mortality weight for 2000-2014, and one for mortality weight 2000-2010
library(survey)
library(survival)
library(tableone)
library(tidyverse)


# Data Management ---------------------------------------------------------


#read in the data: output from file 20190928_NHIS.R
eligible <- read.csv("data\\eligible.csv")

#create a CRN variable
eligible <- eligible %>%
  mutate(CRN = ifelse(is.na(BarrierMedR) & YEAR <=2010, NA,
                      ifelse(is.na(BarrierMedR)& is.na(skipMed) & is.na(lessMed) &is.na(delayMed) & (YEAR >=2011), NA,
                             ifelse(BarrierMedR == 0 & YEAR <=2010, 0,
                                    ifelse(BarrierMedR == 1 | skipMed == 1 | lessMed == 1 | delayMed == 1, 1, 0)))))


table(eligible$CRN, eligible$ASTATFLG, useNA = "ifany")
xtabs(~eligible$MORTHYPR+ eligible$HyperTen +eligible$CRN)


#sum of crn bx
crnSum <- eligible %>%
  dplyr::filter(YEAR > 2010) %>%
  mutate(crnSum = lessMed + skipMed + delayMed) %>%
  summarise(sum(crnSum ==1, na.rm = T)/16297,
            sum(crnSum ==2, na.rm = T)/16297,
            sum(crnSum ==3, na.rm = T)/16297,
            sum(crnSum ==4, na.rm = T)/16297)

#the reason it's 15 and 11 is because 2000-2014 is actually 15 total cycles,
#and 2000-2010 is 11 cycles.. have to divide sampling weight by number of waves
eligible<- eligible %>%
  mutate(sampWeight14 = SAMPWEIGHT / 15,
         sampWeight10 = SAMPWEIGHT / 11,
         sampWeight5 = SAMPWEIGHT / 5)

eligible<- eligible %>%
  mutate(perWeight14 = PERWEIGHT / 15,
         mortWeight14 = MORTWT / 15,
         mortWeight10 = MORTWT / 11,
         mortWeight5 = MORTWT / 5,
         mortWeightSA14 = MORTWTSA / 15,
         mortWeightSA10 = MORTWTSA / 11,
         mortWeightSA5 = MORTWTSA / 5)

#reconstruct the allcause and dz specific mortality variables
eligible <- eligible %>%
  mutate(allCauseMort = ifelse(DEAD == 1, 1, 
                               ifelse(DEAD == 0, 0, NA)))

#died of diabetes is dm flagged as a cause(mortdiab) or leading cod listed as diabetes
#or listed cod (not necessarily leading) listed as diabetes
eligible <- eligible %>%
  mutate(diabMort = ifelse(DEAD == 0, 0,
                           ifelse(is.na(MORTUCODLD) & is.na(MORTUCOD) & is.na(MORTDIAB), NA, 
                           ifelse((MORTUCODLD == 7 | MORTUCOD == 46 | MORTDIAB == 2), 1, 0))))

eligible <- eligible %>%
   mutate(htMort = ifelse(DEAD == 0, 0,
                            ifelse(MORTHYPR == 2, 1,
                                   ifelse(MORTHYPR == 1, 0, NA))))

eligible <- eligible %>%
  mutate(cvdMort = ifelse(DEAD == 0, 0,
                          ifelse(is.na(MORTUCODLD) & is.na(MORTUCOD), NA, 
                           ifelse((MORTUCODLD == 1 | MORTUCODLD == 5 | (MORTUCOD >= 56  & MORTUCOD <= 75)), 1, 0))))

eligible <- eligible %>%
  mutate(cvdHtMort = ifelse(DEAD == 0, 0,
                          ifelse(is.na(MORTUCODLD) & is.na(MORTUCOD) & is.na(MORTHYPR), NA, 
                                 ifelse(MORTHYPR == 2 | MORTUCODLD == 1 | MORTUCODLD == 5 | 
                                          (MORTUCOD >= 56  & MORTUCOD <= 75), 1, 0))))


eligible <- eligible %>%
  mutate(yearStrat = ifelse(YEAR <= 2010, 1, 0))

eligible <- eligible %>%
  select(YEAR, yearStrat, diabMort, cvdMort, htMort, AnyCVD, HyperTen, CancerEvBin, DiabetesRec,
         fuTime, allCauseMort, cvdHtMort, AnyCVDHT, SEX, RaceR, InsType, AGE, BMI,
         SmokeR, CRN, BarrierMedR, skipMed, lessMed, delayMed, sampWeight14, sampWeight5,mortWeight5,
         mortWeight14, mortWeight10, mortWeightSA5, mortWeightSA14, mortWeightSA10, perWeight14,
         IncomeR, EduR, MORTUCOD, MORTUCODLD, MORTHYPR, MORTDIAB, REGION, PSU, STRATA)

write.csv(eligible, "data\\eligibleSubset.csv")


table(eligible$DEAD, eligible$MORTUCOD, useNA = "ifany")
table(eligible$cvdMort, useNA = "ifany")
table(eligible$cvdHtMort, useNA = "ifany")
########################################################
#IPUMS constructed a strata var to use to combine years
#2006 - 2014 is statistically independent from 2000 - 2005; the two need to be pooled
#separately

# Create Survey Design ----------------------------------------------------


#sample adult weights. Need 1 for vars assesed in all years, another
#for those assessed in 2010-2014 (the more descriptive CRN items)
samp14.Svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ sampWeight14,
                        nest = TRUE, data = eligible)

samp5.Svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ sampWeight5,
                        nest = TRUE, data = eligible[eligible$YEAR > 2010,])

#person weights, for demographic variables
per14.Svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ perWeight14,
                       nest = TRUE, data = eligible)

#mortality weights, for COXPH
mort14.Svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight14,
                        nest = TRUE, data = eligible)

mort10.Svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight10,
                        nest = TRUE, data = eligible[eligible$YEAR < 2010,])

mort14sa.Svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeightSA14,
                        nest = TRUE, data = eligible)

mort10sa.Svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeightSA10,
                        nest = TRUE, data = eligible[eligible$YEAR <2010,])

mort5sa.Svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeightSA10,
                          nest = TRUE, data = eligible[eligible$YEAR >=2010,])

#only keep the sample adults (those with a finite sampling probability. If they 
#weren't included in supplement, they have an infinite sampling probability and don't
#have data on questions we need)
finprob <- (is.finite(mort14.Svy$prob))
finprob10 <- (is.finite(mort10.Svy$prob))
finprobsa <- (is.finite(mort14sa.Svy$prob))
finprobsa10 <- (is.finite(mort10sa.Svy$prob))
finprobsa5 <- (is.finite(mort5sa.Svy$prob))

#attach it to the data frame
eligible$finprob <- finprob

#the years where specific CRN bx were asked
eligible.mini <- eligible[eligible$YEAR < 2010,]
eligible.mini$finprob10 <- finprob10

####################################################################
#per survey package guidelines, use subset() to get appropriate subpopulation estimates
diab.samp14 <- subset(samp14.Svy, DiabetesRec == 1)
diab.samp5 <- subset(samp5.Svy, DiabetesRec == 1)
cvd.samp14 <- subset(samp14.Svy, AnyCVD == 1)
cvd.samp5 <- subset(samp5.Svy, AnyCVD == 1)
#cvdht.samp14 <- subset(samp14.Svy, AnyCVDHT == 1)
#cvdht.samp5 <- subset(samp5.Svy, AnyCVDHT == 1)

#per reviewer request, add just a hypertension group
ht.samp14 <- subset(samp14.Svy, HyperTen == 1)
ht.samp5 <- subset(samp5.Svy, HyperTen == 1)

diab.per14 <- subset(per14.Svy, DiabetesRec == 1)
cvd.per14 <- subset(per14.Svy, AnyCVD == 1)
#cvdht.per14 <- subset(per14.Svy, AnyCVDHT == 1)
ht.per14 <- subset(per14.Svy, HyperTen ==1)

diab.mort14 <- subset(mort14.Svy, DiabetesRec == 1)
cvd.mort14 <- subset(mort14.Svy, AnyCVD == 1)
#cvdht.mort14 <- subset(mort14.Svy, AnyCVDHT == 1)
ht.per14 <- subset(mort14.Svy, HyperTen ==1)

diab.mort10 <- subset(mort10.Svy, DiabetesRec == 1)
cvd.mort10 <- subset(mort10.Svy, AnyCVD == 1)
#cvdht.mort10 <- subset(mort10.Svy, AnyCVDHT == 1)
ht.mort10 <- subset(mort10.Svy, HyperTen == 1)

diab.mort14.fin <- subset(mort14.Svy, DiabetesRec == 1 & finprob == TRUE)
cvd.mort14.fin <- subset(mort14.Svy, AnyCVD == 1 & finprob == TRUE)
#cvdht.mort14.fin <- subset(mort14.Svy, AnyCVDHT == 1 & finprob == TRUE)
ht.mort14.fin <- subset(mort14.Svy, HyperTen == 1 & finprob == TRUE)

diab.mort10.fin <- subset(mort10.Svy, DiabetesRec == 1  & finprob10 == TRUE)
cvd.mort10.fin <- subset(mort10.Svy, AnyCVD == 1 & finprob10 == TRUE)
#cvdht.mort10.fin <- subset(mort10.Svy, AnyCVDHT == 1 & finprob10 == TRUE)
ht.mort10.fin <- subset(mort10.Svy, HyperTen == 1 & finprob10 == TRUE)

diab.mort14.fin.sa <- subset(mort14sa.Svy, DiabetesRec == 1 & finprobsa == TRUE)
cvd.mort14.fin.sa <- subset(mort14sa.Svy, AnyCVD == 1 & finprobsa == TRUE)
#cvdht.mort14.fin.sa <- subset(mort14sa.Svy, AnyCVDHT == 1 & finprobsa == TRUE)
ht.mort14.fin.sa <- subset(mort14sa.Svy, AnyCVD == 1 & finprobsa == TRUE)

diab.mort10.fin.sa <- subset(mort10sa.Svy, DiabetesRec == 1  & finprobsa10 == TRUE)
cvd.mort10.fin.sa <- subset(mort10sa.Svy, AnyCVD == 1 & finprobsa10 == TRUE)
#cvdht.mort10.fin.sa <- subset(mort10sa.Svy, AnyCVDHT == 1 & finprobsa10 == TRUE)
ht.mort10.fin.sa <- subset(mort10.Svy, HyperTen == 1 & finprobsa10 == TRUE)

diab.mort5.fin.sa <- subset(mort5sa.Svy, DiabetesRec == 1  & finprobsa5 == TRUE)
cvd.mort5.fin.sa <- subset(mort5sa.Svy, AnyCVD == 1 & finprobsa5 == TRUE)
#cvdht.mort5.fin.sa <- subset(mort5sa.Svy, AnyCVDHT == 1 & finprobsa5 == TRUE)
ht.mort5.fin.sa <- subset(mort5sa.Svy, HyperTen == 1 & finprobsa5 == TRUE)

#No CRN
nocrn.samp14 <- subset(samp14.Svy, CRN == 0 & (DiabetesRec==1 | AnyCVDHT ==1))
nocrn.samp5 <- subset(samp5.Svy, CRN == 0 & (DiabetesRec==1 | AnyCVDHT ==1))
nocrn.per14 <- subset(per14.Svy, CRN == 0 & (DiabetesRec==1 | AnyCVDHT ==1))

# Make Table One ----------------------------------------------------------


######################################################################################
#now, get descriptive statistics
#FIRST, for CRN bx assessed in late years, and write out the files to CSVs so
#I can look at them more easily later
write.csv(
print(
  svyCreateTableOne(vars = c("skipMed", "delayMed", "lessMed"), strata = 'CRN', data = ht.samp5, 
                    factorVars = c("skipMed", "delayMed", "lessMed"), includeNA = FALSE,
                    test = TRUE, smd = TRUE), quote = FALSE, 
  noSpaces = TRUE, printToggle = FALSE),
file = "data\\htCRNtab.csv")



write.csv(
  print(
svyCreateTableOne(vars = c("skipMed", "delayMed", "lessMed"), strata = 'CRN', data = diab.samp5, 
                  factorVars = c("skipMed", "delayMed", "lessMed"), includeNA = FALSE,
                  test = TRUE, smd = TRUE), quote = FALSE,  
noSpaces = TRUE, printToggle = FALSE
),
file = "data\\diabCRNtab.csv")

write.csv(
  print(
svyCreateTableOne(vars = c("skipMed", "delayMed", "lessMed"), strata = 'CRN', data = cvd.samp5, 
                  factorVars = c("skipMed", "delayMed", "lessMed"), includeNA = FALSE,
                  test = TRUE, smd = TRUE), quote = FALSE,  
      noSpaces = TRUE, printToggle = FALSE),
file = "data\\cvdCRNtab.csv")


# write.csv(
#   print(
# svyCreateTableOne(vars = c("skipMed", "delayMed", "lessMed"), strata = 'CRN', data = cvdht.samp5, 
#                   factorVars = c("skipMed", "delayMed", "lessMed"), includeNA = FALSE,
#                   test = TRUE, smd = TRUE), quote = FALSE, 
# noSpaces = TRUE, printToggle = FALSE), file = "data\\cvdhtCRNtab.csv")

#now the remaining two questions asked of sample adults:all yrs
write.csv(
  print(
    svyCreateTableOne(vars = c("BarrierMedR", "SmokeR"), strata = 'CRN', data = ht.samp14, 
                      factorVars = c("BarrierMedR", "SmokeR"), includeNA = FALSE,
                      test = TRUE, smd = TRUE), quote = FALSE, 
    noSpaces = TRUE, printToggle = FALSE), file = "data\\htSmoke.csv")

write.csv(
  print(
svyCreateTableOne(vars = c("BarrierMedR", "SmokeR"), strata = 'CRN', data = diab.samp14, 
                  factorVars = c("BarrierMedR", "SmokeR"), includeNA = FALSE,
                  test = TRUE, smd = TRUE), quote = FALSE,  
noSpaces = TRUE, printToggle = FALSE

), file = "data\\diabSmoke.csv")
write.csv(
  print(
    svyCreateTableOne(vars = c("BarrierMedR", "SmokeR"), strata = 'CRN', data = cvd.samp14, 
                      factorVars = c("BarrierMedR", "SmokeR"), includeNA = FALSE,
                      test = TRUE, smd = TRUE), quote = FALSE,  
    noSpaces = TRUE, printToggle = FALSE
    
  ), file = "data\\cvdSmoke.csv")

# write.csv(
#   print(
#     svyCreateTableOne(vars = c("BarrierMedR", "SmokeR"), strata = 'CRN', data = cvdht.samp14, 
#                       factorVars = c("BarrierMedR", "SmokeR"), includeNA = FALSE,
#                       test = TRUE, smd = TRUE), quote = FALSE,  
#     noSpaces = TRUE, printToggle = FALSE
#     
#   ), file = "data\\cvdhtSmoke.csv")


#now things assessed for all participants: writing to CSVs
write.csv(
  print(svyCreateTableOne(vars = c("AGE", "SEX", "BMI", "REGION",
                                   "RaceR", "InsType", "EduR", "IncomeR"), strata = 'CRN', data = ht.per14, 
                          factorVars = c("SEX", "REGION", "RaceR", "InsType", "EduR", "IncomeR"), includeNA = FALSE,
                          test = TRUE, smd = TRUE), nonnormal = c("AGE", "BMI"), quote = FALSE, 
        noSpaces = TRUE, printToggle = FALSE)
  , file = "data\\htTab1.csv")

write.csv(
  print(svyCreateTableOne(vars = c("AGE", "SEX", "BMI", "REGION",
                           "RaceR", "InsType", "EduR", "IncomeR"), strata = 'CRN', data = diab.per14, 
                  factorVars = c("SEX", "REGION", "RaceR", "InsType", "EduR", "IncomeR"), includeNA = FALSE,
                  test = TRUE, smd = TRUE), nonnormal = c("AGE", "BMI"), quote = FALSE,  
      noSpaces = TRUE, printToggle = FALSE), file = "data\\diabTab1.csv")

 
write.csv( 
print(svyCreateTableOne(vars = c("AGE", "SEX", "BMI", "REGION",
                           "RaceR", "InsType", "EduR", "IncomeR"), strata = 'CRN', data = cvd.per14, 
                  factorVars = c("SEX", "REGION", "RaceR", "InsType", "EduR", "IncomeR"), includeNA = FALSE,
                  test = TRUE, smd = TRUE), nonnormal = c("AGE", "BMI"), 
      quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
, file = "data\\cvdTab1.csv")

# write.csv(
# print(svyCreateTableOne(vars = c("AGE", "SEX", "BMI", "REGION",
#                            "RaceR", "InsType", "EduR", "IncomeR"), strata = 'CRN', data = cvdht.per14, 
#                   factorVars = c("SEX", "REGION", "RaceR", "InsType", "EduR", "IncomeR"), includeNA = FALSE,
#                   test = TRUE, smd = TRUE), nonnormal = c("AGE", "BMI"), quote = FALSE,  
#       noSpaces = TRUE, printToggle = FALSE)
# , file = "data\\cvdhtTab1.csv")


write.csv(
  print(svyCreateTableOne(vars = c("AGE", "SEX", "BMI", "REGION",
                                   "RaceR", "InsType", "EduR", "IncomeR"), data = nocrn.per14, 
                          factorVars = c("SEX", "REGION", "RaceR", "InsType", "EduR", "IncomeR"),
                          includeNA = FALSE), nonnormal = c("AGE", "BMI"), quote = FALSE,  
        noSpaces = TRUE, printToggle = FALSE)
  , file = "data\\nocrnTab1.csv")


# write.csv(
#   print(svyCreateTableOne(vars = c("AGE"), strata = 'CRN', data = cvdht.per14, includeNA = FALSE,
#                           test = TRUE, smd = TRUE), nonnormal = c("AGE"))
#   , file = "data\\cvdhtTab1.csv")


#BMI and Age (my only 2 continuous vars) are most likely not normally distributed
#check their distributions, and change to non-normal tests as needed

ggplot(data = diab.per14$variables, aes(x = BMI, group = CRN, fill = CRN), alpha = .5)+ geom_histogram()
ggplot(data = cvd.per14$variables, aes(x = BMI, group = CRN, fill = CRN), alpha = .5)+ geom_histogram()
#ggplot(data = cvdht.per14$variables, aes(x = BMI, group = CRN, fill = CRN), alpha = .5)+ geom_histogram()

#none of them are horrible, but they do all seem right skewed

ggplot(data = diab.per14$variables, aes(x = AGE, group = CRN, fill = CRN), alpha = .5)+ geom_histogram()
ggplot(data = cvd.per14$variables, aes(x = AGE, group = CRN, fill = CRN), alpha = .5)+ geom_histogram()
#ggplot(data = cvdht.per14$variables, aes(x = AGE, group = CRN, fill = CRN), alpha = .5)+ geom_histogram()
#age is definitely not normal: very left skewed

#kruskal.test(x = cvdht.per14$variables$BMI, g = cvdht.per14$variables$CRN)
kruskal.test(x = cvd.per14$variables$BMI, g = cvd.per14$variables$CRN)
kruskal.test(x = diab.per14$variables$BMI, g = diab.per14$variables$CRN)

#kruskal.test(x = cvdht.per14$variables$AGE, g = cvdht.per14$variables$CRN)
kruskal.test(x = cvd.per14$variables$AGE, g = cvd.per14$variables$CRN)
kruskal.test(x = diab.per14$variables$AGE, g = diab.per14$variables$CRN)

#look at nonsurvey weighted data to see NAs
table(diab.mort14$variables$DzSpecificDiab_NoNA, diab.mort14$variables$CRN, useNA = "ifany")
table(cvd.mort14$variables$DzSpecificCVD_NoNA, cvd.mort14$variables$CRN, useNA = "ifany")
table(cvd.mort14$variables$DzSpecificCVD, cvd.mort14$variables$CRN, useNA = "ifany")
#table(cvdht.mort14$variables$DzSpecificCVDHT, cvdht.mort14$variables$CRN, useNA = "ifany")
table(cvd.mort14$variables$DzSpecificCVDHT_NoNA, cvd.mort14$variables$CRN, useNA = "ifany")
table(diab.mort14$variables$DEAD, diab.mort14$variables$CRN, useNA = "ifany")
