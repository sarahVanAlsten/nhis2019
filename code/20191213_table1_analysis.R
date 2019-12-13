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
#create a CRN measure
#recode delayed medical care due to cost, and other reasons
eligible <- eligible %>%
  mutate(DELAYCOSTR = ifelse(DELAYCOST > 2 | DELAYCOST == 0, NA, 
                             ifelse(DELAYCOST == 1, 0, 1))) %>% #care cost too much
  mutate(DELAYAPPTR = ifelse(DELAYAPPT > 2 | DELAYAPPT == 0, NA, 
                             ifelse(DELAYAPPT == 1, 0, 1))) %>% #couldn't get appt soon enough
  mutate(DELAYHRSR = ifelse(DELAYHRS > 2 | DELAYHRS == 0, NA, 
                            ifelse(DELAYHRS == 1, 0, 1))) %>% #office hours didn't work
  mutate(DELAYPHONER = ifelse(DELAYPHONE > 2 | DELAYPHONE == 0, NA, 
                              ifelse(DELAYPHONE == 1, 0, 1))) %>% #couldn't reach by phone
  mutate(DELAYTRANSR = ifelse(DELAYTRANS > 2 | DELAYTRANS == 0, NA, 
                              ifelse(DELAYTRANS == 1, 0, 1))) %>% #couldn't get transportation
  mutate(DELAYWAITR = ifelse(DELAYWAIT > 2 | DELAYWAIT == 0, NA, 
                             ifelse(DELAYWAIT == 1, 0, 1))) %>% #wait time too long
  mutate(BarrierCareR = ifelse(YBARCARE > 2 | YBARCARE == 0, NA, 
                               ifelse(YBARCARE == 1, 0, 1))) %>% #needed but couldn't afford med care
  mutate(BarrierMedR = ifelse(YBARMEDS > 2 | YBARMEDS == 0, NA, 
                              ifelse(YBARMEDS == 1, 0, 1)))%>%  #needed but couldn't afford medication
  mutate(BarrierFUR = ifelse(YBARFOLLOW > 2 | YBARFOLLOW == 0, NA, 
                             ifelse(YBARFOLLOW == 1, 0, 1)))%>%  #needed but couldn't afford followup
  mutate(BarrierSpecR = ifelse(YBARSPECL > 2 | YBARSPECL == 0, NA, 
                               ifelse(YBARSPECL == 1, 0, 1)))%>%  #needed but couldn't afford specialist
  mutate(BarrierMHR = ifelse(YBARMENTAL > 2 | YBARMENTAL == 0, NA, 
                             ifelse(YBARMENTAL == 1, 0, 1))) #needed but couldn't afford mental health care
#Behaviors to Save Money on Meds
eligible <- eligible %>%
  mutate(skipMed = ifelse(YSKIPMEDYR > 2 | YSKIPMEDYR == 0, NA,
                          ifelse(YSKIPMEDYR == 1, 0,
                                 ifelse(YSKIPMEDYR ==2, 1,NA))))%>%
  mutate(delayMed = ifelse(YDELAYMEDYR > 2| YDELAYMEDYR == 0, NA,
                           ifelse(YDELAYMEDYR == 1, 0,
                                  ifelse(YDELAYMEDYR ==2, 1,NA))))%>%
  mutate(CheapMed = ifelse(YCHEAPMEDYR > 2 | YCHEAPMEDYR == 0, NA,
                           ifelse(YCHEAPMEDYR == 1, 0,
                                  ifelse(YCHEAPMEDYR ==2, 1,NA))))%>%
  mutate(foreignMed = ifelse(YFORNMEDYR > 2 | YFORNMEDYR == 0, NA,
                             ifelse(YFORNMEDYR == 1, 0,
                                    ifelse(YFORNMEDYR ==2, 1,NA))))%>%
  mutate(alternateMed = ifelse(YALTMEDYR > 2 | YALTMEDYR == 0, NA,
                               ifelse(YALTMEDYR == 1, 0,
                                      ifelse(YALTMEDYR ==2, 1,NA))))%>%
  mutate(lessMed = ifelse(YSKIMPMEDYR > 2 | YSKIMPMEDYR == 0, NA,
                          ifelse(YSKIMPMEDYR == 1, 0,
                                 ifelse(YSKIMPMEDYR ==2, 1,NA))))


#yes if ybarmedr is as yes or any of the the 3 specific measures are a yes
eligible <- eligible %>%
  mutate(CRN = ifelse(BarrierMedR == 1 | skipMed == 1 | lessMed == 1 | delayMed == 1, 1, 
                      ifelse(is.na(BarrierMedR), NA, 0)))
table(eligible$CRN, useNA = "ifany")

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

svyCreateTableOne(vars = c("BarrierMedR", "SmokeR"), strata = 'CRN', data = diab.samp14, 
                  factorVars = c("BarrierMedR", "SmokeR"), includeNA = FALSE,
                  test = TRUE, smd = TRUE)
svyCreateTableOne(vars = c("BarrierMedR", "SmokeR"), strata = 'CRN', data = cvd.samp14, 
                  factorVars = c("BarrierMedR", "SmokeR"), includeNA = FALSE,
                  test = TRUE, smd = TRUE)
svyCreateTableOne(vars = c("BarrierMedR", "SmokeR"), strata = 'CRN', data = cvdht.samp14, 
                  factorVars = c("BarrierMedR", "SmokeR"), includeNA = FALSE,
                  test = TRUE, smd = TRUE)