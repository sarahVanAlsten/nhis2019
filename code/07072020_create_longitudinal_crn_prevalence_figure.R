##################################################################
#Sarah Van Alsten
#Created: July 7, 2019
#Get Survey Frequencies of CRN by year in order to make a figure
#Last Update: July 7, 2020
##################################################################

library(survey)
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

eligible <- eligible %>%
  mutate(HyperTenDeath = ifelse(MORTHYPR == 2, 1,
                                ifelse(MORTHYPR == 1, 0, NA)))


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

# eligible <- eligible %>%
#   mutate(htMort = ifelse(DEAD == 0, 0,
#                            ifelse(is.na(MORTUCODLD) & is.na(MORTUCOD) & is.na(MORTHYPR), NA, 
#                                   ifelse((MORTHYPR == 2 | MORTUCOD == 46 | MORTDIAB == 2), 1, 0))))

eligible <- eligible %>%
  mutate(cvdMort = ifelse(DEAD == 0, 0,
                          ifelse(is.na(MORTUCODLD) & is.na(MORTUCOD), NA, 
                                 ifelse((MORTUCODLD == 1 | MORTUCODLD == 5 | (MORTUCOD >= 56  & MORTUCOD <= 75)), 1, 0))))

eligible <- eligible %>%
  mutate(cvdHtMort = ifelse(DEAD == 0, 0,
                            ifelse(is.na(MORTUCODLD) & is.na(MORTUCOD) & is.na(MORTHYPR), NA, 
                                   ifelse(MORTHYPR == 2 | MORTUCODLD == 1 | MORTUCODLD == 5 | 
                                            (MORTUCOD >= 56  & MORTUCOD <= 75), 1, 0))))



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
cvdht.mort14 <- subset(mort14.Svy, AnyCVDHT == 1)
ht.per14 <- subset(mort14.Svy, HyperTen ==1)

diab.mort10 <- subset(mort10.Svy, DiabetesRec == 1)
cvd.mort10 <- subset(mort10.Svy, AnyCVD == 1)
cvdht.mort10 <- subset(mort10.Svy, AnyCVDHT == 1)
ht.mort10 <- subset(mort10.Svy, HyperTen == 1)

diab.mort14.fin <- subset(mort14.Svy, DiabetesRec == 1 & finprob == TRUE)
cvd.mort14.fin <- subset(mort14.Svy, AnyCVD == 1 & finprob == TRUE)
cvdht.mort14.fin <- subset(mort14.Svy, AnyCVDHT == 1 & finprob == TRUE)
ht.mort14.fin <- subset(mort14.Svy, HyperTen == 1 & finprob == TRUE)

diab.mort10.fin <- subset(mort10.Svy, DiabetesRec == 1  & finprob10 == TRUE)
cvd.mort10.fin <- subset(mort10.Svy, AnyCVD == 1 & finprob10 == TRUE)
cvdht.mort10.fin <- subset(mort10.Svy, AnyCVDHT == 1 & finprob10 == TRUE)
ht.mort10.fin <- subset(mort10.Svy, HyperTen == 1 & finprob10 == TRUE)

diab.mort14.fin.sa <- subset(mort14sa.Svy, DiabetesRec == 1 & finprobsa == TRUE)
cvd.mort14.fin.sa <- subset(mort14sa.Svy, AnyCVD == 1 & finprobsa == TRUE)
cvdht.mort14.fin.sa <- subset(mort14sa.Svy, AnyCVDHT == 1 & finprobsa == TRUE)
ht.mort14.fin.sa <- subset(mort14sa.Svy, HyperTen == 1 & finprobsa == TRUE)

diab.mort10.fin.sa <- subset(mort10sa.Svy, DiabetesRec == 1  & finprobsa10 == TRUE)
cvd.mort10.fin.sa <- subset(mort10sa.Svy, AnyCVD == 1 & finprobsa10 == TRUE)
cvdht.mort10.fin.sa <- subset(mort10sa.Svy, AnyCVDHT == 1 & finprobsa10 == TRUE)
ht.mort10.fin.sa <- subset(mort10.Svy, HyperTen == 1 & finprobsa10 == TRUE)

diab.mort5.fin.sa <- subset(mort5sa.Svy, DiabetesRec == 1  & finprobsa5 == TRUE)
cvd.mort5.fin.sa <- subset(mort5sa.Svy, AnyCVD == 1 & finprobsa5 == TRUE)
cvdht.mort5.fin.sa <- subset(mort5sa.Svy, AnyCVDHT == 1 & finprobsa5 == TRUE)
ht.mort5.fin.sa <- subset(mort5sa.Svy, HyperTen == 1 & finprobsa5 == TRUE)

#No CRN
nocrn.samp14 <- subset(samp14.Svy, CRN == 0 &(DiabetesRec==1 | AnyCVDHT ==1))
nocrn.samp5 <- subset(samp5.Svy, CRN == 0 &(DiabetesRec==1 | AnyCVDHT ==1))
nocrn.per14 <- subset(per14.Svy, CRN == 0 &(DiabetesRec==1 | AnyCVDHT ==1))
############################################################################

svytotal(~interaction(factor(YEAR), BarrierMedR), samp14.Svy, na.rm =T)
svytotal(~interaction(factor(YEAR), lessMed), samp14.Svy, na.rm =T)
svytotal(~interaction(factor(YEAR), skipMed), samp14.Svy, na.rm =T)
svytotal(~interaction(factor(YEAR), delayMed), samp14.Svy, na.rm =T)

a <- read.delim("code//barriermed_tot.txt", sep = ",")
a$CRN <- c(rep(0,15), rep(1, 15))
a$YEAR <- c(2000:2014,2000:2014)
a$year_total <- lag(a$total, n = 15)
a$year_total <- a$year_total + a$total
a$percent <- a$total/a$year_total

b <- read.delim("code//lessmed_tot.txt", sep = ",")
b$CRN <- c(rep(0,4), rep(1, 4))
b$YEAR <- c(2011:2014,2011:2014)
b$year_total <- lag(b$total, n = 4)
b$year_total <- b$year_total + b$total
b$percent <- b$total/b$year_total

c <- read.delim("code//skipmed_tot.txt", sep = ",")
c$CRN <- c(rep(0,4), rep(1, 4))
c$YEAR <- c(2011:2014,2011:2014)
c$year_total <- lag(c$total, n = 4)
c$year_total <- c$year_total + c$total
c$percent <- c$total/c$year_total

d <- read.delim("code//delaymed_tot.txt", sep = ",")
d$CRN <- c(rep(0,4), rep(1, 4))
d$YEAR <- c(2011:2014,2011:2014)
d$year_total <- lag(d$total, n = 4)
d$year_total <- d$year_total + d$total
d$percent <- d$total/d$year_total

crnPrev <- rbind(
as.data.frame(svyby(~lessMed, ~YEAR, diab.samp5, svymean, na.rm =T)) %>%
  filter(YEAR > 2010) %>% mutate(CRN_type = "Less", dz = "Diabetes") %>% rename(percent = lessMed),
as.data.frame(svyby(~skipMed, ~YEAR, diab.mort14.fin.sa, svymean, na.rm =T)) %>% 
  filter(YEAR > 2010) %>% mutate(CRN_type = "Skip", dz = "Diabetes") %>% rename(percent = skipMed),
as.data.frame(svyby(~delayMed, ~YEAR, diab.mort14.fin.sa, svymean, na.rm =T))%>%
  filter(YEAR > 2010) %>% mutate(CRN_type = "Delay", dz = "Diabetes") %>% rename(percent = delayMed),
as.data.frame(svyby(~BarrierMedR, ~YEAR, diab.mort14.fin.sa, svymean, na.rm =T)) %>%
  mutate(CRN_type = "Barrier", dz = "Diabetes") %>% rename(percent = BarrierMedR),

as.data.frame(svyby(~lessMed, ~YEAR, cvd.mort14.fin.sa, svymean, na.rm =T)) %>%
  filter(YEAR > 2010) %>% mutate(CRN_type = "Less", dz = "Cardiovascular Disease") %>% rename(percent = lessMed),
as.data.frame(svyby(~skipMed, ~YEAR, cvd.mort14.fin.sa, svymean, na.rm =T)) %>% 
  filter(YEAR > 2010) %>% mutate(CRN_type = "Skip", dz = "Cardiovascular Disease") %>% rename(percent = skipMed),
as.data.frame(svyby(~delayMed, ~YEAR, cvd.mort14.fin.sa, svymean, na.rm =T))%>%
  filter(YEAR > 2010) %>% mutate(CRN_type = "Delay", dz = "Cardiovascular Disease") %>% rename(percent = delayMed),
as.data.frame(svyby(~BarrierMedR, ~YEAR, cvd.mort14.fin.sa, svymean, na.rm =T)) %>%
  mutate(CRN_type = "Barrier", dz = "Cardiovascular Disease") %>% rename(percent = BarrierMedR),

as.data.frame(svyby(~lessMed, ~YEAR, ht.mort14.fin, svymean, na.rm =T)) %>%
  filter(YEAR > 2010) %>% mutate(CRN_type = "Less", dz = "Hypertension") %>% rename(percent = lessMed),
as.data.frame(svyby(~skipMed, ~YEAR, ht.mort14.fin.sa, svymean, na.rm =T)) %>% 
  filter(YEAR > 2010) %>% mutate(CRN_type = "Skip", dz = "Hypertension") %>% rename(percent = skipMed),
as.data.frame(svyby(~delayMed, ~YEAR, ht.mort14.fin.sa, svymean, na.rm =T))%>%
  filter(YEAR > 2010) %>% mutate(CRN_type = "Delay", dz = "Hypertension") %>% rename(percent = delayMed),
as.data.frame(svyby(~BarrierMedR, ~YEAR, ht.mort14.fin.sa, svymean, na.rm =T)) %>%
  mutate(CRN_type = "Barrier", dz = "Hypertension") %>% rename(percent = BarrierMedR)
)

crnPrev %>%
  mutate(percent = percent*100,
         se = se*100) %>%
  rename(`CRN Type` = CRN_type) %>%
  ggplot(aes(x = YEAR, y = percent, group = `CRN Type`, color = `CRN Type`)) + 
  geom_path() + geom_point() + facet_grid(dz~.) + theme_bw() +
  geom_vline(xintercept = 2010, linetype = "dashed") + geom_errorbar(aes(ymin = percent + 1.96*se,
                                                                         ymax = percent - 1.96*se),
                                                                     width = .1) +
  xlab("Year") + ylab("Percent Experiencing CRN") + ylim(0, 20) 

# ###########################################################
# #get totals of people with CRN bx by year for those with DM
# write.csv(svytotal(~interaction(factor(YEAR), BarrierMedR),  diab.mort14.fin.sa, na.rm =T),
#           file = "code//diabBarrMed.csv")
# diabBarMed <- read.csv("code//diabBarrMed.csv")
# 
# write.csv(svytotal(~interaction(factor(YEAR), skipMed),  diab.mort14.fin.sa, na.rm =T),
#           file = "code//diabskipMed.csv")
# diabskipMed <- read.csv("code//diabskipMed.csv")
# 
# write.csv(svytotal(~interaction(factor(YEAR), lessMed),  diab.mort14.fin.sa, na.rm =T),
#           file = "code//diabLessMed.csv")
# diabLessMed <- read.csv("code//diabLessMed.csv")
# 
# write.csv(svytotal(~interaction(factor(YEAR), delayMed),  diab.mort14.fin.sa, na.rm =T),
#           file = "code//diabDelayMed.csv")
# diabDelayMed <- read.csv("code//diabDelayMed.csv")
# ############################################################
# #get totals for CVD
# write.csv(svytotal(~interaction(factor(YEAR), BarrierMedR),  cvd.mort14.fin.sa, na.rm =T),
#           file = "code//cvdBarrMed.csv")
# cvdBarMed <- read.csv("code//cvdBarrMed.csv")
# 
# write.csv(svytotal(~interaction(factor(YEAR), skipMed),  cvd.mort14.fin.sa, na.rm =T),
#           file = "code//cvdskipMed.csv")
# cvdskipMed <- read.csv("code//cvdskipMed.csv")
# 
# write.csv(svytotal(~interaction(factor(YEAR), lessMed),  cvd.mort14.fin.sa, na.rm =T),
#           file = "code//cvdLessMed.csv")
# cvdLessMed <- read.csv("code//cvdLessMed.csv")
# 
# write.csv(svytotal(~interaction(factor(YEAR), delayMed),  cvd.mort14.fin.sa, na.rm =T),
#           file = "code//cvdDelayMed.csv")
# cvdDelayMed <- read.csv("code//cvdDelayMed.csv")
# ############################################################
# #get totals for ht
# write.csv(svytotal(~interaction(factor(YEAR), BarrierMedR),  ht.mort14.fin.sa, na.rm =T),
#           file = "code//htBarrMed.csv")
# htBarMed <- read.csv("code//htBarrMed.csv")
# 
# write.csv(svytotal(~interaction(factor(YEAR), skipMed),  ht.mort14.fin.sa, na.rm =T),
#           file = "code//htskipMed.csv")
# htskipMed <- read.csv("code//htskipMed.csv")
# 
# write.csv(svytotal(~interaction(factor(YEAR), lessMed),  ht.mort14.fin.sa, na.rm =T),
#           file = "code//htLessMed.csv")
# htLessMed <- read.csv("code//htLessMed.csv")
# 
# write.csv(svytotal(~interaction(factor(YEAR), delayMed),  ht.mort14.fin.sa, na.rm =T),
#           file = "code//htDelayMed.csv")
# htDelayMed <- read.csv("code//htDelayMed.csv")
# 
# 
# ##################################
# #combine them all
# crnPrev <- rbind(diabBarMed, diabLessMed, diabDelayMed, diabskipMed,
#                  htBarMed, htLessMed, htDelayMed, htskipMed,
#                  cvdBarMed, cvdLessMed, cvdDelayMed, cvdskipMed)
# 
# #add column for dz
# crnPrev$dz <- c(rep("Diabetes",120), rep("Hypertension", 120), rep("Cardiovascular Disease", 120))
# 
# crnPrev <- crnPrev %>% filter(total != 0)
# 
# crnPrev <- crnPrev %>%
#   mutate(CRN_type = case_when(str_detect(X, "Barrier")  ~ "Could not afford prescriptions",
#          str_detect(X, "less") ~ "Took less medication",
#          str_detect(X, "delay") ~ "Delayed Taking Medication",
#          str_detect(X, "skip") ~ "Skipped medication doses"))
# 
# crnPrev$YEAR <- c(rep(c(2000:2014, 2000:2014, 2011:2014, 2011:2014, 2011:2014, 2011:2014, 2011:2014, 2011:2014), 3))
# crnPrev$CRN <- rep(c(rep(0,15), rep(1,15), rep(c(rep(0,4), rep(1,4)), 3)),3)
# 
# crnPrevBarr <- crnPrev %>% filter(CRN_type == "Could not afford prescriptions")
# crnPrevOth <- crnPrev %>% filter(CRN_type != "Could not afford prescriptions")
# 
# crnPrevOth <- 
#   crnPrevOth %>% group_by(dz, CRN_type) %>%
#   mutate(new_total = lag(total, 4))
# 
# crnPrevBarr <- 
#   crnPrevBarr %>% group_by(dz, CRN_type) %>%
#   mutate(new_total = lag(total, 15))
# 
# 
# crnPrevOth <- crnPrevOth %>% mutate(percent = total / (total + new_total))
# crnPrevBarr <- crnPrevBarr %>% mutate(percent = total / (total + new_total))
