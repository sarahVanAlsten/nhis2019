#Sarah Van Alsten; January 12, 2020; Try and do survival analyses in dz specific models
#as competing risk versions
library(tidyverse)
library(survival)
library(survey)

eligible <- read.csv("data//eligible.csv")

eligible <- eligible %>%
  mutate(CRN = ifelse(is.na(BarrierMedR) & YEAR <=2010, NA,
                      ifelse(is.na(BarrierMedR)& is.na(skipMed) & is.na(lessMed) &is.na(delayMed) & (YEAR >=2011), NA,
                             ifelse(BarrierMedR == 0 & YEAR <=2010, 0,
                                    ifelse(BarrierMedR == 1 | skipMed == 1 | lessMed == 1 | delayMed == 1, 1, 0)))))


table(eligible$CRN, eligible$ASTATFLG, useNA = "ifany")

#the reason it's 15 and 11 is because 2000-2014 is actually 15 total cycles,
#and 2000-2010 is 11 cycles!
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
  mutate(cvdMort = ifelse(DEAD == 0, 0,
                          ifelse(is.na(MORTUCODLD) & is.na(MORTUCOD), NA, 
                                 ifelse((MORTUCODLD == 1 | MORTUCODLD == 5 | (MORTUCOD >= 56  & MORTUCOD <= 75)), 1, 0))))

eligible <- eligible %>%
  mutate(cvdHtMort = ifelse(DEAD == 0, 0,
                            ifelse(is.na(MORTUCODLD) & is.na(MORTUCOD) & is.na(MORTHYPR), NA, 
                                   ifelse(MORTHYPR == 2 | MORTUCODLD == 1 | MORTUCODLD == 5 | 
                                            (MORTUCOD >= 56  & MORTUCOD <= 75), 1, 0))))

eligible <- eligible %>%
  mutate(diabEvent = ifelse(diabMort == 1, 1,
                            ifelse(allCauseMort == 1, 2, 0)),
         cvdEvent = ifelse(cvdMort == 1, 1,
                           ifelse(allCauseMort == 1, 2, 0)),
         cvdHtEvent = ifelse(cvdHtMort == 1, 1,
                             ifelse(allCauseMort == 1, 2, 0)))

# Create Survey Design ----------------------------------------------------

mort14sa.Svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeightSA14,
                          nest = TRUE, data = eligible)

mort10sa.Svy2 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeightSA10,
                          nest = TRUE, data = eligible[eligible$YEAR <2010,])

mort5sa.Svy2 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeightSA10,
                         nest = TRUE, data = eligible[eligible$YEAR >=2010,])

#which ones have a finite probability of selection
finprobsa <- (is.finite(mort14sa.Svy$prob))
finprobsa10 <- (is.finite(mort10sa.Svy2$prob))
finprobsa5 <- (is.finite(mort5sa.Svy2$prob))
prop.table(table(finprob))
prop.table(table(finprob10))
prop.table(table(finprobsa))
prop.table(table(finprobsa10))

eligible$finprobsa <- finprobsa

eligible.mini <- eligible[eligible$YEAR < 2010,]
eligible.mini$finprob10sa <- finprobsa10

####################################################################
#per survey package guidelines, use subset() to get appropriate subpopulation estimates

diab.mort14.fin.sa <- subset(mort14sa.Svy, DiabetesRec == 1 & finprobsa == TRUE)
cvd.mort14.fin.sa <- subset(mort14sa.Svy, AnyCVD == 1 & finprobsa == TRUE)
cvdht.mort14.fin.sa <- subset(mort14sa.Svy, AnyCVDHT == 1 & finprobsa == TRUE)

diab.mort10.fin.sa <- subset(mort10sa.Svy2, DiabetesRec == 1  & finprobsa10 == TRUE)
cvd.mort10.fin.sa <- subset(mort10sa.Svy2, AnyCVD == 1 & finprobsa10 == TRUE)
cvdht.mort10.fin.sa <- subset(mort10sa.Svy2, AnyCVDHT == 1 & finprobsa10 == TRUE)

diab.mort5.fin.sa <- subset(mort5sa.Svy2, DiabetesRec == 1  & finprobsa5 == TRUE)
cvd.mort5.fin.sa <- subset(mort5sa.Svy2, AnyCVD == 1 & finprobsa5 == TRUE)
cvdht.mort5.fin.sa <- subset(mort5sa.Svy2, AnyCVDHT == 1 & finprobsa5 == TRUE)

######################################################
#the competing risk model:
#diabetes first

library(cr17)
diab.cmp <- fitSurvival(time = diab.mort14.fin.sa$variables$fuTime,
                             risk = diab.mort14.fin.sa$variables$diabEvent, 
                             group = diab.mort14.fin.sa$variables$CRN,
                             cens = 0)

SC <- diab.cmp$`1`[c("time", 
                             "n.risk", 
                             "n.event", 
                             "n.censor", 
                             "surv", 
                             "strata",
                             "std.err",
                             "lower",
                             "upper")]

SC <- as.data.frame(SC)
SC <- filter(SC, strata == "CRN")
plotSurvival(fit = diab.cmp,
             target = 1500,
             ggtheme = theme_gray(),
             legendtitle = "CRN")


coxModel <- fitCox(time = diab.mort14.fin.sa$variables$fuTime,
                   risk = diab.mort14.fin.sa$variables$diabEvent, 
                   group = diab.mort14.fin.sa$variables$CRN,
                   cens = 0, conf.int = .95)

#for diabetes death
#lower bound
exp(coxModel$`1`$coefficients[1] + 1.96*coxModel$`1`$coefficients[3])
#upper bound
exp(coxModel$`1`$coefficients[1] - 1.96*coxModel$`1`$coefficients[3])
#estimage
exp(coxModel$`1`$coefficients[1])


#for all other deaths
exp(coxModel$`2`$coefficients[1] + 1.96*coxModel$`2`$coefficients[3])
#upper bound
exp(coxModel$`2`$coefficients[1] - 1.96*coxModel$`2`$coefficients[3])
#estimage
exp(coxModel$`2`$coefficients[1])


