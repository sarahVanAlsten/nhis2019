##################################################################
#Sarah Van Alsten
#Created: Dec 16, 2019
#check assumptions of coxph : proportional hazards and interactions btwn
#interview wave year see if estimates of CRN differ by wave.
#Use data/design created in 20190928_NHIS.R and 20191213_table1_analysis.R to get
#appropriate data to run models
#Packages used: survey, survminer, survival
#Last Update: Feb 1, 2019
################################################################################
library(survminer)

diab.mort14.fin.sa <- update(diab.mort14.fin.sa, yearStrat = ifelse(YEAR <= 2010, 1, 0))

diab.strat <- coxph(formula = Surv(fuTime, diabMort)~factor(CRN)*factor(yearStrat) ,
                    data = diab.mort14.fin.sa$variables)
summary(diab.strat)

diab.strat <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN)*factor(yearStrat) + factor(EduR)+ AGE +
                         factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(CancerEvBin)+
                         factor(AnyCVDHT),design = diab.mort14.fin.sa)
summary(diab.strat)


diab.strat <- coxph(formula = Surv(fuTime, allCauseMort)~factor(CRN)*factor(yearStrat) ,
                    data = diab.mort14.fin.sa$variables)
summary(diab.strat)

diab.strat <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN)*factor(yearStrat) + factor(EduR)+ AGE +
                         factor(IncomeR) + strata(SEX) + factor(InsType) + factor(RaceR) + strata(CancerEvBin)+
                         factor(AnyCVDHT),
                       design = diab.mort14.fin.sa)
summary(diab.strat)

cvd.mort14.fin.sa <- update(cvd.mort14.fin.sa, yearStrat = ifelse(YEAR <= 2010, 1, 0))
cvdht.mort14.fin.sa <- update(cvdht.mort14.fin.sa, yearStrat = ifelse(YEAR <= 2010, 1, 0))

cvd.strat <- coxph(formula = Surv(fuTime, cvdMort)~factor(CRN)*factor(yearStrat) ,
                    data = cvd.mort14.fin.sa$variables)
summary(cvd.strat)

cvdht.strat <- coxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN)*factor(yearStrat) ,
                   data = cvdht.mort14.fin.sa$variables)
summary(cvd.strat)

cvd.strat <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN)*factor(yearStrat) + factor(EduR)+ AGE +
                         factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(CancerEvBin)+
                        factor(DiabetesRec)+ factor(HyperTen),
                       design = cvd.mort14.fin.sa)
summary(cvd.strat)

cvdht.strat <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN)*factor(yearStrat) + factor(EduR)+ AGE +
                        factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(CancerEvBin)+
                        factor(DiabetesRec),
                      design = cvdht.mort14.fin.sa)
summary(cvdht.strat)



cvd.strat <- coxph(formula = Surv(fuTime, allCauseMort)~factor(CRN)*factor(yearStrat) ,
                   data = cvd.mort14.fin.sa$variables)
summary(cvd.strat)

cvdht.strat <- coxph(formula = Surv(fuTime, allCauseMort)~factor(CRN)*factor(yearStrat) ,
                   data = cvdht.mort14.fin.sa$variables)
summary(cvd.strat)

cvd.strat <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN)*factor(yearStrat) + factor(EduR)+ AGE +
                        factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(CancerEvBin)+
                        factor(DiabetesRec)+ factor(HyperTen),
                      design = cvd.mort14.fin.sa)
summary(cvd.strat)

cvdht.strat <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN)*factor(yearStrat) + factor(EduR)+ AGE +
                        factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(CancerEvBin)+
                        factor(DiabetesRec),
                      design = cvdht.mort14.fin.sa)
summary(cvdht.strat)


#############################################################
#now check the assumptions
#first: the proportional hazards assumption
#for dz specific mortality
zph.diab1 <- cox.zph(mod1.diab.sa)
zph.diab2 <- cox.zph(mod2.diab.sa, terms = F)

zph.cvd1 <- cox.zph(mod1.cvd.sa)
zph.cvd2 <- cox.zph(mod2.cvd.sa, terms = F)

zph.ht1 <- cox.zph(mod1.ht.sa)
zph.ht2 <- cox.zph(mod2.ht.sa)

zph.cvdht1 <- cox.zph(mod1.cvdht.sa)
zph.cvdht2 <- cox.zph(mod2.cvdht.sa, terms = F)
############################################################
#for all cause
zph.diab1.ac <- cox.zph(mod1.diab.allcause)
zph.diab2.ac <- cox.zph(mod2.diab.allcause)

zph.cvd1.ac <- cox.zph(mod1.cvd.allcause)
zph.cvd2.ac <- cox.zph(mod2.cvd.allcause)

zph.ht1.ac <- cox.zph(mod1.ht.allcause)
zph.ht2.ac <- cox.zph(mod2.ht.allcause)

zph.cvdht1.ac <- cox.zph(mod1.cvdht.allcause)
zph.cvdht2.ac <- cox.zph(mod2.cvdht.allcause)
##############################################################
#for the earlier years
#for dz specific mortality
zph.diab1e <- cox.zph(mod1.early.diab)
zph.diab2e <- cox.zph(mod2.early.diab)

zph.cvd1e <- cox.zph(mod1.early.cvd)
zph.cvd2e <- cox.zph(mod2.early.cvd)

zph.cvdht1e <- cox.zph(mod1.early.cvdht)
zph.cvdht2e <- cox.zph(mod2.early.cvdht)
############################################
#for all cause
zph.diab1.ace <- cox.zph(mod1.early.diab.allcause)
zph.diab2.ace <- cox.zph(mod2.early.diab.allcause)

zph.cvd1.ace <- cox.zph(mod1.early.cvd.allcause)
zph.cvd2.ace <- cox.zph(mod2.early.cvd.allcause)

zph.cvdht1.ace <- cox.zph(mod1.early.cvdht.allcause)
zph.cvdht2.ace <- cox.zph(mod2.early.cvdht.allcause)

#put them all into a table to make easier to see
#CRN proportionality only
crn.zph.frame <- data.frame(dzSpec = c(zph.diab1$table[1,3], zph.diab2$table[1,3], zph.cvd1$table[1,3], zph.cvd2$table[1,3], zph.cvdht1$table[1,3], zph.cvdht2$table[1,3]),
                        allCause = c(zph.diab1.ac$table[1,3], zph.diab2.ac$table[1,3], zph.cvd1.ac$table[1,3], zph.cvd2.ac$table[1,3], zph.cvdht1.ac$table[1,3], zph.cvdht2.ac$table[1,3]),
                        dzSpecEarly = c(zph.diab1e$table[1,3], zph.diab2e$table[1,3], zph.cvd1e$table[1,3], zph.cvd2e$table[1,3], zph.cvdht1e$table[1,3], zph.cvdht2e$table[1,3]),
                        allCauseEarly = c(zph.diab1.ace$table[1,3], zph.diab2.ace$table[1,3], zph.cvd1.ace$table[1,3], zph.cvd2.ace$table[1,3], zph.cvdht1.ace$table[1,3], zph.cvdht2.ace$table[1,3]),
                        mod = c(rep(c("Unadjusted", "Adjusted"), 6)),
                        condition = c("Diab", "Diab", "CVD", "CVD", "CVDHT", "CVDHT"))
#in adjusted models, the PH assumption is met for the CRN variable, though not usually in unadjusted models

#global prop haz for adjusted models
global.zph.frame <- data.frame(dzSpec = c(zph.diab2$table[nrow(zph.diab2$table),3],
                                          zph.cvd2$table[nrow(zph.cvd2$table),3],
                                          zph.cvdht2$table[nrow(zph.cvdht2$table),3]),
                               allCause = c(zph.diab2.ac$table[nrow(zph.diab2.ac$table),3],
                                            zph.cvd2.ac$table[nrow(zph.cvd2.ac$table),3],
                                            zph.cvdht2.ac$table[nrow(zph.cvdht2.ac$table),3]),
                               dzSpecEarly = c(zph.diab2e$table[nrow(zph.diab2e$table),3],
                                               zph.cvd2e$table[nrow(zph.cvd2e$table),3],
                                               zph.cvdht2e$table[nrow(zph.cvdht2e$table),3]),
                               allCauseEarly = c(zph.diab2.ace$table[nrow(zph.diab2.ace$table),3],
                                                 zph.cvd2.ace$table[nrow(zph.cvd2.ace$table),3],
                                                 zph.cvdht2.ace$table[nrow(zph.cvdht2.ace$table),3]),
                               condition = c("Diab", "CVD", "CVDHT"))
#none of the global PH assumptions are met

##########################################################################################
#Might be easier to just print out all the places where the assumption is violated
#only have to do so for adjusted models since unadjusted are already in crn.zph.frame

#write a function to do so
getPHViolation <- function(zp){
  zp$table <- as.data.frame(zp$table)
  names(zp$table) <- c("rho", "chisq", "p")
  zp$table[zp$table$p < 0.05, ]
}

#list of everything I want to apply it to:
zphList <- list(zph.diab2, zph.cvd2, zph.cvdht2,
                zph.diab2.ac, zph.cvd2.ac, zph.cvdht2.ac,
                zph.diab2e, zph.cvd2e, zph.cvdht2e,
                zph.diab2.ace, zph.cvd2.ace, zph.cvdht2.ace)

violatedPH <- lapply(zphList, FUN = getPHViolation)
#age, sex, income consistently violate
#education(2) violates in cvdht early. Insurance type for diabetes

#look at the schoedenfeld resid for vars in violation
#####################################################################
ggcoxzph(zph.diab2, var = "AGE", caption = "Age in DM Model") #no clear inflection pt
ggcoxzph(zph.diab2, var = "factor(SEX)", caption = "Sex in DM Model") #inflection around 190 wks
ggcoxzph(zph.diab2, var = "factor(IncomeR)4", caption= "Income Cat 4 in DM Model") #doesn't visually look bad... outliers really
ggcoxzph(zph.diab2, var = "factor(IncomeR)5", caption = "Income Cat 5 in DM Model") #same - doesn't look bad just outliers

ggcoxzph(zph.cvd2, var = "AGE", caption = "Age in CVD") #no clear inflection pt
ggcoxzph(zph.cvd2, var = "factor(CRN)1", caption = "CRN in CVD") #seems to be slightly increasing
ggcoxzph(zph.cvd2, var = "factor(SEX)2", caption = "Sex in CVD") #kind of u shaped
ggcoxzph(zph.cvd2, var = "factor(IncomeR)5", caption = "Income Cat 5 in CVD") #same - doesn't look bad just outliers
ggcoxzph(zph.cvd2, var = "factor(InsType)3", caption = "Insurance 3 in CVD") #3 strata

ggcoxzph(zph.cvdht2, var = "AGE", caption = "Age in CVDHT") #no clear inflection pt
ggcoxzph(zph.cvdht2, var = "factor(EduR)2", caption = "Edu2 in CVDHT") #seems to be slightly increasing
ggcoxzph(zph.cvdht2, var = "factor(SEX)2", caption = "Sex cvdht") #inflection around 190 wks
ggcoxzph(zph.cvdht2, var = "factor(IncomeR)5", caption = "income5 cvdht") #same - doesn't look bad just outliers
ggcoxzph(zph.cvdht2, var = "factor(IncomeR)4", caption = "income 4 cvdht") #not bad just outliers
#################################################################################
ggcoxzph(zph.diab2.ac, var = "AGE", caption = "Age DM ac") #no clear inflection pt
ggcoxzph(zph.diab2.ac, var = "factor(InsType)1", caption = "ins1 DM ac") #slight downward slope
ggcoxzph(zph.diab2.ac, var = "factor(InsType)2", caption = "ins2 DM ac") #fairly flat
ggcoxzph(zph.diab2.ac, var = "factor(InsType)4",  caption = "ins4 DM ac") #faily flat
ggcoxzph(zph.diab2.ac, var = "factor(SEX)2", caption = "sex DM ac") #increasing a bit
ggcoxzph(zph.diab2.ac, var = "factor(IncomeR)1", caption = "inc1 DM ac") #doesn't look bad just outliers
ggcoxzph(zph.diab2.ac, var = "factor(IncomeR)2", caption = "inc2 DM ac") #slight increase
ggcoxzph(zph.diab2.ac, var = "factor(IncomeR)4", caption = "inc4 DM ac") #not bad just outliers

ggcoxzph(zph.cvd2.ac, var = "AGE") #not bad
ggcoxzph(zph.cvd2.ac, var = "factor(SEX)2") #not bad
ggcoxzph(zph.cvd2.ac, var = "factor(IncomeR)5") #same - doesn't look bad just outliers
ggcoxzph(zph.cvd2.ac, var = "factor(IncomeR)4") #not bad just outliers

ggcoxzph(zph.cvdht2.ac, var = "AGE") #none of these are bad
ggcoxzph(zph.cvdht2.ac, var = "factor(InsType)3") #
ggcoxzph(zph.cvdht2.ac, var = "factor(IncomeR)3") #
ggcoxzph(zph.cvdht2.ac, var = "factor(SEX)2") #
ggcoxzph(zph.cvdht2.ac, var = "factor(IncomeR)5") #slightly increasing
ggcoxzph(zph.cvdht2.ac, var = "factor(IncomeR)4") #not bad just outliers
###########################################################################
#check for the earlier years
ggcoxzph(zph.diab2e, var = "AGE") #no clear inflection pt - somewhat downward sloping
ggcoxzph(zph.diab2e, var = "factor(SEX)2") #increasing before 190 wks

ggcoxzph(zph.cvd2e, var = "AGE") #rather U shaped. still inflection at 190wks
ggcoxzph(zph.cvd2e, var = "factor(IncomeR)5") #just outliers
ggcoxzph(zph.cvd2e, var = "factor(InsType)3") #outliers on both pos and neg. not straight but not terrible

ggcoxzph(zph.cvdht2e, var = "AGE") #not too bad
ggcoxzph(zph.cvdht2e, var = "factor(SEX)2") #slow increase
ggcoxzph(zph.cvdht2e, var = "factor(IncomeR)3") #not bad
ggcoxzph(zph.cvdht2e, var = "factor(IncomeR)4") #outliers otherwise flat
ggcoxzph(zph.cvdht2e, var = "factor(EduR)2") #inflection at 700 wks
####################################################################
ggcoxzph(zph.diab2.ace, var = "AGE") #slightly u shaped
ggcoxzph(zph.diab2.ace, var = "factor(SEX)2") #slow increase
ggcoxzph(zph.diab2.ace, var = "factor(IncomeR)1") #increase just at end
ggcoxzph(zph.diab2.ace, var = "factor(IncomeR)2") #increase just at end
ggcoxzph(zph.diab2.ace, var = "factor(IncomeR)4") #fairly flat
ggcoxzph(zph.diab2.ace, var = "factor(IncomeR)5") #s shaped
ggcoxzph(zph.diab2.ace, var = "factor(EduR)2") #inflection at 700 wks
ggcoxzph(zph.diab2.ace, var = "factor(InsType)1") #downward slope
ggcoxzph(zph.diab2.ace, var = "factor(InsType)2") #fiarly flat
ggcoxzph(zph.diab2.ace, var = "factor(InsType)4") #slight downward

ggcoxzph(zph.cvd2.ace, var = "factor(SEX)2") #fairly flat
ggcoxzph(zph.cvd2.ace, var = "factor(IncomeR)2") #fairly flat
ggcoxzph(zph.cvd2.ace, var = "factor(IncomeR)4") #fairly flat
ggcoxzph(zph.cvdht2.ace, var = "factor(IncomeR)5") #slight dip to 300

ggcoxzph(zph.cvdht2.ace, var = "factor(SEX)2") #slow increase
ggcoxzph(zph.cvdht2.ace, var = "factor(IncomeR)2") #dip til 300, though small
ggcoxzph(zph.cvdht2.ace, var = "factor(IncomeR)4") #fairly flat
ggcoxzph(zph.cvdht2.ace, var = "factor(IncomeR)5") #fairly flat



# Survival Plots ----------------------------------------------------------
library(survminer)

race.surv <- survfit(Surv(fuTime, allCauseMort) ~ factor(CRN) + factor(EduR)+ AGE +
                       factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(CancerEvBin)+
                       factor(AnyCVDHT),
                     data = eligible[eligible$DiabetesRec ==1,])

ggsurvplot(race.surv, data = eligible[eligible$DiabetesRec ==1,], conf.int = T, risk.table = F,
           tables.theme = clean_theme(), palette = "spectral",
           #make text smaller so all race values fit
           ggtheme = theme_classic2(base_size=8))

eligible$YearStrat <- ifelse(eligible$YEAR <=2010,1,0)
eligible$Year_CRN <- ifelse(eligible$YEAR <=2010 & eligible$CRN == 1,"Early CRN",
                            ifelse(eligible$YEAR <=2010 & eligible$CRN == 0, "Early No CRN",
                            ifelse(eligible$YEAR > 2010 & eligible$CRN == 1, "Late CRN", "Late No CRN")))

race.surv2 <- survfit(Surv(fuTime, diabMort) ~ Year_CRN,
                     data = eligible[eligible$DiabetesRec ==1,])

ggsurvplot(race.surv2, data = eligible[eligible$DiabetesRec ==1,], conf.int = T, risk.table = F,
           tables.theme = clean_theme(), palette = "spectral",
           #make text smaller so all race values fit
           ggtheme = theme_classic2(base_size=8))

race.surv2 <- survfit(Surv(fuTime, allCauseMort) ~ Year_CRN,
                      data = eligible[eligible$DiabetesRec ==1,])

ggsurvplot(race.surv2, data = eligible[eligible$DiabetesRec ==1,], conf.int = T, risk.table = F,
           tables.theme = clean_theme(), palette = "spectral",
           #make text smaller so all race values fit
           ggtheme = theme_classic2(base_size=8))



race.surv <- survfit(Surv(fuTime, allCauseMort) ~ CRN,
                     data = eligible[eligible$AnyCVD ==1,])

ggsurvplot(race.surv, data = eligible[eligible$AnyCVD ==1,], conf.int = T, risk.table = F,
           tables.theme = clean_theme(), palette = "spectral",
           #make text smaller so all race values fit
           ggtheme = theme_classic2(base_size=8))
########################################################################################################
race.surv2 <- survfit(Surv(fuTime, allCauseMort) ~ YearStrat,
                      data = eligible[eligible$AnyCVD ==1,])

ggsurvplot(race.surv2, data = eligible[eligible$AnyCVD ==1,], conf.int = T, risk.table = F,
           tables.theme = clean_theme(), palette = "spectral",
           #make text smaller so all race values fit
           ggtheme = theme_classic2(base_size=8))


race.surv2 <- survfit(Surv(fuTime, cvdMort) ~ Year_CRN,
                      data = eligible[eligible$AnyCVD ==1,])

ggsurvplot(race.surv2, data = eligible[eligible$AnyCVD ==1,], conf.int = T, risk.table = F,
           tables.theme = clean_theme(), palette = "spectral",
           #make text smaller so all race values fit
           ggtheme = theme_classic2(base_size=8))


race.surv2 <- survfit(Surv(fuTime, cvdMort) ~ YearStrat,
                      data = eligible[eligible$AnyCVD ==1,])

ggsurvplot(race.surv2, data = eligible[eligible$AnyCVD ==1,], conf.int = T, risk.table = F,
           tables.theme = clean_theme(), palette = "spectral",
           #make text smaller so all race values fit
           ggtheme = theme_classic2(base_size=8))


race.surv2 <- survfit(Surv(fuTime, cvdMort) ~ Year_CRN,
                      data = eligible[eligible$AnyCVD ==1,])

ggsurvplot(race.surv2, data = eligible[eligible$AnyCVD ==1,], conf.int = T, risk.table = F,
           tables.theme = clean_theme(), palette = "spectral",
           #make text smaller so all race values fit
           ggtheme = theme_classic2(base_size=8))


##################################################################################
 eligible <- eligible %>%
   mutate(htMort = ifelse(DEAD == 0, 0,
                            ifelse(MORTHYPR == 2, 1, 
                                   ifelse(MORTHYPR == 1, 0, NA))))

race.surv2 <- survfit(Surv(fuTime, allCauseMort) ~ YearStrat,
                      data = eligible[eligible$HyperTen ==1,])

ggsurvplot(race.surv2, data = eligible[eligible$HyperTen ==1,], conf.int = T, risk.table = F,
           tables.theme = clean_theme(), palette = "spectral",
           #make text smaller so all race values fit
           ggtheme = theme_classic2(base_size=8))


race.surv2 <- survfit(Surv(fuTime, htMort) ~ Year_CRN,
                      data = eligible[eligible$HyperTen ==1,])

ggsurvplot(race.surv2, data = eligible[eligible$HyperTen ==1,], conf.int = T, risk.table = F,
           tables.theme = clean_theme(), palette = "spectral",
           #make text smaller so all race values fit
           ggtheme = theme_classic2(base_size=8))


race.surv2 <- survfit(Surv(fuTime, htMort) ~ YearStrat,
                      data = eligible[eligible$HyperTen ==1,])

ggsurvplot(race.surv2, data = eligible[eligible$HyperTen ==1,], conf.int = T, risk.table = F,
           tables.theme = clean_theme(), palette = "spectral",
           #make text smaller so all race values fit
           ggtheme = theme_classic2(base_size=8))


race.surv2 <- survfit(Surv(fuTime, allCauseMort) ~ Year_CRN,
                      data = eligible[eligible$HyperTen ==1,])

ggsurvplot(race.surv2, data = eligible[eligible$HyperTen ==1,], conf.int = T, risk.table = F,
           tables.theme = clean_theme(), palette = "spectral",
           #make text smaller so all race values fit
           ggtheme = theme_classic2(base_size=8))

