#Sarah Van Alsten
#July 8, 2020
#Rechecking proportional hazards to see how severe deviations are
################################################################################
library(survey)
library(tidyverse)
library(survminer)

#############################################################################################################
#Diabetes
mod2.diab.strat <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+ AGE +
                           factor(IncomeR) + factor(SEX) +  factor(CancerEvBin) + factor(InsType) + factor(RaceR)  +
                           factor(AnyCVDHT),
                         design = diab.mort14.fin.sa)

summary(mod2.diab.strat)
ggcoxzph(cox.zph(mod2.diab.strat), point.alpha = .1)

mod2.diab.strat <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+ AGE +
                              factor(IncomeR) + factor(SEX) +  factor(CancerEvBin) + factor(InsType) + factor(RaceR)  +
                              factor(AnyCVDHT),
                            design = diab.mort10.fin.sa)

summary(mod2.diab.strat)
ggcoxzph(cox.zph(mod2.diab.strat), point.alpha = .1)

mod2.diab.strat <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+ AGE +
                              factor(IncomeR) + factor(SEX) +  factor(CancerEvBin) + factor(InsType) + factor(RaceR)  +
                              factor(AnyCVDHT),
                            design = diab.mort5.fin.sa)

summary(mod2.diab.strat)
ggcoxzph(cox.zph(mod2.diab.strat), point.alpha = .1)


########################################################################################################################
#CVD
mod2.cvd.strat <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR) + AGE +
                              factor(IncomeR) + factor(SEX) + factor(CancerEvBin) + factor(InsType)+ factor(RaceR)+
                              factor(DiabetesRec) + factor(HyperTen),
                            design = cvd.mort14.fin.sa)

summary(mod2.cvd.strat)

survminer::ggcoxzph(cox.zph(mod2.cvd.strat), point.alpha = .05)

mod2.cvd.strat <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR) + AGE +
                             factor(IncomeR) + factor(SEX) + factor(CancerEvBin) + factor(InsType)+ factor(RaceR)+
                             factor(DiabetesRec) + factor(HyperTen),
                           design = cvd.mort10.fin.sa)

summary(mod2.cvd.strat)

survminer::ggcoxzph(cox.zph(mod2.cvd.strat), point.alpha = .05)

mod2.cvd.strat <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR) + AGE +
                             factor(IncomeR) + factor(SEX) + factor(CancerEvBin) + factor(InsType)+ factor(RaceR)+
                             factor(DiabetesRec) + factor(HyperTen),
                           design = cvd.mort5.fin.sa)

summary(mod2.cvd.strat)

survminer::ggcoxzph(cox.zph(mod2.cvd.strat), point.alpha = .05)



###############################################################################################
mod2.ht.strat <- svycoxph(formula = Surv(fuTime, htMort)~ factor(CRN) + factor(EduR) + AGE +
                             factor(IncomeR) + factor(SEX) + factor(CancerEvBin)+ factor(RaceR) + factor(InsType) +
                             factor(DiabetesRec) + factor(AnyCVD),
                           design = ht.mort14.fin)

summary(mod2.ht.strat)
survminer::ggcoxzph(cox.zph(mod2.ht.strat), point.alpha = .05)

mod2.ht.strat <- svycoxph(formula = Surv(fuTime, htMort)~ factor(CRN) + factor(EduR) + AGE +
                            factor(IncomeR) + factor(SEX) + factor(CancerEvBin)+ factor(RaceR) + factor(InsType) +
                            factor(DiabetesRec) + factor(AnyCVD),
                          design = ht.mort10.fin)

summary(mod2.ht.strat)
survminer::ggcoxzph(cox.zph(mod2.ht.strat), point.alpha = .05)


mod2.ht.strat <- svycoxph(formula = Surv(fuTime, htMort)~ factor(CRN) + factor(EduR) + AGE +
                            factor(IncomeR) + factor(SEX) + factor(CancerEvBin)+ factor(RaceR) + factor(InsType) +
                            factor(DiabetesRec) + factor(AnyCVD),
                          design = ht.mort5.fin)

summary(mod2.ht.strat)
survminer::ggcoxzph(cox.zph(mod2.ht.strat), point.alpha = .05)








