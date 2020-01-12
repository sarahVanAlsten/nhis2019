##################################################################
#Sarah Van Alsten
#Created: Dec 13, 2019
#Use the cleaned data from 20190928_NHIS.R to create cox regressions for table 2
#Packages used: ipumsr, tidyverse, tableone, survival, survey
#Last Update: Jan 12, 2020
################################################################################
########################################################################################
#svycoxph and survival to run the regressions
#crude/unadjusted

# Disease Specific --------------------------------------------------------


mod1.diab.sa <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN),
                         design = diab.mort14.fin.sa)

summary(mod1.diab.sa)
#to get N(%)
#diab_mort_n <- mod1.diab$nevent
#diab_mort_perc <- mod1.diab$nevent / mod1.diab$n
diab_mort_n <- mod1.diab.sa$nevent
diab_mort_perc <- mod1.diab.sa$nevent / mod1.diab.sa$n
#########################################################################
mod1.cvd.sa <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN),
                        design = cvd.mort14.fin.sa)

summary(mod1.cvd.sa)
#to get N(%)
cvd_mort_n <- mod1.cvd.sa$nevent
cvd_mort_perc <- mod1.cvd.sa$nevent / mod1.cvd.sa$n

#mod1.cvd <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN),
#                     design = cvd.mort14.fin)

#summary(mod1.cvd)
#to get N(%)
#cvd_mort_n <- mod1.cvd$nevent
#cvd_mort_perc <- mod1.cvd$nevent / mod1.cvd$n
######################################################
mod1.cvdht.sa <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN),
                          design = cvdht.mort14.fin.sa)

summary(mod1.cvdht.sa)
#to get N(%)
cvdht_mort_n <- mod1.cvdht.sa$nevent
cvdht_mort_perc <- mod1.cvdht.sa$nevent / mod1.cvdht.sa$n

# mod1.cvdht <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN),
#                        design = cvdht.mort14.fin)
# 
# summary(mod1.cvdht)
# #to get N(%)
# cvdht_mort_n <- mod1.cvdht$nevent
# cvdht_mort_perc <- mod1.cvdht$nevent / mod1.cvdht$n

###################################################################################
#now do adjusted models: for sex, age, insurance, income, education 
#(BMI, Race, Smoking per dag shouldn't be adjusted)

#Diabetes
#mod2.diab <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+ AGE +
#                        factor(IncomeR) + factor(SEX) + factor(InsType),
#                      design = diab.mort14.fin)

#summary(mod2.diab)

mod2.diab.sa <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+ AGE +
                           factor(IncomeR) + factor(SEX) + factor(InsType),
                         design = diab.mort14.fin.sa)

summary(mod2.diab.sa)
###########################################################################
#CVD
#mod2.cvd <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+ AGE +
#                       factor(IncomeR) + factor(SEX) + factor(InsType),
#                     design = cvd.mort14.fin)

#summary(mod2.cvd)

mod2.cvd.sa <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+ AGE +
                          factor(IncomeR) + factor(SEX) + factor(InsType),
                        design = cvd.mort14.fin.sa)

summary(mod2.cvd.sa)
###############################################################################
#CVD plus hypertension
mod2.cvdht.sa <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+ AGE +
                            factor(IncomeR) + factor(SEX) + factor(InsType),
                          design = cvdht.mort14.fin.sa)

summary(mod2.cvdht.sa)

#mod2.cvdht <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+ AGE +
#                         factor(IncomeR) + factor(SEX) + factor(InsType),
#                       design = cvdht.mort14.fin)

#summary(mod2.cvdht)
####################################################################################
#get follow up times
svyquantile(~fuTime, design = diab.mort14.fin.sa, quantiles = .5, na.rm = T)
svyquantile(~fuTime, design = diab.mort14.fin.sa, quantiles = .25, na.rm = T)
svyquantile(~fuTime, design = diab.mort14.fin.sa, quantiles = .75, na.rm = T)

svyquantile(~fuTime, design = cvd.mort14.fin.sa, quantiles = .5, na.rm = T)
svyquantile(~fuTime, design = cvd.mort14.fin.sa, quantiles = .25, na.rm = T)
svyquantile(~fuTime, design = cvd.mort14.fin.sa, quantiles = .75, na.rm = T)

svyquantile(~fuTime, design = cvdht.mort14.fin.sa, quantiles = .5, na.rm = T)
svyquantile(~fuTime, design = cvdht.mort14.fin.sa, quantiles = .25, na.rm = T)
svyquantile(~fuTime, design = cvdht.mort14.fin.sa, quantiles = .75, na.rm = T)


# All Cause ---------------------------------------------------------------

##############################################################################
#now do all-cause mortality
#crude/unadjusted
mod1.diab.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                               design = diab.mort14.fin.sa)

summary(mod1.diab.allcause)
#to get N(%)
diabAC_mort_n <- mod1.diab.allcause$nevent
diabAC_mort_perc <- mod1.diab.allcause$nevent / mod1.diab.allcause$n


mod1.cvd.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                              design = cvd.mort14.fin.sa)

summary(mod1.cvd.allcause)
#to get N(%)
cvdAC_mort_n <- mod1.cvd.allcause$nevent
cvdAC_mort_perc <- mod1.cvd.allcause$nevent / mod1.cvd.allcause$n

mod1.cvdht.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                                design = cvdht.mort14.fin.sa)

summary(mod1.cvdht.allcause)
#to get N(%)
cvdhtAC_mort_n <- mod1.cvdht.allcause$nevent
cvdhtAC_mort_perc <- mod1.cvdht.allcause$nevent / mod1.cvdht.allcause$n

##talk to Dr. Salvo about using diff adjustment for all cause vs dz specific...
#Also ask about follow up time: should that be reported for only those with the outcome
#Diabetes
mod2.diab.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                 factor(IncomeR) + factor(SEX) + factor(InsType),
                               design = diab.mort14.fin.sa)

#mod2.diab.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
#                       factor(IncomeR) + factor(SEX) + factor(InsType) + factor(CancerEvBin) +
#                         factor(SmokeR) + BMI + factor(AnyCVDHT),
#                     design = diab.mort14.fin)

summary(mod2.diab.allcause)

#CVD
mod2.cvd.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                factor(IncomeR) + factor(SEX) + factor(InsType),
                              design = cvd.mort14.fin.sa)

summary(mod2.cvd.allcause)

#CVD plus hypertension
mod2.cvdht.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                  factor(IncomeR) + factor(SEX) + factor(InsType),
                                design = cvdht.mort14.fin.sa)

summary(mod2.cvdht.allcause)

#get follow up times
svyquantile(~fuTime, design = diab.mort14.fin, quantiles = .5, na.rm = T)
svyquantile(~fuTime, design = diab.mort14.fin, quantiles = .25, na.rm = T)
svyquantile(~fuTime, design = diab.mort14.fin, quantiles = .75, na.rm = T)

svyquantile(~fuTime, design = cvd.mort14.fin, quantiles = .5, na.rm = T)
svyquantile(~fuTime, design = cvd.mort14.fin, quantiles = .25, na.rm = T)
svyquantile(~fuTime, design = cvd.mort14.fin, quantiles = .75, na.rm = T)

svyquantile(~fuTime, design = cvdht.mort14.fin, quantiles = .5, na.rm = T)
svyquantile(~fuTime, design = cvdht.mort14.fin, quantiles = .25, na.rm = T)
svyquantile(~fuTime, design = cvdht.mort14.fin, quantiles = .75, na.rm = T)


#####################################################################################
#if follow up times are those that are specific to those with the outcome only...
median(diab.mort14.fin$variables[diab.mort14.fin$variables$DiabetesRec ==1 & 
                                   !is.na(diab.mort14.fin$variables$CRN) & 
                                   diab.mort14.fin$variables$diabMort == 1,
                                 "fuTime"], na.rm = T)

median(diab.mort14.fin$variables[diab.mort14.fin$variables$DiabetesRec ==1 & 
                                   !is.na(diab.mort14.fin$variables$CRN) & 
                                   diab.mort14.fin$variables$diabMort == 0,
                                 "fuTime"], na.rm = T)

median(cvd.mort14.fin$variables[cvd.mort14.fin$variables$AnyCVD ==1 & 
                                  !is.na(cvd.mort14.fin$variables$CRN) & 
                                  cvd.mort14.fin$variables$cvdMort == 1,
                                "fuTime"], na.rm = T)

median(cvd.mort14.fin$variables[cvd.mort14.fin$variables$AnyCVD==1 & 
                                  !is.na(cvd.mort14.fin$variables$CRN) & 
                                  cvd.mort14.fin$variables$cvdMort == 0,
                                "fuTime"], na.rm = T)

median(cvdht.mort14.fin$variables[cvdht.mort14.fin$variables$AnyCVDHT ==1 & 
                                    !is.na(cvdht.mort14.fin$variables$CRN) & 
                                    cvdht.mort14.fin$variables$cvdHtMort == 1,
                                  "fuTime"], na.rm = T)

median(cvdht.mort14.fin$variables[cvdht.mort14.fin$variables$AnyCVDHT ==1 & 
                                    !is.na(cvdht.mort14.fin$variables$CRN) & 
                                    cvdht.mort14.fin$variables$cvdHtMort == 0,
                                  "fuTime"], na.rm = T)
#####################################################################################
#Restricting Analysis to 2000 - 2010 years
#######################################################################################


# Early Disease Specific --------------------------------------------------

#svycoxph and survival to run the regressions
#crude/unadjusted

mod1.early.diab <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN),
                            design = diab.mort10.fin.sa)

summary(mod1.early.diab)
#to get N(%)
diab_mort_n.2 <- mod1.early.diab$nevent
diab_mort_perc.2 <- mod1.early.diab$nevent / mod1.early.diab$n


mod1.early.cvd <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN),
                           design = cvd.mort10.fin.sa)

summary(mod1.early.cvd)
#to get N(%)
cvd_mort_n.2 <- mod1.early.cvd$nevent
cvd_mort_perc.2 <- mod1.early.cvd$nevent / mod1.early.cvd$n

mod1.early.cvdht <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN),
                             design = cvdht.mort10.fin.sa)

summary(mod1.early.cvdht)
#to get N(%)
cvdht_mort_n.2 <- mod1.early.cvdht$nevent
cvdht_mort_perc.2 <- mod1.early.cvdht$nevent / mod1.early.cvdht$n

###################################################################################
#now do adjusted models: for sex, age, insurance, income, education 
#(BMI, Race, Smoking per dag shouldn't be adjusted)

#Diabetes
mod2.early.diab <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+ AGE +
                              factor(IncomeR) + factor(SEX) + factor(InsType),
                            design = diab.mort10.fin.sa)

summary(mod2.early.diab)

#CVD
mod2.early.cvd <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+ AGE +
                             factor(IncomeR) + factor(SEX) + factor(InsType),
                           design = cvd.mort10.fin.sa)

summary(mod2.early.cvd)

#CVD plus hypertension
mod2.early.cvdht <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+ AGE +
                               factor(IncomeR) + factor(SEX) + factor(InsType),
                             design = cvdht.mort10.fin.sa)

summary(mod2.early.cvdht)

#get follow up times
svyquantile(~fuTime, design = diab.mort10.fin.sa, quantiles = .5, na.rm = T)
svyquantile(~fuTime, design = diab.mort10.fin.sa, quantiles = .25, na.rm = T)
svyquantile(~fuTime, design = diab.mort10.fin.sa, quantiles = .75, na.rm = T)

svyquantile(~fuTime, design = cvd.mort10.fin.sa, quantiles = .5, na.rm = T)
svyquantile(~fuTime, design = cvd.mort10.fin.sa, quantiles = .25, na.rm = T)
svyquantile(~fuTime, design = cvd.mort10.fin.sa, quantiles = .75, na.rm = T)

svyquantile(~fuTime, design = cvdht.mort10.fin.sa, quantiles = .5, na.rm = T)
svyquantile(~fuTime, design = cvdht.mort10.fin.sa, quantiles = .25, na.rm = T)
svyquantile(~fuTime, design = cvdht.mort10.fin.sa, quantiles = .75, na.rm = T)


# Early All Cause ---------------------------------------------------------

##############################################################################
#now do all-cause mortality
#crude/unadjusted
mod1.early.diab.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                                     design = diab.mort10.fin.sa)

summary(mod1.early.diab.allcause)
#to get N(%)
diabAC_mort_n.2 <- mod1.early.diab.allcause$nevent
diabAC_mort_perc.2 <- mod1.early.diab.allcause$nevent / mod1.early.diab.allcause$n


mod1.early.cvd.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                                    design = cvd.mort10.fin.sa)

summary(mod1.early.cvd.allcause)
#to get N(%)
cvdAC_mort_n.2 <- mod1.early.cvd.allcause$nevent
cvdAC_mort_perc.2 <- mod1.early.cvd.allcause$nevent / mod1.early.cvd.allcause$n

mod1.early.cvdht.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                                      design = cvdht.mort10.fin.sa)

summary(mod1.early.cvdht.allcause)
#to get N(%)
cvdhtAC_mort_n.2 <- mod1.early.cvdht.allcause$nevent
cvdhtAC_mort_perc.2 <- mod1.early.cvdht.allcause$nevent / mod1.early.cvdht.allcause$n

###################################################################################
#now do adjusted models: for sex, age, insurance, income, education 
#(BMI, Race, Smoking per dag shouldn't be adjusted)

##talk to Dr. Salvo about using diff adjustment for all cause vs dz specific...
#Also ask about follow up time: should that be reported for only those with the outcome
#Diabetes
mod2.early.diab.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                       factor(IncomeR) + factor(SEX) + factor(InsType),
                                     design = diab.mort10.fin.sa)


summary(mod2.early.diab.allcause)

#CVD
mod2.early.cvd.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                      factor(IncomeR) + factor(SEX) + factor(InsType),
                                    design = cvd.mort10.fin.sa)

summary(mod2.early.cvd.allcause)

#CVD plus hypertension
mod2.early.cvdht.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                        factor(IncomeR) + factor(SEX) + factor(InsType),
                                      design = cvdht.mort10.fin.sa)

summary(mod2.early.cvdht.allcause)
################################################################

# For All Cause, adjusting also for chronic conditions --------------------
#For diabetes, adjust for CVD, hypertension, cancer
#For CVD, adjust for diabetes, hypertension, cancer
#For CVDHT, adjust for diabetes, cancer

#Diabetes
mod2.diab.othcond <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                factor(IncomeR) + factor(SEX) + factor(InsType) + factor(CancerEvBin) +
                                factor(AnyCVDHT),
                              design = diab.mort14.fin)

summary(mod2.diab.othcond)

#also have here adjusting for smoking and BMI because it's possible someone might ask for that
#although the DAG showed they don't need to be adjusted for
mod2.diab.extra <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                              factor(IncomeR) + factor(SEX) + factor(InsType) + factor(CancerEvBin) +
                              factor(SmokeR) + BMI + factor(AnyCVDHT),
                            design = diab.mort14.fin)

summary(mod2.diab.extra)

#CVD
mod2.cvd.othcond <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                               factor(IncomeR) + factor(SEX) + factor(InsType) + factor(CancerEvBin)+
                               factor(DiabetesRec) + factor(HyperTen),
                             design = cvd.mort14.fin.sa)

summary(mod2.cvd.othcond)

#adjusting for smoking and BMI
mod2.cvd.extra <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(CancerEvBin)+
                             factor(DiabetesRec) + factor(HyperTen) + factor(SmokeR)+ BMI,
                           design = cvd.mort14.fin.sa)

summary(mod2.cvd.extra)

#CVD plus hypertension
mod2.cvdht.othcond <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                 factor(IncomeR) + factor(SEX) + factor(InsType)+ factor(CancerEvBin)+
                                 factor(DiabetesRec),
                               design = cvdht.mort14.fin.sa)

summary(mod2.cvdht.othcond)

mod2.cvdht.extra <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                               factor(IncomeR) + factor(SEX) + factor(InsType)+ factor(CancerEvBin)+
                               factor(DiabetesRec) + factor(SmokeR) + BMI,
                             design = cvdht.mort14.fin.sa)

summary(mod2.cvdht.extra)
#####################################################################
#Early years with the additional adjustments for other chronic conditions
#Diabetes
mod2.early.diab.othcond <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                      factor(IncomeR) + factor(SEX) + factor(InsType) + factor(CancerEvBin)+
                                      factor(AnyCVDHT),
                                    design = diab.mort10.fin.sa)


summary(mod2.early.diab.othcond)

mod2.early.diab.extra <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                    factor(IncomeR) + factor(SEX) + factor(InsType) + factor(CancerEvBin)+
                                    factor(AnyCVDHT) + factor(SmokeR)+ BMI,
                                  design = diab.mort10.fin.sa)


summary(mod2.early.diab.extra)

#CVD
mod2.early.cvd.othcond <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                     factor(IncomeR) + factor(SEX) + factor(InsType)+ factor(CancerEvBin)+
                                     factor(DiabetesRec) + factor(HyperTen),
                                   design = cvd.mort10.fin.sa)

summary(mod2.early.cvd.othcond)

mod2.early.cvd.extra <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                   factor(IncomeR) + factor(SEX) + factor(InsType)+ factor(CancerEvBin)+
                                   factor(DiabetesRec) + factor(HyperTen) + factor(SmokeR) + BMI,
                                 design = cvd.mort10.fin.sa)

summary(mod2.early.cvd.extra)

#CVD plus hypertension
mod2.early.cvdht.othcond <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                       factor(IncomeR) + factor(SEX) + factor(InsType)+ factor(CancerEvBin)+
                                       factor(DiabetesRec),
                                     design = cvdht.mort10.fin.sa)

summary(mod2.early.cvdht.othcond)

mod2.early.cvdht.extra <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                     factor(IncomeR) + factor(SEX) + factor(InsType)+ factor(CancerEvBin)+
                                     factor(DiabetesRec)+ factor(SmokeR) + BMI,
                                   design = cvdht.mort10.fin.sa)

summary(mod2.early.cvdht.extra)
#############################################################################
#Go back and do smoking and BMI adjustment for dz specific
mod1.diab.sa.extra <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN)+ factor(EduR)+ AGE +
                                 factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI,
                               design = diab.mort14.fin.sa)
summary(mod1.diab.sa.extra)

mod1.cvd.sa.extra <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN)+ factor(EduR)+ AGE +
                                factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI,
                              design = cvd.mort14.fin.sa)
summary(mod1.cvd.sa.extra)

mod1.cvdht.sa.extra <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN)+ factor(EduR)+ AGE +
                                  factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI,
                                design = cvdht.mort14.fin.sa)
summary(mod1.cvdht.sa.extra)

#for the early years
mod1.diab.sa.extra.early <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN)+ factor(EduR)+ AGE +
                                       factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI,
                                     design = diab.mort10.fin.sa)
summary(mod1.diab.sa.extra.early)

mod1.cvd.sa.extra.early <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN)+ factor(EduR)+ AGE +
                                      factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI,
                                    design = cvd.mort10.fin.sa)
summary(mod1.cvd.sa.extra.early)

mod1.cvdht.sa.extra.early <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN)+ factor(EduR)+ AGE +
                                        factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI,
                                      design = cvdht.mort10.fin.sa)
summary(mod1.cvdht.sa.extra.early)


#for the late years
mod1.diab.sa.extra.l <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN)+ factor(EduR)+ AGE +
                                   factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI,
                                 design = diab.mort5.fin.sa)
summary(mod1.diab.sa.extra.l)

mod1.cvd.sa.extra.l <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN)+ factor(EduR)+ AGE +
                                  factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI,
                                design = cvd.mort5.fin.sa)
summary(mod1.cvd.sa.extra.l)

mod1.cvdht.sa.extra.l <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN)+ factor(EduR)+ AGE +
                                    factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI,
                                  design = cvdht.mort5.fin.sa)
summary(mod1.cvdht.sa.extra.l)

#for later years with all cause models
mod2.late.diab.othcond<- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                    factor(IncomeR) + factor(SEX) + factor(InsType) + factor(CancerEvBin)+
                                    factor(AnyCVDHT),
                                  design = diab.mort5.fin.sa)


summary(mod2.late.diab.othcond)

mod2.late.diab.extra <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                   factor(IncomeR) + factor(SEX) + factor(InsType) + factor(CancerEvBin)+
                                   factor(AnyCVDHT) + factor(SmokeR)+ BMI,
                                 design = diab.mort5.fin.sa)


summary(mod2.late.diab.extra)

#CVD
mod2.late.cvd.othcond <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                    factor(IncomeR) + factor(SEX) + factor(InsType)+ factor(CancerEvBin)+
                                    factor(DiabetesRec) + factor(HyperTen),
                                  design = cvd.mort5.fin.sa)

summary(mod2.late.cvd.othcond)

mod2.late.cvd.extra <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                  factor(IncomeR) + factor(SEX) + factor(InsType)+ factor(CancerEvBin)+
                                  factor(DiabetesRec) + factor(HyperTen) + factor(SmokeR) + BMI,
                                design = cvd.mort5.fin.sa)

summary(mod2.late.cvd.extra)

#CVD plus hypertension
mod2.late.cvdht.othcond <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                      factor(IncomeR) + factor(SEX) + factor(InsType)+ factor(CancerEvBin)+
                                      factor(DiabetesRec),
                                    design = cvdht.mort5.fin.sa)

summary(mod2.late.cvdht.othcond)

mod2.late.cvdht.extra <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                    factor(IncomeR) + factor(SEX) + factor(InsType)+ factor(CancerEvBin)+
                                    factor(DiabetesRec)+ factor(SmokeR) + BMI,
                                  design = cvdht.mort5.fin.sa)

summary(mod2.late.cvdht.extra)



# Full Model: age, edu, bmi, smoke, sex, income, race, insurance, region--------
#for all years
full.diab <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN)+ factor(EduR)+ AGE +
                        factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI +
                        factor(REGION) + factor(RaceR),
                      design = diab.mort14.fin.sa)
summary(full.diab)

full.cvd <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN)+ factor(EduR)+ AGE +
                       factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI +
                       factor(REGION) + factor(RaceR),
                     design = cvd.mort14.fin.sa)
summary(full.cvd)

full.cvdht <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN)+ factor(EduR)+ AGE +
                         factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI +
                         factor(REGION) + factor(RaceR),
                       design = cvdht.mort14.fin.sa)
summary(full.cvdht)

#for early years
full.diab.e <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN)+ factor(EduR)+ AGE +
                          factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI +
                          factor(REGION) + factor(RaceR),
                        design = diab.mort10.fin.sa)
summary(full.diab.e)

full.cvd.e <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN)+ factor(EduR)+ AGE +
                         factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI +
                         factor(REGION) + factor(RaceR),
                       design = cvd.mort10.fin.sa)
summary(full.cvd.e)

full.cvdht.e <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN)+ factor(EduR)+ AGE +
                           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI +
                           factor(REGION) + factor(RaceR),
                         design = cvdht.mort10.fin.sa)
summary(full.cvdht.e)

#later years
full.diab.l <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN)+ factor(EduR)+ AGE +
                          factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI +
                          factor(REGION) + factor(RaceR),
                        design = diab.mort5.fin.sa)
summary(full.diab.l)

full.cvd.l <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN)+ factor(EduR)+ AGE +
                         factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI +
                         factor(REGION) + factor(RaceR),
                       design = cvd.mort5.fin.sa)
summary(full.cvd.l)

full.cvdht.l <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN)+ factor(EduR)+ AGE +
                           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(SmokeR) + BMI +
                           factor(REGION) + factor(RaceR),
                         design = cvdht.mort5.fin.sa)
summary(full.cvdht.l)

