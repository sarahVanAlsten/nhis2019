#Sarah Van Alsten
#Date: February 2, 2020
#Purpose: add splines to Cox Models to see how it changes results given
#small non-linearity in age after 75
#Packages used: survival, lattice, splines, survey
#######################################
library(survival)
library(lattice)
library(splines)

#disease specific models:
# fit a Cox model with a nonlinear effect for age using natural splines
mod2.diab.sa2 <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+
                             bs(AGE, knots = c(75), degree = 1) +
                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
                           design = diab.mort14.fin.sa)
summary(mod2.diab.sa2)

# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvd.sa2 <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+
                             bs(AGE, knots = c(75), degree = 1) +
                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
                           design = cvd.mort14.fin.sa)
summary(mod2.cvd.sa2)


# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvdht.sa2 <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+
                            bs(AGE, knots = c(75), degree = 1) +
                            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
                          design = cvdht.mort14.fin.sa)
summary(mod2.cvdht.sa2)
#############################################################
#all cause models:
# fit a Cox model with a nonlinear effect for age using natural splines
mod2.diab.ac2 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
                            bs(AGE, knots = c(75), degree = 1) +
                            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                            factor(CancerEvBin) + factor(AnyCVDHT),
                          design = diab.mort14.fin.sa)
summary(mod2.diab.ac2)

# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvd.ac2 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
                           bs(AGE, knots = c(75), degree = 1) +
                           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                           factor(HyperTen) + factor(CancerEvBin) + factor(DiabetesRec),
                         design = cvd.mort14.fin.sa)
summary(mod2.cvd.ac2)


# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvdht.sa2 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
                             bs(AGE, knots = c(75), degree = 1) +
                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                             factor(DiabetesRec)+ factor(CancerEvBin),
                           design = cvdht.mort14.fin.sa)
summary(mod2.cvdht.sa2)

###############################################################################################
#2000 - 2010 years only

#disease specific models:
# fit a Cox model with a nonlinear effect for age using natural splines
mod2.diab.sa10 <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+
                            bs(AGE, knots = c(75), degree = 1) +
                            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
                          design = diab.mort10.fin.sa)
summary(mod2.diab.sa10)

# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvd.sa10 <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+
                           bs(AGE, knots = c(75), degree = 1) +
                           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
                         design = cvd.mort10.fin.sa)
summary(mod2.cvd.sa10)


# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvdht.sa10 <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+
                             bs(AGE, knots = c(75), degree = 1) +
                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
                           design = cvdht.mort10.fin.sa)
summary(mod2.cvdht.sa10)
#############################################################
#all cause models:
# fit a Cox model with a nonlinear effect for age using natural splines
mod2.diab.ac10 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
                            bs(AGE, knots = c(75), degree = 1) +
                            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                            factor(CancerEvBin) + factor(AnyCVDHT),
                          design = diab.mort10.fin.sa)
summary(mod2.diab.ac10)

# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvd.ac10 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
                           bs(AGE, knots = c(75), degree = 1) +
                           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                           factor(HyperTen) + factor(CancerEvBin) + factor(DiabetesRec),
                         design = cvd.mort10.fin.sa)
summary(mod2.cvd.ac10)


# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvdht.sa10 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
                             bs(AGE, knots = c(75), degree = 1) +
                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                             factor(DiabetesRec)+ factor(CancerEvBin),
                           design = cvdht.mort10.fin.sa)
summary(mod2.cvdht.sa10)
#######################################################################
#Later Years only


#disease specific models:
# fit a Cox model with a nonlinear effect for age using natural splines
mod2.diab.sa5 <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+
                             bs(AGE, knots = c(75), degree = 1) +
                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
                           design = diab.mort5.fin.sa)
summary(mod2.diab.sa5)

# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvd.sa5 <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+
                            bs(AGE, knots = c(75), degree = 1) +
                            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
                          design = cvd.mort5.fin.sa)
summary(mod2.cvd.sa5)


# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvdht.sa5 <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+
                              bs(AGE, knots = c(75), degree = 1) +
                              factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
                            design = cvdht.mort5.fin.sa)
summary(mod2.cvdht.sa5)
#############################################################
#all cause models:
# fit a Cox model with a nonlinear effect for age using natural splines
mod2.diab.ac5 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
                             bs(AGE, knots = c(75), degree = 1) +
                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                             factor(CancerEvBin) + factor(AnyCVDHT),
                           design = diab.mort5.fin.sa)
summary(mod2.diab.ac5)

# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvd.ac5 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
                            bs(AGE, knots = c(75), degree = 1) +
                            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                            factor(HyperTen) + factor(CancerEvBin) + factor(DiabetesRec),
                          design = cvd.mort5.fin.sa)
summary(mod2.cvd.ac5)


# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvdht.sa5 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
                              bs(AGE, knots = c(75), degree = 1) +
                              factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                              factor(DiabetesRec)+ factor(CancerEvBin),
                            design = cvdht.mort5.fin.sa)
summary(mod2.cvdht.sa5)