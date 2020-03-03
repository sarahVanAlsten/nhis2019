##################################################################
#Sarah Van Alsten
#Created: Feb 1, 2019
#check assumptions of coxph :influence via sdfbeta
#Packages used: survey, survminer, survival
#Last Update: Feb 1, 2019
################################################################################
library(survminer)

# Influential Observations ------------------------------------------------
#influence: standardized dfbeta
#unadjusted model dm
mod1.diab.sdfbeta <- resid(mod1.diab.sa, type = "dfbetas")
plot(mod1.diab.sdfbeta)
infcase1 <- diab.mort14.fin.sa$variables[mod1.diab.sdfbeta > 0.10,]
#would exclude these cases by SERIAL identifier... however, there doesn't
#seem to be anything unusual about them. I'd leave them in.
table(infcase1$diabMort)
table(infcase1$SEX)
median(infcase1$fuTime)
table(infcase1$YEAR)
table(infcase1$CRN)

#adjusted model dm
mod2.diab.sdfbeta <- resid(mod2.diab.sa, type = "dfbetas")
dim(mod2.diab.sdfbeta)

data.dm.mod2 <- diab.mort14.fin.sa$variables %>%
  drop_na(SEX, CRN, diabMort, RaceR, IncomeR, InsType, EduR, AGE)

data.dm.mod2$resid.crn <- mod2.diab.sdfbeta[,1]
data.dm.mod2$resid.edu2 <- mod2.diab.sdfbeta[,2]
data.dm.mod2$resid.edu3 <- mod2.diab.sdfbeta[,3]
data.dm.mod2$resid.age <- mod2.diab.sdfbeta[,4]
data.dm.mod2$resid.inc1 <- mod2.diab.sdfbeta[,5]
data.dm.mod2$resid.inc2 <- mod2.diab.sdfbeta[,6]
data.dm.mod2$resid.inc3 <- mod2.diab.sdfbeta[,7]
data.dm.mod2$resid.inc4 <- mod2.diab.sdfbeta[,8]
data.dm.mod2$resid.inc5 <- mod2.diab.sdfbeta[,9]
data.dm.mod2$resid.sex <- mod2.diab.sdfbeta[,10]
data.dm.mod2$resid.ins1 <- mod2.diab.sdfbeta[,11]
data.dm.mod2$resid.ins2 <- mod2.diab.sdfbeta[,12]
data.dm.mod2$resid.ins3 <- mod2.diab.sdfbeta[,13]
data.dm.mod2$resid.ins4 <- mod2.diab.sdfbeta[,14]
data.dm.mod2$resid.ins5 <- mod2.diab.sdfbeta[,15]
data.dm.mod2$resid.race2 <- mod2.diab.sdfbeta[,16]
data.dm.mod2$resid.race3 <- mod2.diab.sdfbeta[,17]
data.dm.mod2$resid.race4 <- mod2.diab.sdfbeta[,18]
data.dm.mod2$resid.race5 <- mod2.diab.sdfbeta[,19]
data.dm.mod2$resid.race6 <- mod2.diab.sdfbeta[,20]

data.dm.mod2$id_num <- 1:nrow(data.dm.mod2)

#plot these sdfbetas
ggplot(data.dm.mod2, aes(y = resid.crn, x= id_num)) + geom_point() #over abs(.1)
ggplot(data.dm.mod2, aes(y = resid.edu2, x= id_num)) + geom_point() #< -.1
ggplot(data.dm.mod2, aes(y = resid.edu3, x= id_num)) + geom_point() #>.4
ggplot(data.dm.mod2, aes(y = resid.age, x= id_num)) + geom_point() # abs(.2
ggplot(data.dm.mod2, aes(y = resid.inc1, x= id_num)) + geom_point()#>.1
ggplot(data.dm.mod2, aes(y = resid.inc2, x= id_num)) + geom_point() #>.3 
ggplot(data.dm.mod2, aes(y = resid.inc3, x= id_num)) + geom_point() #>.2
ggplot(data.dm.mod2, aes(y = resid.inc4, x= id_num)) + geom_point()
ggplot(data.dm.mod2, aes(y = resid.inc5, x= id_num)) + geom_point() #>.1
ggplot(data.dm.mod2, aes(y = resid.sex, x= id_num)) + geom_point() #>.2
ggplot(data.dm.mod2, aes(y = resid.ins1, x= id_num)) + geom_point() #<-.2
ggplot(data.dm.mod2, aes(y = resid.ins2, x= id_num)) + geom_point() # <-.15
ggplot(data.dm.mod2, aes(y = resid.ins3, x= id_num)) + geom_point() #abs(.2)
ggplot(data.dm.mod2, aes(y = resid.ins4, x= id_num)) + geom_point() #<-.18
ggplot(data.dm.mod2, aes(y = resid.ins5, x= id_num)) + geom_point() #>.18
ggplot(data.dm.mod2, aes(y = resid.race2, x= id_num)) + geom_point() #<-.1, >.18
ggplot(data.dm.mod2, aes(y = resid.race3, x= id_num)) + geom_point() # >.12
ggplot(data.dm.mod2, aes(y = resid.race4, x= id_num)) + geom_point() #>.12
ggplot(data.dm.mod2, aes(y = resid.race5, x= id_num)) + geom_point()
ggplot(data.dm.mod2, aes(y = resid.race6, x= id_num)) + geom_point() #>.1

#generate a summary measure of how many variables each obs
#was influential on
data.dm.mod2 <- data.dm.mod2 %>%
  mutate(influence.meas = (resid.race6 > 0.1) + (resid.race4 > .12) +
           (resid.race3 > .12) + (resid.race2 < (0-.1) | resid.race2 > .18) +
           (resid.ins5 > .18) + (resid.ins4 < (0-.18)) + (abs(resid.ins3) > .2) +
           (resid.ins2 < (0-.15)) + (resid.ins1 < (0-.2)) + (resid.sex > .2) +
           (resid.inc5 > .1) + (resid.inc3 > .2) + (resid.inc2 > .3) + (resid.inc1 > .1) +
           (abs(resid.age) > .2) + (resid.edu3 > .4) + (resid.edu2 < (0-.1)) + (abs(resid.crn) > .1))

table(data.dm.mod2$influence.meas)
#106 are influential on just 1 measure, 8 on 2, 4 on 3, 1, on 4, and 2 on 6

possible.inf <- data.dm.mod2[data.dm.mod2$influence.meas >=1,]
#write out this data
write_csv(possible.inf, "data//possible_inf_diab.csv")

table(possible.inf$CRN)
table(possible.inf$diabMort)
table(possible.inf$AGE)
table(possible.inf$RaceR)
table(possible.inf$InsType)
table(possible.inf$EduR)
table(possible.inf$SEX)
table(possible.inf$CancerEvBin)
table(possible.inf$AnyCVD)
table(possible.inf$AnyCVDHT)
#nothing especially notable about these. Nothing seems like it's awry: leave them in
#possibly just those with more comorbidity: a lot have cvd and cancer

diab.not.inf <- data.dm.mod2[data.dm.mod2$influence.meas == 0,]

#refit model just to test
diab.not.inf.svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight14,
                             nest = TRUE, data = diab.not.inf)

diab.not.inf.svy20 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight10,
                              nest = TRUE, data = diab.not.inf)
diab.not.inf.svy20 <- subset(diab.not.inf.svy20, YEAR <= 2010)

diab.not.inf.svy5 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight5,
                                nest = TRUE, data = diab.not.inf)
diab.not.inf.svy5 <- subset(diab.not.inf.svy5, YEAR > 2010)

svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
         design = diab.not.inf.svy) #doesn't really change things

svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN),
         design = diab.not.inf.svy) %>% #doesn't really change things
  summary()

#do early and late years
svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN),
         design = diab.not.inf.svy20) %>% #doesn't really change things
  summary()

svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
         design = diab.not.inf.svy20) %>% #doesn't really change things
  summary()

svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN),
         design = diab.not.inf.svy5) %>% #doesn't really change things
  summary()

svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
         design = diab.not.inf.svy5) %>% #doesn't really change things
  summary()

quantile(diab.not.inf.svy$variables$fuTime)
quantile(diab.not.inf.svy20$variables$fuTime)
quantile(diab.not.inf.svy5$variables$fuTime)
#############################################################################
#do same for cvd
#unadjusted model cvd
mod1.cvd.sdfbeta <- resid(mod1.cvd.sa, type = "dfbetas")
plot(mod1.cvd.sdfbeta)
infcase1 <- cvd.mort14.fin.sa$variables[mod1.cvd.sdfbeta > 0.10,]
#would exclude these cases by SERIAL identifier... however, there doesn't
#seem to be anything unusual about them. I'd leave them in.
table(infcase1$cvdMort)
table(infcase1$SEX)
median(infcase1$fuTime)
table(infcase1$YEAR)
table(infcase1$CRN)

#adjusted model cvd
mod2.cvd.sdfbeta <- resid(mod2.cvd.sa, type = "dfbetas")
dim(mod2.cvd.sdfbeta)

data.cvd.mod2 <- cvd.mort14.fin.sa$variables %>%
  drop_na(SEX, CRN, cvdMort, RaceR, IncomeR, InsType, EduR, AGE)

data.cvd.mod2$resid.crn <- mod2.cvd.sdfbeta[,1]
data.cvd.mod2$resid.edu2 <- mod2.cvd.sdfbeta[,2]
data.cvd.mod2$resid.edu3 <- mod2.cvd.sdfbeta[,3]
data.cvd.mod2$resid.age <- mod2.cvd.sdfbeta[,4]
data.cvd.mod2$resid.inc1 <- mod2.cvd.sdfbeta[,5]
data.cvd.mod2$resid.inc2 <- mod2.cvd.sdfbeta[,6]
data.cvd.mod2$resid.inc3 <- mod2.cvd.sdfbeta[,7]
data.cvd.mod2$resid.inc4 <- mod2.cvd.sdfbeta[,8]
data.cvd.mod2$resid.inc5 <- mod2.cvd.sdfbeta[,9]
data.cvd.mod2$resid.sex <- mod2.cvd.sdfbeta[,10]
data.cvd.mod2$resid.ins1 <- mod2.cvd.sdfbeta[,11]
data.cvd.mod2$resid.ins2 <- mod2.cvd.sdfbeta[,12]
data.cvd.mod2$resid.ins3 <- mod2.cvd.sdfbeta[,13]
data.cvd.mod2$resid.ins4 <- mod2.cvd.sdfbeta[,14]
data.cvd.mod2$resid.ins5 <- mod2.cvd.sdfbeta[,15]
data.cvd.mod2$resid.race2 <- mod2.cvd.sdfbeta[,16]
data.cvd.mod2$resid.race3 <- mod2.cvd.sdfbeta[,17]
data.cvd.mod2$resid.race4 <- mod2.cvd.sdfbeta[,18]
data.cvd.mod2$resid.race5 <- mod2.cvd.sdfbeta[,19]
data.cvd.mod2$resid.race6 <- mod2.cvd.sdfbeta[,20]

data.cvd.mod2$id_num <- 1:nrow(data.cvd.mod2)

#plot these sdfbetas
ggplot(data.cvd.mod2, aes(y = resid.crn, x= id_num)) + geom_point() #over .2
ggplot(data.cvd.mod2, aes(y = resid.edu2, x= id_num)) + geom_point() #> .2
ggplot(data.cvd.mod2, aes(y = resid.edu3, x= id_num)) + geom_point() # > .2
ggplot(data.cvd.mod2, aes(y = resid.age, x= id_num)) + geom_point() # <-.2
ggplot(data.cvd.mod2, aes(y = resid.inc1, x= id_num)) + geom_point()# < -.1
ggplot(data.cvd.mod2, aes(y = resid.inc2, x= id_num)) + geom_point() #>.15 
ggplot(data.cvd.mod2, aes(y = resid.inc3, x= id_num)) + geom_point() # <-.2
ggplot(data.cvd.mod2, aes(y = resid.inc4, x= id_num)) + geom_point()
ggplot(data.cvd.mod2, aes(y = resid.inc5, x= id_num)) + geom_point() #>.12
ggplot(data.cvd.mod2, aes(y = resid.sex, x= id_num)) + geom_point() #abs(.1)
ggplot(data.cvd.mod2, aes(y = resid.ins1, x= id_num)) + geom_point() #<-.2
ggplot(data.cvd.mod2, aes(y = resid.ins2, x= id_num)) + geom_point() # abs(.25)
ggplot(data.cvd.mod2, aes(y = resid.ins3, x= id_num)) + geom_point() #abs(.2)
ggplot(data.cvd.mod2, aes(y = resid.ins4, x= id_num)) + geom_point() # >.3
ggplot(data.cvd.mod2, aes(y = resid.ins5, x= id_num)) + geom_point() # abs(.11)
ggplot(data.cvd.mod2, aes(y = resid.race2, x= id_num)) + geom_point() #abs(.1)
ggplot(data.cvd.mod2, aes(y = resid.race3, x= id_num)) + geom_point() 
ggplot(data.cvd.mod2, aes(y = resid.race4, x= id_num)) + geom_point() #>2
ggplot(data.cvd.mod2, aes(y = resid.race5, x= id_num)) + geom_point() #>.3
ggplot(data.cvd.mod2, aes(y = resid.race6, x= id_num)) + geom_point() #>.25

#generate a summary measure of how many variables each obs
#was influential on
data.cvd.mod2 <- data.cvd.mod2 %>%
  mutate(influence.meas = (resid.race6 > 0.25) + (resid.race4 > .2) +
           (abs(resid.race2) > .1) +
           (abs(resid.ins5) > .11) + (resid.ins4 > .3) + (abs(resid.ins3) > .2) +
           (abs(resid.ins2) > .25) + (resid.ins1 < (0-.2)) + (abs(resid.sex) > .1) +
           (resid.inc5 > .12) + (resid.inc3 < (0-.2)) + (resid.inc2 > .15) + (resid.inc1 < (0-.1)) +
           (resid.age < (0-.2)) + (resid.edu3 > .2) + (resid.edu2 > .2) + (resid.crn > .2))


table(data.cvd.mod2$influence.meas)

possible.inf.cvd <- data.cvd.mod2[data.cvd.mod2$influence.meas >=1,]
#write out to csv
write_csv(possible.inf.cvd, "data//possible_inf_cvd.csv")
table(possible.inf.cvd$CRN)
table(possible.inf.cvd$cvdMort)
table(possible.inf.cvd$AGE)
table(possible.inf.cvd$RaceR)
table(possible.inf.cvd$InsType)
table(possible.inf.cvd$EduR)
table(possible.inf.cvd$SEX)
table(possible.inf.cvd$CancerEvBin)
table(possible.inf.cvd$DiabetesRec)
#nothing especially notable about these. Nothing seems like it's awry: leave them in
#possibly just those with more comorbidity: a lot have dm and cancer

cvd.not.inf <- data.cvd.mod2[data.cvd.mod2$influence.meas == 0,]
#refit model just to test
cvd.not.inf.svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight14,
                             nest = TRUE, data = cvd.not.inf)

cvd.not.inf.svy10 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight10,
                             nest = TRUE, data = cvd.not.inf)
cvd.not.inf.svy10 <- subset(cvd.not.inf.svy10, YEAR <= 2010)

cvd.not.inf.svy5 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight5,
                               nest = TRUE, data = cvd.not.inf)
cvd.not.inf.svy5 <- subset(cvd.not.inf.svy5, YEAR > 2010)

#all years
svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+ AGE +
        factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
      design = cvd.not.inf.svy) %>% #not much change
  summary()

svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN),
         design = cvd.not.inf.svy) %>%  #not much change
  summary()

#early
svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
         design = cvd.not.inf.svy10) %>% #not much change
  summary()

svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN),
         design = cvd.not.inf.svy10) %>%  #not much change
  summary()
#late
svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
         design = cvd.not.inf.svy5) %>% #not much change
  summary()

svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN),
         design = cvd.not.inf.svy5) %>%  #not much change
  summary()

quantile(cvd.not.inf.svy$variables$fuTime)
quantile(cvd.not.inf.svy10$variables$fuTime)
quantile(cvd.not.inf.svy5$variables$fuTime)

#############################################################################
#do same for cvdht
#unadjusted model cvdht
mod1.cvdht.sdfbeta <- resid(mod1.cvdht.sa, type = "dfbetas")
plot(mod1.cvdht.sdfbeta)
infcase1 <- cvdht.mort14.fin.sa$variables[mod1.cvdht.sdfbeta > 0.10,]

#would exclude these cases by SERIAL identifier... however, there doesn't
#seem to be anything unusual about them. I'd leave them in.
table(infcase1$cvdhtMort)
table(infcase1$SEX)
median(infcase1$fuTime)
table(infcase1$YEAR)
table(infcase1$CRN)

#adjusted model cvdht
mod2.cvdht.sdfbeta <- resid(mod2.cvdht.sa, type = "dfbetas")

data.cvdht.mod2 <- cvdht.mort14.fin.sa$variables %>%
  drop_na(SEX, CRN, cvdHtMort, RaceR, IncomeR, InsType, EduR, AGE)

data.cvdht.mod2$resid.crn <- mod2.cvdht.sdfbeta[,1]
data.cvdht.mod2$resid.edu2 <- mod2.cvdht.sdfbeta[,2]
data.cvdht.mod2$resid.edu3 <- mod2.cvdht.sdfbeta[,3]
data.cvdht.mod2$resid.age <- mod2.cvdht.sdfbeta[,4]
data.cvdht.mod2$resid.inc1 <- mod2.cvdht.sdfbeta[,5]
data.cvdht.mod2$resid.inc2 <- mod2.cvdht.sdfbeta[,6]
data.cvdht.mod2$resid.inc3 <- mod2.cvdht.sdfbeta[,7]
data.cvdht.mod2$resid.inc4 <- mod2.cvdht.sdfbeta[,8]
data.cvdht.mod2$resid.inc5 <- mod2.cvdht.sdfbeta[,9]
data.cvdht.mod2$resid.sex <- mod2.cvdht.sdfbeta[,10]
data.cvdht.mod2$resid.ins1 <- mod2.cvdht.sdfbeta[,11]
data.cvdht.mod2$resid.ins2 <- mod2.cvdht.sdfbeta[,12]
data.cvdht.mod2$resid.ins3 <- mod2.cvdht.sdfbeta[,13]
data.cvdht.mod2$resid.ins4 <- mod2.cvdht.sdfbeta[,14]
data.cvdht.mod2$resid.ins5 <- mod2.cvdht.sdfbeta[,15]
data.cvdht.mod2$resid.race2 <- mod2.cvdht.sdfbeta[,16]
data.cvdht.mod2$resid.race3 <- mod2.cvdht.sdfbeta[,17]
data.cvdht.mod2$resid.race4 <- mod2.cvdht.sdfbeta[,18]
data.cvdht.mod2$resid.race5 <- mod2.cvdht.sdfbeta[,19]
data.cvdht.mod2$resid.race6 <- mod2.cvdht.sdfbeta[,20]


data.cvdht.mod2$id_num <- 1:nrow(data.cvdht.mod2)

#plot these sdfbetas
ggplot(data.cvdht.mod2, aes(y = resid.crn, x= id_num)) + geom_point() #over .2
ggplot(data.cvdht.mod2, aes(y = resid.edu2, x= id_num)) + geom_point() #> .1
ggplot(data.cvdht.mod2, aes(y = resid.edu3, x= id_num)) + geom_point() # > .15
ggplot(data.cvdht.mod2, aes(y = resid.age, x= id_num)) +  geom_point() # <-.12
ggplot(data.cvdht.mod2, aes(y = resid.inc1, x= id_num)) + geom_point()# abs(.1)
ggplot(data.cvdht.mod2, aes(y = resid.inc2, x= id_num)) + geom_point() # abs(.1)
ggplot(data.cvdht.mod2, aes(y = resid.inc3, x= id_num)) + geom_point() # abs(.15)
ggplot(data.cvdht.mod2, aes(y = resid.inc4, x= id_num)) + geom_point() #.1
ggplot(data.cvdht.mod2, aes(y = resid.inc5, x= id_num)) + geom_point() #>.2
ggplot(data.cvdht.mod2, aes(y = resid.sex, x= id_num)) +  geom_point()  #abs(.07)
ggplot(data.cvdht.mod2, aes(y = resid.ins1, x= id_num)) + geom_point() #< -.15 or >.1
ggplot(data.cvdht.mod2, aes(y = resid.ins2, x= id_num)) + geom_point() # abs(.1)
ggplot(data.cvdht.mod2, aes(y = resid.ins3, x= id_num)) + geom_point() #abs(.2)
ggplot(data.cvdht.mod2, aes(y = resid.ins4, x= id_num)) + geom_point() #  < -.15
ggplot(data.cvdht.mod2, aes(y = resid.ins5, x= id_num)) + geom_point() # abs(.1)
ggplot(data.cvdht.mod2, aes(y = resid.race2, x= id_num)) + geom_point() #abs(.1)
ggplot(data.cvdht.mod2, aes(y = resid.race3, x= id_num)) + geom_point() #>.15
ggplot(data.cvdht.mod2, aes(y = resid.race4, x= id_num)) + geom_point() #>.8
ggplot(data.cvdht.mod2, aes(y = resid.race5, x= id_num)) + geom_point() 
ggplot(data.cvdht.mod2, aes(y = resid.race6, x= id_num)) + geom_point() #>.1

#generate a summary measure of how many variables each obs
#was influential on
data.cvdht.mod2 <- data.cvdht.mod2 %>%
  mutate(influence.meas = (resid.race6 > 0.1)  + (resid.race4 > .8) +
           (resid.race3 > .15) +
           (abs(resid.race2) > .1) +
           (abs(resid.ins5) > .1) + (resid.ins4 < (0-.15)) + (abs(resid.ins3) > .2) +
           (abs(resid.ins2) > .1) + (resid.ins1 < (0-.15) | resid.ins1 > .1) + (abs(resid.sex) > .07) +
           (resid.inc5 > .2) + (abs(resid.inc3) < .15) + (abs(resid.inc2) > .1) + 
           (abs(resid.inc1) > .1) + (resid.inc4 > .1) +
           (resid.age < (0-.12)) + (resid.edu3 > .15) + (resid.edu2 > .1) + (resid.crn > .2)
         )

table(data.cvdht.mod2$influence.meas)

possible.inf.cvdht <- data.cvdht.mod2[data.cvdht.mod2$influence.meas >=2,]
#write to csv
write_csv(possible.inf.cvdht, "data\\possible_inf_cvdht.csv")

table(possible.inf.cvdht$CRN)
table(possible.inf.cvdht$cvdHtMort)
table(possible.inf.cvdht$AGE)
table(possible.inf.cvdht$RaceR)
table(possible.inf.cvdht$InsType)
table(possible.inf.cvdht$EduR)
table(possible.inf.cvdht$SEX)
table(possible.inf.cvdht$CancerEvBin)
table(possible.inf.cvdht$DiabetesRec)
#nothing especially notable about these. Nothing seems like it's awry: leave them in
#possibly just those with more comorbidity: a lot have dm and cancer

cvdht.not.inf <- data.cvdht.mod2[data.cvdht.mod2$influence.meas <2,]
#refit model just to test
cvdht.not.inf.svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight14,
                             nest = TRUE, data = cvdht.not.inf)

cvdht.not.inf.svy10 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight10,
                               nest = TRUE, data = cvdht.not.inf)
cvdht.not.inf.svy10 <- subset(cvdht.not.inf.svy10, YEAR <= 2010)

cvdht.not.inf.svy5 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight5,
                                 nest = TRUE, data = cvdht.not.inf)
cvdht.not.inf.svy5 <- subset(cvdht.not.inf.svy5, YEAR >2010)

svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
         design = cvdht.not.inf.svy) #not much change

svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN),
         design = cvdht.not.inf.svy) #not much change

#early years
svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
         design = cvdht.not.inf.svy10) %>% summary() #not much change

svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN),
         design = cvdht.not.inf.svy10) %>%  summary() #not much change

#late
svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
         design = cvdht.not.inf.svy5) %>% summary() #not much change

svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN),
         design = cvdht.not.inf.svy5) %>%  summary() #not much change

quantile(cvdht.not.inf.svy$variables$fuTime)
quantile(cvdht.not.inf.svy10$variables$fuTime)
quantile(cvdht.not.inf.svy5$variables$fuTime)

##################################################################
#adjusted model dm all cause
mod2.diab.sdfbeta <- resid(mod2.diab.allcause, type = "dfbetas")
dim(mod2.diab.sdfbeta)

data.dm.mod2 <- diab.mort14.fin.sa$variables %>%
  drop_na(SEX, CRN, allCauseMort, RaceR, IncomeR, InsType, EduR, AGE, CancerEvBin, AnyCVDHT)

data.dm.mod2$resid.crn <- mod2.diab.sdfbeta[,1]
data.dm.mod2$resid.edu2 <- mod2.diab.sdfbeta[,2]
data.dm.mod2$resid.edu3 <- mod2.diab.sdfbeta[,3]
data.dm.mod2$resid.age <- mod2.diab.sdfbeta[,4]
data.dm.mod2$resid.inc1 <- mod2.diab.sdfbeta[,5]
data.dm.mod2$resid.inc2 <- mod2.diab.sdfbeta[,6]
data.dm.mod2$resid.inc3 <- mod2.diab.sdfbeta[,7]
data.dm.mod2$resid.inc4 <- mod2.diab.sdfbeta[,8]
data.dm.mod2$resid.inc5 <- mod2.diab.sdfbeta[,9]
data.dm.mod2$resid.sex <- mod2.diab.sdfbeta[,10]
data.dm.mod2$resid.ins1 <- mod2.diab.sdfbeta[,11]
data.dm.mod2$resid.ins2 <- mod2.diab.sdfbeta[,12]
data.dm.mod2$resid.ins3 <- mod2.diab.sdfbeta[,13]
data.dm.mod2$resid.ins4 <- mod2.diab.sdfbeta[,14]
data.dm.mod2$resid.ins5 <- mod2.diab.sdfbeta[,15]
data.dm.mod2$resid.race2 <- mod2.diab.sdfbeta[,16]
data.dm.mod2$resid.race3 <- mod2.diab.sdfbeta[,17]
data.dm.mod2$resid.race4 <- mod2.diab.sdfbeta[,18]
data.dm.mod2$resid.race5 <- mod2.diab.sdfbeta[,19]
data.dm.mod2$resid.race6 <- mod2.diab.sdfbeta[,20]
data.dm.mod2$resid.canc <- mod2.diab.sdfbeta[,21]
data.dm.mod2$resid.cvdh <- mod2.diab.sdfbeta[,22]


data.dm.mod2$id_num <- 1:nrow(data.dm.mod2)



#plot these sdfbetas
plot(data.dm.mod2$resid.crn, x= data.dm.mod2$id_num)
plot(y = data.dm.mod2$resid.edu2, x = data.dm.mod2$id_num) 
plot(y = data.dm.mod2$resid.edu3, x = data.dm.mod2$id_num) #<-.07
plot(y = data.dm.mod2$resid.age, x = data.dm.mod2$id_num) # <-.07
plot(y = data.dm.mod2$resid.inc1, x = data.dm.mod2$id_num)#>.15
plot(y = data.dm.mod2$resid.inc2, x = data.dm.mod2$id_num) #abs(.1) 
plot(y = data.dm.mod2$resid.inc3, x = data.dm.mod2$id_num) #<-.1
plot(y = data.dm.mod2$resid.inc4, x = data.dm.mod2$id_num) # > .05 or <-.1
plot(y = data.dm.mod2$resid.inc5, x = data.dm.mod2$id_num) #>.3
plot(y = data.dm.mod2$resid.sex, x = data.dm.mod2$id_num) #>.1
plot(y = data.dm.mod2$resid.ins1, x = data.dm.mod2$id_num) #<-.1
plot(y = data.dm.mod2$resid.ins2, x = data.dm.mod2$id_num) # <-.1
plot(y = data.dm.mod2$resid.ins3, x = data.dm.mod2$id_num) #abs(.1)
plot(y = data.dm.mod2$resid.ins4, x = data.dm.mod2$id_num) #<-.1
plot(y = data.dm.mod2$resid.ins5, x = data.dm.mod2$id_num) #>.18
plot(y = data.dm.mod2$resid.race2, x = data.dm.mod2$id_num) 
plot(y = data.dm.mod2$resid.race3, x = data.dm.mod2$id_num) # >.1
plot(y = data.dm.mod2$resid.race4, x = data.dm.mod2$id_num) #>.2
plot(y = data.dm.mod2$resid.race5, x = data.dm.mod2$id_num)
plot(y = data.dm.mod2$resid.race6, x = data.dm.mod2$id_num) #abs(.05)
#generate a summary measure of how many variables each obs
#was influential on
data.dm.mod2 <- data.dm.mod2 %>%
  mutate(influence.meas = (abs(resid.race6) > 0.05) + (resid.race4 > .2) +
           (resid.race3 > .1) +
           (resid.ins5 > .18) + (resid.ins4 < (0-.1)) + (abs(resid.ins3) > .1) +
           (resid.ins2 < (0-.1)) + (resid.ins1 < (0-.1)) + (resid.sex > .1) +
           (resid.inc5 > .3) + (resid.inc3 < (0-.1)) + (abs(resid.inc2) > .1) + (resid.inc1 > .15) +
           (resid.age < (0-.07)) + (abs(resid.edu3) < (0-.07)))

table(data.dm.mod2$influence.meas)
#65 are influential on just 1 measure, 5 on 2, 3 on 3, 3 on 4, and 2 on 6

possible.inf <- data.dm.mod2[data.dm.mod2$influence.meas >=1,]
#write it out
write_csv(possible.inf, "data\\possible_inf_diabAC.csv")

table(possible.inf$CRN)
table(possible.inf$allCauseMort)
table(possible.inf$AGE)
table(possible.inf$RaceR)
table(possible.inf$InsType)
table(possible.inf$EduR)
table(possible.inf$SEX)
table(possible.inf$CancerEvBin)
table(possible.inf$AnyCVD)
table(possible.inf$AnyCVDHT)
#nothing especially notable about these. Nothing seems like it's awry: leave them in
#possibly 10% with cancer, 60% with cvdht, 40% with cvd, college grads all ages

diab.not.inf <- data.dm.mod2[data.dm.mod2$influence.meas == 0,]
#refit model just to test
diab.not.inf.svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight14,
                              nest = TRUE, data = diab.not.inf)
diab.not.inf.svy10 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight10,
                              nest = TRUE, data = diab.not.inf)
diab.not.inf.svy5 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight5,
                              nest = TRUE, data = diab.not.inf)
diab.not.inf.svy10 <- subset(diab.not.inf.svy10, YEAR <= 2010)
diab.not.inf.svy5 <- subset(diab.not.inf.svy5, YEAR > 2010)

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
           factor(CancerEvBin) + factor(AnyCVDHT),
         design = diab.not.inf.svy) #doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
         design = diab.not.inf.svy) #doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
           factor(CancerEvBin) + factor(AnyCVDHT),
         design = diab.not.inf.svy10) %>% summary() #doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
         design = diab.not.inf.svy10) %>% summary() #doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
           factor(CancerEvBin) + factor(AnyCVDHT),
         design = diab.not.inf.svy5) %>% summary() #doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
         design = diab.not.inf.svy5) %>% summary() #doesn't really change things

##################################################################
#adjusted model cvd all cause
mod2.cvd.sdfbeta <- resid(mod2.cvd.allcause, type = "dfbetas")
dim(mod2.cvd.sdfbeta)

data.cvd.mod2 <- cvd.mort14.fin.sa$variables %>%
  drop_na(SEX, CRN, allCauseMort, RaceR, IncomeR, InsType, EduR, AGE, CancerEvBin, DiabetesRec,
          HyperTen)

data.cvd.mod2$resid.crn <- mod2.cvd.sdfbeta[,1]
data.cvd.mod2$resid.edu2 <- mod2.cvd.sdfbeta[,2]
data.cvd.mod2$resid.edu3 <- mod2.cvd.sdfbeta[,3]
data.cvd.mod2$resid.age <- mod2.cvd.sdfbeta[,4]
data.cvd.mod2$resid.inc1 <- mod2.cvd.sdfbeta[,5]
data.cvd.mod2$resid.inc2 <- mod2.cvd.sdfbeta[,6]
data.cvd.mod2$resid.inc3 <- mod2.cvd.sdfbeta[,7]
data.cvd.mod2$resid.inc4 <- mod2.cvd.sdfbeta[,8]
data.cvd.mod2$resid.inc5 <- mod2.cvd.sdfbeta[,9]
data.cvd.mod2$resid.sex <- mod2.cvd.sdfbeta[,10]
data.cvd.mod2$resid.ins1 <- mod2.cvd.sdfbeta[,11]
data.cvd.mod2$resid.ins2 <- mod2.cvd.sdfbeta[,12]
data.cvd.mod2$resid.ins3 <- mod2.cvd.sdfbeta[,13]
data.cvd.mod2$resid.ins4 <- mod2.cvd.sdfbeta[,14]
data.cvd.mod2$resid.ins5 <- mod2.cvd.sdfbeta[,15]
data.cvd.mod2$resid.race2 <- mod2.cvd.sdfbeta[,16]
data.cvd.mod2$resid.race3 <- mod2.cvd.sdfbeta[,17]
data.cvd.mod2$resid.race4 <- mod2.cvd.sdfbeta[,18]
data.cvd.mod2$resid.race5 <- mod2.cvd.sdfbeta[,19]
data.cvd.mod2$resid.race6 <- mod2.cvd.sdfbeta[,20]
data.cvd.mod2$resid.canc <- mod2.cvd.sdfbeta[,21]
data.cvd.mod2$resid.cvdh <- mod2.cvd.sdfbeta[,22]


data.cvd.mod2$id_num <- 1:nrow(data.cvd.mod2)

#plot these sdfbetas
plot(y = data.cvd.mod2$resid.crn, x = data.cvd.mod2$id_num) #abs(.1)
plot(y = data.cvd.mod2$resid.edu2, x = data.cvd.mod2$id_num) #abs(.1)
plot(y = data.cvd.mod2$resid.edu3, x = data.cvd.mod2$id_num) #>.1
plot(y = data.cvd.mod2$resid.age, x = data.cvd.mod2$id_num) # abs(.1)
plot(y = data.cvd.mod2$resid.inc1, x = data.cvd.mod2$id_num)# < -.1
plot(y = data.cvd.mod2$resid.inc2, x = data.cvd.mod2$id_num) #abs(.1) 
plot(y = data.cvd.mod2$resid.inc3, x = data.cvd.mod2$id_num) #>.2
plot(y = data.cvd.mod2$resid.inc4, x = data.cvd.mod2$id_num) # > .4
plot(y = data.cvd.mod2$resid.inc5, x = data.cvd.mod2$id_num) #>.15
plot(y = data.cvd.mod2$resid.sex, x = data.cvd.mod2$id_num) #>.1
plot(y = data.cvd.mod2$resid.ins1, x = data.cvd.mod2$id_num) #<-.1
plot(y = data.cvd.mod2$resid.ins2, x = data.cvd.mod2$id_num) # <-.18
plot(y = data.cvd.mod2$resid.ins3, x = data.cvd.mod2$id_num) #abs(.1)
plot(y = data.cvd.mod2$resid.ins4, x = data.cvd.mod2$id_num) #<-.15
plot(y = data.cvd.mod2$resid.ins5, x = data.cvd.mod2$id_num) #>.4
plot(y = data.cvd.mod2$resid.race2, x = data.cvd.mod2$id_num) #abs(.1)
plot(y = data.cvd.mod2$resid.race3, x = data.cvd.mod2$id_num) # >.2
plot(y = data.cvd.mod2$resid.race4, x = data.cvd.mod2$id_num) #abs(.3)
plot(y = data.cvd.mod2$resid.race5, x = data.cvd.mod2$id_num) #>.2
plot(y = data.cvd.mod2$resid.race6, x = data.cvd.mod2$id_num) #abs(.05)
#generate a summary measure of how many variables each obs
#was influential on
data.cvd.mod2 <- data.cvd.mod2 %>%
  mutate(influence.meas = (abs(resid.race6) > 0.05) +(abs(resid.race5) > .2) + (abs(resid.race4) > .3) +
           (resid.race3 > .2) + (abs(resid.race2) > .1) + 
           (abs(resid.ins5) > .4) + (resid.ins4 < (0-.15)) + (abs(resid.ins3) > .1) +
           (resid.ins2 < (0-.18)) + (resid.ins1 < (0-.1)) + (resid.sex > .1) +(resid.inc5 >.15) +
           (resid.inc4 > .4) + (resid.inc3 >.2) + (abs(resid.inc2) > .1) + (resid.inc1 < (0-.1)) +
           (abs(resid.age) > (.1)) + (resid.edu3 >.1) +(abs(resid.edu2) >.1)+(abs(resid.crn) >.1))

table(data.cvd.mod2$influence.meas)
#121 are influential on just 1 measure, 14 on 2, 11 on 3, 5 on 4, and 2 on 6

possible.inf <- data.cvd.mod2[data.cvd.mod2$influence.meas >=1,]
#write it out to csv
write_csv(possible.inf, "data\\possible_inf_cvd_allcause.csv")

table(possible.inf$CRN)
table(possible.inf$allCauseMort)
table(possible.inf$AGE)
table(possible.inf$RaceR)
table(possible.inf$InsType)
table(possible.inf$EduR)
table(possible.inf$SEX)
table(possible.inf$CancerEvBin)
table(possible.inf$DiabetesRec)
median(possible.inf$fuTime)
#nothing especially notable about these. Nothing seems like it's awry: leave them in
#possibly 10% with cancer, 60% with cvdht, 40% with cvd, college grads all ages

cvd.not.inf <- data.cvd.mod2[data.cvd.mod2$influence.meas == 0,]
#refit model just to test
cvd.not.inf.svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight14,
                              nest = TRUE, data = cvd.not.inf)
cvd.not.inf.svy10 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight10,
                             nest = TRUE, data = cvd.not.inf)
cvd.not.inf.svy5 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight5,
                             nest = TRUE, data = cvd.not.inf)

cvd.not.inf.svy10 <- subset(cvd.not.inf.svy10, YEAR <= 2010)
cvd.not.inf.svy5 <- subset(cvd.not.inf.svy5, YEAR > 2010)

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
           factor(CancerEvBin) + factor(DiabetesRec) + factor(HyperTen),
         design = cvd.not.inf.svy) #doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
         design = cvd.not.inf.svy) #doesn't really change things


svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
           factor(CancerEvBin) + factor(DiabetesRec) + factor(HyperTen),
         design = cvd.not.inf.svy10) %>% summary() #doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
         design = cvd.not.inf.svy10) %>% summary() #doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
           factor(CancerEvBin) + factor(DiabetesRec) + factor(HyperTen),
         design = cvd.not.inf.svy5) %>% summary() #doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
         design = cvd.not.inf.svy5) %>% summary() #doesn't really change things

##################################################################
#adjusted model cvdht all cause
mod2.cvdht.sdfbeta <- resid(mod2.cvdht.allcause, type = "dfbetas")
dim(mod2.cvdht.sdfbeta)

data.cvdht.mod2 <- cvdht.mort14.fin.sa$variables %>%
  drop_na(SEX, CRN, allCauseMort, RaceR, IncomeR, InsType, EduR, AGE, CancerEvBin, DiabetesRec)

data.cvdht.mod2$resid.crn <- mod2.cvdht.sdfbeta[,1]
data.cvdht.mod2$resid.edu2 <- mod2.cvdht.sdfbeta[,2]
data.cvdht.mod2$resid.edu3 <- mod2.cvdht.sdfbeta[,3]
data.cvdht.mod2$resid.age <- mod2.cvdht.sdfbeta[,4]
data.cvdht.mod2$resid.inc1 <- mod2.cvdht.sdfbeta[,5]
data.cvdht.mod2$resid.inc2 <- mod2.cvdht.sdfbeta[,6]
data.cvdht.mod2$resid.inc3 <- mod2.cvdht.sdfbeta[,7]
data.cvdht.mod2$resid.inc4 <- mod2.cvdht.sdfbeta[,8]
data.cvdht.mod2$resid.inc5 <- mod2.cvdht.sdfbeta[,9]
data.cvdht.mod2$resid.sex <- mod2.cvdht.sdfbeta[,10]
data.cvdht.mod2$resid.ins1 <- mod2.cvdht.sdfbeta[,11]
data.cvdht.mod2$resid.ins2 <- mod2.cvdht.sdfbeta[,12]
data.cvdht.mod2$resid.ins3 <- mod2.cvdht.sdfbeta[,13]
data.cvdht.mod2$resid.ins4 <- mod2.cvdht.sdfbeta[,14]
data.cvdht.mod2$resid.ins5 <- mod2.cvdht.sdfbeta[,15]
data.cvdht.mod2$resid.race2 <- mod2.cvdht.sdfbeta[,16]
data.cvdht.mod2$resid.race3 <- mod2.cvdht.sdfbeta[,17]
data.cvdht.mod2$resid.race4 <- mod2.cvdht.sdfbeta[,18]
data.cvdht.mod2$resid.race5 <- mod2.cvdht.sdfbeta[,19]
data.cvdht.mod2$resid.race6 <- mod2.cvdht.sdfbeta[,20]
data.cvdht.mod2$resid.canc <- mod2.cvdht.sdfbeta[,21]
data.cvdht.mod2$resid.cvdhth <- mod2.cvdht.sdfbeta[,22]

#add id number
data.cvdht.mod2$id_num <- 1:nrow(data.cvdht.mod2)

#plot these sdfbetas
plot(y = data.cvdht.mod2$resid.crn, x = data.cvdht.mod2$id_num) #abs(.1)
plot(y = data.cvdht.mod2$resid.edu2, x = data.cvdht.mod2$id_num) #>.15
plot(y = data.cvdht.mod2$resid.edu3, x = data.cvdht.mod2$id_num) #> (abs.05)
plot(y = data.cvdht.mod2$resid.age, x = data.cvdht.mod2$id_num) # abs(.1)
plot(y = data.cvdht.mod2$resid.inc1, x = data.cvdht.mod2$id_num)# < -.1
plot(y = data.cvdht.mod2$resid.inc2, x = data.cvdht.mod2$id_num) #abs(.05) 
plot(y = data.cvdht.mod2$resid.inc3, x = data.cvdht.mod2$id_num) #abs(.1)
plot(y = data.cvdht.mod2$resid.inc4, x = data.cvdht.mod2$id_num) # > .1
plot(y = data.cvdht.mod2$resid.inc5, x = data.cvdht.mod2$id_num) #abs(.2)
plot(y = data.cvdht.mod2$resid.sex, x = data.cvdht.mod2$id_num) #>.2
plot(y = data.cvdht.mod2$resid.ins1, x = data.cvdht.mod2$id_num) #>.07
plot(y = data.cvdht.mod2$resid.ins2, x = data.cvdht.mod2$id_num) # abs(.1)
plot(y = data.cvdht.mod2$resid.ins3, x = data.cvdht.mod2$id_num) #abs(.1)
plot(y = data.cvdht.mod2$resid.ins4, x = data.cvdht.mod2$id_num) #>.1
plot(y = data.cvdht.mod2$resid.ins5, x = data.cvdht.mod2$id_num) 
plot(y = data.cvdht.mod2$resid.race2, x = data.cvdht.mod2$id_num) #>.4
plot(y = data.cvdht.mod2$resid.race3, x = data.cvdht.mod2$id_num) # >.1
plot(y = data.cvdht.mod2$resid.race4, x = data.cvdht.mod2$id_num) #>.15
plot(y = data.cvdht.mod2$resid.race5, x = data.cvdht.mod2$id_num) #abs(.2)
plot(y = data.cvdht.mod2$resid.race6, x = data.cvdht.mod2$id_num) #abs(.2)
#generate a summary measure of how many variables each obs
#was influential on
data.cvdht.mod2 <- data.cvdht.mod2 %>%
  mutate(influence.meas = (abs(resid.race6) > .2) +(abs(resid.race5) > .2) + (abs(resid.race4) > .15) +
           (resid.race3 > .1) + (resid.race2 > .4) + 
           (resid.ins4 > .1) + (abs(resid.ins3) > .1) +
           (abs(resid.ins2) > .1) + (resid.ins1 >.07) + (resid.sex > .2) + (resid.inc5 >.15) +
           (resid.inc4 > .1) + (abs(resid.inc3) >.1) + (abs(resid.inc2) > .05) + (resid.inc1 < (0-.1)) +
           (abs(resid.age) > (.1)) + (abs(resid.edu3) >.05) +(abs(resid.edu2) >.15)+(abs(resid.crn) >.1))

table(data.cvdht.mod2$influence.meas)
#89 are influential on just 1 measure, 21 on 2, 9 on 3, 3 on 4, and 1 on 6

possible.inf <- data.cvdht.mod2[data.cvdht.mod2$influence.meas >=1,]
write.csv(possible.inf, "data\\possible_inf_cvdht_allcause.csv")

table(possible.inf$CRN)
table(possible.inf$allCauseMort)
table(possible.inf$AGE)
table(possible.inf$RaceR)
table(possible.inf$InsType)
table(possible.inf$EduR)
table(possible.inf$SEX)
table(possible.inf$CancerEvBin)
table(possible.inf$DiabetesRec)
median(possible.inf$fuTime)
#nothing especially notable about these. Nothing seems like it's awry

cvdht.not.inf <- data.cvdht.mod2[data.cvdht.mod2$influence.meas == 0,]
#refit model just to test
cvdht.not.inf.svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight14,
                             nest = TRUE, data = cvdht.not.inf)
cvdht.not.inf.svy10 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight10,
                               nest = TRUE, data = cvdht.not.inf)
cvdht.not.inf.svy5 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight5,
                               nest = TRUE, data = cvdht.not.inf)

cvdht.not.inf.svy10 <- subset(cvdht.not.inf.svy10, YEAR <= 2010)
cvdht.not.inf.svy5 <- subset(cvdht.not.inf.svy5, YEAR > 2010)

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
           factor(CancerEvBin) + factor(DiabetesRec),
         design = cvdht.not.inf.svy) #doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
         design = cvdht.not.inf.svy) #doesn't really change things


svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
           factor(CancerEvBin) + factor(DiabetesRec),
         design = cvdht.not.inf.svy10) %>% summary() #doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
         design = cvdht.not.inf.svy10) %>% summary() #doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
           factor(CancerEvBin) + factor(DiabetesRec),
         design = cvdht.not.inf.svy5) %>% summary() #doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
         design = cvdht.not.inf.svy5) %>% summary() #doesn't really change things

