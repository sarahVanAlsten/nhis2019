##################################################################
#Sarah Van Alsten
#Created: Feb 1, 2019
#check assumptions of coxph :influence via sdfbeta
#Use data/design from 20190928_NHIS.R and 20191213_table1_analysis.R
#and models from 20200112_coxph_withRace.R before testing assumptions
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
  drop_na(SEX, CRN, diabMort, RaceR, IncomeR, InsType, EduR, AGE, AnyCVDHT, CancerEvBin)

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
data.dm.mod2$resid.cancer <- mod2.diab.sdfbeta[,21]
data.dm.mod2$resid.cvdht <- mod2.diab.sdfbeta[,22]

data.dm.mod2$id_num <- 1:nrow(data.dm.mod2)


#plot these sdfbetas
ggplot(data.dm.mod2, aes(y = resid.crn, x= id_num)) + geom_point() #over abs(.1)
ggplot(data.dm.mod2, aes(y = resid.edu2, x= id_num)) + geom_point() #>.1
ggplot(data.dm.mod2, aes(y = resid.edu3, x= id_num)) + geom_point() #>.3
ggplot(data.dm.mod2, aes(y = resid.age, x= id_num)) + geom_point() # abs(.2)
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
ggplot(data.dm.mod2, aes(y = resid.race2, x= id_num)) + geom_point() #>.1
ggplot(data.dm.mod2, aes(y = resid.race3, x= id_num)) + geom_point() # >.12
ggplot(data.dm.mod2, aes(y = resid.race4, x= id_num)) + geom_point() #>.12
ggplot(data.dm.mod2, aes(y = resid.race5, x= id_num)) + geom_point() # > .05
ggplot(data.dm.mod2, aes(y = resid.race6, x= id_num)) + geom_point() #>.1
ggplot(data.dm.mod2, aes(y = resid.cancer, x= id_num)) + geom_point() #>abs(.1)
ggplot(data.dm.mod2, aes(y = resid.cvdht, x= id_num)) + geom_point() #<-.1

#generate a summary measure of how many variables each obs
#was influential on
data.dm.mod2 <- data.dm.mod2 %>%
  mutate(influence.meas = (resid.race6 > 0.1) + (resid.race4 > .12) + (resid.race5 > .05)+
           (resid.race3 > .12) + (resid.race2 > .1) | resid.race2 > .18) + 
           (resid.ins5 > .18) + (resid.ins4 < (0-.18)) + (abs(resid.ins3) > .2) +
           (resid.ins2 < (0-.15)) + (resid.ins1 < (0-.2)) + (resid.sex > .2) +
           (resid.inc5 > .1) + (resid.inc3 > .2) + (resid.inc2 > .3) + (resid.inc1 > .1) +
           (abs(resid.age) > .2) + (resid.edu3 > .3) + (resid.edu2 < (.1)) + (abs(resid.crn) > .1) +
           (resid.cancer > abs(.1))+ (resid.cvdht < (0-.1)))

table(data.dm.mod2$influence.meas)
#Almost all have influential on just 1 measure, 85 on 2, 12 on 3, 1 on 6, and 1 on 7

possible.inf <- data.dm.mod2[data.dm.mod2$influence.meas >1,]
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

diab.not.inf <- data.dm.mod2[data.dm.mod2$influence.meas <= 2,]

#refit model just to test
diab.not.inf.svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight14,
                             nest = TRUE, data = diab.not.inf)

diab.not.inf.svy10 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight10,
                              nest = TRUE, data = diab.not.inf)
diab.not.inf.svy10 <- subset(diab.not.inf.svy10, YEAR <= 2010)

diab.not.inf.svy5 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeightSA5,
                                nest = TRUE, data = diab.not.inf)
diab.not.inf.svy5 <- subset(diab.not.inf.svy5, YEAR > 2010)

svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(AnyCVDHT)+ factor(CancerEvBin),
         design = diab.not.inf.svy) %>% summary() #doesn't really change things

svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN),
         design = diab.not.inf.svy) %>% #doesn't really change things
  summary()

#do early and late years
svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN),
         design = diab.not.inf.svy20) %>% #doesn't really change things
  summary()

svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(CancerEvBin)+
           factor(AnyCVDHT),
         design = diab.not.inf.svy10) %>% #doesn't really change things
  summary()

svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN),
         design = diab.not.inf.svy5) %>% #doesn't really change things
  summary()

svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(CancerEvBin)+
           factor(AnyCVDHT),
         design = diab.not.inf.svy5) %>% #doesn't really change things
  summary()

quantile(diab.not.inf.svy$variables$fuTime)
quantile(diab.not.inf.svy10$variables$fuTime)
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
  drop_na(SEX, CRN, cvdMort, RaceR, IncomeR, InsType, EduR, AGE, HyperTen, DiabetesRec, CancerEvBin)

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
data.cvd.mod2$resid.ht <- mod2.cvd.sdfbeta[,21]
data.cvd.mod2$resid.canc <- mod2.cvd.sdfbeta[,22]
data.cvd.mod2$resid.diab <- mod2.cvd.sdfbeta[,23]

data.cvd.mod2$id_num <- 1:nrow(data.cvd.mod2)

#plot these sdfbetas
ggplot(data.cvd.mod2, aes(y = resid.crn, x= id_num)) + geom_point() #over .2
ggplot(data.cvd.mod2, aes(y = resid.edu2, x= id_num)) + geom_point() #> .1
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
ggplot(data.cvd.mod2, aes(y = resid.race4, x= id_num)) + geom_point() #>.2
ggplot(data.cvd.mod2, aes(y = resid.race5, x= id_num)) + geom_point() #>.3
ggplot(data.cvd.mod2, aes(y = resid.race6, x= id_num)) + geom_point() #>.25
ggplot(data.cvd.mod2, aes(y = resid.ht, x= id_num)) + geom_point() #><-.7
ggplot(data.cvd.mod2, aes(y = resid.diab, x= id_num)) + geom_point() #<-.2
ggplot(data.cvd.mod2, aes(y = resid.canc, x= id_num)) + geom_point() #<.05

#generate a summary measure of how many variables each obs
#was influential on
data.cvd.mod2 <- data.cvd.mod2 %>%
  mutate(influence.meas = (resid.race6 > 0.25) + (resid.race4 > .2) +
           (abs(resid.race2) > .1) +
           (abs(resid.ins5) > .11) + (resid.ins4 > .3) + (abs(resid.ins3) > .2) +
           (abs(resid.ins2) > .25) + (resid.ins1 < (0-.2)) + (abs(resid.sex) > .1) +
           (resid.inc5 > .12) + (resid.inc3 < (0-.2)) + (resid.inc2 > .15) + (resid.inc1 < (0-.1)) +
           (resid.age < (0-.2)) + (resid.edu3 > .2) + (resid.edu2 > .1) + (resid.crn > .2) + (resid.ht < -.7) +
           (resid.diab < -.2) + (resid.canc < -.05))


table(data.cvd.mod2$influence.meas)
#52372    73     1     4     1 

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

cvd.not.inf.svy5 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeightSA5,
                               nest = TRUE, data = cvd.not.inf)
cvd.not.inf.svy5 <- subset(cvd.not.inf.svy5, YEAR > 2010)

#all years
svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+ AGE +
        factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(HyperTen)+
          factor(DiabetesRec)+ factor(CancerEvBin),
      design = cvd.not.inf.svy) %>% #not much change
  summary()

svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN),
         design = cvd.not.inf.svy) %>%  #not much change
  summary()

#early
svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+ factor(HyperTen)+
           factor(DiabetesRec)+ factor(CancerEvBin),
         design = cvd.not.inf.svy10) %>% #not much change
  summary()

svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN),
         design = cvd.not.inf.svy10) %>%  #not much change
  summary()
#late
svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+ factor(HyperTen)+
           factor(DiabetesRec)+ factor(CancerEvBin),
         design = cvd.not.inf.svy5) %>% #not much change
  summary()

svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN),
         design = cvd.not.inf.svy5) %>%  #not much change
  summary()

quantile(cvd.not.inf.svy$variables$fuTime)
quantile(cvd.not.inf.svy10$variables$fuTime)
quantile(cvd.not.inf.svy5$variables$fuTime)

# #############################################################################
# #do same for cvdht
# #unadjusted model cvdht
# mod1.cvdht.sdfbeta <- resid(mod1.cvdht.sa, type = "dfbetas")
# plot(mod1.cvdht.sdfbeta)
# infcase1 <- cvdht.mort14.fin.sa$variables[mod1.cvdht.sdfbeta > 0.10,]
# 
# #would exclude these cases by SERIAL identifier... however, there doesn't
# #seem to be anything unusual about them. I'd leave them in.
# table(infcase1$cvdhtMort)
# table(infcase1$SEX)
# median(infcase1$fuTime)
# table(infcase1$YEAR)
# table(infcase1$CRN)
# 
# #adjusted model cvdht
# mod2.cvdht.sdfbeta <- resid(mod2.cvdht.sa, type = "dfbetas")
# 
# data.cvdht.mod2 <- cvdht.mort14.fin.sa$variables %>%
#   drop_na(SEX, CRN, cvdHtMort, RaceR, IncomeR, InsType, EduR, AGE)
# 
# data.cvdht.mod2$resid.crn <- mod2.cvdht.sdfbeta[,1]
# data.cvdht.mod2$resid.edu2 <- mod2.cvdht.sdfbeta[,2]
# data.cvdht.mod2$resid.edu3 <- mod2.cvdht.sdfbeta[,3]
# data.cvdht.mod2$resid.age <- mod2.cvdht.sdfbeta[,4]
# data.cvdht.mod2$resid.inc1 <- mod2.cvdht.sdfbeta[,5]
# data.cvdht.mod2$resid.inc2 <- mod2.cvdht.sdfbeta[,6]
# data.cvdht.mod2$resid.inc3 <- mod2.cvdht.sdfbeta[,7]
# data.cvdht.mod2$resid.inc4 <- mod2.cvdht.sdfbeta[,8]
# data.cvdht.mod2$resid.inc5 <- mod2.cvdht.sdfbeta[,9]
# data.cvdht.mod2$resid.sex <- mod2.cvdht.sdfbeta[,10]
# data.cvdht.mod2$resid.ins1 <- mod2.cvdht.sdfbeta[,11]
# data.cvdht.mod2$resid.ins2 <- mod2.cvdht.sdfbeta[,12]
# data.cvdht.mod2$resid.ins3 <- mod2.cvdht.sdfbeta[,13]
# data.cvdht.mod2$resid.ins4 <- mod2.cvdht.sdfbeta[,14]
# data.cvdht.mod2$resid.ins5 <- mod2.cvdht.sdfbeta[,15]
# data.cvdht.mod2$resid.race2 <- mod2.cvdht.sdfbeta[,16]
# data.cvdht.mod2$resid.race3 <- mod2.cvdht.sdfbeta[,17]
# data.cvdht.mod2$resid.race4 <- mod2.cvdht.sdfbeta[,18]
# data.cvdht.mod2$resid.race5 <- mod2.cvdht.sdfbeta[,19]
# data.cvdht.mod2$resid.race6 <- mod2.cvdht.sdfbeta[,20]
# 
# 
# data.cvdht.mod2$id_num <- 1:nrow(data.cvdht.mod2)
# 
# #plot these sdfbetas
# ggplot(data.cvdht.mod2, aes(y = resid.crn, x= id_num)) + geom_point() #over .2
# ggplot(data.cvdht.mod2, aes(y = resid.edu2, x= id_num)) + geom_point() #> .1
# ggplot(data.cvdht.mod2, aes(y = resid.edu3, x= id_num)) + geom_point() # > .15
# ggplot(data.cvdht.mod2, aes(y = resid.age, x= id_num)) +  geom_point() # <-.12
# ggplot(data.cvdht.mod2, aes(y = resid.inc1, x= id_num)) + geom_point()# abs(.1)
# ggplot(data.cvdht.mod2, aes(y = resid.inc2, x= id_num)) + geom_point() # abs(.1)
# ggplot(data.cvdht.mod2, aes(y = resid.inc3, x= id_num)) + geom_point() # abs(.15)
# ggplot(data.cvdht.mod2, aes(y = resid.inc4, x= id_num)) + geom_point() #.1
# ggplot(data.cvdht.mod2, aes(y = resid.inc5, x= id_num)) + geom_point() #>.2
# ggplot(data.cvdht.mod2, aes(y = resid.sex, x= id_num)) +  geom_point()  #abs(.07)
# ggplot(data.cvdht.mod2, aes(y = resid.ins1, x= id_num)) + geom_point() #< -.15 or >.1
# ggplot(data.cvdht.mod2, aes(y = resid.ins2, x= id_num)) + geom_point() # abs(.1)
# ggplot(data.cvdht.mod2, aes(y = resid.ins3, x= id_num)) + geom_point() #abs(.2)
# ggplot(data.cvdht.mod2, aes(y = resid.ins4, x= id_num)) + geom_point() #  < -.15
# ggplot(data.cvdht.mod2, aes(y = resid.ins5, x= id_num)) + geom_point() # abs(.1)
# ggplot(data.cvdht.mod2, aes(y = resid.race2, x= id_num)) + geom_point() #abs(.1)
# ggplot(data.cvdht.mod2, aes(y = resid.race3, x= id_num)) + geom_point() #>.15
# ggplot(data.cvdht.mod2, aes(y = resid.race4, x= id_num)) + geom_point() #>.8
# ggplot(data.cvdht.mod2, aes(y = resid.race5, x= id_num)) + geom_point() 
# ggplot(data.cvdht.mod2, aes(y = resid.race6, x= id_num)) + geom_point() #>.1
# 
# #generate a summary measure of how many variables each obs
# #was influential on
# data.cvdht.mod2 <- data.cvdht.mod2 %>%
#   mutate(influence.meas = (resid.race6 > 0.1)  + (resid.race4 > .8) +
#            (resid.race3 > .15) +
#            (abs(resid.race2) > .1) +
#            (abs(resid.ins5) > .1) + (resid.ins4 < (0-.15)) + (abs(resid.ins3) > .2) +
#            (abs(resid.ins2) > .1) + (resid.ins1 < (0-.15) | resid.ins1 > .1) + (abs(resid.sex) > .07) +
#            (resid.inc5 > .2) + (abs(resid.inc3) < .15) + (abs(resid.inc2) > .1) + 
#            (abs(resid.inc1) > .1) + (resid.inc4 > .1) +
#            (resid.age < (0-.12)) + (resid.edu3 > .15) + (resid.edu2 > .1) + (resid.crn > .2)
#          )
# 
# table(data.cvdht.mod2$influence.meas)
# 
# possible.inf.cvdht <- data.cvdht.mod2[data.cvdht.mod2$influence.meas >=2,]
# #write to csv
# write_csv(possible.inf.cvdht, "data\\possible_inf_cvdht.csv")
# 
# table(possible.inf.cvdht$CRN)
# table(possible.inf.cvdht$cvdHtMort)
# table(possible.inf.cvdht$AGE)
# table(possible.inf.cvdht$RaceR)
# table(possible.inf.cvdht$InsType)
# table(possible.inf.cvdht$EduR)
# table(possible.inf.cvdht$SEX)
# table(possible.inf.cvdht$CancerEvBin)
# table(possible.inf.cvdht$DiabetesRec)
# #nothing especially notable about these. Nothing seems like it's awry: leave them in
# #possibly just those with more comorbidity: a lot have dm and cancer
# 
# cvdht.not.inf <- data.cvdht.mod2[data.cvdht.mod2$influence.meas <2,]
# #refit model just to test
# cvdht.not.inf.svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight14,
#                              nest = TRUE, data = cvdht.not.inf)
# 
# cvdht.not.inf.svy10 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight10,
#                                nest = TRUE, data = cvdht.not.inf)
# cvdht.not.inf.svy10 <- subset(cvdht.not.inf.svy10, YEAR <= 2010)
# 
# cvdht.not.inf.svy5 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight5,
#                                  nest = TRUE, data = cvdht.not.inf)
# cvdht.not.inf.svy5 <- subset(cvdht.not.inf.svy5, YEAR >2010)
# 
# svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+ AGE +
#            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
#          design = cvdht.not.inf.svy) #not much change
# 
# svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN),
#          design = cvdht.not.inf.svy) #not much change
# 
# #early years
# svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+ AGE +
#            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
#          design = cvdht.not.inf.svy10) %>% summary() #not much change
# 
# svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN),
#          design = cvdht.not.inf.svy10) %>%  summary() #not much change
# 
# #late
# svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+ AGE +
#            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
#          design = cvdht.not.inf.svy5) %>% summary() #not much change
# 
# svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN),
#          design = cvdht.not.inf.svy5) %>%  summary() #not much change
# 
# quantile(cvdht.not.inf.svy$variables$fuTime)
# quantile(cvdht.not.inf.svy10$variables$fuTime)
# quantile(cvdht.not.inf.svy5$variables$fuTime)
# 
# ##################################################################

#############################################################################
#do same for ht
#unadjusted model ht
mod1.ht.sdfbeta <- resid(mod1.ht.sa, type = "dfbetas")
plot(mod1.ht.sdfbeta)
infcase1 <- ht.mort14.fin.sa$variables[mod1.ht.sdfbeta > 0.10,]

#would exclude these cases by SERIAL identifier... however, there doesn't
#seem to be anything unusual about them. I'd leave them in.
table(infcase1$htMort)
table(infcase1$SEX)
median(infcase1$fuTime)
table(infcase1$YEAR)
table(infcase1$CRN)

#adjusted model ht
mod2.ht.sdfbeta <- resid(mod2.ht.sa, type = "dfbetas")

data.ht.mod2 <- ht.mort14.fin$variables %>%
  drop_na(SEX, CRN, htMort, RaceR, IncomeR, InsType, EduR, AGE, CancerEvBin, DiabetesRec, AnyCVD,
          fuTime)

data.ht.mod2$resid.crn <- mod2.ht.sdfbeta[,1]
data.ht.mod2$resid.edu2 <- mod2.ht.sdfbeta[,2]
data.ht.mod2$resid.edu3 <- mod2.ht.sdfbeta[,3]
data.ht.mod2$resid.age <- mod2.ht.sdfbeta[,4]
data.ht.mod2$resid.inc1 <- mod2.ht.sdfbeta[,5]
data.ht.mod2$resid.inc2 <- mod2.ht.sdfbeta[,6]
data.ht.mod2$resid.inc3 <- mod2.ht.sdfbeta[,7]
data.ht.mod2$resid.inc4 <- mod2.ht.sdfbeta[,8]
data.ht.mod2$resid.inc5 <- mod2.ht.sdfbeta[,9]
data.ht.mod2$resid.sex <- mod2.ht.sdfbeta[,10]
data.ht.mod2$resid.ins1 <- mod2.ht.sdfbeta[,11]
data.ht.mod2$resid.ins2 <- mod2.ht.sdfbeta[,12]
data.ht.mod2$resid.ins3 <- mod2.ht.sdfbeta[,13]
data.ht.mod2$resid.ins4 <- mod2.ht.sdfbeta[,14]
data.ht.mod2$resid.ins5 <- mod2.ht.sdfbeta[,15]
data.ht.mod2$resid.race2 <- mod2.ht.sdfbeta[,16]
data.ht.mod2$resid.race3 <- mod2.ht.sdfbeta[,17]
data.ht.mod2$resid.race4 <- mod2.ht.sdfbeta[,18]
data.ht.mod2$resid.race5 <- mod2.ht.sdfbeta[,19]
data.ht.mod2$resid.race6 <- mod2.ht.sdfbeta[,20]
data.ht.mod2$resid.canc <- mod2.ht.sdfbeta[,21]
data.ht.mod2$resid.diab <- mod2.ht.sdfbeta[,22]
data.ht.mod2$resid.cvd <- mod2.ht.sdfbeta[,23]


data.ht.mod2$id_num <- 1:nrow(data.ht.mod2)

#plot these sdfbetas
ggplot(data.ht.mod2, aes(y = resid.crn, x= id_num)) + geom_point() #>.1
ggplot(data.ht.mod2, aes(y = resid.edu2, x= id_num)) + geom_point() #> .1
ggplot(data.ht.mod2, aes(y = resid.edu3, x= id_num)) + geom_point() # > .12
ggplot(data.ht.mod2, aes(y = resid.age, x= id_num)) +  geom_point() # > abs(.07)
ggplot(data.ht.mod2, aes(y = resid.inc1, x= id_num)) + geom_point()# abs(.05)
ggplot(data.ht.mod2, aes(y = resid.inc2, x= id_num)) + geom_point() # >.05
ggplot(data.ht.mod2, aes(y = resid.inc3, x= id_num)) + geom_point() # >.1
ggplot(data.ht.mod2, aes(y = resid.inc4, x= id_num)) + geom_point() #.05
ggplot(data.ht.mod2, aes(y = resid.inc5, x= id_num)) + geom_point() #>.2
ggplot(data.ht.mod2, aes(y = resid.sex, x= id_num)) +  geom_point()  #>.05
ggplot(data.ht.mod2, aes(y = resid.ins1, x= id_num)) + geom_point() #< -.1
ggplot(data.ht.mod2, aes(y = resid.ins2, x= id_num)) + geom_point() # <-.1
ggplot(data.ht.mod2, aes(y = resid.ins3, x= id_num)) + geom_point() #abs(.1)
ggplot(data.ht.mod2, aes(y = resid.ins4, x= id_num)) + geom_point() #  < -.15
ggplot(data.ht.mod2, aes(y = resid.ins5, x= id_num)) + geom_point() # abs(.1)
ggplot(data.ht.mod2, aes(y = resid.race2, x= id_num)) + geom_point() #>.07
ggplot(data.ht.mod2, aes(y = resid.race3, x= id_num)) + geom_point() #>.15
ggplot(data.ht.mod2, aes(y = resid.race4, x= id_num)) + geom_point() #>.8
ggplot(data.ht.mod2, aes(y = resid.race5, x= id_num)) + geom_point() #>.05
ggplot(data.ht.mod2, aes(y = resid.race6, x= id_num)) + geom_point() #>.1
ggplot(data.ht.mod2, aes(y = resid.canc, x= id_num)) + geom_point() #>.07
ggplot(data.ht.mod2, aes(y = resid.diab, x= id_num)) + geom_point() #>.07
ggplot(data.ht.mod2, aes(y = resid.cvd, x= id_num)) + geom_point() #>.1

#generate a summary measure of how many variables each obs
#was influential on
data.ht.mod2 <- data.ht.mod2 %>%
  mutate(influence.meas = (resid.race6 > 0.1)  + (resid.race4 > .8) +
           (resid.race3 > .15) + (resid.race5 > 0.05)+
           (resid.race2 > .07) + (resid.diab > 0.07)+
           (abs(resid.ins5) > .1) + (resid.ins4 < (0-.15)) + (abs(resid.ins3) > .1) +
           (resid.ins2 < (0-.1)) + (resid.ins1 < (0-.1)) + (resid.sex > .05) +
           (resid.inc5 > .2) + (resid.inc3 >.1) + (resid.inc2 > .05) + 
           (abs(resid.inc1) > .05) + (resid.inc4 > .05) + (resid.canc > .07)+
           (abs(resid.age) > (.07)) + (resid.edu3 > .12) + (resid.edu2 > .1) + (resid.crn > .1))

table(data.ht.mod2$influence.meas)
#  0      1      2      3      4      5      6      8 
#124285    197     39      5      4      4      1      2 

possible.inf.ht <- data.ht.mod2[data.ht.mod2$influence.meas >=1,]
#write to csv
write_csv(possible.inf.ht, "data\\possible_inf_ht.csv")

table(possible.inf.ht$CRN)
table(possible.inf.ht$htMort)
table(possible.inf.ht$AGE)
table(possible.inf.ht$RaceR)
table(possible.inf.ht$InsType)
table(possible.inf.ht$EduR)
table(possible.inf.ht$SEX)
table(possible.inf.ht$CancerEvBin)
table(possible.inf.ht$DiabetesRec)
#nothing especially notable about these. Nothing seems like it's awry: leave them in
#possibly just those with more comorbidity: a lot have dm and cancer

ht.not.inf <- data.ht.mod2[data.ht.mod2$influence.meas <1,]
#refit model just to test
ht.not.inf.svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight14,
                               nest = TRUE, data = ht.not.inf)

ht.not.inf.svy10 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight10,
                                 nest = TRUE, data = ht.not.inf)
ht.not.inf.svy10 <- subset(ht.not.inf.svy10, YEAR <= 2010)

ht.not.inf.svy5 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeightSA5,
                                nest = TRUE, data = ht.not.inf)
ht.not.inf.svy5 <- subset(ht.not.inf.svy5, YEAR >2010)

svycoxph(formula = Surv(fuTime, htMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(CancerEvBin)+
           factor(AnyCVD)+ factor(DiabetesRec),
         design = ht.not.inf.svy)%>% summary() #not much change

svycoxph(formula = Surv(fuTime, htMort)~factor(CRN),
         design = ht.not.inf.svy) %>% summary() #not much change

#early years
svycoxph(formula = Surv(fuTime, htMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(CancerEvBin)+
           factor(AnyCVD)+ factor(DiabetesRec),
         design = ht.not.inf.svy10) %>% summary() #not much change

svycoxph(formula = Surv(fuTime, htMort)~factor(CRN),
         design = ht.not.inf.svy10) %>%  summary() #not much change

#late
svycoxph(formula = Surv(fuTime, htMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(CancerEvBin)+
           factor(AnyCVD)+ factor(DiabetesRec),
         design = ht.not.inf.svy5) %>% summary() #not much change

svycoxph(formula = Surv(fuTime, htMort)~factor(CRN),
         design = ht.not.inf.svy5) %>%  summary() #not much change

quantile(ht.not.inf.svy$variables$fuTime)
quantile(ht.not.inf.svy10$variables$fuTime)
quantile(ht.not.inf.svy5$variables$fuTime)

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
##################################################################
#adjusted model ht all cause
mod2.ht.sdfbeta <- resid(mod2.ht.allcause, type = "dfbetas")
dim(mod2.ht.sdfbeta)

data.ht.mod2 <- ht.mort14.fin$variables %>%
  drop_na(SEX, CRN, allCauseMort, RaceR, IncomeR, InsType, EduR, AGE, CancerEvBin, DiabetesRec,
          AnyCVD, fuTime)

data.ht.mod2$resid.crn <- mod2.ht.sdfbeta[,1]
data.ht.mod2$resid.edu2 <- mod2.ht.sdfbeta[,2]
data.ht.mod2$resid.edu3 <- mod2.ht.sdfbeta[,3]
data.ht.mod2$resid.age <- mod2.ht.sdfbeta[,4]
data.ht.mod2$resid.inc1 <- mod2.ht.sdfbeta[,5]
data.ht.mod2$resid.inc2 <- mod2.ht.sdfbeta[,6]
data.ht.mod2$resid.inc3 <- mod2.ht.sdfbeta[,7]
data.ht.mod2$resid.inc4 <- mod2.ht.sdfbeta[,8]
data.ht.mod2$resid.inc5 <- mod2.ht.sdfbeta[,9]
data.ht.mod2$resid.sex <- mod2.ht.sdfbeta[,10]
data.ht.mod2$resid.ins1 <- mod2.ht.sdfbeta[,11]
data.ht.mod2$resid.ins2 <- mod2.ht.sdfbeta[,12]
data.ht.mod2$resid.ins3 <- mod2.ht.sdfbeta[,13]
data.ht.mod2$resid.ins4 <- mod2.ht.sdfbeta[,14]
data.ht.mod2$resid.ins5 <- mod2.ht.sdfbeta[,15]
data.ht.mod2$resid.race2 <- mod2.ht.sdfbeta[,16]
data.ht.mod2$resid.race3 <- mod2.ht.sdfbeta[,17]
data.ht.mod2$resid.race4 <- mod2.ht.sdfbeta[,18]
data.ht.mod2$resid.race5 <- mod2.ht.sdfbeta[,19]
data.ht.mod2$resid.race6 <- mod2.ht.sdfbeta[,20]
data.ht.mod2$resid.canc <- mod2.ht.sdfbeta[,21]
data.ht.mod2$resid.hth <- mod2.ht.sdfbeta[,22]
data.ht.mod2$resid.diab <- mod2.ht.sdfbeta[,23]

data.ht.mod2$id_num <- 1:nrow(data.ht.mod2)

#plot these sdfbetas
plot(y = data.ht.mod2$resid.crn, x = data.ht.mod2$id_num) #abs(.1)
plot(y = data.ht.mod2$resid.edu2, x = data.ht.mod2$id_num) #abs(.04)
plot(y = data.ht.mod2$resid.edu3, x = data.ht.mod2$id_num) #abs(.05)
plot(y = data.ht.mod2$resid.age, x = data.ht.mod2$id_num) # <-.07
plot(y = data.ht.mod2$resid.inc1, x = data.ht.mod2$id_num)# >.04
plot(y = data.ht.mod2$resid.inc2, x = data.ht.mod2$id_num) #abs(.04) 
plot(y = data.ht.mod2$resid.inc3, x = data.ht.mod2$id_num) 
plot(y = data.ht.mod2$resid.inc4, x = data.ht.mod2$id_num) # > .12
plot(y = data.ht.mod2$resid.inc5, x = data.ht.mod2$id_num) #>.05
plot(y = data.ht.mod2$resid.sex, x = data.ht.mod2$id_num) #>.1
plot(y = data.ht.mod2$resid.ins1, x = data.ht.mod2$id_num) # > abs(.06)
plot(y = data.ht.mod2$resid.ins2, x = data.ht.mod2$id_num) # > abs(.06)
plot(y = data.ht.mod2$resid.ins3, x = data.ht.mod2$id_num) # > abs(.06)
plot(y = data.ht.mod2$resid.ins4, x = data.ht.mod2$id_num) # > abs(.07)
plot(y = data.ht.mod2$resid.ins5, x = data.ht.mod2$id_num) #>abs(.2)
plot(y = data.ht.mod2$resid.race2, x = data.ht.mod2$id_num) # < -.1
plot(y = data.ht.mod2$resid.race3, x = data.ht.mod2$id_num) # <-.1
plot(y = data.ht.mod2$resid.race4, x = data.ht.mod2$id_num) #>.2
plot(y = data.ht.mod2$resid.race5, x = data.ht.mod2$id_num) 
plot(y = data.ht.mod2$resid.race6, x = data.ht.mod2$id_num) 
plot(y = data.ht.mod2$resid.canc, x = data.ht.mod2$id_num) #<-.1
plot(y = data.ht.mod2$resid.hth, x = data.ht.mod2$id_num) #<-.06
plot(y = data.ht.mod2$resid.diab, x = data.ht.mod2$id_num) #>.04



#generate a summary measure of how many variables each obs
#was influential on
data.ht.mod2 <- data.ht.mod2 %>%
  mutate(influence.meas = (resid.canc < -.1) + (resid.hth < -.06) +(resid.race4 > .2) +
           (resid.race3 < -.1) + (resid.race2 <-.1) + abs(resid.ins5 > (.2)) +
           (abs(resid.ins4) > (.07)) +  (abs(resid.ins3) > (.06)) + (resid.diab > 0.04)+
           (abs(resid.ins2) > (.06)) + (abs(resid.ins1) > (.06)) + (resid.sex > .05) +(resid.inc5 >.15) +
           (resid.inc4 > .4) + (resid.inc3 < -.1) + (abs(resid.inc2) > .04) + (resid.inc1 > 0.04) +
           (resid.age < -.07) + (resid.edu3 >.05) +(abs(resid.edu2) >.04)+(abs(resid.crn) >.1))

table(data.ht.mod2$influence.meas)
#0    1    2    3   4    5 
#124497     56     24      4      7      3 

possible.inf <- data.ht.mod2[data.ht.mod2$influence.meas >=1,]
#write it out to csv
write_csv(possible.inf, "data\\possible_inf_ht_allcause.csv")

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
#possibly 10% with cancer, 60% with htht, 40% with ht, college grads all ages

ht.not.inf <- data.ht.mod2[data.ht.mod2$influence.meas <1,]
#refit model just to test
ht.not.inf.svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight14,
                             nest = TRUE, data = ht.not.inf)
ht.not.inf.svy10 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight10,
                               nest = TRUE, data = ht.not.inf)
ht.not.inf.svy5 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeightSA5,
                              nest = TRUE, data = ht.not.inf)

ht.not.inf.svy10 <- subset(ht.not.inf.svy10, YEAR <= 2010)
ht.not.inf.svy5 <- subset(ht.not.inf.svy5, YEAR > 2010)

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
           factor(CancerEvBin) + factor(DiabetesRec) + factor(AnyCVD),
         design = ht.not.inf.svy) %>% summary()#doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
         design = ht.not.inf.svy) %>% summary()#doesn't really change things


svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
           factor(CancerEvBin) + factor(DiabetesRec) + factor(AnyCVD),
         design = ht.not.inf.svy10) %>% summary() #doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
         design = ht.not.inf.svy10) %>% summary() #doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
           factor(CancerEvBin) + factor(DiabetesRec) + factor(AnyCVD),
         design = ht.not.inf.svy5) %>% summary() #doesn't really change things

svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
         design = ht.not.inf.svy5) %>% summary() #doesn't really change things

svyquantile(~fuTime, ht.not.inf.svy, c(.25, .5, .75))
svyquantile(~fuTime, ht.not.inf.svy10, c(.25, .5, .75))
svyquantile(~fuTime, ht.not.inf.svy5, c(.25, .5, .75))
2670/41325  21049/83172  23719/124497
##################################################################
# 
# 
# #adjusted model cvdht all cause
# mod2.cvdht.sdfbeta <- resid(mod2.cvdht.allcause, type = "dfbetas")
# dim(mod2.cvdht.sdfbeta)
# 
# data.cvdht.mod2 <- cvdht.mort14.fin.sa$variables %>%
#   drop_na(SEX, CRN, allCauseMort, RaceR, IncomeR, InsType, EduR, AGE, CancerEvBin, DiabetesRec)
# 
# data.cvdht.mod2$resid.crn <- mod2.cvdht.sdfbeta[,1]
# data.cvdht.mod2$resid.edu2 <- mod2.cvdht.sdfbeta[,2]
# data.cvdht.mod2$resid.edu3 <- mod2.cvdht.sdfbeta[,3]
# data.cvdht.mod2$resid.age <- mod2.cvdht.sdfbeta[,4]
# data.cvdht.mod2$resid.inc1 <- mod2.cvdht.sdfbeta[,5]
# data.cvdht.mod2$resid.inc2 <- mod2.cvdht.sdfbeta[,6]
# data.cvdht.mod2$resid.inc3 <- mod2.cvdht.sdfbeta[,7]
# data.cvdht.mod2$resid.inc4 <- mod2.cvdht.sdfbeta[,8]
# data.cvdht.mod2$resid.inc5 <- mod2.cvdht.sdfbeta[,9]
# data.cvdht.mod2$resid.sex <- mod2.cvdht.sdfbeta[,10]
# data.cvdht.mod2$resid.ins1 <- mod2.cvdht.sdfbeta[,11]
# data.cvdht.mod2$resid.ins2 <- mod2.cvdht.sdfbeta[,12]
# data.cvdht.mod2$resid.ins3 <- mod2.cvdht.sdfbeta[,13]
# data.cvdht.mod2$resid.ins4 <- mod2.cvdht.sdfbeta[,14]
# data.cvdht.mod2$resid.ins5 <- mod2.cvdht.sdfbeta[,15]
# data.cvdht.mod2$resid.race2 <- mod2.cvdht.sdfbeta[,16]
# data.cvdht.mod2$resid.race3 <- mod2.cvdht.sdfbeta[,17]
# data.cvdht.mod2$resid.race4 <- mod2.cvdht.sdfbeta[,18]
# data.cvdht.mod2$resid.race5 <- mod2.cvdht.sdfbeta[,19]
# data.cvdht.mod2$resid.race6 <- mod2.cvdht.sdfbeta[,20]
# data.cvdht.mod2$resid.canc <- mod2.cvdht.sdfbeta[,21]
# data.cvdht.mod2$resid.cvdhth <- mod2.cvdht.sdfbeta[,22]
# 
# #add id number
# data.cvdht.mod2$id_num <- 1:nrow(data.cvdht.mod2)
# 
# #plot these sdfbetas
# plot(y = data.cvdht.mod2$resid.crn, x = data.cvdht.mod2$id_num) #abs(.1)
# plot(y = data.cvdht.mod2$resid.edu2, x = data.cvdht.mod2$id_num) #>.15
# plot(y = data.cvdht.mod2$resid.edu3, x = data.cvdht.mod2$id_num) #> (abs.05)
# plot(y = data.cvdht.mod2$resid.age, x = data.cvdht.mod2$id_num) # abs(.1)
# plot(y = data.cvdht.mod2$resid.inc1, x = data.cvdht.mod2$id_num)# < -.1
# plot(y = data.cvdht.mod2$resid.inc2, x = data.cvdht.mod2$id_num) #abs(.05) 
# plot(y = data.cvdht.mod2$resid.inc3, x = data.cvdht.mod2$id_num) #abs(.1)
# plot(y = data.cvdht.mod2$resid.inc4, x = data.cvdht.mod2$id_num) # > .1
# plot(y = data.cvdht.mod2$resid.inc5, x = data.cvdht.mod2$id_num) #abs(.2)
# plot(y = data.cvdht.mod2$resid.sex, x = data.cvdht.mod2$id_num) #>.2
# plot(y = data.cvdht.mod2$resid.ins1, x = data.cvdht.mod2$id_num) #>.07
# plot(y = data.cvdht.mod2$resid.ins2, x = data.cvdht.mod2$id_num) # abs(.1)
# plot(y = data.cvdht.mod2$resid.ins3, x = data.cvdht.mod2$id_num) #abs(.1)
# plot(y = data.cvdht.mod2$resid.ins4, x = data.cvdht.mod2$id_num) #>.1
# plot(y = data.cvdht.mod2$resid.ins5, x = data.cvdht.mod2$id_num) 
# plot(y = data.cvdht.mod2$resid.race2, x = data.cvdht.mod2$id_num) #>.4
# plot(y = data.cvdht.mod2$resid.race3, x = data.cvdht.mod2$id_num) # >.1
# plot(y = data.cvdht.mod2$resid.race4, x = data.cvdht.mod2$id_num) #>.15
# plot(y = data.cvdht.mod2$resid.race5, x = data.cvdht.mod2$id_num) #abs(.2)
# plot(y = data.cvdht.mod2$resid.race6, x = data.cvdht.mod2$id_num) #abs(.2)
# #generate a summary measure of how many variables each obs
# #was influential on
# data.cvdht.mod2 <- data.cvdht.mod2 %>%
#   mutate(influence.meas = (abs(resid.race6) > .2) +(abs(resid.race5) > .2) + (abs(resid.race4) > .15) +
#            (resid.race3 > .1) + (resid.race2 > .4) + 
#            (resid.ins4 > .1) + (abs(resid.ins3) > .1) +
#            (abs(resid.ins2) > .1) + (resid.ins1 >.07) + (resid.sex > .2) + (resid.inc5 >.15) +
#            (resid.inc4 > .1) + (abs(resid.inc3) >.1) + (abs(resid.inc2) > .05) + (resid.inc1 < (0-.1)) +
#            (abs(resid.age) > (.1)) + (abs(resid.edu3) >.05) +(abs(resid.edu2) >.15)+(abs(resid.crn) >.1))
# 
# table(data.cvdht.mod2$influence.meas)
# #89 are influential on just 1 measure, 21 on 2, 9 on 3, 3 on 4, and 1 on 6
# 
# possible.inf <- data.cvdht.mod2[data.cvdht.mod2$influence.meas >=1,]
# write.csv(possible.inf, "data\\possible_inf_cvdht_allcause.csv")
# 
# table(possible.inf$CRN)
# table(possible.inf$allCauseMort)
# table(possible.inf$AGE)
# table(possible.inf$RaceR)
# table(possible.inf$InsType)
# table(possible.inf$EduR)
# table(possible.inf$SEX)
# table(possible.inf$CancerEvBin)
# table(possible.inf$DiabetesRec)
# median(possible.inf$fuTime)
# #nothing especially notable about these. Nothing seems like it's awry
# 
# cvdht.not.inf <- data.cvdht.mod2[data.cvdht.mod2$influence.meas == 0,]
# #refit model just to test
# cvdht.not.inf.svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight14,
#                              nest = TRUE, data = cvdht.not.inf)
# cvdht.not.inf.svy10 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight10,
#                                nest = TRUE, data = cvdht.not.inf)
# cvdht.not.inf.svy5 <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ mortWeight5,
#                                nest = TRUE, data = cvdht.not.inf)
# 
# cvdht.not.inf.svy10 <- subset(cvdht.not.inf.svy10, YEAR <= 2010)
# cvdht.not.inf.svy5 <- subset(cvdht.not.inf.svy5, YEAR > 2010)
# 
# svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
#            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
#            factor(CancerEvBin) + factor(DiabetesRec),
#          design = cvdht.not.inf.svy) #doesn't really change things
# 
# svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
#          design = cvdht.not.inf.svy) #doesn't really change things
# 
# 
# svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
#            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
#            factor(CancerEvBin) + factor(DiabetesRec),
#          design = cvdht.not.inf.svy10) %>% summary() #doesn't really change things
# 
# svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
#          design = cvdht.not.inf.svy10) %>% summary() #doesn't really change things
# 
# svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
#            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
#            factor(CancerEvBin) + factor(DiabetesRec),
#          design = cvdht.not.inf.svy5) %>% summary() #doesn't really change things
# 
# svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
#          design = cvdht.not.inf.svy5) %>% summary() #doesn't really change things
# 
