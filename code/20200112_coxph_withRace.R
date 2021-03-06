##################################################################
#Sarah Van Alsten
#Created: Jan 12, 2019
#Use the cleaned data from 20190928_NHIS.R to create cox regressions for table 2
#use survey design created in 20191213_table1_analysis.R to set up for survey
#adjusted weights
#Packages used: ipumsr, tidyverse, tableone, survival, survey
#Last Update: July 7, 2020
################################################################################
library(survey)
library(tidyverse)

#first create an empty table to hold the results so it's easier to look at later
result.data <- data.frame(matrix(ncol=7, nrow=0,
                                 dimnames=list(NULL, c("years", "dz", "model", "outcome",
                                                       "est", "lower", "upper"))))

########################################################################################
#svycoxph and survival to run the regressions
#crude/unadjusted

# Disease Specific --------------------------------------------------------

#diabetes
mod1.diab.sa <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN),
                         design = diab.mort14.fin.sa)

summary(mod1.diab.sa)

#write function to more easily add results to result.data
#mod = the model, yrs = modelled years, dz = condition, model = adj or unadj,
#outcome = dz spec or all cause
addResult <- function(mod, years, dz, model, outcome){
  

  addRow <- c(years,
              dz,
              model,
              outcome,
              as.numeric(exp(mod$coefficients[1])), #est
              as.numeric(exp(confint(mod))[1,1:2])) #lower and upper
  names(addRow) <- c("years", "dz", "model", "outcome", "est", "lower", "upper")
  return(addRow)
  
}

#add to result table
result.data <- rbind(result.data, addResult(mod1.diab.sa, "2000 - 2014", "diabetes", "unadjusted", "dz specific"))
names(result.data) <- c("years", "dz", "model", "outcome", "est", "lower", "upper")

#change column types
result.data$years <- as.character(result.data$years)
result.data$dz <- as.character(result.data$dz)
result.data$model <- as.character(result.data$model)
result.data$outcome <- as.character(result.data$outcome)

result.data$est <- as.numeric(as.character(result.data$est))
result.data$lower <- as.numeric(as.character(result.data$lower))
result.data$upper <- as.numeric(as.character(result.data$upper))

#to get N(%)
diab_mort_n <- mod1.diab.sa$nevent
diab_mort_perc <- mod1.diab.sa$nevent / mod1.diab.sa$n
#########################################################################
#cvd
mod1.cvd.sa <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN),
                        design = cvd.mort14.fin.sa)

summary(mod1.cvd.sa)
result.data <- rbind(result.data, addResult(mod1.cvd.sa, "2000 - 2014", "cvd", "unadjusted", "dz specific"))

#to get N(%)
cvd_mort_n <- mod1.cvd.sa$nevent
cvd_mort_perc <- mod1.cvd.sa$nevent / mod1.cvd.sa$n

######################################################
#cvd with ht
mod1.cvdht.sa <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN),
                          design = cvdht.mort14.fin.sa)

summary(mod1.cvdht.sa)

result.data <- rbind(result.data, addResult(mod1.cvdht.sa, "2000 - 2014", "cvdht", "unadjusted", "dz specific"))

#to get N(%)
cvdht_mort_n <- mod1.cvdht.sa$nevent
cvdht_mort_perc <- mod1.cvdht.sa$nevent / mod1.cvdht.sa$n


#############################################################################
#just HT
ht.mort14.fin.sa <- update(ht.mort14.fin.sa, htMort = ifelse(DEAD == 0, 0,
                                                             ifelse(MORTHYPR == 2, 1,
                                                                    ifelse(MORTHYPR == 1, 0, NA))))


mod1.ht.sa <- svycoxph(formula = Surv(fuTime, htMort)~factor(CRN),
                          design = ht.mort14.fin.sa)

summary(mod1.ht.sa)

result.data <- rbind(result.data, addResult(mod1.ht.sa, "2000 - 2014", "ht", "unadjusted", "dz specific"))

#to get N(%)
ht_mort_n <- mod1.ht.sa$nevent
ht_mort_perc <- mod1.ht.sa$nevent / mod1.ht.sa$n

###################################################################################
#now do adjusted models: for sex, age, insurance, income, education, race 
#(BMI, Smoking per dag shouldn't be adjusted)

#Diabetes
mod2.diab.sa <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+ AGE +
                           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(CancerEvBin) +
                           factor(AnyCVDHT),
                         design = diab.mort14.fin.sa)

summary(mod2.diab.sa)
cox.zph(mod2.diab.sa)
result.data <- rbind(result.data, addResult(mod2.diab.sa, "2000 - 2014", "diabetes", "adjusted", "dz specific"))


###########################################################################
#CVD
mod2.cvd.sa <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+ AGE +
                          factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(DiabetesRec) +
                          factor(CancerEvBin) + factor(HyperTen),
                        design = cvd.mort14.fin.sa)

summary(mod2.cvd.sa)
cox.zph(mod2.cvd.sa)
result.data <- rbind(result.data, addResult(mod2.cvd.sa, "2000 - 2014", "cvd", "adjusted", "dz specific"))

###############################################################################
#CVD plus hypertension
mod2.cvdht.sa <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+ AGE +
                            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                            factor(CancerEvBin) + factor(DiabetesRec),
                          design = cvdht.mort14.fin.sa)

summary(mod2.cvdht.sa)
result.data <- rbind(result.data, addResult(mod2.cvdht.sa, "2000 - 2014", "cvdht", "adjusted", "dz specific"))

####################################################################################
#hypertension
mod2.ht.sa <- svycoxph(formula = Surv(fuTime, htMort)~factor(CRN) + factor(EduR)+ AGE +
                            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                            factor(CancerEvBin) + factor(DiabetesRec) + factor(AnyCVD),
                          design = ht.mort14.fin.sa)

summary(mod2.ht.sa)
cox.zph(mod2.ht.sa)
result.data <- rbind(result.data, addResult(mod2.ht.sa, "2000 - 2014", "ht", "adjusted", "dz specific"))

####################################################################################
#get follow up times/IQR
svyquantile(~fuTime, design = diab.mort14.fin.sa, quantiles = .5, na.rm = T)
svyquantile(~fuTime, design = diab.mort14.fin.sa, quantiles = .25, na.rm = T)
svyquantile(~fuTime, design = diab.mort14.fin.sa, quantiles = .75, na.rm = T)

svyquantile(~fuTime, design = cvd.mort14.fin.sa, quantiles = .5, na.rm = T)
svyquantile(~fuTime, design = cvd.mort14.fin.sa, quantiles = .25, na.rm = T)
svyquantile(~fuTime, design = cvd.mort14.fin.sa, quantiles = .75, na.rm = T)

svyquantile(~fuTime, design = cvdht.mort14.fin.sa, quantiles = .5, na.rm = T)
svyquantile(~fuTime, design = cvdht.mort14.fin.sa, quantiles = .25, na.rm = T)
svyquantile(~fuTime, design = cvdht.mort14.fin.sa, quantiles = .75, na.rm = T)

svyquantile(~fuTime, design = ht.mort14.fin.sa, quantiles = .5, na.rm = T)
svyquantile(~fuTime, design = ht.mort14.fin.sa, quantiles = .25, na.rm = T)
svyquantile(~fuTime, design = ht.mort14.fin.sa, quantiles = .75, na.rm = T)


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
result.data <- rbind(result.data, addResult(mod1.diab.allcause, "2000 - 2014", "diabetes", "unadjusted", "allcause"))
##################################################################################

mod1.cvd.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                              design = cvd.mort14.fin.sa)

summary(mod1.cvd.allcause)
#to get N(%)
cvdAC_mort_n <- mod1.cvd.allcause$nevent
cvdAC_mort_perc <- mod1.cvd.allcause$nevent / mod1.cvd.allcause$n
result.data <- rbind(result.data, addResult(mod1.cvd.allcause, "2000 - 2014", "cvd", "unadjusted", "allcause"))
###############################################################################

mod1.cvdht.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                                design = cvdht.mort14.fin.sa)

summary(mod1.cvdht.allcause)
#to get N(%)
cvdhtAC_mort_n <- mod1.cvdht.allcause$nevent
cvdhtAC_mort_perc <- mod1.cvdht.allcause$nevent / mod1.cvdht.allcause$n

summary(mod1.cvdht.sa)


result.data <- rbind(result.data, addResult(mod1.cvdht.allcause, "2000 - 2014", "cvdht", "unadjusted", "allcause"))

###############################################################################

mod1.ht.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                                design = ht.mort14.fin.sa)

summary(mod1.ht.allcause)
#to get N(%)
htAC_mort_n <- mod1.ht.allcause$nevent
htAC_mort_perc <- mod1.ht.allcause$nevent / mod1.ht.allcause$n

summary(mod1.ht.sa)


result.data <- rbind(result.data, addResult(mod1.ht.allcause, "2000 - 2014", "ht", "unadjusted", "allcause"))
##############################################################################

#Adjusted all cause mortality models
#Diabetes
mod2.diab.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                       factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+
                       factor(CancerEvBin) + factor(AnyCVDHT),
                     design = diab.mort14.fin)

summary(mod2.diab.allcause)
cox.zph(mod2.diab.allcause, terms = F)
result.data <- rbind(result.data, addResult(mod2.diab.allcause, "2000 - 2014", "diabetes", "adjusted", "allcause"))
##############################################################################

#CVD
mod2.cvd.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+
                                factor(DiabetesRec) + factor(HyperTen) + factor(CancerEvBin),
                              design = cvd.mort14.fin.sa)

summary(mod2.cvd.allcause)
cox.zph(mod2.cvd.allcause, terms = F)
result.data <- rbind(result.data, addResult(mod2.cvd.allcause, "2000 - 2014", "cvd", "adjusted", "allcause"))
###############################################################################

#CVD plus hypertension
mod2.cvdht.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                  factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                                  factor(DiabetesRec) + factor(CancerEvBin),
                                design = cvdht.mort14.fin.sa)

summary(mod2.cvdht.allcause)
cox.zph(mod2.cvdht.allcause)
result.data <- rbind(result.data, addResult(mod2.cvdht.allcause, "2000 - 2014", "cvdht", "adjusted", "allcause"))
#########################################################################
#CVD plus hypertension
mod2.ht.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                  factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                                  factor(DiabetesRec) + factor(CancerEvBin) + factor(AnyCVD),
                                design = ht.mort14.fin.sa)

summary(mod2.ht.allcause)


ggcoxzph(cox.zph(mod2.ht.allcause, terms=F), var = "AGE")
result.data <- rbind(result.data, addResult(mod2.ht.allcause, "2000 - 2014", "ht", "adjusted", "allcause"))

##############################################################
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


svyquantile(~fuTime, design = ht.mort14.fin, quantiles = .5, na.rm = T)
svyquantile(~fuTime, design = ht.mort14.fin, quantiles = .25, na.rm = T)
svyquantile(~fuTime, design = ht.mort14.fin, quantiles = .75, na.rm = T)


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
mod1.early.diab$nevent
mod1.early.diab$nevent / mod1.early.diab$n
result.data <- rbind(result.data, addResult(mod1.early.diab, "2000 - 2010", "diabetes", "unadjusted", "dz specific"))

###################################################################################
mod1.early.cvd <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN),
                           design = cvd.mort10.fin.sa)

summary(mod1.early.cvd)
#to get N(%)
mod1.early.cvd$nevent
mod1.early.cvd$nevent / mod1.early.cvd$n
result.data <- rbind(result.data, addResult(mod1.early.cvd, "2000 - 2010", "cvd", "unadjusted", "dz specific"))

#################################################################################
mod1.early.cvdht <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN),
                             design = cvdht.mort10.fin.sa)

summary(mod1.early.cvdht)
#to get N(%)
mod1.early.cvdht$nevent
mod1.early.cvdht$nevent / mod1.early.cvdht$n
result.data <- rbind(result.data, addResult(mod1.early.cvdht, "2000 - 2010", "cvdht", "unadjusted", "dz specific"))

#################################################################################
ht.mort10.fin.sa <- update(ht.mort10.fin.sa, htMort = ifelse(DEAD == 0, 0,
                                                             ifelse(MORTHYPR == 2, 1,
                                                                    ifelse(MORTHYPR == 1, 0, NA))))

mod1.early.ht <- svycoxph(formula = Surv(fuTime, htMort)~factor(CRN),
                             design = ht.mort10.fin.sa)

summary(mod1.early.ht)
#to get N(%)
mod1.early.ht$nevent
mod1.early.ht$nevent / mod1.early.ht$n
result.data <- rbind(result.data, addResult(mod1.early.ht, "2000 - 2010", "ht", "unadjusted", "dz specific"))

###################################################################################
#now do adjusted models: for sex, age, insurance, income, education, race 
#(BMI, Smoking per dag shouldn't be adjusted)

#Diabetes
mod2.early.diab <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+ AGE +
                              factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(CancerEvBin)+
                            factor(AnyCVDHT),
                            design = diab.mort10.fin.sa)

summary(mod2.early.diab)
cox.zph(mod2.early.diab)
result.data <- rbind(result.data, addResult(mod2.early.diab, "2000 - 2010", "diabetes", "adjusted", "dz specific"))
###############################################################################
#CVD
mod2.early.cvd <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+ AGE +
                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + 
                             factor(DiabetesRec)+ factor(CancerEvBin)+ factor(HyperTen),
                           design = cvd.mort10.fin.sa)

summary(mod2.early.cvd)
result.data <- rbind(result.data, addResult(mod2.early.cvd, "2000 - 2010", "cvd", "unadjusted", "dz specific"))

#############################################################################
#CVD plus hypertension
mod2.early.cvdht <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+ AGE +
                               factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(CancerEvBin)+
                               factor(DiabetesRec),
                             design = cvdht.mort10.fin.sa)

summary(mod2.early.cvdht)
result.data <- rbind(result.data, addResult(mod2.early.cvdht, "2000 - 2010", "cvdht", "unadjusted", "dz specific"))
##############################################################
#ht
mod2.early.ht <- svycoxph(formula = Surv(fuTime, htMort)~factor(CRN) + factor(EduR)+ AGE +
                               factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(CancerEvBin)+
                               factor(DiabetesRec) + factor(AnyCVD),
                             design = ht.mort10.fin.sa)


summary(mod2.early.ht)
result.data <- rbind(result.data, addResult(mod2.early.ht, "2000 - 2010", "ht", "unadjusted", "dz specific"))

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

svyquantile(~fuTime, design = ht.mort10.fin.sa, quantiles = .5, na.rm = T)
svyquantile(~fuTime, design = ht.mort10.fin.sa, quantiles = .25, na.rm = T)
svyquantile(~fuTime, design = ht.mort10.fin.sa, quantiles = .75, na.rm = T)

svyquantile(~fuTime, design = ht.mort5.fin.sa, quantiles = .5, na.rm = T)
svyquantile(~fuTime, design = ht.mort5.fin.sa, quantiles = .25, na.rm = T)
svyquantile(~fuTime, design = ht.mort5.fin.sa, quantiles = .75, na.rm = T)

# Early All Cause ---------------------------------------------------------

##############################################################################
#now do all-cause mortality
#crude/unadjusted
mod1.early.diab.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                                     design = diab.mort10.fin.sa)

summary(mod1.early.diab.allcause)
#to get N(%)
mod1.early.diab.allcause$nevent
mod1.early.diab.allcause$nevent / mod1.early.diab.allcause$n
result.data <- rbind(result.data, addResult(mod1.early.diab.allcause, "2000 - 2010", "diabetes", "unadjusted", "allcause"))
####################################################################

mod1.early.cvd.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                                    design = cvd.mort10.fin.sa)

summary(mod1.early.cvd.allcause)
#to get N(%)
mod1.early.cvd.allcause$nevent
mod1.early.cvd.allcause$nevent / mod1.early.cvd.allcause$n
result.data <- rbind(result.data, addResult(mod1.early.cvd.allcause, "2000 - 2010", "cvd", "unadjusted", "allcause"))
##########################################################################################

mod1.early.cvdht.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                                      design = cvdht.mort10.fin.sa)

summary(mod1.early.cvdht.allcause)
#to get N(%)
mod1.early.cvdht.allcause$nevent
mod1.early.cvdht.allcause$nevent / mod1.early.cvdht.allcause$n
result.data <- rbind(result.data, addResult(mod1.early.cvdht.allcause, "2000 - 2010", "cvdht", "unadjusted", "allcause"))

#####################################################################################
mod1.early.ht.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                                      design = ht.mort10.fin.sa)

summary(mod1.early.ht.allcause)
#to get N(%)
mod1.early.ht.allcause$nevent
mod1.early.ht.allcause$nevent / mod1.early.ht.allcause$n
result.data <- rbind(result.data, addResult(mod1.early.ht.allcause, "2000 - 2010", "cvdht", "unadjusted", "allcause"))

###################################################################################
#now do adjusted models: for sex, age, insurance, income, education, race, chronic cond 
#(BMI, Smoking per dag shouldn't be adjusted)

#Diabetes
mod2.early.diab.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                       factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+
                                       factor(AnyCVDHT) + factor(CancerEvBin),
                                     design = diab.mort10.fin.sa)


summary(mod2.early.diab.allcause)
cox.zph(mod2.early.diab.allcause)
result.data <- rbind(result.data, addResult(mod2.early.diab.allcause, "2000 - 2010", "diab", "adjusted", "allcause"))
##################################################################################

#CVD
mod2.early.cvd.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                      factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                                      factor(DiabetesRec) + factor(HyperTen) + factor(CancerEvBin),
                                    design = cvd.mort10.fin.sa)

summary(mod2.early.cvd.allcause)
result.data <- rbind(result.data, addResult(mod2.early.cvd.allcause, "2000 - 2010", "cvd", "adjusted", "allcause"))
#################################################################################

#CVD plus hypertension
mod2.early.cvdht.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                        factor(IncomeR) + factor(SEX) + factor(InsType)+ factor(RaceR) +
                                        factor(DiabetesRec) + factor(CancerEvBin),
                                      design = cvdht.mort10.fin.sa)

summary(mod2.early.cvdht.allcause)
result.data <- rbind(result.data, addResult(mod2.early.cvdht.allcause, "2000 - 2010", "cvdht", "adjusted", "allcause"))

#################################################################################
#hypertension
mod2.early.ht.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                        factor(IncomeR) + factor(SEX) + factor(InsType)+ factor(RaceR) +
                                        factor(DiabetesRec) + factor(CancerEvBin)+ factor(AnyCVD),
                                      design = ht.mort10.fin.sa)

summary(mod2.early.ht.allcause)
result.data <- rbind(result.data, addResult(mod2.early.ht.allcause, "2000 - 2010", "ht", "adjusted", "allcause"))


################################################################
#finally do the LATE years (2011 - 2014)

#the unadjusted models
mod1.diab.sa.l <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN),
                                 design = diab.mort5.fin.sa)
summary(mod1.diab.sa.l)

result.data <- rbind(result.data, addResult(mod1.diab.sa.l, "2011 - 2014", "diabetes", "unadjusted", "dz specific"))

mod1.diab.sa.l$n
mod1.diab.sa.l$nevent/ mod1.diab.sa.l$n

############################################################################
mod1.cvd.sa.l <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN),
                           design = cvd.mort5.fin.sa)
summary(mod1.cvd.sa.l)
result.data <- rbind(result.data, addResult(mod1.cvd.sa.l, "2011 - 2014", "cvd", "unadjusted", "dz specific"))

mod1.cvd.sa.l$n
mod1.cvd.sa.l$nevent/ mod1.cvd.sa.l$n

#############################################################
mod1.cvdht.sa.l <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN),
                          design = cvdht.mort5.fin.sa)
summary(mod1.cvdht.sa.l)
result.data <- rbind(result.data, addResult(mod1.cvdht.sa.l, "2011 - 2014", "cvdht", "unadjusted", "dz specific"))

mod1.cvdht.sa.l$n
mod1.cvdht.sa.l$nevent/ mod1.cvdht.sa.l$n

#############################################################
ht.mort5.fin.sa <- update(ht.mort5.fin.sa, htMort = ifelse(DEAD == 0, 0,
                                                             ifelse(MORTHYPR == 2, 1,
                                                                    ifelse(MORTHYPR == 1, 0, NA))))

mod1.ht.sa.l <- svycoxph(formula = Surv(fuTime, htMort)~factor(CRN),
                            design = ht.mort5.fin.sa)
summary(mod1.ht.sa.l)
result.data <- rbind(result.data, addResult(mod1.ht.sa.l, "2011 - 2014", "ht", "unadjusted", "dz specific"))

mod1.ht.sa.l$n
mod1.ht.sa.l$nevent/ mod1.ht.sa.l$n

#############################################################
#adjusted model

mod2.diab.sa.l <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN)+ factor(EduR)+ AGE +
                                   factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                             factor(CancerEvBin) + factor(AnyCVDHT),
                                 design = diab.mort5.fin.sa)
summary(mod2.diab.sa.l)
result.data <- rbind(result.data, addResult(mod2.diab.sa.l, "2011 - 2014", "diabetes", "adjusted", "dz specific"))

########################################################################
mod2.cvd.sa.l <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN)+ factor(EduR)+ AGE +
                                  factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) + factor(DiabetesRec)+
                            factor(CancerEvBin) + factor(HyperTen),
                                design = cvd.mort5.fin.sa)
summary(mod2.cvd.sa.l)
result.data <- rbind(result.data, addResult(mod2.cvd.sa.l, "2011 - 2014", "cvd", "adjusted", "dz specific"))
#########################################################################

mod2.cvdht.sa.l <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN)+ factor(EduR)+ AGE +
                                    factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                              factor(CancerEvBin) + factor(DiabetesRec),
                                  design = cvdht.mort5.fin.sa)
summary(mod2.cvdht.sa.l)
result.data <- rbind(result.data, addResult(mod2.cvdht.sa.l, "2011 - 2014", "cvdht", "adjusted", "dz specific"))
#########################################################################

mod2.ht.sa.l <- svycoxph(formula = Surv(fuTime, htMort)~factor(CRN)+ factor(EduR)+ AGE +
                              factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                              factor(CancerEvBin) + factor(DiabetesRec) + factor(AnyCVD),
                            design = ht.mort5.fin.sa)
summary(mod2.ht.sa.l)
result.data <- rbind(result.data, addResult(mod2.ht.sa.l, "2011 - 2014", "ht", "adjusted", "dz specific"))

######################################################################################
#for later years with all cause models
#the unadjusted models
mod1.diab.sa.l.ac <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                           design = diab.mort5.fin.sa)
summary(mod1.diab.sa.l.ac)

result.data <- rbind(result.data, addResult(mod1.diab.sa.l.ac, "2011 - 2014", "diabetes", "unadjusted", "allcause"))

mod1.diab.sa.l.ac$nevent
mod1.diab.sa.l.ac$nevent/ mod1.diab.sa.l.ac$n

############################################################################
mod1.cvd.sa.l.ac <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                          design = cvd.mort5.fin.sa)
summary(mod1.cvd.sa.l.ac)
result.data <- rbind(result.data, addResult(mod1.cvd.sa.l.ac, "2011 - 2014", "cvd", "unadjusted", "allcause"))

mod1.cvd.sa.l.ac$nevent
mod1.cvd.sa.l.ac$nevent/ mod1.cvd.sa.l.ac$n


#############################################################
mod1.cvdht.sa.l.ac <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                            design = cvdht.mort5.fin.sa)
summary(mod1.cvdht.sa.l.ac)
result.data <- rbind(result.data, addResult(mod1.cvdht.sa.l.ac, "2011 - 2014", "cvdht", "unadjusted", "allcause"))

mod1.cvdht.sa.l.ac$nevent
mod1.cvdht.sa.l.ac$nevent/ mod1.cvdht.sa.l.ac$n

#############################################################
mod1.ht.sa.l.ac <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                               design = ht.mort5.fin.sa)
summary(mod1.ht.sa.l.ac)
result.data <- rbind(result.data, addResult(mod1.ht.sa.l.ac, "2011 - 2014", "ht", "unadjusted", "allcause"))

mod1.ht.sa.l.ac$nevent
mod1.ht.sa.l.ac$nevent/ mod1.ht.sa.l.ac$n

#############################################################
#adjusted model

mod2.diab.sa.l.ac <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN)+ factor(EduR)+ AGE +
                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                             factor(CancerEvBin) + factor(AnyCVDHT),
                           design = diab.mort5.fin.sa)
summary(mod2.diab.sa.l.ac)
result.data <- rbind(result.data, addResult(mod2.diab.sa.l.ac, "2011 - 2014", "diabetes", "adjusted", "allcause"))

########################################################################
mod2.cvd.sa.l.ac <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN)+ factor(EduR)+ AGE +
                            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                            factor(DiabetesRec) + factor(CancerEvBin) + factor(HyperTen),
                          design = cvd.mort5.fin.sa)
summary(mod2.cvd.sa.l.ac)
result.data <- rbind(result.data, addResult(mod2.cvd.sa.l.ac, "2011 - 2014", "cvd", "adjusted", "allcause"))
#########################################################################

mod2.cvdht.sa.l.ac <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN)+ factor(EduR)+ AGE +
                              factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+
                                factor(DiabetesRec) + factor(CancerEvBin) ,
                            design = cvdht.mort5.fin.sa)
summary(mod2.cvdht.sa.l.ac)
result.data <- rbind(result.data, addResult(mod2.cvdht.sa.l.ac, "2011 - 2014", "cvdht", "adjusted", "allcause"))

###############################################################
mod2.ht.sa.l.ac <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN)+ factor(EduR)+ AGE +
                                 factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+
                                 factor(DiabetesRec) + factor(CancerEvBin) + factor(AnyCVD),
                               design = ht.mort5.fin.sa)
summary(mod2.ht.sa.l.ac)
result.data <- rbind(result.data, addResult(mod2.ht.sa.l.ac, "2011 - 2014", "ht", "adjusted", "allcause"))

######################################################################################
result.data[17, "model"] <- "adjusted"
result.data[18, "model"] <- "adjusted"

#filter to relevant models ie adjusted/not and what years they came from
all.adj.dz <- result.data %>%
  filter(years == "2000 - 2014" & model == "adjusted" & outcome == "dz specific")

all.adj.ac <- result.data %>%
  filter(years == "2000 - 2014" & model == "adjusted" & outcome == "allcause")

early.adj.dz <- result.data %>%
  filter(years == "2000 - 2009" & model == "adjusted" & outcome == "dz specific")

early.adj.ac <- result.data %>%
  filter(years == "2000 - 2009" & model == "adjusted" & outcome == "allcause")

late.adj.dz <- result.data %>%
  filter(years == "2011 - 2014" & model == "adjusted" & outcome == "dz specific")

late.adj.ac <- result.data %>%
  filter(years == "2011 - 2014" & model == "adjusted" & outcome == "allcause")

#function that will print out the HRs with 95% CI in parentheses
myPrintFx <- function(data){
  for (i in 1:nrow(data)){
    est <- as.numeric(data[i, "est"])
    lower <- as.numeric(data[i, "lower"])
    upper <- as.numeric(data[i, "upper"])
    print(paste(sprintf(fmt = "%.4f", est), " (",
                sprintf(fmt = "%.4f", lower), " - ",
                sprintf(fmt = "%.4f", upper), ")", sep = ""))
  }
}

#print out results for the models
myPrintFx(all.adj.dz)
myPrintFx(all.adj.ac)
myPrintFx(early.adj.dz)
myPrintFx(early.adj.ac)
myPrintFx(late.adj.dz)
myPrintFx(late.adj.ac)


