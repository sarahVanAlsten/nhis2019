##################################################################
#Sarah Van Alsten
#Created: Jan 12, 2019
#Use the cleaned data from 20190928_NHIS.R to create cox regressions for table 2
#Packages used: ipumsr, tidyverse, tableone, survival, survey
#Last Update: Jan 12, 2020
################################################################################

#first create an empty table to hold the results
result.data <- data.frame(matrix(ncol=7, nrow=0,
                                 dimnames=list(NULL, c("years", "dz", "model", "outcome", "est", "lower", "upper"))))

########################################################################################
#svycoxph and survival to run the regressions
#crude/unadjusted

# Disease Specific --------------------------------------------------------

#diabetes
mod1.diab.sa <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN),
                         design = diab.mort14.fin.sa)

summary(mod1.diab.sa)

#write function to more easily add results to result.data
#mod = the model, yrs = modelled years, dz = condition, model = adj or unadj, outcome = dz spec or all cause
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

###################################################################################
#now do adjusted models: for sex, age, insurance, income, education, race 
#(BMI, Smoking per dag shouldn't be adjusted)

#Diabetes
mod2.diab.sa <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+ AGE +
                           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
                         design = diab.mort14.fin.sa)

summary(mod2.diab.sa)
result.data <- rbind(result.data, addResult(mod2.diab.sa, "2000 - 2014", "diabetes", "adjusted", "dz specific"))


###########################################################################
#CVD
mod2.cvd.sa <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+ AGE +
                          factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
                        design = cvd.mort14.fin.sa)

summary(mod2.cvd.sa)
result.data <- rbind(result.data, addResult(mod2.cvd.sa, "2000 - 2014", "cvd", "adjusted", "dz specific"))

###############################################################################
#CVD plus hypertension
mod2.cvdht.sa <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+ AGE +
                            factor(IncomeR) + factor(SEX) + factor(InsType),
                          design = cvdht.mort14.fin.sa)

summary(mod2.cvdht.sa)
result.data <- rbind(result.data, addResult(mod2.cvdht.sa, "2000 - 2014", "cvdht", "adjusted", "dz specific"))

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

result.data <- rbind(result.data, addResult(mod1.cvdht.allcause, "2000 - 2014", "cvdht", "unadjusted", "allcause"))
##############################################################################

#Adjusted all cause mortality models
#Diabetes
mod2.diab.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                       factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+
                       factor(CancerEvBin) + factor(AnyCVDHT),
                     design = diab.mort14.fin)

summary(mod2.diab.allcause)
result.data <- rbind(result.data, addResult(mod2.diab.allcause, "2000 - 2014", "diabetes", "adjusted", "allcause"))
##############################################################################

#CVD
mod2.cvd.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+
                                factor(DiabetesRec) + factor(HyperTen) + factor(CancerEvBin),
                              design = cvd.mort14.fin.sa)

summary(mod2.cvd.allcause)
result.data <- rbind(result.data, addResult(mod2.cvd.allcause, "2000 - 2014", "cvd", "adjusted", "allcause"))
###############################################################################

#CVD plus hypertension
mod2.cvdht.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                  factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                                  factor(DiabetesRec) + factor(CancerEvBin),
                                design = cvdht.mort14.fin.sa)

summary(mod2.cvdht.allcause)
result.data <- rbind(result.data, addResult(mod2.cvdht.allcause, "2000 - 2014", "cvdht", "adjusted", "allcause"))

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
result.data <- rbind(result.data, addResult(mod1.early.diab, "2000 - 2010", "diabetes", "unadjusted", "dz specific"))

###################################################################################
mod1.early.cvd <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN),
                           design = cvd.mort10.fin.sa)

summary(mod1.early.cvd)
#to get N(%)
cvd_mort_n.2 <- mod1.early.cvd$nevent
cvd_mort_perc.2 <- mod1.early.cvd$nevent / mod1.early.cvd$n
result.data <- rbind(result.data, addResult(mod1.early.cvd, "2000 - 2010", "cvd", "unadjusted", "dz specific"))

#################################################################################
mod1.early.cvdht <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN),
                             design = cvdht.mort10.fin.sa)

summary(mod1.early.cvdht)
#to get N(%)
cvdht_mort_n.2 <- mod1.early.cvdht$nevent
cvdht_mort_perc.2 <- mod1.early.cvdht$nevent / mod1.early.cvdht$n
result.data <- rbind(result.data, addResult(mod1.early.cvdht, "2000 - 2010", "cvdht", "unadjusted", "dz specific"))

###################################################################################
#now do adjusted models: for sex, age, insurance, income, education, race 
#(BMI, Smoking per dag shouldn't be adjusted)

#Diabetes
mod2.early.diab <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+ AGE +
                              factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
                            design = diab.mort10.fin.sa)

summary(mod2.early.diab)
result.data <- rbind(result.data, addResult(mod2.early.diab, "2000 - 2010", "diabetes", "adjusted", "dz specific"))
###############################################################################
#CVD
mod2.early.cvd <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+ AGE +
                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
                           design = cvd.mort10.fin.sa)

summary(mod2.early.cvd)
result.data <- rbind(result.data, addResult(mod2.early.cvd, "2000 - 2010", "cvd", "unadjusted", "dz specific"))

#############################################################################
#CVD plus hypertension
mod2.early.cvdht <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+ AGE +
                               factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
                             design = cvdht.mort10.fin.sa)

summary(mod2.early.cvdht)
result.data <- rbind(result.data, addResult(mod2.early.cvdht, "2000 - 2010", "cvdht", "unadjusted", "dz specific"))

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
result.data <- rbind(result.data, addResult(mod1.early.diab.allcause, "2000 - 2010", "diabetes", "unadjusted", "allcause"))
####################################################################

mod1.early.cvd.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                                    design = cvd.mort10.fin.sa)

summary(mod1.early.cvd.allcause)
#to get N(%)
cvdAC_mort_n.2 <- mod1.early.cvd.allcause$nevent
cvdAC_mort_perc.2 <- mod1.early.cvd.allcause$nevent / mod1.early.cvd.allcause$n
result.data <- rbind(result.data, addResult(mod1.early.cvd.allcause, "2000 - 2010", "cvd", "unadjusted", "allcause"))
##########################################################################################

mod1.early.cvdht.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                                      design = cvdht.mort10.fin.sa)

summary(mod1.early.cvdht.allcause)
#to get N(%)
cvdhtAC_mort_n.2 <- mod1.early.cvdht.allcause$nevent
cvdhtAC_mort_perc.2 <- mod1.early.cvdht.allcause$nevent / mod1.early.cvdht.allcause$n
result.data <- rbind(result.data, addResult(mod1.early.cvdht.allcause, "2000 - 2010", "cvdht", "unadjusted", "allcause"))

###################################################################################
#now do adjusted models: for sex, age, insurance, income, education, race, chronic cond 
#(BMI, Smoking per dag shouldn't be adjusted)

#Diabetes
mod2.early.diab.allcause <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+ AGE +
                                       factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+
                                       factor(AnyCVDHT) + factor(CancerEvBin),
                                     design = diab.mort10.fin.sa)


summary(mod2.early.diab.allcause)
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

################################################################
#finally do the LATE years (2010 - 2014) (go back and make all 2000 - 2010 be 2000 to 2009)
result.data$years <- str_replace_all(result.data$years, pattern = "2000 - 2010", "2000 - 2009")

#the unadjusted models
mod1.diab.sa.l <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN),
                                 design = diab.mort5.fin.sa)
summary(mod1.diab.sa.l)

result.data <- rbind(result.data, addResult(mod1.diab.sa.l, "2010 - 2014", "diabetes", "unadjusted", "dz specific"))

############################################################################
mod1.cvd.sa.l <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN),
                           design = cvd.mort5.fin.sa)
summary(mod1.cvd.sa.l)
result.data <- rbind(result.data, addResult(mod1.cvd.sa.l, "2010 - 2014", "cvd", "unadjusted", "dz specific"))

#############################################################
mod1.cvdht.sa.l <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN),
                          design = cvdht.mort5.fin.sa)
summary(mod1.cvdht.sa.l)
result.data <- rbind(result.data, addResult(mod1.cvdht.sa.l, "2010 - 2014", "cvdht", "unadjusted", "dz specific"))

#############################################################
#adjusted model

mod2.diab.sa.l <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN)+ factor(EduR)+ AGE +
                                   factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
                                 design = diab.mort5.fin.sa)
summary(mod2.diab.sa.l)
result.data <- rbind(result.data, addResult(mod2.diab.sa.l, "2010 - 2014", "diabetes", "adjusted", "dz specific"))

########################################################################
mod2.cvd.sa.l <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN)+ factor(EduR)+ AGE +
                                  factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
                                design = cvd.mort5.fin.sa)
summary(mod2.cvd.sa.l)
result.data <- rbind(result.data, addResult(mod2.cvd.sa.l, "2010 - 2014", "cvd", "adjusted", "dz specific"))
#########################################################################

mod2.cvdht.sa.l <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN)+ factor(EduR)+ AGE +
                                    factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) ,
                                  design = cvdht.mort5.fin.sa)
summary(mod2.cvdht.sa.l)
result.data <- rbind(result.data, addResult(mod2.cvdht.sa.l, "2010 - 2014", "cvdht", "adjusted", "dz specific"))

######################################################################################
#for later years with all cause models
#the unadjusted models
mod1.diab.sa.l.ac <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                           design = diab.mort5.fin.sa)
summary(mod1.diab.sa.l.ac)

result.data <- rbind(result.data, addResult(mod1.diab.sa.l.ac, "2010 - 2014", "diabetes", "unadjusted", "allcause"))

############################################################################
mod1.cvd.sa.l.ac <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                          design = cvd.mort5.fin.sa)
summary(mod1.cvd.sa.l.ac)
result.data <- rbind(result.data, addResult(mod1.cvd.sa.l.ac, "2010 - 2014", "cvd", "unadjusted", "allcause"))

#############################################################
mod1.cvdht.sa.l.ac <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN),
                            design = cvdht.mort5.fin.sa)
summary(mod1.cvdht.sa.l.ac)
result.data <- rbind(result.data, addResult(mod1.cvdht.sa.l.ac, "2010 - 2014", "cvdht", "unadjusted", "allcause"))

#############################################################
#adjusted model

mod2.diab.sa.l.ac <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN)+ factor(EduR)+ AGE +
                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                             factor(CancerEvBin) + factor(AnyCVDHT),
                           design = diab.mort5.fin.sa)
summary(mod2.diab.sa.l.ac)
result.data <- rbind(result.data, addResult(mod2.diab.sa.l.ac, "2010 - 2014", "diabetes", "adjusted", "allcause"))

########################################################################
mod2.cvd.sa.l.ac <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN)+ factor(EduR)+ AGE +
                            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                            factor(DiabetesRec) + factor(CancerEvBin) + factor(HyperTen),
                          design = cvd.mort5.fin.sa)
summary(mod2.cvd.sa.l.ac)
result.data <- rbind(result.data, addResult(mod2.cvd.sa.l.ac, "2010 - 2014", "cvd", "adjusted", "allcause"))
#########################################################################

mod2.cvdht.sa.l.ac <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN)+ factor(EduR)+ AGE +
                              factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+
                                factor(DiabetesRec) + factor(CancerEvBin),
                            design = cvdht.mort5.fin.sa)
summary(mod2.cvdht.sa.l.ac)
result.data <- rbind(result.data, addResult(mod2.cvdht.sa.l.ac, "2010 - 2014", "cvdht", "adjusted", "allcause"))

######################################################################################