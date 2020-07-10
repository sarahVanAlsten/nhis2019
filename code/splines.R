#Sarah Van Alsten
#Date: February 2, 2020
#Purpose: add splines to Cox Models to see how it changes results given
#small non-linearity in age after 75
#Packages used: survival, lattice, splines, survey
#######################################
library(survival)
library(lattice)
library(splines)
library(knitr)
library(kableExtra)

#disease specific models:
# fit a Cox model with a nonlinear effect for age using natural splines
mod2.diab.sa2 <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+
                             bs(AGE, knots = c(75), degree = 1) +
                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                            factor(CancerEvBin) + factor(AnyCVDHT),
                           design = diab.mort14.fin.sa)
summary(mod2.diab.sa2)

# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvd.sa2 <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+
                             bs(AGE, knots = c(75), degree = 1) +
                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+
                           factor(CancerEvBin) + factor(DiabetesRec) + factor(HyperTen),
                           design = cvd.mort14.fin.sa)
summary(mod2.cvd.sa2)


# # fit a Cox model with a nonlinear effect for age using natural splines
# mod2.cvdht.sa2 <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+
#                             bs(AGE, knots = c(75), degree = 1) +
#                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
#                           design = cvdht.mort14.fin.sa)
# summary(mod2.cvdht.sa2)


# fit a Cox model with a nonlinear effect for age using natural splines
mod2.ht.sa2 <- svycoxph(formula = Surv(fuTime, htMort)~factor(CRN) + factor(EduR)+
                          bs(AGE, knots = c(75), degree = 1) +
                          factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+
                          factor(CancerEvBin) + factor(DiabetesRec),
                        design = ht.mort14.fin.sa)
summary(mod2.ht.sa2)
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


# # fit a Cox model with a nonlinear effect for age using natural splines
# mod2.cvdht.sa2 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
#                              bs(AGE, knots = c(75), degree = 1) +
#                              factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
#                              factor(DiabetesRec)+ factor(CancerEvBin),
#                            design = cvdht.mort14.fin.sa)
# summary(mod2.cvdht.sa2)

# fit a Cox model with a nonlinear effect for age using natural splines
mod2.ht.ac2 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
                             bs(AGE, knots = c(75), degree = 1) +
                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                             factor(DiabetesRec)+ factor(CancerEvBin),
                           design = ht.mort14.fin.sa)
summary(mod2.ht.ac2)

###############################################################################################
#2000 - 2010 years only

#disease specific models:
# fit a Cox model with a nonlinear effect for age using natural splines
mod2.diab.sa3 <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+
                            bs(AGE, knots = c(75), degree = 1) +
                            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                            factor(CancerEvBin) + factor(AnyCVDHT),
                          design = diab.mort10.fin.sa)
summary(mod2.diab.sa3)

# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvd.sa3 <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+
                           bs(AGE, knots = c(75), degree = 1) +
                           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+
                           factor(CancerEvBin) + factor(DiabetesRec) + factor(HyperTen),
                         design = cvd.mort10.fin.sa)
summary(mod2.cvd.sa3)


# # fit a Cox model with a nonlinear effect for age using natural splines
# mod2.cvdht.sa3 <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+
#                             bs(AGE, knots = c(75), degree = 1) +
#                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
#                           design = cvdht.mort10.fin.sa)
# summary(mod2.cvdht.sa3)


# fit a Cox model with a nonlinear effect for age using natural splines
mod2.ht.sa3 <- svycoxph(formula = Surv(fuTime, htMort)~factor(CRN) + factor(EduR)+
                          bs(AGE, knots = c(75), degree = 1) +
                          factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+
                          factor(CancerEvBin) + factor(DiabetesRec) + factor(AnyCVD),
                        design = ht.mort10.fin.sa)
summary(mod2.ht.sa3)
#############################################################
#all cause models:
# fit a Cox model with a nonlinear effect for age using natural splines
mod2.diab.ac3 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
                            bs(AGE, knots = c(75), degree = 1) +
                            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                            factor(CancerEvBin) + factor(AnyCVDHT),
                          design = diab.mort10.fin.sa)
summary(mod2.diab.ac3)

# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvd.ac3 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
                           bs(AGE, knots = c(75), degree = 1) +
                           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                           factor(HyperTen) + factor(CancerEvBin) + factor(DiabetesRec),
                         design = cvd.mort10.fin.sa)
summary(mod2.cvd.ac3)


# # fit a Cox model with a nonlinear effect for age using natural splines
# mod2.cvdht.sa3 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
#                              bs(AGE, knots = c(75), degree = 1) +
#                              factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
#                              factor(DiabetesRec)+ factor(CancerEvBin),
#                            design = cvdht.mort10.fin.sa)
# summary(mod2.cvdht.sa3)

# fit a Cox model with a nonlinear effect for age using natural splines
mod2.ht.ac3 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
                          bs(AGE, knots = c(75), degree = 1) +
                          factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                          factor(DiabetesRec)+ factor(CancerEvBin) + factor(AnyCVD),
                        design = ht.mort10.fin.sa)
summary(mod2.ht.ac3)
#######################################################################
#Later Years only


#disease specific models:
# fit a Cox model with a nonlinear effect for age using natural splines
mod2.diab.sa4 <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+
                            bs(AGE, knots = c(75), degree = 1) +
                            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                            factor(CancerEvBin) + factor(AnyCVDHT),
                          design = diab.mort5.fin.sa)
summary(mod2.diab.sa4)

# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvd.sa4 <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+
                           bs(AGE, knots = c(75), degree = 1) +
                           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+
                           factor(CancerEvBin) + factor(DiabetesRec) + factor(HyperTen),
                         design = cvd.mort5.fin.sa)
summary(mod2.cvd.sa4)


# # fit a Cox model with a nonlinear effect for age using natural splines
# mod2.cvdht.sa4 <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+
#                             bs(AGE, knots = c(75), degree = 1) +
#                             factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR),
#                           design = cvdht.mort.5.fin.sa)
# summary(mod2.cvdht.sa4)


# fit a Cox model with a nonlinear effect for age using natural splines
mod2.ht.sa4 <- svycoxph(formula = Surv(fuTime, htMort)~factor(CRN) + factor(EduR)+
                          bs(AGE, knots = c(75), degree = 1) +
                          factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR)+
                          factor(CancerEvBin) + factor(DiabetesRec) + factor(AnyCVD),
                        design = ht.mort5.fin.sa)
summary(mod2.ht.sa4)
#############################################################
#all cause models:
# fit a Cox model with a nonlinear effect for age using natural splines
mod2.diab.ac4 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
                            bs(AGE, knots = c(75), degree = 1) +
                            factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                            factor(CancerEvBin) + factor(AnyCVDHT),
                          design = diab.mort5.fin.sa)
summary(mod2.diab.ac4)

# fit a Cox model with a nonlinear effect for age using natural splines
mod2.cvd.ac4 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
                           bs(AGE, knots = c(75), degree = 1) +
                           factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                           factor(HyperTen) + factor(CancerEvBin) + factor(DiabetesRec),
                         design = cvd.mort5.fin.sa)
summary(mod2.cvd.ac4)


# # fit a Cox model with a nonlinear effect for age using natural splines
# mod2.cvdht.sa4 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
#                              bs(AGE, knots = c(75), degree = 1) +
#                              factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
#                              factor(DiabetesRec)+ factor(CancerEvBin),
#                            design = cvdht.mort.5.fin.sa)
# summary(mod2.cvdht.sa4)

# fit a Cox model with a nonlinear effect for age using natural splines
mod2.ht.ac4 <- svycoxph(formula = Surv(fuTime, allCauseMort)~factor(CRN) + factor(EduR)+
                          bs(AGE, knots = c(75), degree = 1) +
                          factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
                          factor(DiabetesRec)+ factor(CancerEvBin) + factor(AnyCVD),
                        design = ht.mort5.fin.sa)
summary(mod2.ht.ac4)

#########################################################
#Make a "Supplementary Table 8" to show how splines affect things

table8 <- cbind(
rep(c("CRN For Diabetes", "CRN For CVD", "CRN For Hypertension"), 3),
c("291 (156 - 504)","304 (160 - 534)","340 (187 – 539)",
  "465 (360 - 652)","474 (330 - 630)","482 (360 – 635)",
  "165 (104 - 230)","161 (100 - 230)","174 (113 – 243)"),
c("8909 (23.6)", "16345 (27.8)","24166 (19.2)",
  "7379 (34.1)","13771 (37.4)","20431 (27.0)",
  "1530 (9.5)", "2574 (11.8)","3734 (7.4)"),
c(unname(paste0(sprintf(as.vector(summary(mod2.diab.ac2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.diab.ac2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.diab.ac2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")),
  unname(paste0(sprintf(as.vector(summary(mod2.cvd.ac2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.cvd.ac2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.cvd.ac2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")),
  unname(paste0(sprintf(as.vector(summary(mod2.ht.ac2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.ht.ac2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.ht.ac2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")),
  unname(paste0(sprintf(as.vector(summary(mod2.diab.ac3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.diab.ac3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.diab.ac3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")),
  unname(paste0(sprintf(as.vector(summary(mod2.cvd.ac3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.cvd.ac3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.cvd.ac3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")),
  unname(paste0(sprintf(as.vector(summary(mod2.ht.ac3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.ht.ac3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.ht.ac3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")),
  unname(paste0(sprintf(as.vector(summary(mod2.diab.ac4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.diab.ac4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.diab.ac4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")),
  unname(paste0(sprintf(as.vector(summary(mod2.cvd.ac4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.cvd.ac4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.cvd.ac4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")),
  unname(paste0(sprintf(as.vector(summary(mod2.ht.ac4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.ht.ac4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.ht.ac4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")"))),
c("3045 (8.7)","4845 (9.1)","4128 (3.3)",
  "2563 (13.0)", "4282 (12.9)", "3491 (4.6)",
  "482 (3.2)", "563 (2.8)", "637 (1.3)"),
c(unname(paste0(sprintf(as.vector(summary(mod2.diab.sa2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.diab.sa2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.diab.sa2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")),
  unname(paste0(sprintf(as.vector(summary(mod2.cvd.sa2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.cvd.sa2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.cvd.sa2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")),
  unname(paste0(sprintf(as.vector(summary(mod2.ht.sa2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.ht.sa2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.ht.sa2)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")),
  unname(paste0(sprintf(as.vector(summary(mod2.diab.sa3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.diab.sa3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.diab.sa3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")),
  unname(paste0(sprintf(as.vector(summary(mod2.cvd.sa3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.cvd.sa3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.cvd.sa3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")),
  unname(paste0(sprintf(as.vector(summary(mod2.ht.sa3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.ht.sa3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.ht.sa3)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")),
  unname(paste0(sprintf(as.vector(summary(mod2.diab.sa4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.diab.sa4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.diab.sa4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")),
  unname(paste0(sprintf(as.vector(summary(mod2.cvd.sa4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.cvd.sa4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.cvd.sa4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")),
  unname(paste0(sprintf(as.vector(summary(mod2.ht.sa4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[1], " (", 
                sprintf(as.vector(summary(mod2.ht.sa4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[2], " - ", 
                sprintf(as.vector(summary(mod2.ht.sa4)$conf.int[1, c(1,3,4)]), fmt="%.3f")[3], ")")))
)
table8 <- as.data.frame(table8)
names(table8) <- c(" ", "Follow-Up Median (IQR)", "Died N(%)", "Hazard Ratio (95% CI)", 
                   "Died N(%) ", "Hazard Ratio (95% CI)")


#make the table
table8 %>%
  #specify which ones should be bold
  # mutate(
  #   h1 = cell_spec(h1,"html" , bold = ifelse(substr(h2,8,8) == "1", T, F)),
  #   h2 = cell_spec(h2, "html", bold = ifelse(substr(h2,8,8) == "1", T, F))) %>%
  # rename(`Hazard Ratio (95% CI)` = h1, `Hazard Ratio (95% CI) ` = h2) %>%
  kable() %>%
  kable_styling("striped") %>%
  add_header_above(c(" " = 2, "All-Cause" = 2, "Disease-Specific" = 2)) %>%
  group_rows("Full Sample",start_row = 1, end_row = 3) %>%
  group_rows("2000 - 2010",start_row = 4, end_row = 6) %>%
  group_rows("2011 - 2014",start_row = 7, end_row = 9) %>%
  add_footnote("Note: All hazard ratios are weighted for survey design. Disease specific mortality is defined as having a listed cause of death of diabetes, heart or cerebrovascular disease, or heart, cerebrovascular disease or underlying hypertension for diabetes, CVD, and  hypertension models, respectively. Bold face denotes statistical significance. *The definition of CVD includes heart attack, angina pectoris, coronary heart disease, other heart condition, or stroke. 2. Hazard Ratio adjusted for age, sex, insurance (private, public, Medicare, other, or none), race (white, Black or African American, Hispanic or Latino, Asian, or other), education (≤ high school, some college, college degree or greater), and diagnoses of other chronic conditions: cancer (all models), diabetes (CVD and hypertension models), hypertension (diabetes and CVD models) and CVD (diabetes models). Abbreviations: CRN, cost-related nonadherence; CVD, cardiovascular disease; HR, Hazard Ratio.") %>%
  save_kable("Supplement_8_Splines.pdf")
  
