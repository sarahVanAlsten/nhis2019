#Sarah Van Alsten
#October 27, 2019
#Descriptives only of people with Chrnonic Illness, subset by CRN any

library(tidyverse)
library(tableone)
library(survival)
library(car)
library(survminer)

memory.limit(size = 15000)
#subset dat to include diabetics only
laterYrs <-
  laterYrs %>%
  mutate(BMIcat = ifelse(BMI < 18.5, 0,
                         ifelse(BMI < 25, 1,
                                ifelse(BMI < 30, 2,
                                       ifelse(is.na(BMI), NA, 3)))),
         Agecat = ifelse(AGE < 45, 1,
                         ifelse(AGE < 65, 2,
                                ifelse(AGE < 85, 3,
                                       ifelse(is.na(AGE), NA, 4)))),
         Over64 = ifelse(AGE>=65, 1, 
                         ifelse(is.na(AGE), NA, 0)),
         Medicare = ifelse(InsType==4, 1, 
                           ifelse(is.na(InsType), NA, 0)))

diabDat <- laterYrs[laterYrs$DiabetesRec==1,]
hdDat <-laterYrs[laterYrs$HeartDz==1,]
htDat <-laterYrs[laterYrs$HyperTen==1,]
chdDat <- laterYrs[laterYrs$CHD==1,]
haDat <- laterYrs[laterYrs$HeartAtt==1,]
apDat <- laterYrs[laterYrs$AngPec==1,]
strDat <- laterYrs[laterYrs$Stroke==1,]
hcDat <- laterYrs[laterYrs$AnyHC==1,]
hchtDat <- laterYrs[laterYrs$AnyHCHT==1,]
cvdDat <- laterYrs[laterYrs$AnyCVD==1,]
cvdhtDat <- laterYrs[laterYrs$AnyCVDHT==1,]

#########################################################################################################
kableone(CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                     "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                     "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
                                     "BMI", "AGE", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "InsType",
                                     "Kessler6", "Kessler6Bin", "SmokeR"
),
factorVars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
               "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
               "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
               "SEX", "REGION", "RaceR", "EduR", "IncomeR", 
               "InsType", "Kessler6Bin", "SmokeR"),
data = diabDat,
strata = "CRN"))

kableone(CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                 "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                 "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
                                 "BMI", "AGE", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "InsType",
                                 "Kessler6", "Kessler6Bin", "SmokeR"
),
factorVars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
               "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
               "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
               "SEX", "REGION", "RaceR", "EduR", "IncomeR", 
               "InsType", "Kessler6Bin", "SmokeR"),
data = hdDat,
strata = "CRN"))

kableone(CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                 "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                 "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
                                 "BMI", "AGE", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "InsType",
                                 "Kessler6", "Kessler6Bin", "SmokeR"
),
factorVars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
               "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
               "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
               "SEX", "REGION", "RaceR", "EduR", "IncomeR", 
               "InsType", "Kessler6Bin", "SmokeR"),
data = htDat,
strata = "CRN"))


kableone(CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                 "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                 "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
                                 "BMI", "AGE", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "InsType",
                                 "Kessler6", "Kessler6Bin", "SmokeR"
),
factorVars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
               "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
               "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
               "SEX", "REGION", "RaceR", "EduR", "IncomeR", 
               "InsType", "Kessler6Bin", "SmokeR"),
data = chdDat,
strata = "CRN"))


kableone(CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                 "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                 "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
                                 "BMI", "AGE", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "InsType",
                                 "Kessler6", "Kessler6Bin", "SmokeR"
),
factorVars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
               "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
               "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
               "SEX", "REGION", "RaceR", "EduR", "IncomeR", 
               "InsType", "Kessler6Bin", "SmokeR"),
data = haDat,
strata = "CRN"))


kableone(CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                 "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                 "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
                                 "BMI", "AGE", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "InsType",
                                 "Kessler6", "Kessler6Bin", "SmokeR"
),
factorVars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
               "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
               "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
               "SEX", "REGION", "RaceR", "EduR", "IncomeR", 
               "InsType", "Kessler6Bin", "SmokeR"),
data = apDat,
strata = "CRN"))


kableone(CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                 "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                 "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
                                 "BMI", "AGE", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "InsType",
                                 "Kessler6", "Kessler6Bin", "SmokeR"
),
factorVars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
               "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
               "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
               "SEX", "REGION", "RaceR", "EduR", "IncomeR", 
               "InsType", "Kessler6Bin", "SmokeR"),
data = strDat,
strata = "CRN"))

kableone(CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                 "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                 "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
                                 "BMI", "AGE", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "InsType",
                                 "Kessler6", "Kessler6Bin", "SmokeR"
),
factorVars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
               "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
               "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
               "SEX", "REGION", "RaceR", "EduR", "IncomeR", 
               "InsType", "Kessler6Bin", "SmokeR"),
data = hcDat,
strata = "CRN"))

kableone(CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                 "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                 "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
                                 "BMI", "AGE", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "InsType",
                                 "Kessler6", "Kessler6Bin", "SmokeR"
),
factorVars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
               "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
               "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
               "SEX", "REGION", "RaceR", "EduR", "IncomeR", 
               "InsType", "Kessler6Bin", "SmokeR"),
data = hchtDat,
strata = "CRN"))

kableone(CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                 "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                 "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
                                 "BMI", "AGE", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "InsType",
                                 "Kessler6", "Kessler6Bin", "SmokeR"
),
factorVars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
               "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
               "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
               "SEX", "REGION", "RaceR", "EduR", "IncomeR", 
               "InsType", "Kessler6Bin", "SmokeR"),
data = cvdDat,
strata = "CRN"))

kableone(CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                 "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                 "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
                                 "BMI", "AGE", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "InsType",
                                 "Kessler6", "Kessler6Bin", "SmokeR"
),
factorVars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
               "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
               "BarrierSpecR","skipMed", "delayMed","lessMed", "CheapMed", "foreignMed", "alternateMed",
               "SEX", "REGION", "RaceR", "EduR", "IncomeR", 
               "InsType", "Kessler6Bin", "SmokeR"),
data = hchtDat,
strata = "CRN"))
############################################################################################
CreateCatTable("DEAD", strata = "CRN", diabDat)
CreateCatTable("DEAD", strata = "CRN", hdDat)
CreateCatTable("DEAD", strata = "CRN", htDat)
CreateCatTable("DEAD", strata = "CRN", chdDat)
CreateCatTable("DEAD", strata = "CRN", haDat)
CreateCatTable("DEAD", strata = "CRN", apDat)
CreateCatTable("DEAD", strata = "CRN", strDat)
CreateCatTable("DEAD", strata = "CRN", hcDat)
CreateCatTable("DEAD", strata = "CRN", hchtDat)
CreateCatTable("DEAD", strata = "CRN", cvdDat)
CreateCatTable("DEAD", strata = "CRN", cvdhtDat)

###################################################################################################
ggplot(diabDat, aes(x=fuTime))+geom_density()
ggplot(diabDat, aes(x=AGE))+geom_density()
ggplot(diabDat, aes(x=BMI))+geom_density()
###########################################
ggplot(hdDat, aes(x=fuTime))+geom_density()
ggplot(hdDat, aes(x=AGE))+geom_density()
ggplot(hdDat, aes(x=BMI))+geom_density()
###########################################
ggplot(htDat, aes(x=fuTime))+geom_density()
ggplot(htDat, aes(x=AGE))+geom_density()
ggplot(htDat, aes(x=BMI))+geom_density()

############################################
#function to summarize quartiles
p <- c(0.5, 0.25, 0.75)
p_names <- map_chr(p, ~paste0(.x*100, "%"))

p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)

#diabDat %>%
#hdDat%>%
#htDat%>%
#chdDat %>%
#haDat%>%
#apDat %>%
#strDat %>%
#hcDat%>%
#hchtDat %>%
#cvdDat %>%
cvdhtDat%>%
  group_by(CRN)%>%
  summarize_at(vars(c(AGE, fuTime)), funs(!!!p_funs))

kruskal.test(diabDat$CRN~diabDat$AGE)
kruskal.test(hdDat$CRN~hdDat$AGE)
kruskal.test(htDat$CRN~htDat$AGE)
kruskal.test(strDat$CRN~strDat$AGE)
kruskal.test(haDat$CRN~haDat$AGE)
kruskal.test(apDat$CRN~apDat$AGE)
kruskal.test(chdDat$CRN~chdDat$AGE)
kruskal.test(hcDat$CRN~hcDat$AGE)
kruskal.test(cvdDat$CRN~cvdDat$AGE)
kruskal.test(cvdhtDat$CRN~cvdhtDat$AGE)
kruskal.test(hchtDat$CRN~hchtDat$AGE)

kruskal.test(diabDat$CRN~diabDat$fuTime)
kruskal.test(hdDat$CRN~hdDat$fuTime)
kruskal.test(htDat$CRN~htDat$fuTime)
kruskal.test(strDat$CRN~strDat$fuTime)
kruskal.test(haDat$CRN~haDat$fuTime)
kruskal.test(apDat$CRN~apDat$fuTime)
kruskal.test(chdDat$CRN~chdDat$fuTime)
kruskal.test(hcDat$CRN~hcDat$fuTime)
kruskal.test(cvdDat$CRN~cvdDat$fuTime)
kruskal.test(cvdhtDat$CRN~cvdhtDat$fuTime)
kruskal.test(hchtDat$CRN~hchtDat$fuTime)
