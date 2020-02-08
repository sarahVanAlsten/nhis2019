#########################
# Sarah Van Alsten
# Feb 6, 2020
# Compare influential cases (statistically) to non-influential
#######################
library(tidyverse)
library(tableone)

#read in csv
diab <- read_csv("data\\possible_inf_diab.csv")
diabAC <- read_csv("data\\possible_inf_diabAC.csv")

cvd <- read_csv("data\\possible_inf_cvd.csv")
cvdAC <- read_csv("data\\possible_inf_cvd_allcause.csv")

cvdht <- read_csv("data\\possible_inf_cvdht.csv")
cvdhtAC <- read_csv("data\\possible_inf_cvdht_allcause.csv")


#bind them all together
inf.all <- rbind(diab, cvd, cvdht)
inf.all.ac <- gtools::smartbind(diabAC, cvdAC, cvdhtAC)

#get eligible observations that are NOT influential in order to compare
eligible <- eligible %>%
  mutate(inf.ds = ifelse(NHISPID %in% inf.all$NHISPID, 1, 0),
         inf.ac = ifelse(NHISPID %in% inf.all.ac$NHISPID, 1, 0),
         any.inf = ifelse(NHISPID %in% c(inf.all$NHISPID, inf.all.ac$NHISPID), 1, 0))


#only look at persons with dz
eligible.dz <- eligible %>%
  filter(DiabetesRec == 1 | AnyCVD == 1 | AnyCVDHT == 1)

eligible.dz.diab <- eligible.dz %>%
  filter(DiabetesRec == 1)
eligible.dz.cvd <- eligible.dz %>%
  filter(AnyCVD == 1)
eligible.dz.cvdht <- eligible.dz %>%
  filter(AnyCVDHT == 1)

dz2des <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ perWeight14,
                                    nest = TRUE, data = eligible)


#look at descriptives
a <- svyCreateTableOne(vars = c("AGE", "SEX", "BMI", "REGION","RaceR", "InsType", "EduR", "IncomeR",
                        "CRN", "SmokeR", "fuTime", "CancerEvBin", "DiabetesRec", "AnyCVD", "HyperTen"), 
               strata = 'inf.ds', data = dz2des, 
                factorVars = c("SEX", "REGION", "RaceR", "InsType", "EduR", "IncomeR", "SmokeR",
                               "CancerEvBin", "DiabetesRec", "AnyCVD", "HyperTen", "CRN"),
               includeNA = FALSE,
                test = TRUE, smd = TRUE)

print(a, nonnormal = c("AGE"))


