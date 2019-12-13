##################################################################
#Sarah Van Alsten
#Created: Dec 13, 2019
#Use the cleaned data from 20190928_NHIS.R to create table 1 (descriptive stats)
#Packages used: ipumsr, tidyverse, tableone, survival, survey
#Last Update: Dec 13, 2019
################################################################################
#first, create the survey design for the study, using the appropriate
#weight variables. 'PERWEIGHT' is for variables asked of everyone, whereas
#'SAMPWEIGHT' is for variables in the supplements, only asked of selected
#participants. Finally, for survival analyses, MORTWTSA is used to correct
#for eligibility issues with those not provding enough data for linkage.
#'STRATA' represents sampling stratum, 'PSU' represent primary sampling unit
#For pooled analyses, divide WT by number of included years. 
#I also will need to use subpop option because I am choosing select participants
#(eg with diabetes, CVD...)
##################################################################################
#YBARMEDS is in access to care supplement, so it should have SAMPWEIGHT.
#CRN (the 3 item responses) also asked of SAMPLE adults so should use SAMPWEIGHT
#smokev is also a sample adult so it used sampweight.
#conditions are for sample adults
#others are demographic and use PERWEIGHT

#make four survey designs: one for sampweight, one for perweight, one for
#mortality weight for 2000-2014, and one for mortality weight 2000-2010
library(survey)
library(tableone)
eligible <- read.csv("data\\eligible.csv")

eligible<- eligible %>%
  mutate(sampWeight14 = SAMPWEIGHT / 15,
         sampWeight10 = SAMPWEIGHT / 11)
eligible$samp10 <- 
  samp.Svy <- svydesign(ids = ~ PSU, strata = ~ STRATA, weights = ~ (SAMPWEIGHT/14),
                        nest = TRUE, data = eligible)


svyCreateTableOne2(vars = c("HI_CHOL","race","agecat","RIAGENDR"),
                   strata = "RIAGENDR", data = nhanesSvy)

tableone::svyCreateTableOne(vars, strata = STRATA, data = eligible, 
                            factorVars, includeNA = FALSE,
                            test = TRUE, testApprox = svyTestChisq, argsApprox = NULL,
                            testNormal = svyTestNormal, argsNormal = list(method = "Wald"),
                            testNonNormal = svyTestNonNormal, argsNonNormal = NULL, smd = TRUE)


