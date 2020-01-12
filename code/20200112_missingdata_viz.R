#Author: Sarah Van Alsten
#Date Created: January 12, 2020
#Purpose: visualize the missing data for NHIS/CRN/Mortality Study
#Last Update: January 12, 2020
#Packages used: tidyverse, UpSetR
###############################
##look at missing data patterns
library(tidyverse)
library(VIM)
library(UpSetR)


#take subsets of the vars we are interested in
diab.dat <- diab.mort14.fin.sa$variables[ , c("CRN", "diabMort", "allCauseMort", 
                                              "HyperTen", "AnyCVD", "EduR", 
                                              "IncomeR", "AGE", "SEX", "RaceR",
                                              "CancerEvBin", "DiabetesRec",
                                              "InsType")]

aggr(diab.dat, prop = T, numbers = F) #less than 1% missing on these vars


cvd.dat <- cvd.mort14.fin.sa$variables[ , c("CRN", "cvdMort", "allCauseMort", 
                                              "HyperTen", "AnyCVD", "EduR", 
                                              "IncomeR", "AGE", "SEX", "RaceR",
                                              "CancerEvBin", "DiabetesRec",
                                              "InsType")]

aggr(cvd.dat, prop = T, numbers = F) #less than 1% missing

cvdht.dat <- cvdht.mort14.fin.sa$variables[ , c("CRN", "diabMort", "allCauseMort", 
                                              "HyperTen", "AnyCVD", "EduR", 
                                              "IncomeR", "AGE", "SEX", "RaceR",
                                              "CancerEvBin", "DiabetesRec",
                                              "InsType")]

aggr(cvdht.dat, prop = T, numbers = F)
