#Sarah Van Alsten
#Febraury 3, 2020
#Purpose: Check Multicolliearity by fitting linear model
#and checking vifs.
#Packages Used: Car

#read in data
eligible <- read.csv("data//eligible.csv")

#################################################################
#check multicollinearity
vifmod <- lm(data = eligible, formula =DEAD~ factor(CRN) + factor(EduR)+ AGE +
               factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR))
car::vif(vifmod)

vifmod <- lm(data = eligible, formula =DEAD~ factor(CRN) + factor(EduR)+ AGE +
               factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) +
               factor(CancerEvBin) + factor(DiabetesRec) + factor(AnyCVDHT))
car::vif(vifmod)
