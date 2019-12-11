#Sarah Van Alsten
#Extend Prior Recoding of NHIS variables to include 'any' heart condition or cardiovascular disease.
#Also potentially bundle up hypertension with in that category given that is a strong risk factor


#Add up number of conditions
eligible$AnyHC <-ifelse(eligible$CHD==1 | eligible$HeartAtt==1 | eligible$AngPec==1 |
                          eligible$HeartDz==1, 1,
                        ifelse(eligible$CHD==0 & eligible$HeartAtt==0 & eligible$AngPec==0 &
                                 eligible$HeartDz==0, 0, NA))

eligible$AnyHCHT <- ifelse(eligible$AnyHC==1 | eligible$HyperTen==1,1,
                           ifelse(eligible$AnyHC==0 & eligible$HyperTen==0, 0 , NA))

eligible$AnyCVD <- ifelse(eligible$AnyHC==1 | eligible$Stroke==1, 1,
                          ifelse(eligible$AnyHC == 0 & eligible$Stroke==0, 0, NA))

eligible$AnyCVDHT <- ifelse(eligible$AnyCVD==1 | eligible$HyperTen==1, 1,
                            ifelse(eligible$AnyCVD == 0 & eligible$HyperTen==0, 0, NA))

#frequencies
table(eligible$AnyHC, eligible$DEAD)
table(eligible$AnyHC, eligible$DELAYHRSR)
table(eligible$AnyHCHT, eligible$DEAD)
table(eligible$AnyCVD, eligible$DEAD)
table(eligible$AnyCVDHT, eligible$DEAD)

#row perccentages
prop.table(table(eligible$AnyHC, eligible$DEAD),1) 
prop.table(table(eligible$AnyHCHT, eligible$DEAD),1)
prop.table(table(eligible$AnyCVD, eligible$DEAD),1)
prop.table(table(eligible$AnyCVDHT, eligible$DEAD),1)

#any heart condition
t <- eligible %>%
  #group_by(YEAR)%>%
  summarise(sum(AnyHC==1, na.rm = T),
            sum(AnyHC==1 & YEAR>=2010, na.rm = T),
            sum(AnyHC==1 & YEAR>=2011, na.rm = T),
            sum(AnyHC==1 & YEAR>=2013, na.rm = T),
            sum(AnyHC==1 & DEAD ==1, na.rm=T),
            sum(AnyHC==1 & YEAR>=2010 &DEAD ==1, na.rm = T),
            sum(AnyHC==1 & YEAR>=2011 &DEAD ==1, na.rm=T),
            sum(AnyHC==1 & YEAR>=2013 &DEAD ==1, na.rm=T))

t[2,]<-eligible %>%
  #group_by(YEAR)%>%
  summarise(sum(AnyHC==0, na.rm = T),
            sum(AnyHC==0 & YEAR>=2010, na.rm = T),
            sum(AnyHC==0 & YEAR>=2011, na.rm = T),
            sum(AnyHC==0 & YEAR>=2013, na.rm = T),
            sum(AnyHC==0 & DEAD ==1, na.rm=T),
            sum(AnyHC==0 & YEAR>=2010 &DEAD ==1, na.rm = T),
            sum(AnyHC==0 & YEAR>=2011 &DEAD ==1, na.rm=T),
            sum(AnyHC==0 & YEAR>=2013 &DEAD ==1, na.rm=T))

#any HCHT plus HT
t[3,] <- eligible %>%
  #group_by(YEAR)%>%
  summarise(sum(AnyHCHT==1, na.rm = T),
            sum(AnyHCHT==1 & YEAR>=2010, na.rm = T),
            sum(AnyHCHT==1 & YEAR>=2011, na.rm = T),
            sum(AnyHCHT==1 & YEAR>=2013, na.rm = T),
            sum(AnyHCHT==1 & DEAD ==1, na.rm=T),
            sum(AnyHCHT==1 & YEAR>=2010 &DEAD ==1, na.rm = T),
            sum(AnyHCHT==1 & YEAR>=2011 &DEAD ==1, na.rm=T),
            sum(AnyHCHT==1 & YEAR>=2013 &DEAD ==1, na.rm=T))

t[4,]<-eligible %>%
  #group_by(YEAR)%>%
  summarise(sum(AnyHCHT==0, na.rm = T),
            sum(AnyHCHT==0 & YEAR>=2010, na.rm = T),
            sum(AnyHCHT==0 & YEAR>=2011, na.rm = T),
            sum(AnyHCHT==0 & YEAR>=2013, na.rm = T),
            sum(AnyHCHT==0 & DEAD ==1, na.rm=T),
            sum(AnyHCHT==0 & YEAR>=2010 &DEAD ==1, na.rm = T),
            sum(AnyHCHT==0 & YEAR>=2011 &DEAD ==1, na.rm=T),
            sum(AnyHCHT==0 & YEAR>=2013 &DEAD ==1, na.rm=T))
#any cvd
t[5,] <- eligible %>%
  #group_by(YEAR)%>%
  summarise(sum(AnyCVD==1, na.rm = T),
            sum(AnyCVD==1 & YEAR>=2010, na.rm = T),
            sum(AnyCVD==1 & YEAR>=2011, na.rm = T),
            sum(AnyCVD==1 & YEAR>=2013, na.rm = T),
            sum(AnyCVD==1 & DEAD ==1, na.rm=T),
            sum(AnyCVD==1 & YEAR>=2010 &DEAD ==1, na.rm = T),
            sum(AnyCVD==1 & YEAR>=2011 &DEAD ==1, na.rm=T),
            sum(AnyCVD==1 & YEAR>=2013 &DEAD ==1, na.rm=T))

t[6,]<-eligible %>%
  #group_by(YEAR)%>%
  summarise(sum(AnyCVD==0, na.rm = T),
            sum(AnyCVD==0 & YEAR>=2010, na.rm = T),
            sum(AnyCVD==0 & YEAR>=2011, na.rm = T),
            sum(AnyCVD==0 & YEAR>=2013, na.rm = T),
            sum(AnyCVD==0 & DEAD ==1, na.rm=T),
            sum(AnyCVD==0 & YEAR>=2010 &DEAD ==1, na.rm = T),
            sum(AnyCVD==0 & YEAR>=2011 &DEAD ==1, na.rm=T),
            sum(AnyCVD==0 & YEAR>=2013 &DEAD ==1, na.rm=T))

#any CVD plus HT
t[7,] <- eligible %>%
  #group_by(YEAR)%>%
  summarise(sum(AnyCVDHT==1, na.rm = T),
            sum(AnyCVDHT==1 & YEAR>=2010, na.rm = T),
            sum(AnyCVDHT==1 & YEAR>=2011, na.rm = T),
            sum(AnyCVDHT==1 & YEAR>=2013, na.rm = T),
            sum(AnyCVDHT==1 & DEAD ==1, na.rm=T),
            sum(AnyCVDHT==1 & YEAR>=2010 &DEAD ==1, na.rm = T),
            sum(AnyCVDHT==1 & YEAR>=2011 &DEAD ==1, na.rm=T),
            sum(AnyCVDHT==1 & YEAR>=2013 &DEAD ==1, na.rm=T))

t[8,]<-eligible %>%
  #group_by(YEAR)%>%
  summarise(sum(AnyCVDHT==0, na.rm = T),
            sum(AnyCVDHT==0 & YEAR>=2010, na.rm = T),
            sum(AnyCVDHT==0 & YEAR>=2011, na.rm = T),
            sum(AnyCVDHT==0 & YEAR>=2013, na.rm = T),
            sum(AnyCVDHT==0 & DEAD ==1, na.rm=T),
            sum(AnyCVDHT==0 & YEAR>=2010 &DEAD ==1, na.rm = T),
            sum(AnyCVDHT==0 & YEAR>=2011 &DEAD ==1, na.rm=T),
            sum(AnyCVDHT==0 & YEAR>=2013 &DEAD ==1, na.rm=T))


#add % died to the table (ie column 5/1; 6/2; 7/3; 8/4)
for (i in 1:8){
  for (j in 5:8){
    divisor <- j-4
    percDied <- (as.numeric(t[i,j])/as.numeric(t[i,divisor]))*100 #total # died/total number per year
    t[i,j] <- paste(t[i,j], "(", sprintf(percDied, fmt="%.1f"), ")") #make string and add to cell
  }
}

#transpose table for pasting into Excel
t <- t(t)
rm(t) #don't need anymore - clear it out of ws
####################################################################################
#get descriptive stats
#library(tableone)
#all for HC
HCDescrip <- CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                        "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                        "BarrierSpecR","skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed",
                        "WorryHC","WorrySerIll", "BMI", "AGE", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "PayOvTime",'ProbPayMedBill',
                        "SatisHC","UnablePayMedBill", "InsType","Kessler6", "Kessler6Bin", "SmokeR"
                        ),
               factorVars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                              "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                              "BarrierSpecR","skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed",
                              "WorryHC","WorrySerIll", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "PayOvTime",'ProbPayMedBill',
                              "SatisHC","UnablePayMedBill", "InsType", "Kessler6Bin", "SmokeR"),
               data = eligible,
               strata = "AnyHC")


#all for HC + HT
HCHTDescrip <- CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                     "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                     "BarrierSpecR","skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed",
                                     "WorryHC","WorrySerIll", "BMI", "AGE", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "PayOvTime",'ProbPayMedBill',
                                     "SatisHC","UnablePayMedBill", "InsType","Kessler6", "Kessler6Bin", "SmokeR"
                              ),
                factorVars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                "BarrierSpecR","skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed",
                                "WorryHC","WorrySerIll", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "PayOvTime",'ProbPayMedBill',
                                "SatisHC","UnablePayMedBill", "InsType", "Kessler6Bin", "SmokeR"),
                          data = eligible,
              strata = "AnyHCHT")

#all for CVD
CVDDescrip <- CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                     "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                     "BarrierSpecR","skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed",
                                     "WorryHC","WorrySerIll", "BMI", "AGE", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "PayOvTime",'ProbPayMedBill',
                                     "SatisHC","UnablePayMedBill", "InsType","Kessler6", "Kessler6Bin", "SmokeR"
                                     ),
                            factorVars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                          "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                          "BarrierSpecR","skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed",
                                          "WorryHC","WorrySerIll", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "PayOvTime",'ProbPayMedBill',
                                          "SatisHC","UnablePayMedBill", "InsType", "Kessler6Bin", "SmokeR"),
                            data = eligible,
                            strata = "AnyCVD")


#all for CVD + HT
CVDHTDescrip <- CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                     "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                     "BarrierSpecR","skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed",
                                     "WorryHC","WorrySerIll", "BMI", "AGE", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "PayOvTime",'ProbPayMedBill',
                                     "SatisHC","UnablePayMedBill", "InsType","Kessler6", "Kessler6Bin", "SmokeR"
                                ),
                                factorVars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                                              "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                                              "BarrierSpecR","skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed",
                                              "WorryHC","WorrySerIll", "SEX", "REGION", "RaceR", "EduR", "IncomeR", "PayOvTime",'ProbPayMedBill',
                                              "SatisHC","UnablePayMedBill", "InsType", "Kessler6Bin", "SmokeR"),
                              data = eligible,
                              strata = "AnyCVDHT")

#print results
kableone(HCDescrip)
kableone(HCHTDescrip)
kableone(CVDDescrip)
kableone(CVDHTDescrip)


#get mean and sd of worry HC costs for each group
eligible %>%
  group_by(AnyHC)%>%
  summarise(mean(WorryHC, na.rm = T),
            sd(WorryHC, na.rm =T),
            mean(WorrySerIll, na.rm = T),
            sd(WorrySerIll, na.rm =T),
            median(Kessler6, na.rm = T),
            IQR(Kessler6, na.rm = T))
eligible %>%
  group_by(AnyHCHT)%>%
  summarise(mean(WorryHC, na.rm = T),
            sd(WorryHC, na.rm =T),
            mean(WorrySerIll, na.rm = T),
            sd(WorrySerIll, na.rm =T),
            median(Kessler6, na.rm = T),
            IQR(Kessler6, na.rm = T))
eligible %>%
  group_by(AnyCVD)%>%
  summarise(mean(WorryHC, na.rm = T),
            sd(WorryHC, na.rm =T),
            mean(WorrySerIll, na.rm = T),
            sd(WorrySerIll, na.rm =T),
            median(Kessler6, na.rm = T),
            IQR(Kessler6, na.rm = T))

eligible %>%
  group_by(AnyCVDHT)%>%
  summarise(mean(WorryHC, na.rm = T),
            sd(WorryHC, na.rm =T),
            mean(WorrySerIll, na.rm = T),
            sd(WorrySerIll, na.rm =T),
            median(Kessler6, na.rm = T),
            IQR(Kessler6, na.rm = T))

#non parametric alternatives to test if these worries differ by dz status
kruskal.test(eligible$WorryHC~eligible$AngPec)
kruskal.test(eligible$WorryHC~eligible$Stroke)
kruskal.test(eligible$WorryHC~eligible$HeartAtt)
kruskal.test(eligible$WorryHC~eligible$AnyHC)
kruskal.test(eligible$WorryHC~eligible$AnyHCHT)
kruskal.test(eligible$WorryHC~eligible$AnyCVD)
kruskal.test(eligible$WorryHC~eligible$AnyCVDHT)
kruskal.test(eligible$WorrySerIll~eligible$AnyHC)
kruskal.test(eligible$WorrySerIll~eligible$AnyHCHT)
kruskal.test(eligible$WorrySerIll~eligible$AnyCVD)
kruskal.test(eligible$WorrySerIll~eligible$AnyCVDHT)

#follow up time
t(eligible %>%
  group_by(DiabetesRec)%>%
  summarise(mean(fuTime, na.rm =T),
            sd(fuTime, na.rm = T)))

a <- t(eligible %>%
          group_by(HeartDz)%>%
          summarise(mean(fuTime, na.rm =T),
                    sd(fuTime, na.rm = T)))[2:3,]

a<-rbind(a, t(eligible %>%
              group_by(HeartAtt)%>%
              summarise(mean(fuTime, na.rm =T),
                        sd(fuTime, na.rm = T)))[2:3,])

a<-rbind(a, t(eligible %>%
                group_by(CHD)%>%
                summarise(mean(fuTime, na.rm =T),
                          sd(fuTime, na.rm = T)))[2:3,])

a<-rbind(a, t(eligible %>%
                group_by(HyperTen)%>%
                summarise(mean(fuTime, na.rm =T),
                          sd(fuTime, na.rm = T)))[2:3,])

a<-rbind(a, t(eligible %>%
                group_by(AngPec)%>%
                summarise(mean(fuTime, na.rm =T),
                          sd(fuTime, na.rm = T)))[2:3,])

a<-rbind(a, t(eligible %>%
                group_by(Stroke)%>%
                summarise(mean(fuTime, na.rm =T),
                          sd(fuTime, na.rm = T)))[2:3,])

a<-rbind(a, t(eligible %>%
                group_by(AnyHC)%>%
                summarise(mean(fuTime, na.rm =T),
                          sd(fuTime, na.rm = T)))[2:3,])

a<-rbind(a, t(eligible %>%
                group_by(AnyHCHT)%>%
                summarise(mean(fuTime, na.rm =T),
                          sd(fuTime, na.rm = T)))[2:3,])

a<-rbind(a, t(eligible %>%
                group_by(AnyCVD)%>%
                summarise(mean(fuTime, na.rm =T),
                          sd(fuTime, na.rm = T)))[2:3,])
a<-rbind(a, t(eligible %>%
                group_by(AnyCVDHT)%>%
                summarise(mean(fuTime, na.rm =T),
                          sd(fuTime, na.rm = T)))[2:3,])
#remove column for NA on dz
a <- a[,-3]

#now concatenate to make easier for excel
for (i in 1:nrow(a)){
  a[i,1] <- paste(round(as.numeric(a[i,1]),1), "(", round(as.numeric(a[i,2]), 1), ")")
}
a <- a[,1]
a <- t(a)


#skimp meds variable
CreateCatTable("lessMed", strata = "DiabetesRec", data = eligible)
CreateCatTable("lessMed", strata = "HeartDz", data = eligible)
CreateCatTable("lessMed", strata = "HyperTen", data = eligible)
CreateCatTable("lessMed", strata = "CHD", data = eligible)
CreateCatTable("lessMed", strata = "HeartAtt", data = eligible)
CreateCatTable("lessMed", strata = "AngPec", data = eligible)
CreateCatTable("lessMed", strata = "Stroke", data = eligible)
CreateCatTable("lessMed", strata = "AnyHC", data = eligible)
CreateCatTable("lessMed", strata = "AnyHCHT", data = eligible)
CreateCatTable("lessMed", strata = "AnyCVD", data = eligible)
CreateCatTable("lessMed", strata = "AnyCVDHT", data = eligible)
###################################################################################
#2010 on
laterYrs <- eligible[eligible$YEAR >=2010,]

DMDescrip <- CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
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
data = laterYrs,
strata = "DiabetesRec")

HDDescrip <- CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
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
data = laterYrs,
strata = "HeartDz")

HTDescrip <- CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
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
data = laterYrs,
strata = "HyperTen")

CHDDescrip <- CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
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
data = laterYrs,
strata = "CHD")

HADescrip <- CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
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
data = laterYrs,
strata = "HeartAtt")

APDescrip <- CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
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
data = laterYrs,
strata = "AngPec")

StrDescrip <- CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
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
data = laterYrs,
strata = "Stroke")

HCDescrip <- CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
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
data = laterYrs,
strata = "AnyHC")

HCHTDescrip <- CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
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
data = laterYrs,
strata = "AnyHCHT")

CVDDescrip <- CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
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
data = laterYrs,
strata = "AnyCVD")

CVDHTDescrip <- CreateTableOne(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
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
data = laterYrs,
strata = "AnyCVDHT")

table(laterYrs$CRNsum, laterYrs$DiabetesRec)
prop.table(table(laterYrs$CRNsum, laterYrs$DiabetesRec),2)
