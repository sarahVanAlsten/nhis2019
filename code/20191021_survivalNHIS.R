#Sarah Van Alsten
#Use prior recodings of variables to do Cox PH regression and find association of Med non adherence to Mortaliry

#load additional packages
library(survival)
library(survminer)
eligible <- eligible %>%
  mutate(lessMed = ifelse(YSKIMPMEDYR > 2, NA,
                          ifelse(YSKIMPMEDYR == 1, 0,
                                 ifelse(YSKIMPMEDYR ==2, 1,NA))))%>%
  mutate(CRN = ifelse(lessMed==1 |delayMed ==1 | skipMed == 1, 1,
                      ifelse(lessMed == 0 & delayMed == 0 & skipMed == 0, 0, NA)))

eligible$CRNsum <- rowSums(eligible[, names(eligible)%in% c("lessMed","skipMed","delayMed")])

#Making subsets of the data
hd <- eligible[eligible$AnyHC==1,]
hdht <- eligible[eligible$AnyHCHT==1,]
cvd <- eligible[eligible$AnyCVD==1,]
cvdht <- eligible[eligible$AnyCVDHT==1,]
diabDat <- eligible[eligible$DiabetesRec==1,]
stroke <- eligible[eligible$Stroke==1,]
angpec <- eligible[eligible$AngPec==1,]
chd <- eligible[eligible$CHD==1,]
ht <- eligible[eligible$HyperTen==1,]
otherHtDz <- eligible[eligible$HeartDz==1,]
canc <- eligible[eligible$CancerEvBin==1,]
asth <- eligible[eligible$Asthma==1,]
cf <- eligible[eligible$CysFib==1,]
hatt <- eligible[eligible$HeartAtt==1,]

diabDat$CRNsum <- rowSums(diabDat[, names(diabDat)%in% c("lessMed","skipMed","delayMed")])

#f1 <- survfit(Surv(fuTime, DEAD)~1, data = eligible)
# ggsurvplot(
#   fit = f1, 
#   xlab = "Weeks", 
#   ylab = "Overall survival probability")
# 
# f1 <- survfit(Surv(fuTime, DEAD)~DiabetesRec, data = eligible, weights = MORTWTSA)
# ggsurvplot(
#   fit = f1, 
#   xlab = "Weeks", 
#   ylab = "Overall survival probability",
#   conf.int = TRUE,
#   pval = TRUE,
#   censor.size = 1)
# 
# eligible <- eligible %>%
#   mutate(InsulinRec = ifelse(INSULIN == 2,1,
#                              ifelse(INSULIN == 1, 0, NA)))
# 
# f1 <- coxph(Surv(fuTime, DEAD)~anyCostBarrier*DiabetesRec +AGE+SEX+BMI, data = eligible, weights = MORTWTSA)
# summary(f1)
# f1 <- coxph(Surv(fuTime, DEAD)~anyMedBx*DiabetesRec +AGE+SEX+BMI, data = eligible, weights = MORTWTSA)
# summary(f1)

diabDat <- eligible[eligible$DiabetesRec==1,]
diabDat <-
  diabDat %>%
  mutate(CRN_plus = ifelse(CRN ==1 | BarrierMedR ==1, 1,
                           ifelse(BarrierMedR == 0 & (CRN == 0 | (is.na(CRN)& YEAR <2010)), 0, NA))) %>%
  mutate(CRN_plus2 = ifelse(CRN_plus == 1 | anyMedBx2 ==1, 1,
                            ifelse(CRN_plus == 0 & (anyMedBx2 == 0 | (is.na(anyMedBx2)& YEAR <2010)), 0, NA)))

diabDat %>%
  group_by(YEAR)%>%
  summarise(sum(CRN==1, na.rm = T),
            sum(CRN_plus==1, na.rm = T),
            sum(CRN_plus2==1, na.rm = T),
            sum(anyMedBx==1, na.rm = T),
            sum(anyMedBx2==1, na.rm = T),
            sum(BarrierMedR==1, na.rm = T),
            sum(InsType==1, na.rm =T))


f1 <- coxph(Surv(fuTime, DEAD)~CRN_plus2 +AGE+SEX+BMI, data = diabDat, weights = MORTWTSA)
summary(f1)

f1 <- coxph(Surv(fuTime, DEAD)~anyMedBx2 +AGE+SEX+BMI, data = diabDat, weights = MORTWTSA)
summary(f1)

f1 <- coxph(Surv(fuTime, DEAD)~BarrierMedR+AGE+SEX+BMI, data = diabDat, weights = MORTWTSA)
summary(f1)

f1 <- coxph(Surv(fuTime, DEAD)~CRN_plus2+AGE+SEX+BMI+ factor(InsType)+ 
              factor(RaceR)+ factor(IncomeR)+factor(Kessler6Bin)+ factor(SmokeR), 
            data = diabDat, weights = MORTWTSA)
summary(f1)
zf1 <-cox.zph(f1) #PH assumption met
ggcoxdiagnostics(f1)
ggcoxzph(zf1)
#ggcoxdiagnostics(f1, type = "dfbeta",
#                 linear.predictions = FALSE, ggtheme = theme_bw())
#get residuals
mresid <- resid(f1, weighted = TRUE, type = "dfbetas")
mresid <- as.data.frame(mresid)
names(mresid) <- c("CRN","AGE","SEX","BMI","INSTYPE1","INSTYPE2","INSTYPE3","INSTYPE4","INSTYPE5",
                   "RACE_BLACK","RACE_HIS","RACE_AIAN","RACE_ASIAN","RACE_OTHER","INCOME1","INCOME2",
                   "INCOME3","INCOME4","INCOME5","DEPRESSION", "SMOKE_FORMER", "SMOKE_CURR")
dfbetaFullModel <- mresid

mresid <- resid(f1, weighted = TRUE, type = "deviance")
mresid <- as.data.frame(mresid)
names(mresid) <- c("Deviance")
devFullModel <- mresid

mresid <- resid(f1, weighted = TRUE, type = "schoenfeld")
mresid <- as.data.frame(mresid)
names(mresid) <- c("CRN","AGE","SEX","BMI","INSTYPE1","INSTYPE2","INSTYPE3","INSTYPE4","INSTYPE5",
                   "RACE_BLACK","RACE_HIS","RACE_AIAN","RACE_ASIAN","RACE_OTHER","INCOME1","INCOME2",
                   "INCOME3","INCOME4","INCOME5","DEPRESSION", "SMOKE_FORMER", "SMOKE_CURR")
schFullModel <- mresid

mresid <- resid(f1, weighted = TRUE, type = "martingale")
mresid <- as.data.frame(mresid)
martFullModel <- mresid

#deviance plot
plot(f1$linear.predictor, devFullModel$Deviance,
     xlab="Risk Score",ylab="Deviance Residuals")
abline(0,0,lty=2,col='red')



f1 <- coxph(Surv(fuTime, DEAD)~anyMedBx2+AGE+SEX+BMI+ factor(InsType)+ factor(RaceR)+ factor(IncomeR),
            data = diabDat, weights = MORTWTSA)
summary(f1)

f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+AGE+SEX+BMI+ factor(InsType)+ factor(RaceR)+ factor(IncomeR),
            data = diabDat, weights = MORTWTSA)
summary(f1)

f1 <- coxph(Surv(fuTime, DEAD)~factor(BarrierMedR)+AGE+SEX+BMI+factor(InsType)+ordered(IncomeR), data = diabDat, weights = MORTWTSA)
summary(f1)
ggcoxdiagnostics(f1)

f1 <- coxph(Surv(fuTime, DEAD)~AGE+ordered(IncomeR)+InsulinRec+anyMedBx+SEX, data = diabDat, weights = MORTWTSA)
summary(f1)
zf1 <-cox.zph(f1) #PH assumption met
memory.limit(size = 15000)
ggadjustedcurves(f1, 
                 data = diabDat, 
                 variable = 'anyMedBx')




hd$anyMedBx = relevel(as.factor(hd$anyMedBx), ref = "0")
f1 <- coxph(Surv(fuTime, DEAD)~AGE+ordered(IncomeR)+BarrierMedR+SEX+factor(InsType)+ factor(RaceR), data = hd, weights = MORTWTSA)
summary(f1)

f1 <- coxph(Surv(fuTime, CvdDeath)~AGE+ordered(IncomeR)+BarrierMedR+SEX+factor(InsType)+ factor(RaceR), data = hd, weights = MORTWTSA)
summary(f1)

f1 <- coxph(Surv(fuTime, DiabDeath)~AGE+ordered(IncomeR)+CRN_plus2+SEX+factor(InsType)+ factor(RaceR), 
            data = diabDat, weights = MORTWTSA)
summary(f1)


zf1 <-cox.zph(f1) #PH assumption met
memory.limit(size = 15000)
ggadjustedcurves(f1, 
                 data = diabDat, 
                 variable = 'anyMedBx')

