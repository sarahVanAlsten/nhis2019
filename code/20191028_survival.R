
#########################################################################################
#diabetes
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN),
            data = diabDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed),
            data = diabDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed),
            data = diabDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed),
            data = diabDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum),
            data = diabDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum),
            data = diabDat, 
            weights = MORTWTSA)
summary(f1)
##########################################################
#heart disease any
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN),
            data = hcDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed),
            data = hcDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed),
            data = hcDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed),
            data = hcDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum),
            data = hcDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum),
            data = hcDat, 
            weights = MORTWTSA)
summary(f1)

###########################################################
##########################################################
#ypertension
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN),
            data = htDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed),
            data = htDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed),
            data = htDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed),
            data = htDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum),
            data = htDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum),
            data = htDat, 
            weights = MORTWTSA)
summary(f1)
#####################################################################################################3
#########################################################################################
row1 <- c("","","")
mod2 <- t(data.frame(row1))
mod2 <- unname(mod2)
#diabetes
#library(broom)
#library(gtools)
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = diabDat, 
            weights = MORTWTSA)
summary(f1)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod2<-mod2[2,]
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = diabDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = diabDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = diabDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = diabDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = diabDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
##########################################################
#heart disease any
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = hcDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = hcDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = hcDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = hcDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = hcDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = hcDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
###########################################################
#hypertension
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = htDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = htDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = htDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = htDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = htDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR),
            data = htDat, 
            weights = MORTWTSA)
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod2<-gtools::smartbind(mod2,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
#####################
#print results
mod2[,1]<- as.numeric(mod2[,1])
mod2[,2]<- as.numeric(mod2[,2])
mod2[,3]<- as.numeric(mod2[,3])
for (i in 1:nrow(mod2)){
  print(paste0(sprintf("%.3f", mod2[i,1]), " (",
               sprintf("%.3f", mod2[i,2]), " - ",
               sprintf("%.3f", mod2[i,3]), ")"))
}
########################################################3
#########################################################################################
row1 <- c("","","")
mod3 <- t(data.frame(row1))
mod3 <- unname(mod3)
#diabetes
#library(broom)
#library(gtools)
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
summary(f1)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3<-mod3[2,]
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
##########################################################
#heart disease any
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = hcDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = hcDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = hcDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = hcDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = hcDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = hcDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
###########################################################
#hypertension
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = htDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = htDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = htDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = htDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = htDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = htDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
#####################
#print results
mod3[,1]<- as.numeric(mod3[,1])
mod3[,2]<- as.numeric(mod3[,2])
mod3[,3]<- as.numeric(mod3[,3])
for (i in 1:nrow(mod3)){
  print(paste0(sprintf("%.3f", mod3[i,1]), " (",
               sprintf("%.3f", mod3[i,2]), " - ",
               sprintf("%.3f", mod3[i,3]), ")"))
}
########################################################3
#########################################################################################
row1 <- c("","","")
mod3b <- t(data.frame(row1))
mod3b <- unname(mod3b)
#diabetes
#library(broom)
#library(gtools)
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = diabDat, 
            weights = MORTWTSA)
summary(f1)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3b<-mod3b[2,]
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = diabDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = diabDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = diabDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = diabDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = diabDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
##########################################################
#heart disease any
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = hcDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = hcDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = hcDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = hcDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = hcDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = hcDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
###########################################################
#hypertension
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = htDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = htDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = htDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = htDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = htDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR) + factor(Kessler6Bin),
            data = htDat, 
            weights = MORTWTSA)
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod3b<-gtools::smartbind(mod3b,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
#####################
#print results
mod3b[,1]<- as.numeric(mod3b[,1])
mod3b[,2]<- as.numeric(mod3b[,2])
mod3b[,3]<- as.numeric(mod3b[,3])
for (i in 1:nrow(mod3b)){
  print(paste0(sprintf("%.3f", mod3b[i,1]), " (",
               sprintf("%.3f", mod3b[i,2]), " - ",
               sprintf("%.3f", mod3b[i,3]), ")"))
}
###########################################################################
diabDat <-
  diabDat %>%
  mutate(InsulinR = ifelse(INSULIN == 2, 1,
                           ifelse(INSULIN %in% c(0,7,8,9), NA, 0)))

row1 <- c("","","")
mod4 <- t(data.frame(row1))
mod4 <- unname(mod4)

f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)*factor(InsulinR)+factor(SEX)+BMI+AGE+factor(RaceR)+
              factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR)+factor(REGION)+
              factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
summary(f1)
mod4<-gtools::smartbind(mod4,tidy(f1, exponentiate=TRUE)[26,c(2,6:7)])
mod4<-mod4[2,]
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)*factor(InsulinR)+
              factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
mod4<-gtools::smartbind(mod4,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)*factor(InsulinR)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
mod4<-gtools::smartbind(mod4,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)*factor(InsulinR)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
mod4<-gtools::smartbind(mod4,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)*factor(InsulinR)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
mod4<-gtools::smartbind(mod4,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)*factor(InsulinR)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
mod4<-gtools::smartbind(mod4,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod4<-gtools::smartbind(mod4,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod4<-gtools::smartbind(mod4,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
#####################
#print results
mod4[,1]<- as.numeric(mod4[,1])
mod4[,2]<- as.numeric(mod4[,2])
mod4[,3]<- as.numeric(mod4[,3])
for (i in 1:nrow(mod4)){
  print(paste0(sprintf("%.3f", mod4[i,1]), " (",
               sprintf("%.3f", mod4[i,2]), " - ",
               sprintf("%.3f", mod4[i,3]), ")"))
}
########################################################################################################
#########################################################################################
row1 <- c("","","")
mod3 <- t(data.frame(row1))
mod3 <- unname(mod3)
#diabetes
#library(broom)
#library(gtools)
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
summary(f1)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3<-mod3[2,]
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = diabDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
##########################################################
#heart disease any
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = hcDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = hcDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = hcDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = hcDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = hcDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = hcDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
###########################################################
#hypertension
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = htDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = htDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = htDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = htDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = htDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE+factor(RaceR)+factor(REGION)+factor(SmokeR)+factor(InsType)+ordered(IncomeR),
            data = htDat, 
            weights = MORTWTSA)
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod3<-gtools::smartbind(mod3,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
#####################
#print results
mod3[,1]<- as.numeric(mod3[,1])
mod3[,2]<- as.numeric(mod3[,2])
mod3[,3]<- as.numeric(mod3[,3])
for (i in 1:nrow(mod3)){
  print(paste0(sprintf("%.3f", mod3[i,1]), " (",
               sprintf("%.3f", mod3[i,2]), " - ",
               sprintf("%.3f", mod3[i,3]), ")"))
}
########################################################3
#########################################################################################
row1 <- c("","","")
mod3c <- t(data.frame(row1))
mod3c <- unname(mod3c)
#diabetes
#library(broom)
#library(gtools)
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE,
            data = diabDat, 
            weights = MORTWTSA)
summary(f1)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3c<-mod3c[2,]
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE,
            data = diabDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE,
            data = diabDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE,
            data = diabDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE,
            data = diabDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE,
            data = diabDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
##########################################################
#heart disease any
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE,
            data = hcDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE,
            data = hcDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE,
            data = hcDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE,
            data = hcDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE,
            data = hcDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE,
            data = hcDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
###########################################################
#hypertension
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE,
            data = htDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE,
            data = htDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE,
            data = htDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE,
            data = htDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE,
            data = htDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE,
            data = htDat, 
            weights = MORTWTSA)
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod3c<-gtools::smartbind(mod3c,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
#####################
#print results
mod3c[,1]<- as.numeric(mod3c[,1])
mod3c[,2]<- as.numeric(mod3c[,2])
mod3c[,3]<- as.numeric(mod3c[,3])
for (i in 1:nrow(mod3c)){
  print(paste0(sprintf("%.3f", mod3c[i,1]), " (",
               sprintf("%.3f", mod3c[i,2]), " - ",
               sprintf("%.3f", mod3c[i,3]), ")"))
}
############################################################################
row1 <- c("","","")
mod3d <- t(data.frame(row1))
mod3d <- unname(mod3d)
#diabetes
#library(broom)
#library(gtools)
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = diabDat, 
            weights = MORTWTSA)
summary(f1)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3d<-mod3d[2,]
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = diabDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = diabDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = diabDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = diabDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = diabDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
##########################################################
#heart disease any
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = hcDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = hcDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = hcDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = hcDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = hcDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = hcDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
###########################################################
#hypertension
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = htDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(skipMed)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = htDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(delayMed)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = htDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(lessMed)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = htDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~ordered(CRNsum)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = htDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
f1 <- coxph(Surv(fuTime, DEAD)~factor(CRNsum)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = htDat, 
            weights = MORTWTSA)
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[1,c(2,6:7)])
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[2,c(2,6:7)])
mod3d<-gtools::smartbind(mod3d,tidy(f1, exponentiate=TRUE)[3,c(2,6:7)])
#####################
#print results
mod3d[,1]<- as.numeric(mod3d[,1])
mod3d[,2]<- as.numeric(mod3d[,2])
mod3d[,3]<- as.numeric(mod3d[,3])
for (i in 1:nrow(mod3d)){
  print(paste0(sprintf("%.3f", mod3d[i,1]), " (",
               sprintf("%.3f", mod3d[i,2]), " - ",
               sprintf("%.3f", mod3d[i,3]), ")"))
}
###########################################################################
diabDat <-
  #check multicollinearity: All good (all well <10; all <2)
  vif(lm(DEAD~factor(CRN)+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin)+SEX+BMI+factor(RaceR)+ factor(InsType)+ ordered(IncomeR),
         data = hcDat, 
         weights = MORTWTSA))

f1 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = diabDat, 
            weights = MORTWTSA)
f2 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = hcDat, 
            weights = MORTWTSA)
f3 <- coxph(Surv(fuTime, DEAD)~factor(CRN)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = htDat, 
            weights = MORTWTSA)

zf1 <-cox.zph(f1) #PH assumption met
library(survminer)
memory.limit(size = 15000)
ggadjustedcurves(f1, 
                 data = diabDat, 
                 strata ='CRN',
                 variable = "CRN")
ggadjustedcurves(f2, 
                 data = hcDat, 
                 strata ='CRN',
                 variable = "CRN")
ggadjustedcurves(f3, 
                 data = htDat, 
                 strata ='CRN',
                 variable = "CRN")


f1 <- coxph(Surv(fuTime, DiabDeath)~factor(CRN)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = diabDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, CvdDeath)~factor(CRN)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = diabDat, 
            weights = MORTWTSA)
summary(f1)

f1 <- coxph(Surv(fuTime, DiabDeath)~factor(CRN)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = hcDat, 
            weights = MORTWTSA)
summary(f1)
f1 <- coxph(Surv(fuTime, CvdDeath)~factor(CRN)+factor(SEX)+BMI+AGE+factor(SmokeR)+factor(InsType)+factor(Kessler6Bin),
            data = hcDat, 
            weights = MORTWTSA)
summary(f1)

