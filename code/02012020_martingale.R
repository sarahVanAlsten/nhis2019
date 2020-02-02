##################################################################
#Sarah Van Alsten
#Created: Feb 1, 2019
#check assumptions of coxph : linearity in the log hazard by age
#Packages used: survey, survminer, survival
#Last Update: Feb 1, 2019
###################################################################

#also need to test for linearity btwn log hazard and continuous predictors
data.dm.mod2 <- diab.mort14.fin.sa$variables %>%
  drop_na(SEX, CRN, diabMort, RaceR, IncomeR, InsType, EduR, AGE)


diab.noage <- svycoxph(formula = Surv(fuTime, diabMort)~factor(CRN) + factor(EduR)+
                         factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) ,
                       design = diab.mort14.fin.sa)

martingale.diab <- residuals(diab.noage, type = "martingale")

#log
plot(log(data.dm.mod2$AGE), martingale.diab)
lines(smooth.spline(log(data.dm.mod2$AGE), martingale.diab), col="red", lwd=2)

#cube root
plot((data.dm.mod2$AGE)^(.33), martingale.diab)
lines(smooth.spline((data.dm.mod2$AGE)^(.33), martingale.diab), col="red", lwd=2)

#sqrt
plot((data.dm.mod2$AGE)^(.5), martingale.diab)
lines(smooth.spline((data.dm.mod2$AGE)^(.5), martingale.diab), col="red", lwd=2)
#it's basically linear until age 65... Medicare?

####################################################################
data.cvd.mod2 <- cvd.mort14.fin.sa$variables %>%
  drop_na(SEX, CRN, cvdMort, RaceR, IncomeR, InsType, EduR, AGE)


cvd.noage <- svycoxph(formula = Surv(fuTime, cvdMort)~factor(CRN) + factor(EduR)+
                         factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) ,
                       design = cvd.mort14.fin.sa)

martingale.cvd <- residuals(cvd.noage, type = "martingale")

#log
plot(log(data.cvd.mod2$AGE), martingale.cvd)
lines(smooth.spline(log(data.cvd.mod2$AGE), martingale.cvd), col="red", lwd=2)

#cube root
plot((data.cvd.mod2$AGE)^(.33), martingale.cvd)
lines(smooth.spline((data.cvd.mod2$AGE)^(.33), martingale.cvd), col="red", lwd=2)

#sqrt
plot((data.cvd.mod2$AGE)^(.5), martingale.cvd)
lines(smooth.spline((data.cvd.mod2$AGE)^(.5), martingale.cvd), col="red", lwd=2)
#same thing - it's basically linear until age 65... Medicare?


#######################################################################
data.cvdHt.mod2 <- cvdht.mort14.fin.sa$variables %>%
  drop_na(SEX, CRN, cvdHtMort, RaceR, IncomeR, InsType, EduR, AGE)


cvdHt.noage <- svycoxph(formula = Surv(fuTime, cvdHtMort)~factor(CRN) + factor(EduR)+
                        factor(IncomeR) + factor(SEX) + factor(InsType) + factor(RaceR) ,
                      design = cvdht.mort14.fin.sa)

martingale.cvdHt <- residuals(cvdHt.noage, type = "martingale")

#log
plot(log(data.cvdHt.mod2$AGE), martingale.cvdHt)
lines(smooth.spline(log(data.cvdHt.mod2$AGE), martingale.cvdHt), col="red", lwd=2)

#cube root
plot((data.cvdHt.mod2$AGE)^(.33), martingale.cvdHt)
lines(smooth.spline((data.cvdHt.mod2$AGE)^(.33), martingale.cvdHt), col="red", lwd=2)

#sqrt
plot((data.cvdHt.mod2$AGE)^(.5), martingale.cvdHt)
lines(smooth.spline((data.cvdHt.mod2$AGE)^(.5), martingale.cvdHt), col="red", lwd=2)
#same thing - it's basically linear until age 65... Medicare?
