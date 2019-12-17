##################################################################
#Sarah Van Alsten
#Created: Dec 16, 2019
#check assumptions of coxph
#Packages used: survey, survminer, survival
#Last Update: Dec 16, 2019
################################################################################

#############################################################
#now check the assumptions
#first: the proportional hazards assumption
#for dz specific mortality
zph.diab1 <- cox.zph(mod1.diab.sa)
zph.diab2 <- cox.zph(mod2.diab.sa)

zph.cvd1 <- cox.zph(mod1.cvd.sa)
zph.cvd2 <- cox.zph(mod2.cvd.sa)

zph.cvdht1 <- cox.zph(mod1.cvdht.sa)
zph.cvdht2 <- cox.zph(mod2.cvdht.sa)
############################################################
#for all cause
zph.diab1.ac <- cox.zph(mod1.diab.allcause)
zph.diab2.ac <- cox.zph(mod2.diab.allcause)

zph.cvd1.ac <- cox.zph(mod1.cvd.allcause)
zph.cvd2.ac <- cox.zph(mod2.cvd.allcause)

zph.cvdht1.ac <- cox.zph(mod1.cvdht.allcause)
zph.cvdht2.ac <- cox.zph(mod2.cvdht.allcause)
##############################################################
#for the earlier years
#for dz specific mortality
zph.diab1e <- cox.zph(mod1.early.diab)
zph.diab2e <- cox.zph(mod2.early.diab)

zph.cvd1e <- cox.zph(mod1.early.cvd)
zph.cvd2e <- cox.zph(mod2.early.cvd)

zph.cvdht1e <- cox.zph(mod1.early.cvdht)
zph.cvdht2e <- cox.zph(mod2.early.cvdht)
############################################
#for all cause
zph.diab1.ace <- cox.zph(mod1.early.diab.allcause)
zph.diab2.ace <- cox.zph(mod2.early.diab.allcause)

zph.cvd1.ace <- cox.zph(mod1.early.cvd.allcause)
zph.cvd2.ace <- cox.zph(mod2.early.cvd.allcause)

zph.cvdht1.ace <- cox.zph(mod1.early.cvdht.allcause)
zph.cvdht2.ace <- cox.zph(mod2.early.cvdht.allcause)

#put them all into a table to make easier to see
zph.frame <- data.frame(dzSpec = c(zph.diab1$table[1,3], zph.diab2$table[1,3], zph.cvd1$table[1,3], zph.cvd2$table[1,3], zph.cvdht1$table[1,3], zph.cvdht2$table[1,3]),
                        allCause = c(zph.diab1.ac$table[1,3], zph.diab2.ac$table[1,3], zph.cvd1.ac$table[1,3], zph.cvd2.ac$table[1,3], zph.cvdht1.ac$table[1,3], zph.cvdht2.ac$table[1,3]),
                        dzSpecEarly = c(zph.diab1e$table[1,3], zph.diab2e$table[1,3], zph.cvd1e$table[1,3], zph.cvd2e$table[1,3], zph.cvdht1e$table[1,3], zph.cvdht2e$table[1,3]),
                        allCauseEarly = c(zph.diab1.ace$table[1,3], zph.diab2.ace$table[1,3], zph.cvd1.ace$table[1,3], zph.cvd2.ace$table[1,3], zph.cvdht1.ace$table[1,3], zph.cvdht2.ace$table[1,3]),
                        mod = c(rep(c("Unadjusted", "Adjusted"), 6)),
                        condition = c("Diab", "Diab", "CVD", "CVD", "CVDHT", "CVDHT"))
#in adjusted models, the PH assumption is met for the CRN variable,
#however, the global PH is not met

###########################################################################
#also need to test for linearity btwn log hazard and continuous predictors
#and for influential observations
library(survminer)
#influence: dfbeta
ggcoxdiagnostics(mod1.diab, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw()) #looks pretty ok
ggcoxdiagnostics(mod1.diab, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw()) #that does not look not random

ggcoxdiagnostics(mod2.diab, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(mod2.diab, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw()) #that does not look not random

#####################################################
#the graphical evaluation is very slow. Get another way.
martingale.diab <- residuals(mod1.diab, type = "martingale")
summary(martingale.diab)

#plot predictions vs residuals
dresids <- residuals(mod1.diab, type="deviance" )
lp <- predict(mod1.diab, type="lp" )
plot(lp, dresids, xlab="Linear Predictor", ylab="Deviance Residual")

#dfbetas
dfbeta <- residuals(mod1.diab, type="dfbeta")
summary(dfbeta)

plot(1:length(dfbeta), dfbeta) # doesn't look like any big jumps
plot(1:length(martingale.diab), martingale.diab)

################################
#Assess Model Fit:
extractAIC(mod1.diab)

dfbetas <- residuals(fitCPH, type="dfbetas")

par(mfrow=c(2, 2), cex.main=1.4, cex.lab=1.4)
plot(dfbetas[ , 1], type="h", main="DfBETAS for X",    ylab="DfBETAS", lwd=2)
plot(dfbetas[ , 2], type="h", main="DfBETAS for IV-B", ylab="DfBETAS", lwd=2)
plot(dfbetas[ , 3], type="h", main="DfBETAS for IV-C", ylab="DfBETAS", lwd=2)

#linearity of log hazard
resMart <- residuals(fitCPH, type="martingale")
plot(dfSurv$X, resMart, main="Martingale-residuals for X",
     xlab="X", ylab="Residuen", pch=20)
lines(loess.smooth(dfSurv$X, resMart), lwd=2, col="blue")
legend(x="bottomleft", col="blue", lwd=2, legend="LOESS fit", cex=1.4)

#MEMs:
hazRat <- predict(fitCPH, type="risk")
head(hazRat)


for (j in 1:2) { # residual plots
  plot(X[, j], res, xlab=c("age", "prio")[j], ylab="residuals")
  abline(h=0, lty=2)
  lines(lowess(X[, j], res, iter=0))
}

#martingale are for testing linearity: want a linear rlship btwn martingale and cont predictors