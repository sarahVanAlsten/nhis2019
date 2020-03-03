#Sarah Van Alsten
#get IQRs for age and BMI for descriptives for table one
##########################################################################

IQR(cvd.mort14.fin.sa$variables[which(cvdht.mort14.fin.sa$variables$CRN == 0), "AGE"], na.rm =T)

IQR(cvd.mort14.fin.sa$variables[which(cvdht.mort14.fin.sa$variables$CRN == 1), "AGE"], na.rm =T)

IQR(diab.mort14.fin.sa$variables[which(cvdht.mort14.fin.sa$variables$CRN == 1), "AGE"], na.rm =T)

IQR(diab.mort14.fin.sa$variables[which(cvdht.mort14.fin.sa$variables$CRN == 0), "AGE"], na.rm =T)

IQR(cvdht.mort14.fin.sa$variables[which(cvdht.mort14.fin.sa$variables$CRN == 1), "AGE"])

IQR(cvdht.mort14.fin.sa$variables[which(cvdht.mort14.fin.sa$variables$CRN == 0), "AGE"])

IQR(cvd.mort14.fin.sa$variables[which(cvdht.mort14.fin.sa$variables$CRN == 0), "AGE"])


IQR(cvd.mort14.fin.sa$variables[which(cvdht.mort14.fin.sa$variables$CRN == 0), "BMI"], na.rm =T)

IQR(cvd.mort14.fin.sa$variables[which(cvdht.mort14.fin.sa$variables$CRN == 1), "BMI"], na.rm =T)

IQR(diab.mort14.fin.sa$variables[which(cvdht.mort14.fin.sa$variables$CRN == 1), "BMI"], na.rm =T)

IQR(diab.mort14.fin.sa$variables[which(cvdht.mort14.fin.sa$variables$CRN == 0), "BMI"], na.rm =T)

IQR(cvdht.mort14.fin.sa$variables[which(cvdht.mort14.fin.sa$variables$CRN == 1), "BMI"], na.rm = T)

IQR(cvdht.mort14.fin.sa$variables[which(cvdht.mort14.fin.sa$variables$CRN == 0), "BMI"], na.rm = T)

IQR(cvd.mort14.fin.sa$variables[which(cvdht.mort14.fin.sa$variables$CRN == 0), "BMI"], na.rm = T)
