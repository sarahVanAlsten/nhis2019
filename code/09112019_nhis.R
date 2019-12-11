adult <- read.csv("C:/Users/svana/OneDrive/Documents/Fall_2019/Capstone/samadultcsv/samadult.csv")
table(adult$DIBEV1)
table(adult$DIBPRE2)
table(adult$DIBAGE1)
table(adult$CANEV)
adult$t1d <- ifelse(adult$DIBAGE1 < 30 & adult$DIBPILL1 !=1 & adult$INSLN1 ==1, 1, 
                    ifelse(adult$DIBEV1 == 7 | adult$DIBEV1 == 9, NA, 0))
adult$insulin <- ifelse(adult$INSLN1 == 1, 1, 0)
adult$cancer <- ifelse(adult$CANEV == 1, 1,
                       ifelse(adult$CANEV == 2, 0, NA))
adult$diabetes <- ifelse(adult$DIBEV1 == 1, 1, 
                         ifelse(adult$DIBEV1 == 2, 0, NA))
adult$diabetesExpanded <- ifelse(adult$DIBEV1 == 1, 1, 
                                 ifelse(adult$DIBEV1 == 2, 0, 
                                        ifelse(adult$DIBEV1 == 3, 1, NA)))
table(adult$t1d, adult$insulin)
table(adult$INSLN1, adult$DIBPILL1)
table(adult$ARX12_1)
adult$skipMedAfford <- ifelse(adult$ARX12_1 == 1, 1, 
                              ifelse(adult$ARX12_1 == 2, 0, NA)) #skipped med to save money
adult$lessMedAfford <- ifelse(adult$ARX12_2 == 1, 1, 
                              ifelse(adult$ARX12_2 == 2, 0, NA)) #took less med to save money
adult$nofillMedAfford <- ifelse(adult$ARX12_3 == 1, 1, 
                              ifelse(adult$ARX12_3 == 2, 0, NA)) #didn't fill on time to save money
adult$otherCountryMedAfford <- ifelse(adult$ARX12_5 == 1, 1, 
                                ifelse(adult$ARX12_5 == 2, 0, NA)) #got med from other country
adult$sawMentalHealh <- ifelse(adult$AHCSYR1 == 1, 1, 
                               ifelse(adult$AHCSYR1 == 2, 0, NA)) #saw mental health
adult$ERVisitsOrd <- ifelse(adult$AHERNOY2 < 97, adult$AHERNOY2, NA)
adult$liverCond <- ifelse(adult$LIVEV == 1, 1, 
                          ifelse(adult$LIVEV == 2, 0 , NA))
adult$straightM <- ifelse(adult$ASISIM == 2, 1,
                          ifelse(adult$ASISIM == 1 | adult$ASISIM == 3 | adult$ASISIM == 4, 0, NA))
adult$straightF <- ifelse(adult$ASISIF == 2, 1,
                          ifelse(adult$ASISIF == 1 | adult$ASISIF == 3 | adult$ASISIF == 4, 0, NA))
adult$affordNormalHealthCareOrd <- ifelse(adult$ASICNHC < 5, adult$ASICNHC, NA) #worried about affording healthcare
adult$affordBillOrd <- ifelse(adult$ASINBILL < 5, adult$ASINBILL, NA) #worried about affording healthcare

table(adult$diabetes, adult$skipMedAfford)
table(adult$diabetesExpanded, adult$skipMedAfford)
table(adult$diabetes, adult$lessMedAfford)
table(adult$diabetesExpanded, adult$lessMedAfford)
table(adult$diabetes, adult$nofillMedAfford)
table(adult$diabetesExpanded, adult$nofillMedAfford)
table(adult$diabetes, adult$otherCountryMedAfford)
table(adult$diabetesExpanded, adult$otherCountryMedAfford)
table(adult$diabetes, adult$sawMentalHealh)
table(adult$diabetesExpanded, adult$otherCountryMedAfford)
table(adult$diabetes, adult$ERVisitsOrd)

table(adult$AHCAFYR3)
table(adult$AHCSYR1)
table(adult$AHERNOY2)
(226*13348)/(733*2487)


