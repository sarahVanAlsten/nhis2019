# # *****************************************************************************************
# # September 2018
# # 
# # ** PUBLIC-USE LINKED MORTALITY FOLLOW-UP THROUGH DECEMBER 31, 2015 **
# #
# # The following R code can be used to read the fixed-width format ASCII public-use Linked
# # Mortality Files (LMFs) from a stored location into a R data frame.  Basic frequencies
# # are also produced.  
# # 
# # NOTE:   With the exception of linkage eligibility-status (ELIGSTAT), the other discrete
# #         variables (e.g. MORTSTAT) are designated as integers.  We provide the definitions
# #         of the variable values in the comments but leave it up to the user to decide 
# #         whether integer or factor variables is/are preferred for their analyses.  
# #
# # NOTE:   As some variables are survey specific, we have created two versions of the program: 
# #         one for NHIS and another for NHANES.
# # 
# # *****************************************************************************************   
# #
# # NOTE:   To download and save the public-use LMFs to your hard-drive, follow these steps:
# # 
# # (1)     Designate a folder on your hard-drive to download the public-use LMF.  In this example,
# #         the data will be saved to "C:\PUBLIC USE DATA"
# #
# # (2)     The public-uses can be downloaded from this website:  
# #         ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/
# #
# #         Right click on the desired survey link and select "Save target as...".  A "Save As"
# #         screen will appear where you will need to select and input a location where to
# #         save the data file on your hard drive.  
# #
# #         Also note that the "Save as type:" box should read "DAT File (*.dat)".  This will ensure
# #         that the data file is saved to your hard drive in the correct format.  
# #
# #         In this example, the data file is saved in the folder, "C:\PUBLIC USE DATA", and the 
# #         data file is saved as "<SURVEYNAME>_MORT_2015_PUBLIC.DAT". 
# #
# # *****************************************************************************************   
# #
# # R NOTES:
# # (1)     For convenience, the user can place the name of the public-use LMF they are reading
# #         in and creating as a R data frame in just two places:  (1) the line beginning with
# #         srvyin; and (2) the line beginning with srvyout.  The resultant R data frame
# #         will have the shorthand name assigned in the srvyout line.   
# #
# # (2)     Variable labels are not provided.  Please see the public-use LMF codebook for 
# #         this information.
# #
# # (3)     Variable value formats are not attached to the variables. The value formats, 
# #         however, are included in comment blocks in the Variable Frequencies section.
# #           
# # *****************************************************************************************
# 
# 
# #install.packages("readr")    #if package is not installed already, then uncomment and run this line
# #install.packages("dplyr")    #if package is not installed already, then uncomment and run this line
# library(readr)
# library(dplyr)
# 
# 
# # the location where the .DAT file is saved:
setwd("C:\\Users\\svana\\OneDrive\\Documents\\Fall_2019\\Capstone\\public_use_data")
# 
# # remove all objects from the R environment
# rm(list=ls())
# 
# 
# ##############
# #NHIS VERSION#
# ##############
# 
# # place survey name here (substitute survey name where <SURVEY> is):
# 
# # Example syntax:
# #empty frame to hold file names and short names
# nameFrame <- matrix(nrow = (114-85), ncol = 2)
# #all start with NHIS_
# baseName <- "NHIS_"
# 
# #for each of the years, create a name and put it in the frame
# for (i in 1986:2014){
#   chari <- as.character(i)
#   shortName <- paste0(baseName, chari)
#   longName <- paste0(shortName, "_MORT_2015_PUBLIC.dat")
#   nameFrame[i-1985,1]<- longName
#   nameFrame[i-1985,2]<- shortName  
# }
# 
# 
# # read in the fixed-width format ASCII file
# dsn <- read_fwf(file=nameFrame[1,1],
#                 col_types = "ciiiiiiidd",
#                 fwf_cols(publicid = c(1,14),
#                          eligstat = c(15,15),
#                          mortstat = c(16,16),
#                          ucod_leading = c(17,19),
#                          diabetes = c(20,20),
#                          hyperten = c(21,21),
#                          dodqtr = c(22,22),
#                          dodyear = c(23,26),
#                          wgt_new = c(27,34),
#                          sa_wgt_new = c(35,42)
#                 ),
#                 na = "."
# )
# 
# # NOTE:   PUBLICID is the Unique ID for NHIS.
# # Structure and contents of data
# str(dsn)
# 
# 
# # Variable frequencies
# 
# #ELIGSTAT: Eligibility Status for Mortality Follow-up
# table(dsn$eligstat)
# #1 = "Eligible"
# #2 = "Under age 18, not available for public release"
# #3 = "Ineligible"
# 
# #MORTSTAT: Final Mortality Status
# table(dsn$mortstat, useNA="ifany")
# # 0 = Assumed alive
# # 1 = Assumed deceased
# # <NA> = Ineligible or under age 18
# 
# #UCOD_LEADING: Underlying Cause of Death: Recode
# table(dsn$ucod_leading, useNA="ifany")
# # 1 = Diseases of heart (I00-I09, I11, I13, I20-I51)
# # 2 = Malignant neoplasms (C00-C97)
# # 3 = Chronic lower respiratory diseases (J40-J47)
# # 4 = Accidents (unintentional injuries) (V01-X59, Y85-Y86)
# # 5 = Cerebrovascular diseases (I60-I69)
# # 6 = Alzheimer's disease (G30)
# # 7 = Diabetes mellitus (E10-E14)
# # 8 = Influenza and pneumonia (J09-J18)
# # 9 = Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)
# # 10 = All other causes (residual)
# # <NA> = Ineligible, under age 18, assumed alive, or no cause of death data
# 
# #DIABETES: Diabetes Flag from Multiple Cause of Death (MCOD)
# table(dsn$diabetes, useNA="ifany")
# # 0 = No - Condition not listed as a multiple cause of death
# # 1 = Yes - Condition listed as a multiple cause of death
# # <NA> = Assumed alive, under age 18, ineligible for mortality follow-up, or MCOD not available
# 
# #HYPERTEN: Hypertension Flag from Multiple Cause of Death (MCOD)
# table(dsn$hyperten, useNA="ifany")
# # 0 = No - Condition not listed as a multiple cause of death
# # 1 = Yes - Condition listed as a multiple cause of death
# # <NA> = Assumed alive, under age 18, ineligible for mortality follow-up, or MCOD not available
# 
# table(dsn$dodqtr, useNA="ifany")
# # 1 = January-March
# # 2 = April-June
# # 3 = July-September
# # 4 = October-December
# # <NA> = Ineligible, under age 18, or assumed alive
# 
# table(dsn$dodyear, useNA="ifany")
# # <NA> = Ineligible, under age 18, or assumed alive
# 
# # Re-name the dataset, DSN, to the short survey name then remove other R objects
# assign(paste0(nameFrame[1,2]), dsn)
# rm(dsn, srvyin, srvyout)
# 
# #function to read in and rename all of the data files
# readIn <- function(n){
#   # read in the fixed-width format ASCII file
#   dsn <- read_fwf(file=nameFrame[n,1],
#                   col_types = "ciiiiiiidd",
#                   fwf_cols(publicid = c(1,14),
#                            eligstat = c(15,15),
#                            mortstat = c(16,16),
#                            ucod_leading = c(17,19),
#                            diabetes = c(20,20),
#                            hyperten = c(21,21),
#                            dodqtr = c(22,22),
#                            dodyear = c(23,26),
#                            wgt_new = c(27,34),
#                            sa_wgt_new = c(35,42)
#                   ),
#                   na = "."
#   )
#   assign(paste0(nameFrame[n,2]), dsn, envir = globalenv())
#   rm(dsn)
#   
# }
# 
# for (i in 2:29){
#   readIn(i)
# }
# rm(NHIS_1986)
# rm(NHIS_1987)
# rm(NHIS_1988)
# rm(NHIS_1989)
# rm(NHIS_1990)
# rm(NHIS_1991)
# rm(NHIS_1992)
# rm(NHIS_1993)
# rm(NHIS_1994)
# rm(NHIS_1995)
# rm(NHIS_1996)
# rm(NHIS_1997)
# rm(NHIS_1998)
# rm(NHIS_1999)
# rm(NHIS_2000)
# rm(NHIS_2001)
# rm(NHIS_2002)
# rm(NHIS_2003)
# rm(NHIS_2004)
# rm(NHIS_2005)
# rm(NHIS_2006)
# rm(NHIS_2007)
# rm(NHIS_2008)
# rm(NHIS_2009)
# rm(NHIS_2010)
# rm(NHIS_2011)
# rm(NHIS_2012)
# rm(NHIS_2013)
# rm(NHIS_2014)
# rm(nameFrame)
#######################################################################################################
#read in the IPUMS data 
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

#if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
library(ipumsr)
library(tidyverse)
ddi <- read_ipums_ddi("nhis_00002.xml")
data <- read_ipums_micro(ddi)
ddi2 <- read_ipums_ddi("nhis_00003.xml")
data2 <- read_ipums_micro(ddi2)
#memory.limit(size=1000000)
names(data)
#memory.limit(size = 100000)

#subData <- data[,c(1:87,101:128,150:268, 424:437)]
subData <- data
#rm(list = nameFrame[,2])
#how many observations by wave
observationsByYear <- subData %>%
  group_by(YEAR)%>%
  summarise(n = n())

#total people who died from something including diabetes
subData %>%
  group_by(YEAR)%>%
  group_by(MORTDIAB)%>%
  summarise(n = n())

#total people who died from something including hypertension
subData %>%
  group_by(YEAR)%>%
  group_by(MORTHYPR)%>%
  summarise(n = n())

#total people eligible to have mortality followup
subData %>%
  group_by(YEAR)%>%
  group_by(MORTELIG)%>%
  summarise(n = n())

options(haven.show_pillar_labels = FALSE)
options(ipumsr.show_pillar_labels = FALSE)

#only include the people eligible for follow up
subData<- as.data.frame(subData)
subData[] <- lapply(subData, unclass)
memory.limit(size = 370000)
data3 <- merge(subData, data2, by.x = "NHISHID", by.y = "NHISHID")
rm(ddi)
rm(data)
rm(ddi2)
rm(data2)
eligible <- subData[as.integer(subData$MORTELIG) !=2,] #1= eligible, 2 = under 18, 3= ineligible, will have to weight for eligibility
names(subData)

#diabetes mortality
eligible %>%
  group_by(YEAR)%>%
  group_by(MORTDIAB)%>%
  summarise(n = n()) #1 = no, 2 = yes, 9 = niu

#ever had bladder cancer
eligible %>%
  group_by(YEAR)%>%
  group_by(CNBLAD)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had blood cancer
eligible %>%
  group_by(CNBLOD)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had breast cancer
eligible %>%
  group_by(CNBRES)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had brain cancer
eligible %>%
  group_by(CNBRAN)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had bone cancer
eligible %>%
  group_by(CNBONE)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had cervical cancer
eligible %>%
  group_by(CNCERV)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had colon cancer
eligible %>%
  group_by(CNCOLN)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had esophogeal cancer
eligible %>%
  group_by(CNESOP)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had kidney cancer
eligible %>%
  group_by(CNKIDN)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had gallbladder cancer
eligible %>%
  group_by(CNGALL)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had larynx cancer
eligible %>%
  group_by(CNLARX)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had leukemia cancer
eligible %>%
  group_by(CNLEUK)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had liver cancer
eligible %>%
  group_by(CNLIVR)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had lung cancer
eligible %>%
  group_by(CNLUNG)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had lymphoma
eligible %>%
  group_by(CNLYMP)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had melanoma
eligible %>%
  group_by(CNMELN)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had mouth/lip/tongue cancer
eligible %>%
  group_by(CNMOTH)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had ovarian cancer
eligible %>%
  group_by(CNOVAR)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had pancreatic cancer
eligible %>%
  group_by(CNPANC)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had prostate cancer
eligible %>%
  group_by(CNPROS)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had rectal cancer
eligible %>%
  group_by(CNRECT)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had non melanoma skin cancer
eligible %>%
  group_by(CNSKNM)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had skin cancer, unknown type
eligible %>%
  group_by(CNSKDK)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had soft tissue cancer
eligible %>%
  group_by(CNSOFT)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had stomach cancer
eligible %>%
  group_by(CNSTOM)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had testicular cancer
eligible %>%
  group_by(CNTEST)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had throat/pharynx cancer
eligible %>%
  group_by(CNTHRO)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had thyroid cancer
eligible %>%
  group_by(CNTHYR)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had uterine cancer
eligible %>%
  group_by(CNUTER)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#ever had other cancer
eligible %>%
  group_by(CNOTHR)%>%
  summarise(n = n()) #1 = no, 2 = yes, 0 = niu, 7 and 9 not ascertained/refused

#cancer by type and by year
cancer_Type_Year <- eligible %>%
  group_by(YEAR)%>%
  summarise(Breast = sum(CNBRES==2),
            Uterine = sum(CNUTER==2),
            Thyroid = sum(CNTHYR==2),
            ThroatPharynx = sum(CNTHRO==2),
            Testicular = sum(CNTEST==2),
            Stomach = sum(CNSTOM==2),
            SoftTissue = sum(CNSOFT==2),
            SkinUnknown = sum(CNSKDK==2),
            SkinNonMelanoma = sum(CNSKNM==2),
            SkinMelanoma = sum(CNMELN==2),
            Rectal = sum(CNRECT==2),
            Colon =sum(CNCOLN==2),
            Kidney = sum(CNKIDN==2),
            Prostate = sum(CNPROS==2),
            Gallbladder = sum(CNGALL==2),
            Pancreas = sum(CNPANC==2),
            Ovarian = sum(CNOVAR==2),
            MouthLipTongue = sum(CNMOTH==2),
            Lymphoma = sum(CNLYMP==2),
            Lung = sum(CNLUNG==2),
            Leukemia = sum(CNLEUK==2),
            Liver =sum(CNLIVR==2),
            Larynx = sum(CNLARX==2),
            Esophagus = sum(CNESOP==2),
            Cervix = sum(CNCERV==2),
            Bone = sum(CNBONE==2),
            Brain = sum(CNBRAN==2),
            Blood = sum(CNBLOD==2),
            Bladder = sum(CNBLAD==2),
            Other = sum(CNOTHR==2),
            TotalPeople = n())
cancer_Type_Year <- cancer_Type_Year[-16,] #get rid of blank row
cancer_Type_Year$TotalCancer <- rowSums(cancer_Type_Year[,1:31])

#cancerPrevalence = cancer_Type_Year$TotalCancer/cancer_Type_Year$TotalPeople

#underlying cause of death
#this variable only for 2000-2005 respondents
table(eligible$MORTUCOD)
#mortality status: 1 = dead, 2= alive
table(eligible$MORTSTAT)
eligible$DEAD <- ifelse(eligible$MORTSTAT == 1, 1, 0)
#leading cause of death: 1= heart dz, 2=cancer/neoplasm, 3=chronic lower respiratory, 4=accident, 5=cerebrovascular dz
#6 = alzheimers, 7 = diabetes, 8 =influenza, 9=nephritis, 10=all other, 96 = NIU/NA
table(eligible$MORTUCODLD)

names(eligible)
#mortdody, mortwt