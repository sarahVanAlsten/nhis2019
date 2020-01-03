#Sarah Van Alsten
#September 29, 2019
#DAGs for diabetes mortality and medication stress
library(ggdag)
#theme_set(theme_dag())



dag <-dagify(Mortality ~ Uncontrolled_Diabetes + Smoking + BMI + Insurance,
       Diabetes ~ SES + BMI+ Smoking,
       Smoking ~ SES,
       Insurance ~ SES,
       Uncontrolled_Diabetes ~ Med_Afford,
       Med_Afford ~Insurance + Diabetes,
       labels = c("Med_Afford" = "Cannot Afford\n Meds", 
                  "Smoking" = "Smoking",
                  "Uncontrolled_Diabetes" = "Morbidity",
                  "Insurance" = "Health\nInsurance",
                  "Diabetes" = "Diabetes",
                  "BMI" = "BMI", 
                  "SES" = "SES",
                  "Mortality" = "Mortality"),
       exposure = "Med_Afford",
       outcome = "Mortality") 


dag2 <-dagify(Mortality ~ Uncontrolled_Diabetes + Smoking + BMI + Insurance + SES,
             Diabetes ~ SES + BMI+ Smoking,
             Smoking ~ SES,
             Insurance ~ SES,
             Uncontrolled_Diabetes ~ Med_Afford,
             Med_Afford ~Insurance + Diabetes + SES,
             labels = c("Med_Afford" = "Cannot Afford\n Meds", 
                        "Smoking" = "Smoking",
                        "Uncontrolled_Diabetes" = "Morbidity",
                        "Insurance" = "Health\nInsurance",
                        "Diabetes" = "Diabetes",
                        "BMI" = "BMI", 
                        "SES" = "SES",
                        "Mortality" = "Mortality"),
             exposure = "Med_Afford",
             outcome = "Mortality") 


dag3 <-dagify(Mortality ~ Smoking + BMI + Insurance +CRN + Age + Sex + Dep,
              Smoking ~ SES + Race,
              Insurance ~ SES + Age,
              CRN ~ SES + Race + Dep + Age + Sex + Insurance,
              BMI ~ Race + SES,
              Dep ~ SES + Race,
              SES ~ Race,
              labels = c("CRN" = "CRN", 
                         "Smoking" = "Smoking",
                         "Insurance" = "Health\nInsurance",
                         "BMI" = "BMI", 
                         "SES" = "SES",
                         "Mortality" = "Mortality",
                         "Race"= "Race",
                         "Age"= "Age",
                         "Sex"= "Sex",
                         "Dep"= "Depression"),
              exposure = "CRN",
              outcome = "Mortality") 


ggdag(dag, text =F, use_labels = "label")
ggdag(dag3, text =F, use_labels = "label")

ggdag_adjustment_set(dag, text = FALSE, use_labels = "label", shadow = TRUE)
ggdag_adjustment_set(dag3, text = TRUE)

dag4 <-dagify(Mortality ~ Smoking + Edu + Race + BMI + Insurance + CRN + Age + Sex + Income,
              Smoking ~ Edu,
              Insurance ~ Income + Age,
              CRN ~ Income + Age + Sex + Insurance + Edu,
              BMI ~ Race + Income + Edu + Smoking + Sex,
              Income ~ Race + Edu + Sex + Age,
              Edu ~ Race,
              Smoking ~ Edu + Race + Sex + Income,
              labels = c("CRN" = "CRN", 
                         "Smoking" = "Smoking",
                         "Insurance" = "Insurance",
                         "BMI" = "BMI", 
                         "Income" = "Income",
                         "Mortality" = "Dz Specific Mortality",
                         "Race"= "Race",
                         "Age"= "Age",
                         "Sex"= "Sex",
                         "Edu" = "Education"),
              exposure = "CRN",
              outcome = "Mortality") 
ggdag(dag4, text = T) + theme_dag()
ggdag_adjustment_set(dag4, text = T, use_labels = F, shadow = TRUE)


allCauseDag <- dagify(Mortality ~ Smoking + Edu + Race + BMI + Insurance + CRN + Age + Sex + Income + Chronic,
                      Smoking ~ Edu + Race + Sex + Income,
                      Insurance ~ Income + Age,
                      CRN ~ Income + Age + Sex + Insurance + Edu + Chronic,
                      BMI ~ Race + Income + Edu + Smoking + Sex,
                      Income ~ Race + Edu + Sex + Age,
                      Edu ~ Race,
                      Chronic ~ Income + Race + Smoking + Edu + Age,
                      labels = c("CRN" = "CRN", 
                                 "Smoking" = "Smoking",
                                 "Insurance" = "Insurance",
                                 "BMI" = "BMI", 
                                 "Income" = "Income",
                                 "Mortality" = "AC Mortality",
                                 "Race"= "Race",
                                 "Age"= "Age",
                                 "Sex"= "Sex",
                                 "Edu" = "Education",
                                 "Chronic" = "Chronic"),
                      exposure = "CRN",
                      outcome = "Mortality")

ggdag(allCauseDag, text = T) + theme_dag()
ggdag_adjustment_set(allCauseDag, text = T, use_labels = F, shadow = TRUE)
#same adjustments as earlier PLUS chronic conditions
