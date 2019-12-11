#Sarah Van Alsten
#September 29, 2019
#DAGs for diabetes mortality and medication stress
library(ggdag)
theme_set(theme_dag())



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



dag3 <-dagify(ER ~ Concord + SRhealth + MenDis + Chronic + Insurance+ Age+ Edu + Income +Race,
              Concord ~ Insurance + Race + Edu + Income,
              Insurance ~ Income + Age + Race + Edu,
              Edu ~ Race,
              Income ~ Edu + Race,
              MenDis ~ Race,
              Chronic ~ Edu + Income + Age + Race +MenDis,
              SRhealth ~ Chronic + MenDis + Age + Race + Income,
              exposure = "Concord",
              outcome = "ER") 

