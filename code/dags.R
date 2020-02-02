#Sarah Van Alsten
#September 29, 2019
#DAGs for diabetes mortality and medication stress
library(ggdag)
theme_set(theme_dag())

dag3 <-dagify(Mortality ~ Smoking + BMI + Insurance + CRN + Age + Sex + Dep,
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
              Insurance ~ Income + Age + Edu,
              CRN ~ Income + Age + Sex + Insurance + Edu + Race,
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

coords <- list(
        x = c(Mortality = 7, CRN = 5, Sex = 3, Race = 3, Insurance = 4.5,
              Income = 4.5, BMI = 5, Edu = 2, Smoking = 1, Age = 7),
        y = c(Mortality = 5, CRN = 5, Sex = 6, Race = 3, Insurance = 7,
              Income = 3, BMI = 1, Edu = 1, Smoking = 4, Age =  2)
        )

#put into data frame and change coords to make visually appealing
coord_df <- coords2df(coords)
coordinates(dag4) <- coords2list(coord_df)
ggdag(dag4, text = T, text_size = 2.5) + theme_dag()
ggdag_adjustment_set(dag4, text = T, shadow = TRUE, text_col = "black") + theme_dag()

#
allCauseDag <- dagify(Mortality ~ Smoking + Edu + Race + BMI + Insurance + CRN + Age + Sex + Income + Chronic,
                      Insurance ~ Income + Age + Edu,
                      CRN ~ Income + Age + Sex + Insurance + Edu + Race + Chronic,
                      BMI ~ Race + Income + Edu + Smoking + Sex,
                      Income ~ Race + Edu + Sex + Age,
                      Edu ~ Race,
                      Smoking ~ Edu + Race + Sex + Income,
                      Chronic ~ Age + Race + Smoking + Income + Edu + BMI + Sex,
                      labels = c("CRN" = "CRN", 
                                 "Smoking" = "Smoking",
                                 "Insurance" = "Insurance",
                                 "BMI" = "BMI", 
                                 "Income" = "Income",
                                 "Mortality" = "All Cause Mortality",
                                 "Race"= "Race",
                                 "Age"= "Age",
                                 "Sex"= "Sex",
                                 "Edu" = "Education",
                                 "Chronic" = "Chronic Conditions"),
                      exposure = "CRN",
                      outcome = "Mortality") 


coordsAC <- list(
        x = c(Mortality = 8, CRN = 5, Sex = 3, Race = 3, Insurance = 4.5,
              Income = 4.5, BMI = 5, Edu = 2, Smoking = 1, Age = 7, Chronic = 7),
        y = c(Mortality = 3, CRN = 5, Sex = 6, Race = 3, Insurance = 7,
              Income = 3.4, BMI = 1, Edu = 1, Smoking = 4, Age =  2, Chronic = 5)
)

#put into data frame
coord_df_AC <- coords2df(coordsAC)

dagitty::coordinates(allCauseDag) <- coords2list(coord_df_AC)
ggdag(allCauseDag, text = T) + theme_dag()
ggdag_adjustment_set(allCauseDag, text = T, shadow = TRUE, text_col = "black") + theme_dag()
#same adjustments as earlier PLUS chronic conditions


allCauseDag2 <- dagify(Mortality ~ Smoking + Edu + Race + BMI + Insurance + CRN + Age + Sex + Income + Chronic + U,
                      Insurance ~ Income + Age + Edu,
                      CRN ~ Income + Age + Sex + Insurance + Edu + Race + Chronic + U,
                      BMI ~ Race + Income + Edu + Smoking + Sex,
                      Income ~ Race + Edu + Sex + Age,
                      Edu ~ Race,
                      Smoking ~ Edu + Race + Sex + Income,
                      Chronic ~ Age + Race + Smoking + Income + Edu + BMI + Sex,
                      labels = c("CRN" = "CRN", 
                                 "Smoking" = "Smoking",
                                 "Insurance" = "Insurance",
                                 "BMI" = "BMI", 
                                 "Income" = "Income",
                                 "Mortality" = "All Cause Mortality",
                                 "Race"= "Race",
                                 "Age"= "Age",
                                 "Sex"= "Sex",
                                 "Edu" = "Education",
                                 "Chronic" = "Chronic Conditions",
                                 "U" = "Unmeasured"),
                      exposure = "CRN",
                      outcome = "Mortality")
ggdag(allCauseDag2)
