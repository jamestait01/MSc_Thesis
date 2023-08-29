data <- readRDS("~/Summer_project/baseline_models/model_3a/final_data.rds")

poisson_model <- glm(n_hospitalisations_unique ~ Sex + `Smoking status` + BMI + Age + `Number of comorbidities`+ 
  `Days of vigorous physical activity` + `Days of moderate physical activity`+ 
  `Weekly alcohol intake` + `Index of Multiple Deprivation` +
  `Medication status`,family="poisson",data=data)

cardiac_poisson <- glm(n_cardiac_unique ~ Sex + `Smoking status` + BMI + Age + `Number of comorbidities`+ 
                         `Days of vigorous physical activity` + `Days of moderate physical activity`+ 
                         `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                         `Medication status`,family="poisson",data=data)

library(tableone)
library(gtsummary)
library(dplyr)

poisson_model %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()


cardiac_poisson %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

#look for those on stable meds
stable_meds <- data[data$`Medication status`=="Stable medications",]

#relevel medication combination 
stable_meds$`Medication combination` <- relevel(stable_meds$`Medication combination`,ref="ACE inhibitors/ARBs")

#remove untreated factor level 
stable_meds$`Medication combination` <- droplevels(stable_meds$`Medication combination`)

poisson_meds <- glm(n_hospitalisations_unique ~ Sex + `Smoking status` + BMI + Age + `Number of comorbidities`+ 
                       `Days of vigorous physical activity` + `Days of moderate physical activity`+ 
                       `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                       `Medication combination`,family="poisson",data=stable_meds)

cardiac_meds <- glm(n_cardiac_unique ~ Sex + `Smoking status` + BMI + Age + `Number of comorbidities`+ 
                      `Days of vigorous physical activity` + `Days of moderate physical activity`+ 
                      `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                      `Medication combination`,family="poisson",data=stable_meds)

poisson_meds%>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()


cardiac_meds %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()
