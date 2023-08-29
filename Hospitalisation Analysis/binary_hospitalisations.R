data <- readRDS("~/Summer_project/baseline_models/model_3a/final_data.rds")

data$binary_hospitalisation <- ifelse(data$n_hospitalisations_unique==0,0,1)
data$binary_cardiac <- ifelse(data$n_cardiac_unique==0,0,1)

hosp_model <- glm(binary_hospitalisation ~ Sex + `Smoking status` + Ethnicity + BMI + Age + `Number of comorbidities`+ 
               `Days of vigorous physical activity` + `Days of moderate physical activity`+ 
               `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education +
               `Medication status`,family=binomial(link='logit'),data=data)

cardiac_model <- glm(binary_cardiac ~ Sex + `Smoking status` + Ethnicity + BMI + Age + `Number of comorbidities`+ 
                       `Days of vigorous physical activity` + `Days of moderate physical activity`+ 
                       `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education +
                       `Medication status`,family=binomial(link='logit'),data=data)

library(gtsummary)

hosp_model %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

cardiac_model %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()


#now let's look at those who stayed on the same medications...

stable_meds <- data[data$`Medication status`=="Stable medications",]

#relevel medication combination 
stable_meds$`Medication combination` <- relevel(stable_meds$`Medication combination`,ref="ACE inhibitors/ARBs")

#remove untreated factor level 
stable_meds$`Medication combination` <- droplevels(stable_meds$`Medication combination`)

med_hosp <- glm(binary_hospitalisation ~ Sex + `Smoking status` + Ethnicity + BMI + Age + `Number of comorbidities`+ 
                   `Days of vigorous physical activity` + `Days of moderate physical activity`+ 
                   `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education + `Medication combination`,
                 family=binomial(link='logit'),data=stable_meds)

med_cardiac <- glm(binary_cardiac ~ Sex + `Smoking status` + Ethnicity + BMI + Age + `Number of comorbidities`+ 
                  `Days of vigorous physical activity` + `Days of moderate physical activity`+ 
                  `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education + `Medication combination`,
                family=binomial(link='logit'),data=stable_meds)

med_hosp %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

med_cardiac %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()
