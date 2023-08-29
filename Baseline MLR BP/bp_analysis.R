#Objective: Analyse differences in baseline blood pressure measurements for those on treatment vs untreated at baseline,
#And within the 7 different categories of NICE guidelines...

clean_baseline_data <- readRDS("~/Summer_project/baseline_models/clean_baseline_data.rds")

#remove previously treated people

clean_baseline_data <- clean_baseline_data[clean_baseline_data$med_status_0!="prev_treated_at_recr",]

clean_baseline_data$med_status_0 <- ifelse(clean_baseline_data$med_status_0=="treated_at_recr","Treated","Untreated")

clean_baseline_data$med_status_0 <- as.factor(clean_baseline_data$med_status_0)

library(dplyr)
#rename variables
clean_baseline_data <- clean_baseline_data %>%
  rename(
    Sex = sex.0.0,
    `Smoking status` = smoking_status.0.0,
    Ethnicity = ethnicity.0.0,
    BMI = bmi.0.0,
    Age = age_recr.0.0,
    `Number of comorbidities` = n_comorbid_cat_no_hypert,
    `Days of vigorous physical activity` = days_vig_phys_activ.0.0,
    `Days of moderate physical activity` = days_mod_phys_activ.0.0,
    `Weekly alcohol intake` = alc_intake.0.0,
    `Index of Multiple Deprivation` = idx_multdep,
    Education = education,
    `Medication status` = med_status_0,
    `Medication combination` = med_combo)


#relevel relevant factors model
clean_baseline_data$`Medication status` <- relevel(clean_baseline_data$`Medication status`,ref="Untreated")
clean_baseline_data$`Smoking status` <- relevel(clean_baseline_data$`Smoking status`,ref="Never")
clean_baseline_data$`Weekly alcohol intake` <- relevel(clean_baseline_data$`Weekly alcohol intake`,ref="Never")
clean_baseline_data$Education <- relevel(clean_baseline_data$Education,ref="College/university degree")
clean_baseline_data$`Medication combination` <- relevel(clean_baseline_data$`Medication combination`,ref="Untreated")

saveRDS(clean_baseline_data,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_1/baseline_data_1a.rds")

#only keep those on treatment that fall within NICE guidelines 
model_NICE_data <- clean_baseline_data[clean_baseline_data$`Medication combination`=="Untreated" |
                                         clean_baseline_data$`Medication combination`=="ACE inhibitors/ARBs" |
                                         clean_baseline_data$`Medication combination`=="Calcium channel blockers" |
                                         clean_baseline_data$`Medication combination`=="ACE inhibitors/ARBs + Diuretics" |
                                         clean_baseline_data$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers" |
                                         clean_baseline_data$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics" |
                                         clean_baseline_data$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics + B blockers" |
                                         clean_baseline_data$`Medication combination`=="Calcium channel blockers + Diuretics",
                                         ]

#drop redundant factor levels
model_NICE_data$`Medication combination` <- droplevels(model_NICE_data$`Medication combination`)

saveRDS(model_NICE_data,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_1/baseline_data_NICE.rds")

MLR_data <- subset(clean_baseline_data,select=c(eid,Sex,`Smoking status`, Ethnicity, BMI, Age, `Number of comorbidities`, 
                                                `Days of vigorous physical activity`, `Days of moderate physical activity`, 
                                                `Weekly alcohol intake`, `Index of Multiple Deprivation`,`Medication status`, 
                                                Education,sys_bp.0,dias_bp.0))
  
MLR_NICE_data <- subset(model_NICE_data,select=c(eid,Sex,`Smoking status`, Ethnicity, BMI, Age, `Number of comorbidities`, 
                                                     `Days of vigorous physical activity`, `Days of moderate physical activity`, 
                                                     `Weekly alcohol intake`, `Index of Multiple Deprivation`,`Medication status`,
                                                 `Medication combination`, Education,sys_bp.0,dias_bp.0))


sys_all <- lm(sys_bp.0 ~ Sex + `Smoking status` + Ethnicity + BMI + Age +`Number of comorbidities` +
              `Weekly alcohol intake` + `Index of Multiple Deprivation` + `Medication status`+ 
              Education,data=MLR_data)

dias_all <- lm(dias_bp.0 ~ Sex + `Smoking status` + Ethnicity + BMI + Age +`Number of comorbidities` +
                 `Weekly alcohol intake` + `Index of Multiple Deprivation` + `Medication status`+ 
                 Education,data=MLR_data)

sys_NICE <- lm(sys_bp.0 ~ Sex + `Smoking status` + Ethnicity + BMI + Age +`Number of comorbidities` +
                 `Weekly alcohol intake` + `Index of Multiple Deprivation` + `Medication status`+ 
                 Education,data=MLR_NICE_data)

dias_NICE <- lm(dias_bp.0 ~ Sex + `Smoking status` + Ethnicity + BMI + Age +`Number of comorbidities` +
                 `Weekly alcohol intake` + `Index of Multiple Deprivation` + `Medication status`+ 
                 Education,data=MLR_NICE_data)

sys_meds <- lm(sys_bp.0 ~ Sex + `Smoking status` + Ethnicity + BMI + Age +`Number of comorbidities` +
                 `Weekly alcohol intake` + `Index of Multiple Deprivation` + `Medication combination`+ 
                 Education,data=MLR_NICE_data)

dias_meds <- lm(dias_bp.0 ~ Sex + `Smoking status` + Ethnicity + BMI + Age +`Number of comorbidities` +
                  `Weekly alcohol intake` + `Index of Multiple Deprivation` + `Medication combination`+ 
                  Education,data=MLR_NICE_data)

library(gtsummary)

tbl_regression(sys_all,exponentiate=F)

tbl_regression(dias_all,exponentiate=F)

tbl_regression(sys_NICE,exponentiate=F)

tbl_regression(dias_NICE,exponentiate=F)

tbl_regression(sys_meds,exponentiate=F)

tbl_regression(dias_meds,exponentiate=F)

