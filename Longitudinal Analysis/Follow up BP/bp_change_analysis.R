data <- readRDS("/rds/general/user/jt1019/home/Summer_project/baseline_models/model_2/NICE_bp_change.rds")

MLR_data <- subset(data,select=c(eid,Sex,`Smoking status`, Ethnicity, BMI, Age, `Number of comorbidities`, 
                                                `Days of vigorous physical activity`, `Days of moderate physical activity`, 
                                                `Weekly alcohol intake`, `Index of Multiple Deprivation`,`Medication status`, 
                                                Education,sys_change,dias_change,`Daily dose`,`Follow-up daily dose`,`Follow-up time`,
                                 `Medication status` ,`Medication combination`,`Medication combination at follow-up`,
                                 sys_bp.0, dias_bp.0, sys_first_follow_up, dias_first_follow_up
                                 ))

library(dplyr)

MLR_data <- MLR_data %>%
  rename(`Baseline systolic blood pressure` = sys_bp.0,
         `Baseline diastolic blood pressure` = dias_bp.0)

sys_change <- lm(sys_change ~ Sex + `Smoking status` + Ethnicity + BMI + Age +`Number of comorbidities` +
                `Days of vigorous physical activity` + `Days of moderate physical activity` +
                `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education + `Medication status` + `Follow-up time`
                ,data=MLR_data)

dias_change <- lm(dias_change ~ Sex + `Smoking status` + Ethnicity + BMI + Age +`Number of comorbidities` +
                   `Days of vigorous physical activity` + `Days of moderate physical activity` +
                   `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education + `Medication status` + `Follow-up time`
                 ,data=MLR_data)

#check assumptions - looking for a horizontal red line here
plot(sys_change,which=1)
plot(dias_change,which=1)

#look at qq plots
plot(sys_change,which=2)
plot(dias_change,which=2)

#both plots seem reasonable enough
library(gtsummary)

tbl_regression(sys_change,exponentiate=F)

tbl_regression(dias_change,exponentiate=F)

#this gives me very strange results for the follow-up time variable, let's try stratifying the models separately for follow-up 1 and follow-up 2.

fu_1 <- data[data$`Follow-up date`=="2013-01-01",]
fu_2 <- data[data$`Follow-up date`=="2016-01-01",]

sys_change_fu1 <- lm(sys_change ~ Sex + `Smoking status` + Ethnicity + BMI + Age +`Number of comorbidities` +
                   `Days of vigorous physical activity` + `Days of moderate physical activity` +
                   `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education + `Medication status` + `Follow-up time`
                 ,data=fu_1)

dias_change_fu1 <- lm(dias_change ~ Sex + `Smoking status` + Ethnicity + BMI + Age +`Number of comorbidities` +
                    `Days of vigorous physical activity` + `Days of moderate physical activity` +
                    `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education + `Medication status` + `Follow-up time`
                  ,data=fu_1)

sys_change_fu2 <- lm(sys_change ~ Sex + `Smoking status` + Ethnicity + BMI + Age +`Number of comorbidities` +
                       `Days of vigorous physical activity` + `Days of moderate physical activity` +
                       `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education + `Medication status` + `Follow-up time`
                     ,data=fu_2)

dias_change_fu2 <- lm(dias_change ~ Sex + `Smoking status` + Ethnicity + BMI + Age +`Number of comorbidities` +
                        `Days of vigorous physical activity` + `Days of moderate physical activity` +
                        `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education + `Medication status` + `Follow-up time`
                      ,data=fu_2)

tbl_regression(sys_change_fu1,exponentiate=F)

tbl_regression(dias_change_fu1,exponentiate=F)

tbl_regression(sys_change_fu2,exponentiate=F)

tbl_regression(dias_change_fu2,exponentiate=F)


#analyse those who changed medications
changed_meds <- readRDS("/rds/general/user/jt1019/home/Summer_project/baseline_models/model_2/NICE_changed_meds.rds")

#create new column indicating which people have switched to what
changed_meds$Switch <- paste(changed_meds$`Medication combination`, "-->", changed_meds$`Medication combination at follow-up`)
changed_meds$Switch <- as.factor(changed_meds$Switch)

MLR_data2 <- subset(changed_meds,select=c(eid,Sex,`Smoking status`, Ethnicity, BMI, Age, `Number of comorbidities`, 
                                 `Days of vigorous physical activity`, `Days of moderate physical activity`, 
                                 `Weekly alcohol intake`, `Index of Multiple Deprivation`,`Medication status`, 
                                 Education,sys_change,dias_change,Switch,
                                 `Follow-up time`))

sys_change2 <- lm(sys_change ~ Sex + `Smoking status` + Ethnicity + BMI + Age +`Number of comorbidities` +
                   `Days of vigorous physical activity` + `Days of moderate physical activity` +
                   `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education + Switch + `Follow-up time`
                 ,data=MLR_data2)

dias_change2 <- lm(dias_change ~ Sex + `Smoking status` + Ethnicity + BMI + Age +`Number of comorbidities` +
                    `Days of vigorous physical activity` + `Days of moderate physical activity` +
                    `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education + Switch + `Follow-up time`
                  ,data=MLR_data2)


tbl_regression(sys_change2,exponentiate=F)

tbl_regression(dias_change2,exponentiate=F)

####################look at follow-up blood pressure as a function of baseline blood pressure and medications at follow-up

sys_fu <- lm(sys_first_follow_up ~ Sex + `Smoking status` + Ethnicity + BMI + Age +`Number of comorbidities` +
               `Days of vigorous physical activity` + `Days of moderate physical activity` +
               `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education + `Baseline systolic blood pressure` +
               `Baseline diastolic blood pressure` + `Medication combination at follow-up` +
               `Follow-up time`
             ,data=MLR_data)

dias_fu <- lm(dias_first_follow_up ~ Sex + `Smoking status` + Ethnicity + BMI + Age +`Number of comorbidities` +
               `Days of vigorous physical activity` + `Days of moderate physical activity` +
               `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education + `Baseline systolic blood pressure` +
               `Baseline diastolic blood pressure` + `Medication combination at follow-up` +
               `Follow-up time`
             ,data=MLR_data)


tbl_regression(sys_fu,exponentiate=F)

tbl_regression(dias_fu,exponentiate=F)

