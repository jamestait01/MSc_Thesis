data <- readRDS("/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3b/final_data.rds")

#check for cardiac mortality
myocardial_codes <- c("G45","I20","I21","I22","I23","I24","I25","I50","I60","I61","I62","I63", "I64","I65","I66",
                      "I67")

check_myocardial <- function(row) {
  any(row %in% myocardial_codes)
}

library(dplyr)
data <- data %>%
  rename(`Baseline systolic blood pressure` = sys_bp.0.x,
         `Baseline diastolic blood pressure` = dias_bp.0.x)
# Apply the function to each row of columns 130 to 154 and create a new column
data$cardiac_death <- apply(data[, 130:154], 1, check_myocardial)

#this code tells me if someone dies of a cvd-related death but it doesn't consider people
#dying after the follow-up... 
data <- data %>%
  mutate(
    cardiac_death2 = ifelse(
      cardiac_death == 1 &
        ((data_provider.x == 2 & date_death.x <= as.Date("2017-05-04")) |
           (data_provider.x == 3 & date_death.x <= as.Date("2016-08-01")) |
           (is.na(data_provider.x) & date_death.x <= as.Date("2017-05-04"))),
      1,  # If the conditions are met, assign 1
      ifelse(
        is.na(date_death.x) |
          (data_provider.x == 2 & date_death.x >= as.Date("2017-05-04")) |
          (data_provider.x == 3 & date_death.x >= as.Date("2016-08-01")),
        0,  # Otherwise, assign 0
        NA   # Set NA for any other cases (optional)
      )
    )
  )

data$cardiac_death2[is.na(data$cardiac_death2)] <- 0


#make age group column
data$`Age group` <- cut(data$Age, breaks=c(40,55,65,Inf),labels=c("40-55","55-65","65+")
                        ,right = FALSE)

data$`Medication status` <- relevel(data$`Medication status`,ref="Stayed untreated")

#log_data <- subset(data,select=c(eid,Sex,`Smoking status`, BMI, Age,`Age group`, `Number of comorbidities`, 
`Weekly alcohol intake`, `Index of Multiple Deprivation`,`Medication combination`,
`Last medication combination`, death,cardiac_death2,`Medication status`,
`Baseline systolic blood pressure`,`Baseline diastolic blood pressure`))

#relevel relevant factors for log model
#log_data$`Medication combination` <- as.factor(log_data$`Medication combination`)
#log_data$`Last medication combination` <- as.factor(log_data$`Last medication combination`)
#log_data$`Medication combination` <- relevel(log_data$`Medication combination`,ref="Untreated")
#log_data$`Last medication combination` <- relevel(log_data$`Last medication combination`,ref="Untreated")
#log_data$`Smoking status` <- relevel(log_data$`Smoking status`,ref="Never")
#log_data$`Weekly alcohol intake` <- relevel(log_data$`Weekly alcohol intake`,ref="Never")
#log_data$Education <- relevel(log_data$Education,ref="College/university degree")

#model <-  glm(death ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                `Index of Multiple Deprivation` + `Number of comorbidities` +
                `Medication status`,
              family=binomial(link='logit'),data=data)

library(gtsummary)

#death_model <- model %>%
 # tbl_regression(exponentiate=T) %>%
  #bold_labels()

#look at cardiac mortality

#cardiac_model <- glm(cardiac_death2 ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                       `Index of Multiple Deprivation` + `Number of comorbidities` +
                       `Medication status`,family=binomial(link='logit'),data=data)

#cardiac_death_model <- cardiac_model %>%
 # tbl_regression(exponentiate=T) %>%
#  bold_labels()

#merge the 2 tables together
#merged_mortality <- tbl_merge(list(death_model,cardiac_death_model),tab_spanner = c("**Overall mortality**",
                                                                                    "**CVD-specific mortality**"))

#gt::gtsave(as_gt(merged_mortality),"merged_mortality_table.png",expand=40) 


#now let's look at those who stayed on the same medications...

stable_meds <- data[data$`Medication status`=="Stable medications",]

#relevel medication combination 
stable_meds$`Medication combination` <- relevel(stable_meds$`Medication combination`,ref="ACE inhibitors/ARBs")

#remove untreated factor level 
stable_meds$`Medication combination` <- droplevels(stable_meds$`Medication combination`)

levels(stable_meds$`Medication combination`) <- c("ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                                                  "ACE inhibitor/ARB + CCB + Diuretic",
                                                  "ACE inhibitor/ARB + Diuretic",
                                                  "CCB", "CCB + Diuretic")

med_model <- glm(death ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                   `Index of Multiple Deprivation` + `Number of comorbidities` +
                   `Medication combination`,
                 family=binomial(link='logit'),data=stable_meds)


stable_meds_death <- med_model %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

med_model_bp <- glm(death ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                      `Index of Multiple Deprivation` + `Number of comorbidities` +
                      `Medication combination` +  `Baseline systolic blood pressure` + 
                      `Baseline diastolic blood pressure`,
                    family=binomial(link='logit'),data=stable_meds)

stable_meds_death_bp <- med_model_bp %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

#merge the 2 tables together
merged_mortality_stable_meds <- tbl_merge(list(stable_meds_death,stable_meds_death_bp),tab_spanner = c("**Model 1: Without blood pressure**",
                                                                                                       "**Model 2: With blood pressure**"))

gt::gtsave(as_gt(merged_mortality_stable_meds),"merged_mortality_stable_meds_table.png",expand=40)

#only 77 people died for cardicac-related reasons out of 26612 people... and if we look at stable meds, it's 14/3286
#still worth a shot?

#make stable meds cardiac death models with and without bp adjustment

cardiac_med_model <- glm(cardiac_death2 ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                           `Index of Multiple Deprivation` + `Number of comorbidities` +
                           `Medication combination`,
                         family=binomial(link='logit'),data=stable_meds)


cardiac_med_tbl <- cardiac_med_model %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

cardiac_med_model_bp <- glm(cardiac_death2 ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                              `Index of Multiple Deprivation` + `Number of comorbidities` +
                              `Medication combination` +  `Baseline systolic blood pressure` + 
                              `Baseline diastolic blood pressure`,
                            family=binomial(link='logit'),data=stable_meds)

cardiac_med_tbl_bp <- cardiac_med_model_bp %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

#merge the 2 tables together
merged_cardiac_mortality_stable_meds <- tbl_merge(list(cardiac_med_tbl,cardiac_med_tbl_bp),tab_spanner = c("**Model 1: Before blood pressure adjustment**",
                                                                                                           "**Model 2: After blood pressure adjustment**"))

gt::gtsave(as_gt(merged_cardiac_mortality_stable_meds),"merged_cardiac_mortality_stable_meds_table.png",expand=600)

#now let's do the same thing but for cox regression...

#those that changed meds 
data$follow_up_date <- ifelse(data$data_provider ==2 | is.na(data$data_provider),"2017-05-04","2016-08-01")

data$follow_up_date <- as.Date(data$follow_up_date, "%Y-%m-%d")

data$time <- ifelse(#if people died during follow-up calculate follow-up time,
  data$data_provider == 2 & data$date_death <= as.Date("2017-05-04") |
    data$data_provider == 3 & data$date_death <= as.Date("2016-08-01") |
    is.na(data$data_provider) & data$date_death <= as.Date("2017-05-04"),
  as.numeric(data$date_death - data$date_recr),
  #for everyone else, it's the follow-up date - date of recruitment
  as.numeric(data$follow_up_date - data$date_recr))

#those with NA's are due to not dying - redo their follow-up time as end of study - date of enrolment
data$time <- ifelse(is.na(data$time),as.numeric(data$follow_up_date - data$date_recr),data$time)

#add censored status, 1 = dead, 0 = censored
data$censor_status <- ifelse(data$data_provider == 2 & data$date_death <= as.Date("2017-05-04") |
                               data$data_provider == 3 & data$date_death <= as.Date("2016-08-01") |
                               is.na(data$data_provider) & data$date_death <= as.Date("2017-05-04")
                             ,1,0)

data$censor_status <- ifelse(is.na(data$censor_status),0,data$censor_status)


library(survival)
library(survminer)

overall_cox <- coxph(Surv(time, censor_status) ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                       `Index of Multiple Deprivation` + `Number of comorbidities` +
                       `Medication status`, data = data)

#repeat the same stuff but for cardiac_deaths

cardiac_data <- data

cardiac_data$time <- ifelse(#if people died during follow-up calculate follow-up time,
  cardiac_data$data_provider == 2 & cardiac_data$date_death <= as.Date("2017-05-04") & cardiac_data$cardiac_death==T |
    data$data_provider == 3 & cardiac_data$date_death <= as.Date("2016-08-01") & cardiac_data$cardiac_death==T |
    is.na(cardiac_data$data_provider) & cardiac_data$date_death <= as.Date("2017-05-04") & cardiac_data$cardiac_death ==T,
  as.numeric(cardiac_data$date_death - cardiac_data$date_recr),
  #for everyone else, it's the follow-up date - date of recruitment
  as.numeric(cardiac_data$follow_up_date - cardiac_data$date_recr))

#those with NA's are due to not dying - redo their follow-up time as end of study - date of enrolment
cardiac_data$time <- ifelse(is.na(cardiac_data$time),as.numeric(cardiac_data$follow_up_date - cardiac_data$date_recr),cardiac_data$time)

#add censored status, 1 = dead, 0 = censored
cardiac_data$censor_status <- ifelse(cardiac_data$data_provider == 2 & cardiac_data$date_death <= as.Date("2017-05-04") & cardiac_data$cardiac_death==T |
                                       cardiac_data$data_provider == 3 & cardiac_data$date_death <= as.Date("2016-08-01") & cardiac_data$cardiac_death==T |
                                       is.na(cardiac_data$data_provider) & cardiac_data$date_death <= as.Date("2017-05-04") & cardiac_data$cardiac_death==T
                                     ,1,0)

cardiac_data$censor_status <- ifelse(is.na(cardiac_data$censor_status),0,cardiac_data$censor_status)

cardiac_cox <- coxph(Surv(time, censor_status) ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                       `Index of Multiple Deprivation` + `Number of comorbidities` +
                       `Medication status`, data = cardiac_data)

#make function to extract coefficients and 95% CI.s from Cox model
makeHRTable <- function(mod, ref_levels = NULL, dp = 3, categorical_variables = NULL) {
  tab <- summary(mod)
  
  # Extract coefficients, standard errors, z-values, and p-values
  coef_table <- data.frame(Coefficient = tab$coef[, 1],
                           SE = tab$coef[, 3],
                           z = tab$coef[, 4],
                           P_value = tab$coef[, 5])
  
  # Calculate Hazard Ratios (HRs) and 95% Confidence Intervals (CIs)
  coef_table$HR <- exp(coef_table$Coefficient)
  coef_table$Lower_CI <- exp(coef_table$Coefficient - 1.96 * coef_table$SE)
  coef_table$Upper_CI <- exp(coef_table$Coefficient + 1.96 * coef_table$SE)
  
  # Add level names
  coef_table$Level <- rownames(coef_table)
  
  # Create a reference row for categorical variables
  if (!is.null(categorical_variables)) {
    for (i in seq_along(categorical_variables)) {
      ref_level_name <- ifelse(is.null(ref_levels), 
                               paste0(categorical_variables[i], " [reference]"),
                               paste0(ref_levels[i], " [reference]"))
      ref_row <- data.frame(Coefficient = 0,
                            SE = NA_real_,
                            z = NA_real_,
                            P_value = NA_real_,
                            HR = 1,
                            Lower_CI = NA_real_,
                            Upper_CI = NA_real_,
                            Level = ref_level_name)
      coef_table <- rbind(coef_table, ref_row)
    }
  }
  
  # Round numeric values
  coef_table[, c("Coefficient", "SE", "z", "HR", "Lower_CI", "Upper_CI", "P_value")] <-
    round(coef_table[, c("Coefficient", "SE", "z", "HR", "Lower_CI", "Upper_CI", "P_value")], dp)
  
  # Reorder columns
  coef_table <- coef_table[, c("Level", "HR", "Lower_CI", "Upper_CI", "P_value")]
  
  # Add row numbers as row names
  rownames(coef_table) <- 1:nrow(coef_table)
  
  return(coef_table)
}
#make list of categorical variables
categorical_variables <- c("Sex","`Age group`","`Smoking status`","`Weekly alcohol intake`","`Number of comorbidities`",
                           "`Medication status`")

#make list of reference levels for each categorical variable included in model
ref_levels <- c("Female","40-55","Never","Never","0","Stayed untreated")

total_mortality <- makeHRTable(overall_cox,ref_levels=ref_levels, categorical_variables=categorical_variables)

cardiac_mortality <- makeHRTable(cardiac_cox,ref_levels=ref_levels, categorical_variables=categorical_variables)

#rename appropriate variables
total_mortality$Level <- c("Male","55-65","65+","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                           "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                           "Index of Multiple Deprivation","1-2",">=3","Stable medications","Changed medications","Decreased dosage",
                           "Ended medication","Increased dosage","Started medication","Female [reference]","40-55 [reference]","Never [reference]",
                           "Never [reference]","0 [reference]","Stayed untreated [reference]")

cardiac_mortality$Level <- c("Male","55-65","65+","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                             "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                             "Index of Multiple Deprivation","1-2",">=3","Stable medications","Changed medications","Decreased dosage",
                             "Ended medication","Increased dosage","Started medication","Female [reference]","40-55 [reference]","Never [reference]",
                             "Never [reference]","0 [reference]","Stayed untreated [reference]")



predictors <- c("Sex","`Age group`", "BMI","`Smoking status`", "`Weekly alcohol intake`", 
                "`Index of Multiple Deprivation`", "`Number of comorbidities`",
                "`Medication status`")

#add predictor names to table
total_mortality$predictor <- c("Sex","Age group","Age group", "BMI","Smoking status","Smoking status","Smoking status",
                               "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                               "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                               "Number of comorbidities","Number of comorbidities","Medication status",
                               "Medication status","Medication status","Medication status","Medication status","Medication status",
                               "Sex","Age group","Smoking status","Weekly alcohol intake","Number of comorbidities","Medication status")

cardiac_mortality$predictor <- c("Sex","Age group","Age group", "BMI","Smoking status","Smoking status","Smoking status",
                                 "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                                 "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                                 "Number of comorbidities","Number of comorbidities","Medication status",
                                 "Medication status","Medication status","Medication status","Medication status","Medication status",
                                 "Sex","Age group","Smoking status","Weekly alcohol intake","Number of comorbidities","Medication status")

# Combine the two OR tables
table <- bind_rows(
  mutate(total_mortality, model_type = "Overall mortality"),
  mutate(cardiac_mortality, model_type = "CVD-specific mortality"))

table <- table[-c(34,40,48,49),]

# Create the forest plot with facets for each predictor and color by model type
ggplot(data = table, aes(x = Level, y = HR, color = model_type)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), position = position_dodge(width = 0.5), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  scale_y_log10() +
  labs(col = "Model", x = "", y = "Hazard Ratio") +
  theme_bw() + #get rid of grey background 
  theme(strip.background =element_rect(fill="white"),legend.position="bottom")+
  theme(strip.text = element_text(colour = 'black',face="bold")) +
  facet_col(~ predictor, scales = "free_y", space = "free") +
  scale_color_manual(values = c("Overall mortality" = "blue", "CVD-specific mortality" = "red")) +
  labs(x="Characteristic") +
  theme(legend.margin=margin(-10, 0, 0, 0)) #move legend label closer to the x-axis for less white space





#cox_death_tbl <- overall_cox %>%
# tbl_regression(exponentiate=T) %>%
#bold_labels()

cardiac_cox_tbl <- cardiac_cox %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

#merge the 2 tables together
merged_cox <- tbl_merge(list(cox_death_tbl,cardiac_cox_tbl),tab_spanner = c("**Overall mortality**",
                                                                            "**CVD-specific mortality**"))
merged_cox

gt::gtsave(as_gt(merged_cox),"merged_cox_changed_meds.png",expand=40)

#and again, for those on stable meds...

stable_meds_cox <-  data[data$`Medication status`=="Stable medications",]

stable_meds_cardiac_cox <- cardiac_data[cardiac_data$`Medication status`=="Stable medications",]

stable_meds_cox$`Medication combination` <- droplevels(stable_meds_cox$`Medication combination`)
stable_meds_cardiac_cox$`Medication combination` <- droplevels(stable_meds_cardiac_cox$`Medication combination`)


levels(stable_meds_cox$`Medication combination`) <- c("ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                                                      "ACE inhibitor/ARB + CCB + Diuretic",
                                                      "ACE inhibitor/ARB + Diuretic",
                                                      "CCB", "CCB + Diuretic")

levels(stable_meds_cardiac_cox$`Medication combination`) <- c("ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                                                              "ACE inhibitor/ARB + CCB + Diuretic",
                                                              "ACE inhibitor/ARB + Diuretic",
                                                              "CCB", "CCB + Diuretic")
stable_meds_overall_cox <- coxph(Surv(time, censor_status) ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                                   `Index of Multiple Deprivation` + `Number of comorbidities` +
                                   `Medication combination`, data = stable_meds_cox)

stable_meds_overall_cox_bp <- coxph(Surv(time, censor_status) ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                                      `Index of Multiple Deprivation` + `Number of comorbidities` +
                                      `Medication combination` + `Baseline systolic blood pressure` +
                                      `Baseline diastolic blood pressure`, data = stable_meds_cox)

#make list of categorical variables
categorical_variables <- c("Sex","`Age group`","`Smoking status`","`Weekly alcohol intake`","`Number of comorbidities`",
                           "`Medication combination`")

#make list of reference levels for each categorical variable included in model
ref_levels <- c("Female","40-55","Never","Never","0","ACE inhibitor/ARB")

stable_meds_table <- makeHRTable(stable_meds_overall_cox,ref_levels=ref_levels, categorical_variables=categorical_variables)

stable_meds_table_with_bp <- makeHRTable(stable_meds_overall_cox_bp,ref_levels=ref_levels, categorical_variables=categorical_variables)

#rename appropriate variables
stable_meds_table$Level <- c("Male","55-65","65+","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                             "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                             "Index of Multiple Deprivation","1-2",">=3","ACE inhibitor/ARB + CCB",
                             "ACE inhibitor/ARB + CCB + Diuretic","ACE inhibitor/ARB + Diuretic","CCB","CCB + Diuretic",
                             "Female [reference]","40-55 [reference]","Never [reference]",
                             "Never [reference]","0 [reference]","ACE inhibitor/ARB [reference]")

stable_meds_table_with_bp$Level <- c("Male","55-65","65+","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                                     "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                                     "Index of Multiple Deprivation","1-2",">=3","ACE inhibitor/ARB + CCB",
                                     "ACE inhibitor/ARB + CCB + Diuretic","ACE inhibitor/ARB + Diuretic","CCB","CCB + Diuretic",
                                     "Baseline systolic blood pressure (mmHg)","Baseline diastolic blood pressure (mmHg)",
                                     "Female [reference]","40-55 [reference]","Never [reference]",
                                     "Never [reference]","0 [reference]","ACE inhibitor/ARB [reference]")


predictors <- c("Sex","`Age group`", "BMI","`Smoking status`", "`Weekly alcohol intake`", 
                "`Index of Multiple Deprivation`", "`Number of comorbidities`",
                "`Medication combination`")

#add predictor names to table
stable_meds_table$predictor <- c("Sex","Age group","Age group", "BMI","Smoking status","Smoking status","Smoking status",
                                 "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                                 "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                                 "Number of comorbidities","Number of comorbidities","Medication combination",
                                 "Medication combination","Medication combination","Medication combination",
                                 "Medication combination",
                                 "Sex","Age group","Smoking status","Weekly alcohol intake","Number of comorbidities","Medication combination")

stable_meds_table_with_bp$predictor <- c("Sex","Age group","Age group", "BMI","Smoking status","Smoking status","Smoking status",
                                         "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                                         "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                                         "Number of comorbidities","Number of comorbidities","Medication combination",
                                         "Medication combination","Medication combination","Medication combination",
                                         "Medication combination", "Baseline systolic blood pressure (mmHg)",
                                         "Baseline diastolic blood pressure (mmHg)",
                                         "Sex","Age group","Smoking status","Weekly alcohol intake",
                                         "Number of comorbidities","Medication combination")


# Combine the two OR tables
combined_stable_meds_table <- bind_rows(
  mutate(stable_meds_table, model_type = "Without blood pressure"),
  mutate(stable_meds_table_with_bp, model_type = "With blood pressure"))

library(ggplot2)
library(ggforce)

#remove Inf confidence interval rows
combined_stable_meds_table <- combined_stable_meds_table[-c(6,12,33,39),]
# Create the forest plot with facets for each predictor and color by model type
ggplot(data = combined_stable_meds_table, aes(x = Level, y = HR,color=model_type)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), position = position_dodge(width = 0.5), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  scale_y_log10() +
  labs(col = "Model", x = "", y = "Odds Ratio") +
  theme_bw() + #get rid of grey background 
  theme(strip.background =element_rect(fill="white"),legend.position="bottom")+
  theme(strip.text = element_text(colour = 'black',face="bold")) +
  facet_col(~ predictor, scales = "free_y", space = "free") +
  scale_color_manual(values = c("Without blood pressure" = "blue", "With blood pressure" = "red")) +
  
  labs(x="Characteristic") +
  theme(legend.margin=margin(-10, 0, 0, 0)) #move legend label closer to the x-axis for less white space


ggplot(data = combined_IRR_table, aes(x = Level, y = OR, color = model_type)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(width = 0.5), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  scale_y_log10() +
  labs(col = "Model", x = "", y = "Incidence Rate Ratio") +
  theme_bw() + #get rid of grey background 
  theme(strip.background =element_rect(fill="white"),legend.position="bottom")+
  theme(strip.text = element_text(colour = 'black',face="bold")) +
  facet_col(~ predictor, scales = "free_y", space = "free") +
  labs(x="Characteristic") +
  
  
  #stable_meds_overall_cox_tbl <- stable_meds_overall_cox %>%
  # tbl_regression(exponentiate=T) %>%
  #bold_labels()
  
  #stable_meds_overall_cox_bp_tbl <- stable_meds_overall_cox_bp %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()


#merge the 2 tables together
stable_meds_merged_overall_cox_tbl <- tbl_merge(list(stable_meds_overall_cox_tbl,stable_meds_overall_cox_bp_tbl),
                                                tab_spanner = c("**Model 1: Without blood pressure**",
                                                                "**Model 2: With blood pressure**"))
gt::gtsave(as_gt(stable_meds_merged_overall_cox_tbl),"merged_cox_stable_meds.png",expand=60)



stable_meds_cardiac_cox_model <- coxph(Surv(time, censor_status) ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                                         `Index of Multiple Deprivation` + `Number of comorbidities` +
                                         `Medication combination`, data = stable_meds_cardiac_cox)

stable_meds_cardiac_cox_model_bp <- coxph(Surv(time, censor_status) ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                                            `Index of Multiple Deprivation` + `Number of comorbidities` +
                                            `Medication combination` + `Baseline systolic blood pressure` +
                                            `Baseline diastolic blood pressure`, data = stable_meds_cardiac_cox)


stable_meds_overall_cvd_tbl <- stable_meds_cardiac_cox_model %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

stable_meds_overall_cvd_bp_tbl <- stable_meds_cardiac_cox_model_bp %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()


#merge the 2 tables together
stable_meds_merged_cvd_cox_tbl <- tbl_merge(list(stable_meds_overall_cvd_tbl,stable_meds_overall_cvd_bp_tbl),tab_spanner = c("**Not adjusted for baseline blood pressure**",
                                                                                                                             "**Adjusted for baseline blood pressure**"))
gt::gtsave(as_gt(stable_meds_merged_cvd_cox_tbl),"merged_cvd_stable_meds.png",expand=80)

