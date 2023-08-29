setwd("~/Documents/MSc/Thesis3/baseline_models/")
data <- readRDS("model_1/baseline_data_NICE.rds")

#make age group column
data$`Age group` <- cut(data$Age, breaks=c(40,55,65,Inf),labels=c("40-55","55-65","65+")
                        ,right = FALSE)

levels(data$`Medication combination`) <- c("Untreated","ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                                           "ACE inhibitor/ARB + CCB + Diuretic",
                                           "ACE inhibitor/ARB + Diuretic",
                                           "CCB", "CCB + Diuretic")

#create variable indicating whether someone had died until the 04-05-2017 if data provider = 2
#or 01-08-2016 if = 3, or those who had not died (is.na())

data$death <- ifelse(
  data$data_provider == 2 & data$date_death <= as.Date("2017-05-04") |
    data$data_provider == 3 & data$date_death <= as.Date("2016-08-01") |
    is.na(data$data_provider) & data$date_death <= as.Date("2017-05-04"),
  1,  # If the conditions are met, assign 1
  ifelse(
    is.na(data$date_death) |
      data$data_provider == 2 & data$date_death >= as.Date("2017-05-04") |
      data$data_provider == 3 & data$date_death >= as.Date("2016-08-01"),
    0,  # Otherwise, assign 0
    NA   # Set NA for any other cases (optional)
  )
)
data$death[is.na(data$death)] <- 0

#check for cardiac mortality
myocardial_codes <- c("G45","I20","I21","I22","I23","I24","I25","I50","I60","I61","I62","I63", "I64","I65","I66",
                      "I67")

check_myocardial <- function(row) {
  any(row %in% myocardial_codes)
}

# Apply the function to each row of columns 130 to 154 and create a new column
data$cardiac_death <- apply(data[, 130:154], 1, check_myocardial)

#this code tells me if someone dies of a cvd-related death but it doesn't consider people
#dying after the follow-up... 
library(dplyr)

data <- data %>%
  mutate(
    cardiac_death2 = ifelse(
      cardiac_death == 1 &
        ((data_provider == 2 & date_death <= as.Date("2017-05-04")) |
           (data_provider == 3 & date_death <= as.Date("2016-08-01")) |
           (is.na(data_provider) & date_death <= as.Date("2017-05-04"))),
      1,  # If the conditions are met, assign 1
      ifelse(
        is.na(date_death) |
          (data_provider == 2 & date_death >= as.Date("2017-05-04")) |
          (data_provider == 3 & date_death >= as.Date("2016-08-01")),
        0,  # Otherwise, assign 0
        NA   # Set NA for any other cases (optional)
      )
    )
  )
data$cardiac_death2[is.na(data$cardiac_death2)] <- 0

library(gtsummary)

data$follow_up_date <- ifelse(data$data_provider ==2 | is.na(data$data_provider),"2017-05-04","2016-08-01")

data$follow_up_date <- as.Date(data$follow_up_date, "%Y-%m-%d")

#make date of birth variable
library(lubridate)
data$date_of_birth <- data$date_recr - years(data$Age)

#calculate follow_up time in terms of age (age at start vs end of study)
data$time <- ifelse(#if people died during follow-up calculate follow-up time,
  data$data_provider == 2 & data$date_death <= as.Date("2017-05-04") |
    data$data_provider == 3 & data$date_death <= as.Date("2016-08-01") |
    is.na(data$data_provider) & data$date_death <= as.Date("2017-05-04"),
  as.numeric(data$date_death - data$date_of_birth),
  #for everyone else, it's their age at the end of the follow-up 
  as.numeric(data$follow_up_date - data$date_of_birth))


#those with NA's are due to not dying - redo their follow-up time as age at end of study
data$time <- ifelse(is.na(data$time),as.numeric(data$follow_up_date - data$date_of_birth),data$time)

#convert time in days to years
data$time <- data$time/365.25

#add censored status, 1 = dead, 0 = censored
data$censor_status <- ifelse(data$data_provider == 2 & data$date_death <= as.Date("2017-05-04") |
                               data$data_provider == 3 & data$date_death <= as.Date("2016-08-01") |
                               is.na(data$data_provider) & data$date_death <= as.Date("2017-05-04")
                             ,1,0)

data$censor_status <- ifelse(is.na(data$censor_status),0,data$censor_status)


library(survival)
library(survminer)

cox <- coxph(Surv(Age,time, censor_status) ~ Sex + BMI + `Smoking status` +  `Weekly alcohol intake` +
               `Index of Multiple Deprivation` + `Number of comorbidities` +
               `Medication combination`, data = data)

#repeat the same stuff but for cardiac_deaths

cardiac_data <- data

cardiac_data$time <- ifelse(#if people died during follow-up calculate follow-up time,
  cardiac_data$data_provider == 2 & cardiac_data$date_death <= as.Date("2017-05-04") & cardiac_data$cardiac_death==T |
    data$data_provider == 3 & cardiac_data$date_death <= as.Date("2016-08-01") & cardiac_data$cardiac_death==T |
    is.na(cardiac_data$data_provider) & cardiac_data$date_death <= as.Date("2017-05-04") & cardiac_data$cardiac_death ==T,
  as.numeric(cardiac_data$date_death - cardiac_data$date_of_birth),
  #for everyone else, it's the follow-up age
  as.numeric(cardiac_data$follow_up_date - cardiac_data$date_of_birth))

#those with NA's are due to not dying - redo their follow-up time as end of study - date of enrolment
cardiac_data$time <- ifelse(is.na(cardiac_data$time),as.numeric(cardiac_data$follow_up_date - cardiac_data$date_of_birth),cardiac_data$time)

#convert follow-up age to years
cardiac_data$time <- cardiac_data$time / 365.25
#add censored status, 1 = dead, 0 = alive
cardiac_data$censor_status <- ifelse(cardiac_data$data_provider == 2 & cardiac_data$date_death <= as.Date("2017-05-04") & cardiac_data$cardiac_death==T |
                                       cardiac_data$data_provider == 3 & cardiac_data$date_death <= as.Date("2016-08-01") & cardiac_data$cardiac_death==T |
                                       is.na(cardiac_data$data_provider) & cardiac_data$date_death <= as.Date("2017-05-04") & cardiac_data$cardiac_death==T
                                     ,1,0)

cardiac_data$censor_status <- ifelse(is.na(cardiac_data$censor_status),0,cardiac_data$censor_status)

cardiac_cox <- coxph(Surv(Age,time, censor_status) ~ Sex + BMI + `Smoking status` +  `Weekly alcohol intake` +
                       `Index of Multiple Deprivation` + `Number of comorbidities` +
                       `Medication combination`, data = cardiac_data)


cox_death_tbl <- cox %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

cardiac_cox_tbl <- cardiac_cox %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

#merge the 2 tables together
merged_cox <- tbl_merge(list(cox_death_tbl,cardiac_cox_tbl),tab_spanner = c("**Overall mortality**",
                                                                            "**CVD-specific mortality**"))
merged_cox

gt::gtsave(as_gt(merged_cox),"merged_baseline_cox_age_axis.png",expand=60)

###mattss code
}
#make list of categorical variables
categorical_variables <- c("Sex","`Smoking status`","`Weekly alcohol intake`","`Number of comorbidities`",
                           "`Medication combination`")

#make list of reference levels for each categorical variable included in model
ref_levels <- c("Female","Never","Never","0","Untreated")

total_mortality <- makeHRTable(cox,ref_levels=ref_levels, categorical_variables=categorical_variables)

cardiac_mortality <- makeHRTable(cardiac_cox,ref_levels=ref_levels, categorical_variables=categorical_variables)

#rename appropriate variables
total_mortality$Level <- c("Male","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                           "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                           "Index of Multiple Deprivation","1-2",">=3","ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                           "ACE inhibitor/ARB + CCB + Diuretic","ACE inhibitor/ARB + Diuretic","CCB","CCB + Diuretic",
                           "Female [reference]","Never [reference]",
                           "Never [reference]","0 [reference]","Untreated [reference]")

cardiac_mortality$Level <- c("Male","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                             "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                             "Index of Multiple Deprivation","1-2",">=3","ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                             "ACE inhibitor/ARB + CCB + Diuretic","ACE inhibitor/ARB + Diuretic","CCB","CCB + Diuretic",
                             "Female [reference]","Never [reference]",
                             "Never [reference]","0 [reference]","Untreated [reference]")


predictors <- c("Sex", "BMI","`Smoking status`", "`Weekly alcohol intake`", 
                "`Index of Multiple Deprivation`", "`Number of comorbidities`",
                "`Medication combination`")

#add predictor names to table
total_mortality$predictor <- c("Sex", "BMI","Smoking status","Smoking status","Smoking status",
                               "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                               "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                               "Number of comorbidities","Number of comorbidities","Medication combination",
                               "Medication combination","Medication combination","Medication combination",
                               "Medication combination","Medication combination",
                               "Sex","Smoking status","Weekly alcohol intake","Number of comorbidities","Medication combination")

cardiac_mortality$predictor <- c("Sex", "BMI","Smoking status","Smoking status","Smoking status",
                                 "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                                 "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                                 "Number of comorbidities","Number of comorbidities","Medication combination",
                                 "Medication combination","Medication combination","Medication combination",
                                 "Medication combination","Medication combination",
                                 "Sex","Smoking status","Weekly alcohol intake",
                                 "Number of comorbidities","Medication combination")


# Combine the two OR tables
table <- bind_rows(
  mutate(total_mortality, model_type = "Overall mortality"),
  mutate(cardiac_mortality, model_type = "CVD-specific mortality"))

#remove the rows with infinite confidence intervals (prefer not to answer alcohol)
table <- table[-35,]

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




























#load follow-up data
data <- readRDS("model_3b/final_data.rds")

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

stable_meds <- data[data$`Medication status`=="Stable medications",]

#relevel medication combination 
stable_meds$`Medication combination` <- relevel(stable_meds$`Medication combination`,ref="ACE inhibitors/ARBs")

#remove untreated factor level 
stable_meds$`Medication combination` <- droplevels(stable_meds$`Medication combination`)

levels(stable_meds$`Medication combination`) <- c("ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                                                  "ACE inhibitor/ARB + CCB + Diuretic",
                                                  "ACE inhibitor/ARB + Diuretic",
                                                  "CCB", "CCB + Diuretic")

data$follow_up_date <- ifelse(data$data_provider ==2 | is.na(data$data_provider),"2017-05-04","2016-08-01")

data$follow_up_date <- as.Date(data$follow_up_date, "%Y-%m-%d")

#make date of birth variable
library(lubridate)
data$date_of_birth <- data$date_recr - years(data$Age)

#calculate follow_up time in terms of age (age at start vs end of study)
data$time <- ifelse(#if people died during follow-up calculate follow-up time,
  data$data_provider == 2 & data$date_death <= as.Date("2017-05-04") |
    data$data_provider == 3 & data$date_death <= as.Date("2016-08-01") |
    is.na(data$data_provider) & data$date_death <= as.Date("2017-05-04"),
  as.numeric(data$date_death - data$date_of_birth),
  #for everyone else, it's their age at the end of the follow-up 
  as.numeric(data$follow_up_date - data$date_of_birth))


#those with NA's are due to not dying - redo their follow-up time as age at end of study
data$time <- ifelse(is.na(data$time),as.numeric(data$follow_up_date - data$date_of_birth),data$time)

#convert time in days to years
data$time <- data$time/365.25

#add censored status, 1 = dead, 0 = censored
data$censor_status <- ifelse(data$data_provider == 2 & data$date_death <= as.Date("2017-05-04") |
                               data$data_provider == 3 & data$date_death <= as.Date("2016-08-01") |
                               is.na(data$data_provider) & data$date_death <= as.Date("2017-05-04")
                             ,1,0)

data$censor_status <- ifelse(is.na(data$censor_status),0,data$censor_status)


cox <- coxph(Surv(Age,time, censor_status) ~ Sex + BMI + `Smoking status` +  `Weekly alcohol intake` +
               `Index of Multiple Deprivation` + `Number of comorbidities` +
               `Medication status`, data = data)

#repeat the same stuff but for cardiac_deaths

cardiac_data <- data

cardiac_data$time <- ifelse(#if people died during follow-up calculate follow-up time,
  cardiac_data$data_provider == 2 & cardiac_data$date_death <= as.Date("2017-05-04") & cardiac_data$cardiac_death==T |
    data$data_provider == 3 & cardiac_data$date_death <= as.Date("2016-08-01") & cardiac_data$cardiac_death==T |
    is.na(cardiac_data$data_provider) & cardiac_data$date_death <= as.Date("2017-05-04") & cardiac_data$cardiac_death ==T,
  as.numeric(cardiac_data$date_death - cardiac_data$date_of_birth),
  #for everyone else, it's the follow-up age
  as.numeric(cardiac_data$follow_up_date - cardiac_data$date_of_birth))

#those with NA's are due to not dying - redo their follow-up time as end of study - date of enrolment
cardiac_data$time <- ifelse(is.na(cardiac_data$time),as.numeric(cardiac_data$follow_up_date - cardiac_data$date_of_birth),cardiac_data$time)

#convert follow-up age to years
cardiac_data$time <- cardiac_data$time / 365.25
#add censored status, 1 = dead, 0 = alive
cardiac_data$censor_status <- ifelse(cardiac_data$data_provider == 2 & cardiac_data$date_death <= as.Date("2017-05-04") & cardiac_data$cardiac_death==T |
                                       cardiac_data$data_provider == 3 & cardiac_data$date_death <= as.Date("2016-08-01") & cardiac_data$cardiac_death==T |
                                       is.na(cardiac_data$data_provider) & cardiac_data$date_death <= as.Date("2017-05-04") & cardiac_data$cardiac_death==T
                                     ,1,0)

cardiac_data$censor_status <- ifelse(is.na(cardiac_data$censor_status),0,cardiac_data$censor_status)

cardiac_cox <- coxph(Surv(Age,time, censor_status) ~ Sex + BMI + `Smoking status` +  `Weekly alcohol intake` +
                       `Index of Multiple Deprivation` + `Number of comorbidities` +
                       `Medication status`, data = cardiac_data)

cox_death_tbl <- cox %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

cardiac_cox_tbl <- cardiac_cox %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

#merge the 2 tables together
merged_cox <- tbl_merge(list(cox_death_tbl,cardiac_cox_tbl),tab_spanner = c("**Overall mortality**",
                                                                            "**CVD-specific mortality**"))
merged_cox

gt::gtsave(as_gt(merged_cox),"merged_followup_cox_age_axis.png",expand=60)



#repeat the same models for people on stable meds

stable_meds <- data[data$`Medication status`=="Stable medications",]

stable_meds_cardiac <- cardiac_data[cardiac_data$`Medication status`=="Stable medications",]

#relevel medication combination 
stable_meds$`Medication combination` <- relevel(stable_meds$`Medication combination`,ref="ACE inhibitors/ARBs")
stable_meds_cardiac$`Medication combination` <- relevel(stable_meds_cardiac$`Medication combination`,ref="ACE inhibitors/ARBs")


#remove untreated factor level 
stable_meds$`Medication combination` <- droplevels(stable_meds$`Medication combination`)
stable_meds_cardiac$`Medication combination` <- droplevels(stable_meds_cardiac$`Medication combination`)


levels(stable_meds$`Medication combination`) <- c("ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                                                  "ACE inhibitor/ARB + CCB + Diuretic",
                                                  "ACE inhibitor/ARB + Diuretic",
                                                  "CCB", "CCB + Diuretic")

levels(stable_meds_cardiac$`Medication combination`) <- c("ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                                                  "ACE inhibitor/ARB + CCB + Diuretic",
                                                  "ACE inhibitor/ARB + Diuretic",
                                                  "CCB", "CCB + Diuretic")


stable_meds_cox <- coxph(Surv(Age,time,censor_status) ~ Sex + BMI + `Smoking status`
                         +  `Weekly alcohol intake` +`Index of Multiple Deprivation` + `Number of comorbidities` +
                           `Medication combination`, data = stable_meds)

cardiac_meds_cox <- coxph(Surv(Age,time, censor_status) ~ Sex + BMI + `Smoking status` +  `Weekly alcohol intake` +
                       `Index of Multiple Deprivation` + `Number of comorbidities` +
                       `Medication combination`, data = stable_meds_cardiac)

cox_meds_death_tbl <- stable_meds_cox %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

cardiac_meds_cox_tbl <- cardiac_meds_cox %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

#repeat the above but with blood pressure measurements...
stable_meds_cox_bp <- coxph(Surv(Age,time,censor_status) ~ Sex + BMI + `Smoking status`
                         +  `Weekly alcohol intake` +`Index of Multiple Deprivation` + `Number of comorbidities` +
                           `Medication combination` +
                           `Baseline systolic blood pressure` +
                           `Baseline diastolic blood pressure`, data = stable_meds)

cardiac_meds_cox_bp <- coxph(Surv(Age,time, censor_status) ~ Sex + BMI + `Smoking status` +  `Weekly alcohol intake` +
                            `Index of Multiple Deprivation` + `Number of comorbidities` +
                            `Medication combination` +
                              `Baseline systolic blood pressure` + 
                              `Baseline diastolic blood pressure`, data = stable_meds_cardiac)

cox_meds_death_tbl_bp <- stable_meds_cox_bp %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

cardiac_meds_cox_tbl_bp <- cardiac_meds_cox_bp %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

#merge the 2 tables together
merged_meds_cox_bp <- tbl_merge(list(cox_meds_death_tbl,cox_meds_death_tbl_bp),tab_spanner = c("**Model 1: Without blood pressure**",
                                                                            "**Model 2: With blood pressure**"))
merged_meds_cvd_cox_bp <- tbl_merge(list(cardiac_meds_cox_tbl,cardiac_meds_cox_tbl_bp),
                                    tab_spanner = c("**Model 1: Without blood pressure**",
                                                    "**Model 2: With blood pressure**"))
gt::gtsave(as_gt(merged_meds_cox_bp),"merged_stable_meds_cox_age_axis.png",expand=60)

gt::gtsave(as_gt(merged_meds_cvd_cox_bp),"merged_stable_meds_cvd_cox_age_axis.png",expand=60)
