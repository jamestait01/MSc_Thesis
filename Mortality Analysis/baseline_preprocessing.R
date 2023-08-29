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


test <- subset(data,select=c(eid,date_death,cardiac_death,cardiac_death2))

data$cardiac_death2[is.na(data$cardiac_death2)] <- 0


death_model <-  glm(death ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                `Index of Multiple Deprivation` + `Number of comorbidities` +
                `Medication combination`,
              family=binomial(link='logit'),data=data)

library(dplyr)
library(gtsummary)
model_tbl <- death_model %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels
#look at cardiac mortality

cardiac_model <- glm(cardiac_death2 ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                       `Index of Multiple Deprivation` + `Number of comorbidities` +
                       `Medication combination`,family=binomial(link='logit'),data=data)

cardiac_death_model <- cardiac_model %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

#merge the 2 tables together
merged_mortality <- tbl_merge(list(model_tbl,cardiac_death_model),tab_spanner = c("**Overall mortality**",
                                                                                    "**CVD-specific mortality**"))
merged_mortality
#Now let's do a cox model

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

cox <- coxph(Surv(time, censor_status) ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
               `Index of Multiple Deprivation` + `Number of comorbidities` +
               `Medication combination`, data = data)

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

#add censored status, 1 = dead, 0 = alive
cardiac_data$censor_status <- ifelse(cardiac_data$data_provider == 2 & cardiac_data$date_death <= as.Date("2017-05-04") & cardiac_data$cardiac_death==T |
                                       cardiac_data$data_provider == 3 & cardiac_data$date_death <= as.Date("2016-08-01") & cardiac_data$cardiac_death==T |
                                       is.na(cardiac_data$data_provider) & cardiac_data$date_death <= as.Date("2017-05-04") & cardiac_data$cardiac_death==T
                                     ,1,0)

cardiac_data$censor_status <- ifelse(is.na(cardiac_data$censor_status),0,cardiac_data$censor_status)

cardiac_cox <- coxph(Surv(time, censor_status) ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
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

gt::gtsave(as_gt(merged_cox),"merged_baseline_cox.png",expand=60)

###try matts code 

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
                           "`Medication combination`")

#make list of reference levels for each categorical variable included in model
ref_levels <- c("Female","40-55","Never","Never","0","Untreated")

total_mortality <- makeHRTable(cox,ref_levels=ref_levels, categorical_variables=categorical_variables)

cardiac_mortality <- makeHRTable(cardiac_cox,ref_levels=ref_levels, categorical_variables=categorical_variables)

#rename appropriate variables
total_mortality$Level <- c("Male","55-65","65+","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                      "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                      "Index of Multiple Deprivation","1-2",">=3","ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                      "ACE inhibitor/ARB + CCB + Diuretic","ACE inhibitor/ARB + Diuretic","CCB","CCB + Diuretic",
                      "Female [reference]","40-55 [reference]","Never [reference]",
                      "Never [reference]","0 [reference]","Untreated [reference]")

cardiac_mortality$Level <- c("Male","55-65","65+","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                        "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                        "Index of Multiple Deprivation","1-2",">=3","ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                        "ACE inhibitor/ARB + CCB + Diuretic","ACE inhibitor/ARB + Diuretic","CCB","CCB + Diuretic",
                        "Female [reference]","40-55 [reference]","Never [reference]",
                        "Never [reference]","0 [reference]","Untreated [reference]")


predictors <- c("Sex","`Age group`", "BMI","`Smoking status`", "`Weekly alcohol intake`", 
                "`Index of Multiple Deprivation`", "`Number of comorbidities`",
                "`Medication combination`")

#add predictor names to table
total_mortality$predictor <- c("Sex","Age group","Age group", "BMI","Smoking status","Smoking status","Smoking status",
                          "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                          "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                          "Number of comorbidities","Number of comorbidities","Medication combination",
                          "Medication combination","Medication combination","Medication combination",
                          "Medication combination","Medication combination",
                          "Sex","Age group","Smoking status","Weekly alcohol intake","Number of comorbidities","Medication combination")

cardiac_mortality$predictor <- c("Sex","Age group","Age group", "BMI","Smoking status","Smoking status","Smoking status",
                            "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                            "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                            "Number of comorbidities","Number of comorbidities","Medication combination",
                            "Medication combination","Medication combination","Medication combination",
                            "Medication combination","Medication combination",
                            "Sex","Age group","Smoking status","Weekly alcohol intake",
                            "Number of comorbidities","Medication combination")


# Combine the two OR tables
table <- bind_rows(
  mutate(total_mortality, model_type = "Overall mortality"),
  mutate(cardiac_mortality, model_type = "CVD-specific mortality"))

#remove the rows with infinite confidence intervals (prefer not to answer alcohol)
table <- table[-40,]

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

