data <- readRDS("~/Summer_project/baseline_models/model_3a/final_data.rds")

#group hospitalisations into category, 0, 1,2,3-5, 5+
# Define the breakpoints and labels for the intervals
breakpoints <- c(-Inf, 0.5, 1.5, 2.5, 5.5, Inf)
labels <- c("0", "1", "2", "3-5", "5+")


# Convert the numeric column to a categorical column using cut()
data$n_hospitalisations_unique_cat <- cut(data$n_hospitalisations_unique, breaks = breakpoints, labels = labels, right = FALSE)

breakpoints <- c(-Inf, 0.5, 1.5, 2.5, 5.5, Inf)
labels <- c("0", "1", "2", "3-5", "5+")


# Convert the numeric column to a categorical column using cut()
data$n_hospitalisations_unique_cat <- cut(data$n_hospitalisations_unique, breaks = breakpoints, labels = labels, right = FALSE)

breakpoints <- c(-Inf,0.5,1.5,Inf)
labels <- c("0","1","2+")
data$n_cardiac_unique_cat <- cut(data$n_cardiac_unique, breaks = breakpoints, labels = labels, right = FALSE)

saveRDS(data,"~/Summer_project/baseline_models/model_3a/final_data.rds")

library(nnet)

hosp_test <- multinom(n_hospitalisations_unique_cat ~ Sex + `Smoking status` + Ethnicity + BMI + Age + `Number of comorbidities`+ 
                        `Days of vigorous physical activity` + `Days of moderate physical activity`+ 
                        `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education +
                        `Medication status`, data = data,maxit=1000)



cardiac_test <- multinom(n_cardiac_unique_cat ~ Sex + `Smoking status` + Ethnicity + BMI + Age + `Number of comorbidities`+ 
                        `Days of vigorous physical activity` + `Days of moderate physical activity`+ 
                        `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education +
                        `Medication status`, data = data,maxit=1000)

#calculate z-scores and p-values for each variable
# Extract coefficients and exponentiated coefficients for both models
hosp_coefs <- coef(hosp_test)
hosp_exp_coefs <- exp(coef(hosp_test))

cardiac_coefs <- coef(cardiac_test)
cardiac_exp_coefs <- exp(coef(cardiac_test))

# Extract exponentiated standard errors and calculate 95% confidence intervals for both models
z <- summary(test)$coefficients/summary(test)$standard.errors

hosp_exp_se <- exp(summary(hosp_test)$standard.errors)
hosp_ci_low <- hosp_exp_coefs - (1.96 * hosp_exp_se)
hosp_ci_high <- hosp_exp_coefs + (1.96 * hosp_exp_se)

cardiac_exp_se <- exp(summary(cardiac_test)$standard.errors)
cardiac_ci_low <- cardiac_exp_coefs - (1.96 * cardiac_exp_se)
cardiac_ci_high <- cardiac_exp_coefs + (1.96 * cardiac_exp_se)

# Calculate z-scores and p-values for both models
hosp_z <- summary(hosp_test)$coefficients / summary(hosp_test)$standard.errors
hosp_p <- (1 - pnorm(abs(hosp_z), 0, 1)) * 2

cardiac_z <- summary(cardiac_test)$coefficients / summary(cardiac_test)$standard.errors
cardiac_p <- (1 - pnorm(abs(cardiac_z), 0, 1)) * 2

# Create data frames for each model with the required information
hosp_results_df <- data.frame(
  Coefficients = hosp_coefs,
  Exp_Coefficients = hosp_exp_coefs,
  Exp_Standard_Errors = hosp_exp_se,
  CI_Lower = hosp_ci_low,
  CI_Upper = hosp_ci_high,
  Z_Scores = hosp_z,
  P_Values = hosp_p
)

cardiac_results_df <- data.frame(
  Coefficients = cardiac_coefs,
  Exp_Coefficients = cardiac_exp_coefs,
  Exp_Standard_Errors = cardiac_exp_se,
  CI_Lower = cardiac_ci_low,
  CI_Upper = cardiac_ci_high,
  Z_Scores = cardiac_z,
  P_Values = cardiac_p
)

# Print or further process the data frames if needed
print(hosp_results_df)
print(cardiac_results_df)

library(gtsummary)

hosp_results <- hosp_test %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

cardiac_results <- cardiac_test %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

cardiac_results


##########################################################

#now let's look at those who stayed on the same medications...

stable_meds <- data[data$`Medication status`=="Stable medications",]

#relevel medication combination 
stable_meds$`Medication combination` <- relevel(stable_meds$`Medication combination`,ref="ACE inhibitors/ARBs")

#remove untreated factor level 
stable_meds$`Medication combination` <- droplevels(stable_meds$`Medication combination`)

med_hosp_model <- multinom(n_hospitalisations_unique_cat ~ Sex + `Smoking status` + Ethnicity + BMI + Age + `Number of comorbidities`+ 
                   `Days of vigorous physical activity` + `Days of moderate physical activity`+ 
                   `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education + `Medication combination`,
              data=stable_meds,maxit=1000)

med_cardiac_model <- multinom(n_cardiac_unique_cat ~ Sex + `Smoking status` + Ethnicity + BMI + Age + `Number of comorbidities`+ 
                        `Days of vigorous physical activity` + `Days of moderate physical activity`+ 
                        `Weekly alcohol intake` + `Index of Multiple Deprivation` + Education + `Medication combination`,
                      data=stable_meds,maxit=1000)

med_hosp_results <- med_hosp_model %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

med_cardiac_results <- med_cardiac_model %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

med_hosp_results

med_cardiac_results
