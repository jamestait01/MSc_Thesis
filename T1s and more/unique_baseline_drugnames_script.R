data <- readRDS("~/Documents/MSc/Thesis3/baseline_models/model_1/baseline_data_NICE.rds")

library(dplyr)
library(tidyr)

#select columns starting with drug_names and med_cat
med_data <- data %>%
  select(starts_with("drug_name"), starts_with("med_cat"))

med_data$med_cat <- NULL

#pivot data to longform
med_data <- med_data %>%
  pivot_longer(cols = starts_with("drug_name"), names_to = "drug_name_id", values_to = "drug_name") %>%
  pivot_longer(cols = starts_with("med_cat"), names_to = "med_cat_id", values_to = "med_cat")

med_data <- med_data[complete.cases(med_data),]

filtered_med_data <- med_data %>%
  filter(sub(".*_", "", drug_name_id) == sub(".*_", "", med_cat_id))

filtered_med_data$drug_name_id <- NULL
filtered_med_data$med_cat_id <- NULL

distinct_meds <- unique(filtered_med_data)

write.csv(distinct_meds,"~/Documents/MSc/Thesis3/baseline_models/model_1/distinct_meds.csv")
