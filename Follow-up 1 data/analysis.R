setwd("Summer_project/Data/")
meds <- readRDS("follow_up1/meds_1.rds")
data <- readRDS("follow_up1/merged_data_1.rds")

#1047 out of the 1857 people had medications at the first follow-up

#1st January is the mid-point between August 2012 and June 2013, however not all prescriptions would be 30 days.
summary(as.factor(meds$quantity))

#assume follow-up date is 1st January 2013
meds$follow_up1_date <- as.Date("2013-01-01")

#calculate difference between issue date and follow_up date for each row
meds$date_diff <- as.numeric(meds$issue_date - meds$follow_up1_date)

#for each person, keep whichever medications are closest to the follow_up date

library(dplyr)

meds <- meds %>%
  group_by(eid) %>%
  slice_min(date_diff,n=1)

hist(meds$date_diff)
#the date_diff isn't accurate of what prescriptions someone was on as it's just an arbritary date.

#load all cases and baseline meds data for people where appropriate
baseline_data <- readRDS("follow_up1/merged_data_1.rds")

colnames(baseline_data)
#baseline_data has transposed the medication data so we have one row for each person on meds, we have 1857 people so why 7121 rows?
test <- baseline_data[duplicated(baseline_data$eid),]
summary(as.factor(test$med_status_0))
#those untreated at baseline are duplicated for some reason, let's remove duplicate eids..

baseline_data <- baseline_data[!duplicated(baseline_data$eid),]

saveRDS(baseline_data,"follow_up1/merged_data_1.rds")

#sanity check to make sure all med eids are also in baseline_data.
summary(meds$eid %in% baseline_data$eid)

#remove irrelevant columns as already contained in merged_data_1
meds_subset <- subset(meds, select=-c(date_recr,date_diagnosis,date_death,case,prevalent_case,incident_case,time_to_diagnosis,sex.0.0,
                               smoking_status.0.0,pack_years.0.0,ethnicity.0.0,bmi.0.0,age_recr.0.0,sys_bp.0,sys_bp.1,sys_bp.2,sys_bp.3,
                               dias_bp.0,dias_bp.1,dias_bp.2,dias_bp.3,data_provider))

library(tidyverse)
# Transpose the dataframe
transposed_meds <- meds_subset %>%
  group_by(eid) %>%
  mutate(medication_index = paste0("follow_up_", row_number())) %>%
  pivot_wider(
    id_cols = eid,
    names_from = medication_index,
    values_from = c(issue_date,bnf_code, drug_name, quantity, first_word, dosage, unit, med_cat)
    
  )

merged_df <- merge(baseline_data,transposed_meds,by="eid",all.x=TRUE)

saveRDS(merged_df,"follow_up1/final_data.rds")

#make new med_combo category at baseline and follow_up to see which combination of meds people were on at each point...

final_data <- readRDS("follow_up1/final_data.rds")

# Create a new column with the combined med_cat values
final_data <- final_data %>%
  mutate(med_combo_0 = paste(med_cat_1, med_cat_2, med_cat_3, med_cat_4,med_cat_5,med_cat_6,med_cat_7,med_cat_8,
                             sep = " + "))

final_data <- final_data %>%
  mutate(med_combo_1 = paste(med_cat_follow_up_1, med_cat_follow_up_2, med_cat_follow_up_3,med_cat_follow_up_4,med_cat_follow_up_5,
                             med_cat_follow_up_6,med_cat_follow_up_7,med_cat_follow_up_7,med_cat_follow_up_8,sep= " + "))

#someone may be on ARBs and CCBs, but depending on which one is prescribed first, will either appear
#as ARBs/CCBs or CCBs/ARBs. Can we combine these 2 options together?

#make function to combine identical meds but in different orders
combine_meds <- function(string) {
  meds <- strsplit(string, " \\+ ")[[1]]  # Split the string into separate medication types
  
  combined_meds <- sort(meds)  # Sort the medication types alphabetically
  
  combined_string <- paste(combined_meds, collapse = " + ")  # Combine the medication types into a single string
  
  return(combined_string)
}
# Apply the helper function to the medication_column and create a new combined_medication_column
final_data <- final_data %>%
  mutate(med_combo_0 = sapply(med_combo_0, combine_meds))

final_data <- final_data %>%
  mutate(med_combo_1 = sapply(med_combo_1, combine_meds))

#remove NA strings from med_combo
final_data$med_combo_0 <- gsub("NA","",final_data$med_combo_0)
final_data$med_combo_1 <- gsub("NA","",final_data$med_combo_1)

# Remove "+ + " strings from med_combo
final_data$med_combo_0 <- sub("[^A-Za-z]+$", "", final_data$med_combo_0)
final_data$med_combo_1 <- sub("[^A-Za-z]+$", "", final_data$med_combo_1)

summary(as.factor(final_data$med_combo_0))
summary(as.factor(final_data$med_combo_1))

saveRDS(final_data,"follow_up1/final_data.rds")


