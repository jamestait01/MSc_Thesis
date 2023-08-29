#load baseline data
data <- readRDS("follow_up1/merged_data.rds")

#only keep prevalent cases 
library(dplyr)
data <- data %>%
  filter(prevalent_case==1)

#only keep people with BP measurements at second follow_up

data_2 <- data[!is.na(data$sys_bp.2) & !is.na(data$dias_bp.2),]

#store eids
eids <- unique(data_2$eid)

#load medication data
meds <- readRDS("cases_meds_final.rds")

#subset meds to only keep those with bp.2 measurements and icd diagnoses
meds <- meds[meds$eid %in% eids,]

#only keep medications issued after enrolment, we already have baseline meds.
meds <- meds[meds$issue_date > meds$date_recr,]

#remove irrelevant columns
meds$dmd_code <- NULL
meds$read_2 <- NULL

#only keep medications issued between 2014 and 2018, when follow up 2 was
meds <- meds[meds$issue_date <= as.Date("2018-12-31"),]
meds <- meds[meds$issue_date >= as.Date("2014-01-01"),]

#take the last medications and most recent medications from 2014 - 2018 for each person
meds_subset <- meds %>%
  group_by(eid) %>%
  filter(issue_date == min(issue_date) | issue_date == max(issue_date))

#check how many people have the same minimum and maximum meds
meds_comparison <- meds_subset %>%
  group_by(eid, drug_name) %>%
  mutate(duplicated_drug = n() > 1) %>%
  ungroup()


true_eids_count <- meds_comparison %>%
  group_by(eid) %>%
  summarise(contains_only_true = all(duplicated_drug==TRUE)) %>%
  filter(contains_only_true) %>%
  nrow()

#1803 / 3040 on meds had the same meds at the start of 2014 as they did at the end of 2018. 

#take meds closest to the 1st January 2016 as we assume this is the midpoint for BP measurements...

#assume follow-up date is 1st January 2016
meds$follow_up2_date <- as.Date("2016-01-01")

#calculate difference between issue date and follow_up date for each row
meds$date_diff <- as.numeric(meds$issue_date - meds$follow_up2_date)

#for each person, keep whichever medications are closest to the follow_up date
library(dplyr)

meds <- meds %>%
  group_by(eid) %>%
  slice_min(date_diff,n=1)

saveRDS(meds,"follow_up2/meds_2.rds")


#remove irrelevant columns as already contained in data_2
meds <- subset(meds, select=-c(date_recr,date_diagnosis,date_death,case,prevalent_case,incident_case,time_to_diagnosis,sex.0.0,
                                      smoking_status.0.0,pack_years.0.0,ethnicity.0.0,bmi.0.0,age_recr.0.0,sys_bp.0,sys_bp.1,sys_bp.2,sys_bp.3,
                                      dias_bp.0,dias_bp.1,dias_bp.2,dias_bp.3,data_provider))

library(tidyverse)
# Transpose the dataframe
transposed_meds <- meds %>%
  group_by(eid) %>%
  mutate(medication_index = paste0("follow_up_", row_number())) %>%
  pivot_wider(
    id_cols = eid,
    names_from = medication_index,
    values_from = c(issue_date,bnf_code, drug_name, quantity, first_word, dosage, unit, med_cat)
    
  )

merged_df <- merge(data_2,transposed_meds,by="eid",all.x=TRUE)

saveRDS(merged_df,"follow_up2/final_data.rds")

#make new med_combo category at baseline and follow_up to see which combination of meds people were on at each point...

final_data <- readRDS("follow_up2/final_data.rds")

# Create a new column with the combined med_cat values
final_data <- final_data %>%
  mutate(med_combo_0 = paste(med_cat_1, med_cat_2, med_cat_3, med_cat_4,med_cat_5,med_cat_6,med_cat_7,med_cat_8,
                             sep = " + "))

final_data <- final_data %>%
  mutate(med_combo_2 = paste(med_cat_follow_up_1, med_cat_follow_up_2, med_cat_follow_up_3,med_cat_follow_up_4,med_cat_follow_up_5,
                             med_cat_follow_up_6, sep= " + "))

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
  mutate(med_combo_2 = sapply(med_combo_2, combine_meds))

#remove NA strings from med_combo
final_data$med_combo_0 <- gsub("NA","",final_data$med_combo_0)
final_data$med_combo_2 <- gsub("NA","",final_data$med_combo_2)

# Remove "+ + " strings from med_combo
final_data$med_combo_0 <- sub("[^A-Za-z]+$", "", final_data$med_combo_0)
final_data$med_combo_2 <- sub("[^A-Za-z]+$", "", final_data$med_combo_2)

summary(as.factor(final_data$med_combo_0))
summary(as.factor(final_data$med_combo_2))

saveRDS(final_data,"follow_up2/final_data.rds")
