setwd("Summer_project/Data/")

#load all prevalent cases from their corresponding dataframes at baseline
oldmeds_icd_prev <- readRDS("baseline_analysis/oldmeds_icd_prev.rds")
recentmeds_icd_prev <- readRDS("baseline_analysis/recentmeds_icd_prev.rds")
nomeds_icd_prev <- readRDS("baseline_analysis/nomeds_icd_prev.rds")

#load all incident cases from their corresponding dataframes at baseline
nomeds_icd_incid <- readRDS("baseline_analysis/incident_cases/incident_base_nomeds.rds")
recentmeds_icd_incid <- readRDS("baseline_analysis/incident_cases/recentmeds.rds")
oldmeds_icd_incid <- readRDS("baseline_analysis/incident_cases/oldmeds.rds")

#add differentiable column to incident cases to identify which category they were in at baseline
nomeds_icd_incid$med_status_0 <- "untreated_at_recr"
recentmeds_icd_incid$med_status_0 <- "treated_at_recr"
oldmeds_icd_incid$med_status_0 <- "prev_treated_at_recr"

#convert case column to factor for merging
nomeds_icd_incid$case <- as.factor(nomeds_icd_incid$case)
recentmeds_icd_incid$case <- as.factor(recentmeds_icd_incid$case)
oldmeds_icd_incid$case <- as.factor(oldmeds_icd_incid$case)

#we want to transpose the dataframes with medications, such that, we have for each dataset, one row for each person, and all their medications

# Transpose the recentmeds dataframe to have 1 row for each person with all medications at baseline
recentmeds_icd_prev <- recentmeds_icd_prev %>%
  group_by(eid) %>%
  mutate(medication_index = row_number()) %>%
  pivot_wider(
    id_cols = c(eid, date_recr, date_diagnosis, date_death, case, prevalent_case, sex.0.0, smoking_status.0.0,
                pack_years.0.0, ethnicity.0.0, bmi.0.0, age_recr.0.0, sys_bp.0, sys_bp.1, sys_bp.2,
                sys_bp.3, dias_bp.0, dias_bp.1, dias_bp.2, dias_bp.3, data_provider, issue_date,
                idx_multdep, prescription_before_recruitment, date_diff, daily_dose,med_status_0),
    names_from = medication_index,
    values_from = c(bnf_code, drug_name, quantity, first_word, dosage, unit, med_cat)
    
  )

#rename datediff to date_diff
recentmeds_icd_incid <- recentmeds_icd_incid %>% 
  rename(date_diff = datediff)

#transpose recentmeds_icd_incid to have 1 row for each person with all meds at baseline.
recentmeds_icd_incid <- recentmeds_icd_incid %>%
  group_by(eid) %>%
  mutate(medication_index = row_number()) %>%
  pivot_wider(
    id_cols = c(eid, date_recr, date_diagnosis, date_death, case, prevalent_case, sex.0.0, smoking_status.0.0,
                pack_years.0.0, ethnicity.0.0, bmi.0.0, age_recr.0.0, sys_bp.0, sys_bp.1, sys_bp.2,
                sys_bp.3, dias_bp.0, dias_bp.1, dias_bp.2, dias_bp.3, data_provider, issue_date,
                idx_multdep, prescription_before_recruitment, date_diff,med_status_0),
    names_from = medication_index,
    values_from = c(bnf_code, drug_name, quantity, first_word, dosage, unit, med_cat)
    
  )

#the oldmeds dataframes currently contain people that did not have prescriptions within 6 months of biobank recruitment, however,
#it contains ALL prescriptions > 6 months before their recruitment. This information isn't informative, so why don't we just remove that?
#and bear in mind the fact that they were given some medication before recruitment at some stage...

oldmeds_icd_prev <- oldmeds_icd_prev[!duplicated(oldmeds_icd_prev$eid),]
oldmeds_icd_incid <- oldmeds_icd_incid[!duplicated(oldmeds_icd_incid$eid),]

#remove medication data
oldmeds_icd_incid <- subset(oldmeds_icd_incid, select = -c(issue_date, read_2, bnf_code, dmd_code, drug_name, quantity, first_word, dosage, unit, med_cat, prescription_before_recruitment, datediff))

oldmeds_icd_prev <- subset(oldmeds_icd_prev, select = -c(issue_date, read_2, bnf_code, dmd_code, drug_name, quantity, first_word, dosage, unit, med_cat, prescription_before_recruitment, date_diff))



#merge recentmeds/nomeds/oldmeds dataframes for prev and incident cases together
final_data <- bind_rows(recentmeds_icd_prev,recentmeds_icd_incid,
                        nomeds_icd_incid,nomeds_icd_prev,oldmeds_icd_incid,oldmeds_icd_prev)

data_eids <- unique(final_data$eid)

View(final_data)

#only keep people with bp measurements in sys_bp.1 and dias_bp.1

final_data_1 <- final_data[!is.na(final_data$sys_bp.1) & !is.na(final_data$dias_bp.1),]

n_distinct(final_data_1$eid)
#1,857 people have follow-up measurements in sys_bp.1 and dias_bp.1
final_data_2 <- final_data[!is.na(final_data$sys_bp.2) & !is.na(final_data$dias_bp.2),]

n_distinct(final_data_2$eid)
#5,333 people have follow-up measurements in sys_bp.2 and dias_bp.2 (not all these people have sys/dias_bp.1 data)

#483 people have follow-up measurements in sys_bp.3 and dias_bp.3 (not all these people have sys/dias_bp.1/2 data)

saveRDS(final_data,"follow_up1/merged_data.rds")

#################################################################################################
data <- readRDS("follow_up1/merged_data.rds")

#only keep people who have follow up 1 data

data_1 <- data[!is.na(data$sys_bp.1) & !is.na(data$dias_bp.1),]


saveRDS(data_1,"follow_up1/merged_data_1.rds")

#store their eids
eids <- unique(data_1$eid)

#load medication data
meds <- readRDS("cases_meds_final.rds")

#subset meds to only keep those with bp.1 measurements and icd diagnoses
meds <- meds[meds$eid %in% eids,]

#only keep medications issued after enrolment, we already have baseline meds.
meds <- meds[meds$issue_date > meds$date_recr,]

#remove irrelevant columns
meds$dmd_code <- NULL
meds$read_2 <- NULL

#only keep medications issued between August 2012 and June 2013, when follow up 1 was
meds <- meds[meds$issue_date <= as.Date("2013-06-30"),]
meds <- meds[meds$issue_date >= as.Date("2012-08-01"),]

#take one individual to get a feel for the data
test <- meds %>%
  filter(eid==1002908)

View(test)


library(dplyr)

#take the last medications and most recent medications from August 2012 - June 2013 for each person
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

saveRDS(meds,"follow_up1/meds_1.rds")

# Transpose the dataframe
transposed_meds <- meds %>%
  group_by(eid) %>%
  mutate(medication_index = row_number()) %>%
  pivot_wider(
    id_cols = c(eid, date_recr, date_diagnosis, date_death, case, prevalent_case,incident_case, time_to_diagnosis,
                sex.0.0, smoking_status.0.0, pack_years.0.0, ethnicity.0.0, bmi.0.0, age_recr.0.0, sys_bp.0, sys_bp.1, sys_bp.2,
                sys_bp.3, dias_bp.0, dias_bp.1, dias_bp.2, dias_bp.3, data_provider),
    names_from = medication_index,
    values_from = c(issue_date,bnf_code, drug_name, quantity, first_word, dosage, unit, med_cat)
    
  )

