#load hospitalisations data
hospitalisations <-readRDS("~/Summer_project/Data/comorbidities/icd_final.rds")

#load baseline data
data <- readRDS("~/Summer_project/baseline_models/model_1/baseline_data_NICE.rds")

#keep hospitalisations after date_recr for each person
hospitalisations <- hospitalisations[hospitalisations$icd10_date > hospitalisations$date_recr,]

#only keep hospitalisations of individuals in our dataset
hospitalisations <- hospitalisations[hospitalisations$eid %in% data$eid,]

#make list of cardiac codes we're interested in
myocardial_codes <- c("G45","I20","I21","I22","I23","I24","I25","I50","I60","I61","I62","I63", "I64","I65","I66",
                      "I67")


#subset to only include hospitalisations relating to cardiac events
cardiac_hospitalisations <- hospitalisations[grepl(paste0("^(",paste(myocardial_codes,collapse="|"),")"),hospitalisations$icd10),]

provider <- subset(data,select=c(eid,data_provider))


#people can have multiple hospitalisations on the same date... only take the first one?
hospitalisations_unique <- hospitalisations %>% distinct(eid, icd10_date, .keep_all = TRUE)

cardiac_hospitalisations_unique <- cardiac_hospitalisations %>% distinct(eid, icd10_date, .keep_all = TRUE)

#only keep hospitalisations up until the 04-05-2017 if data provider = 2 or 01-08-2016 if = 3 
#Filter rows based on the given conditions
merged_df_unique <- merge(hospitalisations_unique,provider,by="eid")

merged_df_unique <- subset(merged_df_unique, 
                    (data_provider == 2 & icd10_date <= as.Date("2017-05-04")) |
                      (data_provider == 3 & icd10_date <= as.Date("2016-08-01")) |
                      (is.na(data_provider) & icd10_date <= as.Date("2017-05-04")))

#count number of hospitalisations for each person
#Let's look at people's medications at baseline, and at the end of the follow-up, then with this data we can
#see who's changed medications, who started treatment, who increased dosage etc, the same as with mortality
#we can then look at the number of hospitalisations and then the number of cardiac-specific hospitalisations
#for each person. Then we look at people with the same prescriptions throughout and do the same thing
#we can also do a mixed poisson model for those on the same prescriptions throughout...

n_hospitalisations_unique <- merged_df_unique %>%
  group_by(eid) %>%
  count()
#only keep hypertension-related hospitalisations
cardiac_hosp_unique <- merged_df_unique[grepl(paste0("^(", paste(myocardial_codes, collapse = "|"), ")"), merged_df_unique$icd10), ]

n_cardiac_unique <- cardiac_hosp_unique %>%
  group_by(eid) %>%
  count()
#make histogram of cardiac-related hospitalisations including those with none
#cardiac_hist <- merge(data,n_cardiac_unique,by="eid",all.x=T)
#cardiac_hist$n <- ifelse(is.na(cardiac_hist$n),0,cardiac_hist$n)
#hist(cardiac_hist$n)

#load medications data
meds <- readRDS("~/Summer_project/Data/cases_meds_final.rds")

#keep meds of eids in our dataset
meds <- meds[meds$eid %in% data$eid,]

#keep the latest medication for each person 
library(dplyr)

last_meds <- meds %>%
  group_by(eid) %>%
  filter(issue_date == max(issue_date)) %>%
  ungroup()

#calculate the date_diff for each person's meds
last_meds$date_diff <- ifelse(last_meds$data_provider==2,as.numeric(as.Date("2017-05-04") - last_meds$issue_date),
                                                                    as.numeric(as.Date("2016-08-01") - last_meds$issue_date))

#if someone's last prescription is >=6 months from the end date, assume they are untreated and remove rows
last_meds <- last_meds[last_meds$date_diff<=180,]

last_meds <- last_meds %>%
  rename(`Last medication combination` = med_cat)

#re-do daily_dose column 
last_meds <- last_meds %>%
  group_by(eid) %>%
  mutate(`Last daily dose` = sum(dosage))


last_meds2 <- subset(last_meds,select=c(eid,`Last medication combination`,`Last daily dose`))

last_meds_transposed <- last_meds2 %>%
  group_by(eid) %>%
  mutate(medication_index = row_number()) %>%
  pivot_wider(
    id_cols = c(eid,`Last daily dose`),
    names_from = medication_index,
    values_from = `Last medication combination`)

last_meds_transposed <- last_meds_transposed %>%
  rename(med_cat_1 = 3, med_cat_2 = 4, med_cat_3 = 5, med_cat_4 = 6, med_cat_5 = 7, med_cat_6 = 8,
         med_cat_7 = 9)

# Create a new column with the combined med_cat values
last_meds_transposed <- last_meds_transposed %>%
  mutate(`Last medication combination` = paste(med_cat_1, med_cat_2, med_cat_3, med_cat_4,med_cat_5,
                                               med_cat_6,med_cat_7,
                           sep = " + "))

#remove NA strings from med_combo
last_meds_transposed$`Last medication combination` <- gsub("NA","",last_meds_transposed$`Last medication combination`)

# Remove "+ + " strings from transposed_meds
last_meds_transposed$`Last medication combination` <- sub("[^A-Za-z]+$", "", last_meds_transposed$`Last medication combination`)

#someone may be on ARBs and CCBs, but depending on which one is prescribed first, will either appear
#as ARBs/CCBs or CCBs/ARBs. Can we combine these 2 options together?

# Define a helper function to combine and sort medication types within a string
combine_meds <- function(string) {
  meds <- strsplit(string, "_")[[1]]  # Split the string into separate medication types
  combined_meds <- sort(meds)  # Sort the medication types alphabetically
  combined_string <- paste(combined_meds, collapse = "_")  # Combine the medication types into a single string
  return(combined_string)
}

# Apply the helper function to the medication_column and create a new combined_medication_column
last_meds_transposed <- last_meds_transposed %>%
  mutate(`Last medication combination` = sapply(`Last medication combination`, combine_meds))

last_meds_transposed <- subset(last_meds_transposed,select=c(eid,`Last daily dose`,`Last medication combination`))

merged_df <- merge(data,last_meds_transposed,by="eid",all.x=T)

merged_df$`Last medication combination` <- ifelse(is.na(merged_df$`Last medication combination`),"Untreated",
                                                  merged_df$`Last medication combination`)

merged_df$`Last daily dose` <- ifelse(is.na(merged_df$`Last daily dose`),0,
                                                  merged_df$`Last daily dose`)

n_hospitalisations_unique <- n_hospitalisations_unique %>%
  rename(n_hospitalisations_unique = n)

n_cardiac_unique <- n_cardiac_unique %>%
  rename(n_cardiac_unique = n)


merged_df <- merge(merged_df,n_hospitalisations_unique,by="eid",all.x=T)

merged_df$n_hospitalisations_unique <- ifelse(is.na(merged_df$n_hospitalisations_unique),0,merged_df$n_hospitalisations_unique)

merged_df <- merge(merged_df,n_cardiac_unique,by="eid",all.x=T)

merged_df$n_cardiac_unique <- ifelse(is.na(merged_df$n_cardiac_unique),0,merged_df$n_cardiac_unique)

saveRDS(merged_df,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3a/merged_df.rds")

#we only want to keep the medication prescriptions that fall within NICE guidelines
merged_df <- merged_df[merged_df$`Medication combination`=="Untreated" |
                                     merged_df$`Medication combination`=="ACE inhibitors/ARBs" |
                                     merged_df$`Medication combination`=="Calcium channel blockers" |
                                     merged_df$`Medication combination`=="ACE inhibitors/ARBs + Diuretics" |
                                     merged_df$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers" |
                                     merged_df$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics" |
                                     merged_df$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics + B blockers" |
                                     merged_df$`Medication combination`=="Calcium channel blockers + Diuretics",
]
#only keep people that are either untreated or had a final medication combination falling within NICE guidelines
merged_df <- merged_df[merged_df$`Last medication combination`=="Untreated" |
                                       merged_df$`Last medication combination`=="ACE inhibitors/ARBs" |
                                       merged_df$`Last medication combination`=="Calcium channel blockers" |
                                       merged_df$`Last medication combination`=="ACE inhibitors/ARBs + Diuretics" |
                                       merged_df$`Last medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers" |
                                       merged_df$`Last medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics" |
                                       merged_df$`Last medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics + B blockers" |
                                       merged_df$`Last medication combination`=="Calcium channel blockers + Diuretics",
]

saveRDS(merged_df,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3a/merged_df.rds")

##################################################
data <- readRDS("/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3a/merged_df.rds")

#make dummy variables indicating who started medications, who ended them, who increased dosage etc...
#make new variable to see who's started medications
data$`Started medication` <- as.numeric((data$`Medication combination` == "Untreated" & !(data$`Last medication combination`=="Untreated")))

#make new variable to see who stopped medications
data$`Ended medication` <- as.numeric((!(data$`Medication combination` == "Untreated") & data$`Last medication combination`=="Untreated"))

#make new variable to see who changed medications
data$`Changed medications` <- as.numeric(as.character(data$`Medication combination`)!= as.character(data$`Last medication combination`) & data$`Medication combination`!="Untreated" & data$`Last medication combination`!="Untreated")


#make new variable to see who was still untreated
data$`Stayed untreated` <- as.numeric(data$`Medication combination` == "Untreated" & data$`Last medication combination`=="Untreated")

#make variable to see who stayed on the same medications at the same dosage
data$`Stable medications` <- as.numeric(as.character(data$`Medication combination`) == as.character(data$`Last medication combination`) & data$`Medication combination`!="Untreated" 
                                        & data$`daily_dose.y` == data$`Last daily dose`)

#make column to see who increased their dosage but stayed on the same medications
data$`Increased dosage` <- ifelse(as.character(data$`Medication combination`) == data$`Last medication combination` & data$`daily_dose.y` < data$`Last daily dose` ,1,0)

#make column to see who decreased their dosage but stayed on the same medications
data$`Decreased dosage` <- ifelse(as.character(data$`Medication combination`) == data$`Last medication combination` & data$`daily_dose.y` > data$`Last daily dose` ,1,0)

saveRDS(data,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3a/final_data.rds")

######################################################################
data <- readRDS("/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3a/final_data.rds")

#Make new column identifying which medication status people belong to.
library(dplyr)
data <- data %>%
  rename(med_stat = `Medication status`)

data$`Medication status` <- ifelse(data$`Started medication` == 1, "Started medication",
                                   ifelse(data$`Ended medication` == 1, "Ended medication",
                                          ifelse(data$`Changed medications` == 1, "Changed medications",
                                                 ifelse(data$`Stayed untreated` == 1, "Stayed untreated",
                                                        ifelse(data$`Increased dosage` == 1, "Increased dosage",
                                                               ifelse(data$`Stable medications` == 1, "Stable medications",
                                                                      "Decreased dosage"))))))

data$`Medication status` <- as.factor(data$`Medication status`)
data$`Medication status` <- relevel(data$`Medication status`,ref="Stable medications")

saveRDS(data,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3a/final_data.rds")

