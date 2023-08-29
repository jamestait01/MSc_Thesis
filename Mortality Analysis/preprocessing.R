data <- readRDS("~/Summer_project/baseline_models/model_1/baseline_data_NICE.rds")

#I need to find out the medication information for each person...
#load medication data
cases_meds_final <- readRDS("~/Summer_project/Data/cases_meds_final.rds")

#keep meds of eids in our dataset
cases_meds_final <- cases_meds_final[cases_meds_final$eid %in% data$eid,]

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

death_data <- subset(data,select=c(eid,death))

cases_meds_final <- merge(cases_meds_final,death_data,by="eid",all.x=T)

#all meds taken for those that are dead
meds_dead <- cases_meds_final[cases_meds_final$death==1,]

#all meds taken for those alive at end of study period
meds_alive <- cases_meds_final[cases_meds_final$death==0,]

#calculate date diff
meds_dead$date_diff <- meds_dead$date_death - meds_dead$issue_date

#take the medication closest to each person's death
# Step 1: Sort the dataframe by "issue_date" column
sorted_df <- meds_dead[order(meds_dead$issue_date), ]

# Step 2-4: Iterate over unique eids and select rows with closest issue_date to date_death
result_df <- data.frame()  # Create an empty dataframe to store the result

unique_eids <- unique(meds_dead$eid)
for (eid in unique_eids) {
  eid_rows <- sorted_df[sorted_df$eid == eid, ]  # Select rows with the current eid
  distances <- eid_rows$date_death - eid_rows$issue_date  # Calculate time differences
  valid_rows <- distances >= 0  # Rows where issue_date is not after date_death
  min_distance <- min(distances[valid_rows])  # Find the minimum valid distance
  
  closest_rows <- eid_rows[distances == min_distance & valid_rows, ]  # Select all valid rows with the minimum distance
  result_df <- rbind(result_df, closest_rows)  # Append the closest rows to the result dataframe
}

#find most recent meds for those still alive
# Step 1: Sort the dataframe by "issue_date" column in descending order
sorted_alive <- meds_alive[order(meds_alive$issue_date, decreasing = TRUE), ]

# Step 2: Find the maximum issue_date for each eid
max_dates <- aggregate(issue_date ~ eid, data = sorted_alive, FUN = max)

# Step 3: Subset the dataframe to include all rows with the maximum issue_date for each eid
result_alive <- merge(sorted_alive, max_dates, by = c("eid", "issue_date"))

saveRDS(result_df,"~/Summer_project/baseline_models/model_3b/dead_meds.rds")

saveRDS(result_alive,"~/Summer_project/baseline_models/model_3b/alive_meds.rds")

saveRDS(data,"~/Summer_project/baseline_models/model_3b/death_data.rds")

#####################################################################################
alive_meds <- readRDS("~/Summer_project/baseline_models/model_3b/alive_meds.rds")
dead_meds <- readRDS("~/Summer_project/baseline_models/model_3b/dead_meds.rds")
death_data <- readRDS("~/Summer_project/baseline_models/model_3b/death_data.rds")

#if someone was last prescribed >= 6 months before their death -> assume they are untreated
dead_meds$date_diff <- as.numeric(dead_meds$date_diff)

eids <- dead_meds[dead_meds$date_diff>180,]
n_distinct(eids$eid)
#245 people we assume were untreated at death...

#if someone was last prescribed >= 6 months before end of follow-up -> assume they are untreated
alive_meds$date_diff <- ifelse(alive_meds$data_provider==2,as.numeric(as.Date("2017-05-04") - alive_meds$issue_date),
                               as.numeric(as.Date("2016-08-01") - alive_meds$issue_date))

alive_eids <- alive_meds[alive_meds$date_diff > 180,]
n_distinct(alive_eids$eid)
#6404 people we assume untreated at end of follow-up...


#take meds of those with a prescription within 6 months of their death
dead_meds <- dead_meds[dead_meds$date_diff<=180,]
#1287 people that died were prescribed within the 6 months prior to their death
alive_meds <- alive_meds[alive_meds$date_diff<=180,]
#re-do daily_dose column 
dead_meds <- dead_meds %>%
  group_by(eid) %>%
  mutate(`Last daily dose` = sum(dosage))

alive_meds <- alive_meds %>%
  group_by(eid) %>%
  mutate(`Last daily dose` = sum(dosage))

#remove dmd_code and read_2 codes as not needed

dead_meds$dmd_code <- NULL
dead_meds$read_2 <- NULL

#remove dmd_code and read_2 codes as not needed

alive_meds$dmd_code <- NULL
alive_meds$read_2 <- NULL

# Transpose the dataframes
transposed_alive <- alive_meds %>%
  group_by(eid) %>%
  mutate(medication_index = row_number()) %>%
  pivot_wider(
    id_cols = c(eid, date_recr, date_diagnosis, date_death, case, prevalent_case, sex.0.0, smoking_status.0.0,
                pack_years.0.0, ethnicity.0.0, bmi.0.0, age_recr.0.0, sys_bp.0, sys_bp.1, sys_bp.2,
                sys_bp.3, dias_bp.0, dias_bp.1, dias_bp.2, dias_bp.3, data_provider, issue_date,
                 `Last daily dose`),
    names_from = medication_index,
    values_from = c(bnf_code, drug_name, quantity, first_word, dosage, unit, med_cat)
    
  )

transposed_dead <- dead_meds %>%
  group_by(eid) %>%
  mutate(medication_index = row_number()) %>%
  pivot_wider(
    id_cols = c(eid, date_recr, date_diagnosis, date_death, case, prevalent_case, sex.0.0, smoking_status.0.0,
                pack_years.0.0, ethnicity.0.0, bmi.0.0, age_recr.0.0, sys_bp.0, sys_bp.1, sys_bp.2,
                sys_bp.3, dias_bp.0, dias_bp.1, dias_bp.2, dias_bp.3, data_provider, issue_date,
                `Last daily dose`),
    names_from = medication_index,
    values_from = c(bnf_code, drug_name, quantity, first_word, dosage, unit, med_cat))

#remove columns with only NA's
transposed_dead <- transposed_dead[,colSums(is.na(transposed_dead))<nrow(transposed_dead)]


# Create a new column with the combined med_cat values
transposed_dead <- transposed_dead %>%
  mutate(med_combo = paste(med_cat_1, med_cat_2, med_cat_3, med_cat_4,med_cat_5,
                           sep = " + "),na.rm=TRUE)

transposed_alive <- transposed_alive %>%
  mutate(med_combo = paste(med_cat_1, med_cat_2, med_cat_3, med_cat_4,med_cat_5, med_cat_6,med_cat_7,
                           sep = " + "),na.rm=TRUE)

#remove NA strings from med_combo
transposed_dead$med_combo <- gsub("NA","",transposed_dead$med_combo)
transposed_alive$med_combo <- gsub("NA","",transposed_alive$med_combo)

# Remove "+ + " strings from transposed_meds
transposed_dead$med_combo <- sub("[^A-Za-z]+$", "", transposed_dead$med_combo)
# Remove "+ + " strings from transposed_meds
transposed_alive$med_combo <- sub("[^A-Za-z]+$", "", transposed_alive$med_combo)

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
transposed_dead <- transposed_dead %>%
  mutate(med_combo = sapply(med_combo, combine_meds))

transposed_alive <- transposed_alive %>%
  mutate(med_combo = sapply(med_combo, combine_meds))

#rename the med_combo column to Medication combination
transposed_alive <- transposed_alive %>%
  rename(`Medication combination` = med_combo)

transposed_dead <- transposed_dead %>%
  rename(`Medication combination` = med_combo)

#transposed dead contains the last meds for people that had died 
#we only want to keep the medication prescriptions that fall within NICE guidelines
transposed_dead_NICE <- transposed_dead[transposed_dead$`Medication combination`=="Untreated" |
                                         transposed_dead$`Medication combination`=="ACE inhibitors/ARBs" |
                                         transposed_dead$`Medication combination`=="Calcium channel blockers" |
                                         transposed_dead$`Medication combination`=="ACE inhibitors/ARBs + Diuretics" |
                                         transposed_dead$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers" |
                                         transposed_dead$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics" |
                                         transposed_dead$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics + B blockers" |
                                         transposed_dead$`Medication combination`=="Calcium channel blockers + Diuretics",
]

#transposed_alive contains the last medications for people that are alive
transposed_alive_NICE <- transposed_alive[transposed_alive$`Medication combination`=="Untreated" |
                                     transposed_alive$`Medication combination`=="ACE inhibitors/ARBs" |
                                     transposed_alive$`Medication combination`=="Calcium channel blockers" |
                                     transposed_alive$`Medication combination`=="ACE inhibitors/ARBs + Diuretics" |
                                     transposed_alive$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers" |
                                     transposed_alive$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics" |
                                     transposed_alive$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics + B blockers" |
                                     transposed_alive$`Medication combination`=="Calcium channel blockers + Diuretics",
]

#get eids of people that were not on NICE medications at their death/end of study and remove them...
removeable_eids <- transposed_alive[!(transposed_alive$eid %in% transposed_alive_NICE$eid),]

removeable_eids2 <- transposed_dead[!(transposed_dead$eid %in% transposed_dead_NICE$eid),]

removeable_eids3 <- merge(removeable_eids,removeable_eids2,all.x=T,all.y=T)

#removeable_eids3 contains all of the people that were not on NICE medications at the end of treatment, 
#so we need to remove them from the dataset...

#eids contains people that have died but were classified as untreated due to last prescription >6 months before death
eids$`Medication combination` <- "Untreated"
alive_eids$`Medication combination` <- "Untreated"

#add last to the column names so we can distinguish between original meds at baseline when merging back together

# Get the column names
col_names <- colnames(transposed_dead_NICE[,22:56])

# Add "last" to the start of each column name
new_col_names <- paste0("last_", col_names)

# Assign the modified column names back to the dataframe
colnames(transposed_dead_NICE)[22:56] <- new_col_names


#add last to the column names so we can distinguish between original meds when merging back together

# Get the column names
col_names <- colnames(transposed_alive_NICE[,24:72])

# Add "last" to the start of each column name
new_col_names <- paste0("last_", col_names)

# Assign the modified column names back to the dataframe
colnames(transposed_alive_NICE)[24:72] <- new_col_names

merged_df <- merge(transposed_alive_NICE,transposed_dead_NICE,all.x=T,all.y=T)

merged_df <- merged_df %>%
  rename(`Last medication combination` = `Medication combination`)

merged_df2 <- merge(death_data,merged_df,all.x=T,by="eid")

#remove eids that are in the removeable_eids3 as they weren't on NICE medications at the end of study
merged_df2 <- merged_df2[!(merged_df2$eid %in% removeable_eids3$eid),]

#if eids are present in eids or alive_eids, they are untreated at the end as their last meds are >6 months
merged_df2$`Last medication combination` <- ifelse(merged_df2$eid %in% eids$eid | merged_df2$eid %in% alive_eids$eid,"Untreated",merged_df2$`Last medication combination`)

merged_df2_updated <- merged_df2 %>% select(-ends_with(".y"))

merged_df2_updated$`Last medication combination` <- ifelse(is.na(merged_df2_updated$`Last medication combination`),"Untreated",merged_df2_updated$`Last medication combination`)

saveRDS(merged_df2_updated,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3b/merg.rds")

################################################################

data <- readRDS("/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3b/merg.rds")

#redo daily dose columns at baseline and at the end of the study
data$`Daily dose` <- rowSums(data[,58:65],na.rm=T)
data$`Last daily dose` <- ifelse(is.na(data$`Last daily dose`),0,data$`Last daily dose`)

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
                                                   & data$`Daily dose` == data$`Last daily dose`)

#make column to see who increased their dosage but stayed on the same medications
data$`Increased dosage` <- ifelse(as.character(data$`Medication combination`) == data$`Last medication combination` & data$`Daily dose` < data$`Last daily dose` ,1,0)

#make column to see who decreased their dosage but stayed on the same medications
data$`Decreased dosage` <- ifelse(as.character(data$`Medication combination`) == data$`Last medication combination` & data$`Daily dose` > data$`Last daily dose` ,1,0)

saveRDS(data,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3b/final_data.rds")

######################################################################
data <- readRDS("/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3b/final_data.rds")

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

saveRDS(data,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3b/final_data.rds")

