setwd("/rds/general/user/jt1019/home/Summer_project")

data <- readRDS("Data/comorbidities/icd_baseline.rds")

#break data into chunks otherwise the code crashes

subset_1 <- data[1:50000,]
subset_2 <- data[50001:100000,]
subset_3 <- data[100001:150000,]
subset_4 <- data[150001:200000,]
subset_4 <- data[150001:200000,]
subset_5 <- data[200001:250000,]
subset_6 <- data[250001:300000,]
subset_7 <- data[300001:335412,]

library(comorbidity)

elixhauser_1 <- comorbidity(x = subset_1, id = "eid", code = "icd10", map = "elixhauser_icd10_quan", assign0 = FALSE)

elixhauser_2 <- comorbidity(x = subset_2, id = "eid", code = "icd10", map = "elixhauser_icd10_quan", assign0 = FALSE)

elixhauser_3 <- comorbidity(x = subset_3, id = "eid", code = "icd10", map = "elixhauser_icd10_quan", assign0 = FALSE)

elixhauser_4 <- comorbidity(x = subset_4, id = "eid", code = "icd10", map = "elixhauser_icd10_quan", assign0 = FALSE)

elixhauser_5 <- comorbidity(x = subset_5, id = "eid", code = "icd10", map = "elixhauser_icd10_quan", assign0 = FALSE)

elixhauser_6 <- comorbidity(x = subset_6, id = "eid", code = "icd10", map = "elixhauser_icd10_quan", assign0 = FALSE)

elixhauser_7 <- comorbidity(x = subset_7, id = "eid", code = "icd10", map = "elixhauser_icd10_quan", assign0 = FALSE)

elixhauser <- bind_rows(elixhauser_1,elixhauser_2,elixhauser_3,elixhauser_4,elixhauser_5,elixhauser_6,elixhauser_7)

#there are 58,069 people in elixhauser but 58,063 people in data, this means 6 people were repeated twice.
#we need to redo the analysis for these people as their data was spread across multiple chunks

#find duplicate ids
dup_eids <- elixhauser[duplicated(elixhauser$eid),]

#find duplicate ids
dup_eids <- elixhauser[elixhauser$eid %in% dup_eids$eid,]

#remove these eids from elixhauser
elixhauser <- elixhauser[!(elixhauser$eid %in% dup_eids$eid),]

#take icd data for these 6 individuals 
data_dups <- data[data$eid %in% dup_eids$eid,]

#calculate elixhauser comorbidity index for these individuals
elixhauser_8 <- comorbidity(x = data_dups, id = "eid", code = "icd10", map = "elixhauser_icd10_quan", assign0 = FALSE)

#merge the 2 together
elixhauser <- bind_rows(elixhauser,elixhauser_8)

#compute number of comorbidities for each person
elixhauser$n_comorbid <- rowSums(elixhauser[, -1])
elixhauser$n_comorbid <- as.factor(elixhauser$n_comorbid)

summary(elixhauser$n_comorbid)

#calculate weighted ECI score using the most recent weights from the swiss paper (Sharma et al 2021)
elixhauser$ECI_weighted <- score(elixhauser,weights="swiss",assign0=FALSE)

summary(elixhauser$ECI_weighted)

saveRDS(elixhauser,"Data/comorbidities/elixhauser_baseline.rds")

##########################################################################

comorbid <- readRDS("Data/comorbidities/elixhauser_baseline.rds")

#load dataframe with all cases and their meds at baseline
data <- readRDS("Data/follow_up1/merged_data.rds")

merged_df <- merge(data,comorbid,by="eid",all.x=TRUE)


library(ggplot2)
library(tidyverse)

#select only prevalent cases
prev <- merged_df %>%
  filter(prevalent_case==1)

#group by med status at baseline

treated <- prev %>%
  filter(med_status_0=="treated_at_recr")

untreated <- prev %>%
  filter(med_status_0=="untreated_at_recr")

prev_treated <- prev %>%
  filter(med_status_0=="prev_treated_at_recr")

#produce histogram of eci score for treated, untreated and previously treated at baseline
ggplot(prev, aes(x = ECI_weighted, fill = med_status_0)) +
  geom_density(position = "identity", alpha = 0.2,na.rm=TRUE)

#sum the total number of occurrences for each comorbidity 
sums <- colSums(comorbid[,2:32])

# Create a new dataframe containing the number of occurrences for each comorbidity 
comorbid_counts <- data.frame(Column = names(sums), Sum = sums)

View(comorbid_counts)

#save comorbid_counts as csv file
write_csv(comorbid_counts,"Data/comorbidities/overall_comorbid_counts.csv")

library(psych)

describeBy(prev$ECI_weighted,group=prev$med_status_0)

#calculate comorbidity counts for each treated, untreated and previously treated

#sum the total number of occurrences for each comorbidity 
prev_treated_sums <- colSums(prev_treated[,96:126],na.rm=T)

untreated_sums <- colSums(untreated[,96:126],na.rm=T)

treated_sums <- colSums(treated[,96:126],na.rm=T)

# Create a new dataframe containing the number of occurrences for each comorbidity 
prev_treated_counts <- data.frame(Column = names(prev_treated_sums), Sum = prev_treated_sums)
untreated_counts <- data.frame(Column=names(untreated_sums),Sum=untreated_sums)
treated_counts <- data.frame(Column=names(treated_sums),Sum=treated_sums)

#rename column name to identify which dataset the numbers come from
colnames(treated_counts)[colnames(treated_counts) == "Sum"] <- "Treated_sum"
colnames(untreated_counts)[colnames(untreated_counts) == "Sum"] <- "Untreated_sum"
colnames(prev_treated_counts)[colnames(prev_treated_counts) == "Sum"] <- "Prev_treated_sum"


test <- merge(prev_treated_counts,untreated_counts,by="row.names")

#remove redundant columns
test$Column.x <- NULL
test$Column.y <- NULL

test2 <- merge(test,treated_counts,by.x = "Row.names",by.y="Column")

#rename column
colnames(test2)[colnames(test2) == "Row.names"] <- "Comorbidity"

#convert from AsIs class to characters
test2$Comorbidity <- as.factor(test2$Comorbidity)

library(plyr)

#map shorthand comorbidity strings to full words
test2$Comorbidity <- mapvalues(test2$Comorbidity, 
                               from=c("aids","alcohol","blane","carit","chf","coag","cpd","dane","depre","diabc",
                                      "diabunc","drug","fed","hypc","hypothy","hypunc","ld","lymph","metacanc",
                                      "obes","ond","para","pcd","psycho","pud","pvd","rf","rheumd","solidtum","valv",
                                      "wloss"), 
                               to=c("AIDS/HIV","Alcohol_abuse","Blood_loss_anaemia","Cardiac_arrhythmias","Congestive_heart_failure",
                                    "Coagulopathy","Chronic_pulmonary_disease","Deficiency_anaemia",
                                    "Depression","Diabetes_complicated","Diabetes_uncomplicated","Drug_abuse",
                                    "Fluid_and_electrolyte_disorders","Hypertension_complicated","Hypothyroidism",
                                    "Hypertension_uncomplicated","Liver_disease","Lymphoma","Metastatic_cancer",
                                    "Obesity","Other_neurological_disorders","Paralysis","Pulmonary_circulation_disorders",
                                    "Psychoses","Peptic_ulcer_disease","Peripheral_vascular_disease","Renal_failure","Rheumatoid_arthritis",
                                    "Solid_tumour","Valvular_heart_disease","Weight_loss"))

write_csv(test2,"Data/comorbidities/dataset_comorbid_counts.csv")

saveRDS(merged_df,"Data/comorbidities/elixhauser_all.rds")

saveRDS(prev,"Data/comorbidities/elixhauser_prev.rds")

#find number and proportion of people with 0, 1-2 or >=3 for each treatment group

#convert NA's to 0.
summary(prev$n_comorbid)
prev$n_comorbid <- ifelse(is.na(prev$n_comorbid),0,prev$n_comorbid)
summary(as.factor(prev$n_comorbid))
#it's added a new level so 0 is now the NA's the original 0 has become 1, and the original 12 has become 13 etc.

prev$n_comorbid <- ifelse(prev$n_comorbid ==1,0,prev$n_comorbid)
prev$n_comorbid <- ifelse(prev$n_comorbid ==2,1,prev$n_comorbid)
prev$n_comorbid <- ifelse(prev$n_comorbid ==3,2,prev$n_comorbid)
prev$n_comorbid <- ifelse(prev$n_comorbid ==4,3,prev$n_comorbid)
prev$n_comorbid <- ifelse(prev$n_comorbid ==5,4,prev$n_comorbid)
prev$n_comorbid <- ifelse(prev$n_comorbid ==6,5,prev$n_comorbid)
prev$n_comorbid <- ifelse(prev$n_comorbid ==7,6,prev$n_comorbid)
prev$n_comorbid <- ifelse(prev$n_comorbid ==8,7,prev$n_comorbid)
prev$n_comorbid <- ifelse(prev$n_comorbid ==9,8,prev$n_comorbid)
prev$n_comorbid <- ifelse(prev$n_comorbid ==10,9,prev$n_comorbid)
prev$n_comorbid <- ifelse(prev$n_comorbid ==11,10,prev$n_comorbid)
prev$n_comorbid <- ifelse(prev$n_comorbid ==12,11,prev$n_comorbid)
prev$n_comorbid <- ifelse(prev$n_comorbid ==13,12,prev$n_comorbid)

######################################################################
breaks <- c(-Inf,0, 2, Inf)

# Create the n_comorbid category column
prev$n_comorbid_cat <- cut(prev$n_comorbid, breaks = breaks, labels = c("0", "1-2", ">=3"))

summary(prev$n_comorbid_cat)

table(prev$n_comorbid_cat,prev$med_status_0)

#redo calcs but ignore hypertension 
# Specify the columns to include and exclude in comorbid sum
included_columns <- 96:126
excluded_columns <- c(101, 102)
#make new column for n_comorbid counts for each person, excluding hypertension
prev$n_comorbid_no_hypert <- rowSums(prev[, setdiff(included_columns, excluded_columns)])

summary(as.factor(prev$n_comorbid_no_hypert))
#convert NA's to 0.
prev$n_comorbid_no_hypert <- ifelse(is.na(prev$n_comorbid_no_hypert),0,prev$n_comorbid_no_hypert)
summary(as.factor(prev$n_comorbid_no_hypert))

#make n_comorbid no hypertension category
prev$n_comorbid_cat_no_hypert <- cut(prev$n_comorbid_no_hypert, breaks = breaks, labels = c("0", "1-2", ">=3"))

table(prev$n_comorbid_cat_no_hypert,prev$med_status_0)

#work out mean eci for each treatment group with 95% C.I's
summary(prev$ECI_weighted)
prev$ECI_weighted <- ifelse(is.na(prev$ECI_weighted),0,prev$ECI_weighted)
summary(prev$ECI_weighted)

saveRDS(prev,"Data/comorbidities/elixhauser_prev.rds")

library(psych)
describeBy(prev$ECI_weighted,group=prev$med_status_0)

############################################## ANALYSE COMORBIDITIES AND ECI SCORE FOR DIFFERENT GROUPS OF MEDICATIONS

# Create a new column with the combined med_cat values for those on treatment at baseline
prev <- prev %>%
  mutate(med_combo = paste(med_cat_1, med_cat_2, med_cat_3, med_cat_4,med_cat_5,med_cat_6,med_cat_7,med_cat_8,
                           sep = " + "))

summary(as.factor(prev$med_combo))

#someone may be on ARBs and CCBs, but depending on which one is prescribed first, will either appear
#as ARBs/CCBs or CCBs/ARBs. Can we combine these 2 options together?

combine_meds <- function(string) {
  meds <- strsplit(string, " \\+ ")[[1]]  # Split the string into separate medication types
  
  combined_meds <- sort(meds)  # Sort the medication types alphabetically
  
  combined_string <- paste(combined_meds, collapse = " + ")  # Combine the medication types into a single string
  
  return(combined_string)
}
# Apply the helper function to the medication_column and create a new combined_medication_column
prev <- prev %>%
  mutate(med_combo = sapply(med_combo, combine_meds))

prev$med_combo <- as.factor(prev$med_combo)

#remove NA strings from med_combo
prev$med_combo <- gsub("NA","",prev$med_combo)

# Remove "+ + " strings from med_combo
prev$med_combo <- sub("[^A-Za-z]+$", "", prev$med_combo)

#save prev dataset as final dataset
saveRDS(prev,"baseline_models/baseline_data.rds")
# Create a table of med_combo factor levels and their counts
med_combo_table <- table(prev$med_combo)

# Sort the table in descending order
sorted_table <- sort(med_combo_table, decreasing = TRUE)

View(sorted_table)
#find number of comorbidities for each category of med combo
table <- as.data.frame(table(prev$n_comorbid_cat_no_hypert,prev$med_combo))

  
write_csv(table,"Data/comorbidities/n_comorbid_no_hypert_med_combo.csv")

##produce summary stats of ECI_weighted for each med combo
summary_stats <- describeBy(prev$ECI_weighted,group=prev$med_combo)

View(summary_stats)

#create function to merge all med_combo summary stats together
interleave <- function(l, how = c('cbind', 'rbind')) {
  how <- match.arg(how)
  if (how %in% 'rbind')
    do.call(how, l)[order(sequence(sapply(l, nrow))), ]
  else do.call(how, l)[, order(sequence(sapply(l, ncol))), ]
}

#merge all summary_stats together
test <- interleave(summary_stats, 'r')

View(test)

#remove redundant columns
test$median <- NULL
test$skew <- NULL
test$trimmed <- NULL
test$mad <- NULL
test$min <- NULL
test$max <- NULL
test$range <- NULL
test$kurtosis <- NULL

View(test)

#add 95% CI's
test$lower_ci <- test$mean - 1.96*(test$se)
test$upper_ci <- test$mean + 1.96*(test$se)

View(test)

write_csv(test,"Data/comorbidities/ECI_score_med_combos.csv")
