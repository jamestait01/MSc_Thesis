setwd("/rds/general/project/hda-22-23/live/Summer_projects/jt1019")
meds <- readRDS("Data/cases_unique_meds.rds")

#some of the extracted medications have been incorrectly classified as hypertensive meds, let's get a list of all unique
#medications in the drug name column

#Let's first remove all of the nitrates and other antianginal/vascular disease drugs

#Nitrates are coded as 020601 whereas CCBs are 020602. Other antianginal drugs are 020603
#020604 drugs are used for vascular diseases (not hypertension seemingly)

# Specify the codes to be removed
codes_to_remove <- c("020601","02.06.01", "020603","02.06.03", "020604","02.06.04")

# Remove rows with the specified codes
meds_filtered <- meds[!grepl(paste(codes_to_remove, collapse = "|"), meds$bnf_code), ]

saveRDS(meds_filtered,"Data/cases_unique_meds_final.rds")



# Extract unique medication names
medications <- unique(gsub("\\d.*", "", meds_filtered$drug_name))

write.csv(medications,"preprocessing/prescriptions/medications_list.csv")

##################################################################################
meds <- readRDS("Data/cases_unique_meds_final.rds")
med_list <- read.csv("preprocessing/prescriptions/medications_list_cleaned.csv",header=T)

#creates new column to store matched medicines
meds$matched_medicine <- NA

# Iterate over rows in meds
for (i in seq_len(nrow(meds))) {
  drug_name <- meds$drug_name[i]
  
  # Iterate over rows in med_list
  for (j in seq_len(nrow(med_list))) {
    medicine <- med_list$Medicine[j]
    
    # Check if the medicine name in med_list is found in the drug name in meds
    if (grepl(medicine, drug_name)) {
      meds$matched_medicine[i] <- medicine
      break  # Exit the inner loop once a match is found
    }
  }
}

saveRDS(meds,"Data/cases_unique_meds_final.rds")
########################################################
meds <- readRDS("Data/cases_unique_meds_final.rds")

meds$matched_medicine <- as.factor(meds$matched_medicine)
summary(meds$matched_medicine)

med_counts <- data.frame(summary(meds$matched_medicine))

#makes vector of drugs identified as non-hypertensive treatments

drugs_to_investigate <- c("Nimodipine ","Atenix ","Burinex ","Torem ","DIUMIDE K-P ","Univer ","flunarizine (roi) capsules ",
                          "Acetazolamide ","ADALAT ","Aldactone ","Co-amilofruse ","Aldactide ","Bumetanide ",
                          "Cardicor ","Entresto ","Sacubitril And Valsartan  Tablets ","Diamox ","Eytazox ",
                          "Timolol ", "Alfuzosin ", "Combodart ", "Diffundox XL ", "Doralese Tiltab ", "Flomax MR ",
                          "Indoramin ", "Kelanu XL ", "Omnic MR ", "Pamsvax XL ", "Prosurin XL ", "Stronazon ",
                          "Tabphyn MR ", "Tamsulosin ", "Vesomni ", "Cialis ", "Sildenafil ", "Beta-Cardone Tablets ",
                          "Betaloc ", "Sotalol ", "Tildiem ", "Optil ", "Slofedipine XL ", "Zufal XL ","Hydrosaluric ")

library(dplyr)

#counts no. of each matched medicine 
med_counts <-meds %>%
  group_by(matched_medicine) %>%
  summarise(no_rows=length(matched_medicine))

#selects the non-hypertensive medications from med_list
med_list_bad <- med_list[complete.cases(med_list[, 2]) & nchar(med_list[, 2]) > 0, ]

#merges med_list_bad and test to produce df with counts of observations with each non-hypertensive med
merged_df <- merge(med_list_bad,test,by.x="Medicine",by.y="matched_medicine",all.x=T)

levels(meds$matched_medicine)
#counts no. of unique ids taking each of the non-hypertensive medications
meds %>%
  filter(matched_medicine == "Tamsulosin ") %>%
  summarize(num_unique_ids = n_distinct(eid))

#8204 unique IDs are taking Tamsulosin
meds %>%
  filter(matched_medicine == "Sildenafil ") %>%
  summarize(num_unique_ids = n_distinct(eid))
#7099 people are taking sidenafil
meds %>%
  filter(matched_medicine == "Alfuzosin ") %>%
  summarize(num_unique_ids = n_distinct(eid))
#1710 people are taking Alfuzosin

# Create a new column to store the unique counts
merged_df$unique_eids <- NA

# Iterate through each medication
for (medication in drugs_to_investigate) {
  # Filter the dataframe and calculate the number of unique IDs
  num_unique_ids <- meds %>%
    filter(matched_medicine == medication) %>%
    summarize(num_unique_ids = n_distinct(eid))
  
  # Update the corresponding rows in the new column
  merged_df[merged_df$Medicine == medication, "unique_eids"] <- num_unique_ids$num_unique_ids
}

#remove rows in the meds dataframe that contain the drugs_to_investigate

subset_meds <- meds[!(meds$matched_medicine %in% drugs_to_investigate), ]
n_distinct(subset_meds$eid)

#drop the now removed medications from matched_medicine factor
subset_meds$matched_medicine <- droplevels(subset_meds$matched_medicine)

saveRDS(subset_meds,"Data/cases_unique_meds_final.rds")

###############################################################################
meds <- readRDS("Data/cases_unique_meds_final.rds")

#count number of prescriptions for each medicine
n_meds <- data.frame(table(meds$matched_medicine))

#rename medicine column to matched_medicine instead of Var1
n_meds <- rename(n_meds, matched_medicine = Var1)

# Count the number of unique eids for each medication in meds dataframe
unique_eids <- aggregate(eid ~ matched_medicine, data = meds, 
                         FUN = function(x) length(unique(x)))

# Merge the two data frames based on the medication name
n_meds <- merge(n_meds, unique_eids, by = "matched_medicine")

#see how many medications each eid is on

# Calculate the number of medications for each eid
eid_med_counts <- aggregate(matched_medicine ~ eid, data = meds, FUN = function(x) length(unique(x)))

# Count the frequency of medication counts
med_count_freq <- table(eid_med_counts$matched_medicine)

med_count_freq <- rename(med_count_freq,number_of_meds = Var1)

#convert issue_date to date

meds$issue_date <- as.Date(meds$issue_date,format = "%d/%m/%Y")

#saveRDS(meds,"Data/cases_unique_meds_final.rds")

###########################################dosage cleaning###############################

meds <- readRDS("Data/cases_unique_meds_final.rds")

#convert rows with prescription dates in 1902 or 2037 to NA
meds$issue_date[format(meds$issue_date, "%Y") == "1902"] <- NA
meds$issue_date[format(meds$issue_date, "%Y") == "2037"] <- NA
sum(is.na(meds$issue_date))

#count number of each type of quantity
quantity_counts <- data.frame(summary(as.factor(meds$quantity)))

#count number of each type of medicine
drug_counts <- data.frame(summary(as.factor(meds$drug_name)))

#create copy of medication columns to experiment with code
meds_test <- meds

#remove alphabetical letters from quantity column, keeping only numbers and symbols
#meds_test$quantity <- gsub("[A-Za-z]", "", meds_test$quantity)

#make new columns for first word in drug name column
meds_test$drug_name <- as.character(meds_test$drug_name)
# Splitting drug names into separate words
words <- strsplit(meds_test$drug_name, " ")

# Creating new column for first word (drug name)
meds_test$first_word <- sapply(words, function(x) x[1])

#removed matched_medicine column as redundant
meds_test$matched_medicine <- NULL

# Extracting dosage and unit using regular expressions
dosage <- gsub(".*?([0-9.]+)\\s*([a-zA-Z]+).*", "\\1", meds_test$drug_name)
unit <- gsub(".*?([0-9.]+)\\s*([a-zA-Z]+).*", "\\2", meds_test$drug_name)

# Adding dosage and unit as new columns
meds_test$dosage <- dosage
meds_test$unit <- unit

# Count the number of rows without an integer in the dosage column
contains_integer <- function(text) {
  result <- grepl("\\d", text)
  return(!result)
}
sum(sapply(meds_test$dosage, contains_integer))
#796 rows without an integer in the dosage column

#remove these rows
# Subsetting to remove rows with non-integer values in the dosage column
meds_test <- meds_test[!sapply(meds_test$dosage, contains_integer), ]

#counts no. of rows with alphabetical letters in dosage column
sum(grepl("[a-zA-Z]", meds_test$dosage))

#remove these rows
meds_test <- meds_test[!grepl("[a-zA-Z]", meds_test$dosage), ]

#check different units in unit column
summary(as.factor(meds_test$unit))

#subset subset_df into smaller dfs based on units
#subset_df <- meds_test
#capsules <- subset_df[subset_df$unit=='capsules',]
#MG <- subset_df[subset_df$unit=='MG',]
#tablets <- subset_df[subset_df$unit=='tablets',]
#hour <- subset_df[subset_df$unit=='hour',]
#XL <- subset_df[subset_df$unit=='XL',]

#summary(as.factor(capsules$first_word))

#All capsules are Angitil/Dilzem -> convert to mg
meds_test$unit <- ifelse(meds_test$unit=="capsules","mg",meds_test$unit)
meds_test$unit <- ifelse(meds_test$unit=="Capsules","mg",meds_test$unit)
meds_test$unit <- ifelse(meds_test$unit=="CAPSULES","mg",meds_test$unit)


#MG = mg -> convert to mg
meds_test$unit <- ifelse(meds_test$unit=="MG","mg",meds_test$unit)
meds_test$unit <- ifelse(meds_test$unit=="MGM","mg",meds_test$unit)
meds_test$unit <- ifelse(meds_test$unit=="mgs","mg",meds_test$unit)
meds_test$unit <- ifelse(meds_test$unit=="MGS","mg",meds_test$unit)
meds_test$unit <- ifelse(meds_test$unit=="MGTAB","mg",meds_test$unit)

#convert all micrograms to same category
meds_test$unit <- ifelse(meds_test$unit=="micrograms","microgram",meds_test$unit)
meds_test$unit <- ifelse(meds_test$unit=="MICROGRAMS","microgram",meds_test$unit)




summary(as.factor(tablets$first_word))
#most tablets are from adalat or Adipine -> convert to mg

meds_test$unit <- ifelse(meds_test$unit=="Tablet","mg",meds_test$unit)
meds_test$unit <- ifelse(meds_test$unit=="tablet","mg",meds_test$unit)
meds_test$unit <- ifelse(meds_test$unit=="TABLET","mg",meds_test$unit)
meds_test$unit <- ifelse(meds_test$unit=="tablets","mg",meds_test$unit)
meds_test$unit <- ifelse(meds_test$unit=="Tablets","mg",meds_test$unit)
meds_test$unit <- ifelse(meds_test$unit=="TABLETS","mg",meds_test$unit)
meds_test$unit <- ifelse(meds_test$unit=="tabs","mg",meds_test$unit)
meds_test$unit <- ifelse(meds_test$unit=="TABS","mg",meds_test$unit)

saveRDS(meds_test,"Data/meds_test.rds")
############################################################################################################

meds_test <- readRDS("Data/meds_test.rds") 

hour <- meds_test[meds_test$unit =="hour",]
#check hour units
View(hour)
summary(as.factor(hour$first_word))
#hour units are all diltiazem and nifedipine, need to clean this a bit more.
summary(as.factor(hour$drug_name))

#convert 12 hour dosages into 24 hour dosages
meds_test$dosage <- ifelse(meds_test$drug_name=="diltiazem 12 hour modified release capsules 120mg",
                           "240", meds_test$dosage)

meds_test$dosage <- ifelse(meds_test$drug_name=="diltiazem 12 hour modified release capsules 180mg",
                           "360", meds_test$dosage)

meds_test$dosage <- ifelse(meds_test$drug_name=="diltiazem 24 hour modified release capsules 120mg",
                           "120", meds_test$dosage)

meds_test$dosage <- ifelse(meds_test$drug_name=="diltiazem 24 hour modified release capsules 180mg",
                           "180", meds_test$dosage)

meds_test$dosage <- ifelse(meds_test$drug_name=="diltiazem 24 hour modified release capsules 300mg",
                           "300", meds_test$dosage)

meds_test$dosage <- ifelse(meds_test$drug_name=="nifedipine 12 hour modified release capsules 20mg",
                           "40", meds_test$dosage)

meds_test$dosage <- ifelse(meds_test$drug_name=="nifedipine 24 hour modified release capsules 20mg",
                           "20", meds_test$dosage)

#convert hours units to mg
meds_test$unit <- ifelse(meds_test$unit=="hour","mg",meds_test$unit)

meds_test$dosage <- as.numeric(meds_test$dosage)

summary(as.factor(meds_test$unit))

View(XL)
summary(as.factor(XL$first_word))
#everyone in XL is Zemtard, all their prescriptions are in mg, convert XL to mg
meds_test$unit <- ifelse(meds_test$unit=="XL",
                           "mg", meds_test$unit)

summary(as.factor(meds_test$unit))

#572320 people have mg, 8983 have micrograms, remove the other units

meds_final <- subset(meds_test, unit %in% c("mg", "microgram"))

#convert microgram to mg
meds_final$dosage <- ifelse(meds_final$unit=="microgram",meds_final$dosage/1000,meds_final$dosage)

meds_final$unit <- ifelse(meds_final$unit=="microgram","mg",meds_final$unit)
summary(as.factor(meds_final$unit))

#I've cleaned the units and the dosages, now I just need to clean the quantity, or is that really necessary?
#I don't see how we'd need to use the quantity column... Let's not clean it for now.

saveRDS(meds_final,"Data/cases_unique_meds_final.rds")

#but what about the actual prescription someone is on? How to clean that?

meds <- readRDS("Data/cases_unique_meds_final.rds")

#create test dataset with specific columns to figure out what to do with daily dosages
test <- meds[,c("eid","issue_date","drug_name","quantity","med_cat","first_word","dosage","unit")]

#order rows for each person from oldest to newest prescriptions
# Order rows within each ID based on the Date column
test <- test %>% 
  arrange(eid, issue_date)

