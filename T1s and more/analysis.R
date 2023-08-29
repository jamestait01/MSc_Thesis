data <- readRDS("~/Documents/MSc/Thesis2/baseline_models/model_1/baseline_data_NICE.rds")

#sum the total number of occurrences for each comorbidity 
sums <- colSums(data[,93:123],na.rm=T)

# Create a new dataframe containing the number of occurrences for each comorbidity 
comorbid_counts <- data.frame(Column = names(sums), Sum = sums)

View(comorbid_counts)

library(plyr)
#map shorthand comorbidity strings to full words
comorbid_counts$Column <- mapvalues(comorbid_counts$Column, 
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

#look individually at each medication combination

#Create a new dataframe for grouping by medication

comorbid <- data[,c(93:123,129)]
medication_grouped <- aggregate(. ~ `Medication combination`, data = comorbid, FUN = sum, na.rm = TRUE)

# Print the grouped sums
View(medication_grouped)

# Load the necessary library
library(dplyr)

medication_grouped <- medication_grouped %>%
  rename("AIDS/HIV" = "aids", "Alcohol_abuse" = "alcohol", "Blood_loss_anaemia" = "blane",
         "Cardiac_arrhythmias" = "carit", "Congestive_heart_failure" = "chf",
         "Coagulopathy" = "coag", "Chronic_pulmonary_disease" = "cpd",
         "Deficiency_anaemia" = "dane", "Depression" = "depre",
         "Diabetes_complicated" = "diabc", "Diabetes_uncomplicated" = "diabunc",
         "Drug_abuse" = "drug", "Fluid_and_electrolyte_disorders" = "fed",
         "Hypertension_complicated" = "hypc", "Hypothyroidism" = "hypothy",
         "Hypertension_uncomplicated" = "hypunc", "Liver_disease" = "ld",
         "Lymphoma" = "lymph", "Metastatic_cancer" = "metacanc",
         "Obesity" = "obes", "Other_neurological_disorders" = "ond",
         "Paralysis" = "para", "Pulmonary_circulation_disorders" = "pcd",
         "Psychoses" = "psycho", "Peptic_ulcer_disease" = "pud",
         "Peripheral_vascular_disease" = "pvd", "Renal_failure" = "rf",
         "Rheumatoid_arthritis" = "rheumd", "Solid_tumour" = "solidtum",
         "Valvular_heart_disease" = "valv", "Weight_loss" = "wloss")

comorbid_counts$Column <- gsub("_", " ", comorbid_counts$Column)


comorbid_counts <- comorbid_counts %>%
  rename(Comorbidity = Column)

comorbid_counts <- comorbid_counts %>%
  arrange(desc(Sum))

comorbid_counts$`Proportion of baseline population` <- round(((comorbid_counts$Sum / 38364)*100),1)
  
write.csv(comorbid_counts,"~/Documents/MSc/Thesis3/comorbid/baseline_comorbid_counts.csv")

medication_grouped[, 2:32] <- sapply(medication_grouped[, 2:32], as.numeric)

medication_grouped$Sum <- rowSums(medication_grouped[,2:32])


#transpose df
medication_grouped <- as.data.frame(t(medication_grouped))
colnames(medication_grouped) <- medication_grouped[1,]

medication_grouped<- medication_grouped[-1,]

write.csv(medication_grouped,"~/Documents/MSc/Thesis3/comorbid/baseline_med_comorbid_counts.csv")



# Assuming your_dataframe is the name of your dataframe
group_names <- c("Untreated", "ACE inhibitors/ARBs", "ACE inhibitors/ARBs + Calcium channel blockers",
                 "ACE inhibitors/ARBs + Calcium channel blockers + Diuretics",
                 "ACE inhibitors/ARBs + Diuretics", "Calcium channel blockers",
                 "Calcium channel blockers + Diuretics")

# Print the updated dataframe with percentages
print(your_dataframe)

