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
#Let's look at people's medications at baseline
#we can then look at the number of hospitalisations and then the number of cardiac-specific hospitalisations
#for each person. 
n_hospitalisations_unique <- merged_df_unique %>%
  group_by(eid) %>%
  count()

#only keep hypertension-related hospitalisations
cardiac_hosp_unique <- merged_df_unique[grepl(paste0("^(", paste(myocardial_codes, collapse = "|"), ")"), merged_df_unique$icd10), ]

n_cardiac_unique <- cardiac_hosp_unique %>%
  group_by(eid) %>%
  count()

n_hospitalisations_unique <- n_hospitalisations_unique %>%
  rename(n_hospitalisations_unique = n)

n_cardiac_unique <- n_cardiac_unique %>%
  rename(n_cardiac_unique = n)


merged_df <- merge(data,n_hospitalisations_unique,by="eid",all.x=T)

merged_df$n_hospitalisations_unique <- ifelse(is.na(merged_df$n_hospitalisations_unique),0,merged_df$n_hospitalisations_unique)

merged_df <- merge(merged_df,n_cardiac_unique,by="eid",all.x=T)

merged_df$n_cardiac_unique <- ifelse(is.na(merged_df$n_cardiac_unique),0,merged_df$n_cardiac_unique)

saveRDS(merged_df,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3a/merged_df_baseline.rds")

