
#load baseline data
baseline_data <- readRDS("~/Summer_project/baseline_models/model_1/baseline_data_NICE.rds")

baseline_data2 <- subset(baseline_data,select=c(eid,date_recr,date_death,`Medication combination`))
baseline_data2$Time <- 0
#load medication data
meds<- readRDS("~/Summer_project/Data/cases_meds_final.rds")

#keep meds of eids in our dataset
meds <- meds[meds$eid %in% baseline_data$eid,]

#we have baseline medications for each person, let's only keep medications that are beyond the baseline for each person
date_data <- subset(baseline_data,select=c(eid,date_recr,issue_date))

#set the baseline date to either be the baseline prescription date or recruitment date for each person
library(dplyr)
date_data$meds_after_date <- if_else(is.na(date_data$issue_date), date_data$date_recr, date_data$issue_date)

#merge the dfs together
merged_df <- inner_join(meds, date_data, by = "eid")

merged_df$date_recr.y <- NULL
merged_df$issue_date.y <- NULL
#filter to only keep medications that are prescribed after the meds_after_date (baseline meds)
merged_df <- merged_df %>%
  filter(issue_date.x > meds_after_date)

#make new variable indicating time since baseline for each prescription
merged_df$Time <- as.numeric(merged_df$issue_date.x - merged_df$meds_after_date)


merged_df2 <- subset(merged_df,select=c(eid,date_recr.x,date_death,med_cat,Time))

merged_df2 <- merged_df2 %>% 
  rename(
  date_recr = date_recr.x, `Medication combination` = med_cat)

merged_df2 <- rbind(merged_df2,baseline_data2)

merged_df2 <- merged_df2 %>%
  arrange(eid,Time)

saveRDS(merged_df2,"~/Summer_project/baseline_models/sankey/merged_df.rds")



