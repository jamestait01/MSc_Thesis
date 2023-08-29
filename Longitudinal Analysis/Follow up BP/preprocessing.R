data <- readRDS("~/Summer_project/baseline_models/final_data.rds")

#remove prev_treated people

data <- data[data$med_status_0!="prev_treated_at_recr",]

data$med_status_0 <- ifelse(data$med_status_0=="treated_at_recr","Treated","Untreated")

data$med_status_0 <- as.factor(data$med_status_0)

#group physical active days into 0, 1-3 and >=4

library(dplyr)

data$days_mod_phys_activ.0.0 <- as.character(data$days_mod_phys_activ.0.0)
data$days_vig_phys_activ.0.0 <- as.character(data$days_vig_phys_activ.0.0)


data$days_mod_phys_activ.0.0 <- ifelse(data$days_mod_phys_activ.0.0 =="1" | 
                                                        data$days_mod_phys_activ.0.0 =="2" |
                                                        data$days_mod_phys_activ.0.0 =="3","1-3",
                                                      data$days_mod_phys_activ.0.0)

data$days_mod_phys_activ.0.0 <- ifelse(data$days_mod_phys_activ.0.0 =="4" | 
                                                        data$days_mod_phys_activ.0.0 =="5" |
                                                        data$days_mod_phys_activ.0.0 =="6" |
                                                        data$days_mod_phys_activ.0.0 =="7",">=4",
                                                      data$days_mod_phys_activ.0.0)

data$days_vig_phys_activ.0.0 <- ifelse(data$days_vig_phys_activ.0.0 =="1" | 
                                                        data$days_vig_phys_activ.0.0 =="2" |
                                                        data$days_vig_phys_activ.0.0 =="3","1-3",
                                                      data$days_vig_phys_activ.0.0)

data$days_vig_phys_activ.0.0 <- ifelse(data$days_vig_phys_activ.0.0 =="4" | 
                                                        data$days_vig_phys_activ.0.0 =="5" |
                                                        data$days_vig_phys_activ.0.0 =="6" |
                                                        data$days_vig_phys_activ.0.0 =="7",">=4",
                                                      data$days_vig_phys_activ.0.0)


data$days_mod_phys_activ.0.0 <- as.factor(data$days_mod_phys_activ.0.0)
data$days_vig_phys_activ.0.0 <- as.factor(data$days_vig_phys_activ.0.0)

library(dplyr)
#rename variables
data <- data %>%
  rename(
    Sex = sex.0.0,
    `Smoking status` = smoking_status.0.0,
    Ethnicity = ethnicity.0.0,
    BMI = bmi.0.0,
    Age = age_recr.0.0,
    `Number of comorbidities` = n_comorbid_cat_no_hypert,
    `Days of vigorous physical activity` = days_vig_phys_activ.0.0,
    `Days of moderate physical activity` = days_mod_phys_activ.0.0,
    `Weekly alcohol intake` = alc_intake.0.0,
    `Index of Multiple Deprivation` = idx_multdep,
    Education = education,
    `Medication status` = med_status_0,
    `Medication combination` = med_combo)


#relevel relevant factors model
data$`Medication status` <- relevel(data$`Medication status`,ref="Untreated")
data$`Smoking status` <- relevel(data$`Smoking status`,ref="Never")
data$`Weekly alcohol intake` <- relevel(data$`Weekly alcohol intake`,ref="Never")
data$Education <- relevel(data$Education,ref="College/university degree")
data$`Days of moderate physical activity` <- relevel(data$`Days of moderate physical activity`,ref="0")
data$`Days of vigorous physical activity` <- relevel(data$`Days of vigorous physical activity`,ref="0")
data$`Medication combination` <- relevel(data$`Medication combination`,ref="Untreated")

#for each person, we want to keep their first-follow up measurement

data$sys_first_follow_up <- ifelse(!is.na(data$sys_bp.1),data$sys_bp.1,data$sys_bp.2)

data$dias_first_follow_up <- ifelse(!is.na(data$dias_bp.1),data$dias_bp.1,data$dias_bp.2)


saveRDS(data,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_2/data.rds")

#only keep those with a follow_up bp measurement

bp_change_df <- data[!is.na(data$sys_first_follow_up),]

saveRDS(bp_change_df,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_2/bp_change_data.rds")

#create 2 separate dfs based on whether we take someone's first or second follow-up measurement

bp_change_df <- readRDS("/rds/general/user/jt1019/home/Summer_project/baseline_models/model_2/bp_change_data.rds")

# Create a logical condition to identify rows where sys_bp.2 has a value but sys_bp.1 is missing
condition <- !is.na(bp_change_df$sys_bp.2) & is.na(bp_change_df$sys_bp.1)

fu_1 <- bp_change_df[!condition,]

fu_2 <- bp_change_df[condition,]

fu_1 <- fu_1[,c(1:226,276:277)]

fu_2 <- fu_2[, c(1:161, 227:277)]

fu_colnames <- colnames(fu_1[,162:225])
#create new follow up colnames 
new_colnames <- gsub("follow_up1_", "follow_up_", fu_colnames)

cols <- 162:225
colnames(fu_1)[cols] <- new_colnames

library(dplyr)

fu_1 <- fu_1 %>%
  rename(`Medication combination at follow-up` = med_combo_1)

#repeat the above for fu_2

fu_colnames <- colnames(fu_2[,162:209])

#create new follow up colnames 
new_colnames <- gsub("follow_up2_", "follow_up_", fu_colnames)

cols <- 162:209
colnames(fu_2)[cols] <- new_colnames

fu_2 <- fu_2 %>%
  rename(`Medication combination at follow-up` = med_combo_2)

#calculate follow_up time for each person taking the average follow_up date, 1st January 2013 for fu_1 and
#1st January 2016 for fu_2

fu_1$`Follow-up date` <- as.Date("2013-01-01")
fu_1$`Follow-up time` <- as.numeric(fu_1$`Follow-up date` - fu_1$date_recr)
fu_2$`Follow-up date` <- as.Date("2016-01-01")
fu_2$`Follow-up time` <- as.numeric(fu_2$`Follow-up date` - fu_2$date_recr)

df <- merge(fu_1,fu_2,all.x=T,all.y=T)

df$`Medication combination` <- as.character(df$`Medication combination`)
df$`Medication combination at follow-up` <- as.character(df$`Medication combination at follow-up`)

#only keep those on treatment that fall within NICE guidelines 
model_NICE_data <- df[df$`Medication combination`=="Untreated" |
                                         df$`Medication combination`=="ACE inhibitors/ARBs" |
                                         df$`Medication combination`=="Calcium channel blockers" |
                                         df$`Medication combination`=="ACE inhibitors/ARBs + Diuretics" |
                                         df$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers" |
                                         df$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics" |
                                         df$`Medication combination`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics + B blockers" |
                                         df$`Medication combination`=="Calcium channel blockers + Diuretics",
]

#untreated people have "", replace with "Untreated"
model_NICE_data$`Medication combination at follow-up` <- ifelse(model_NICE_data$`Medication combination at follow-up`=="","Untreated",model_NICE_data$`Medication combination at follow-up`)

model_NICE_data <- model_NICE_data[model_NICE_data$`Medication combination at follow-up`=="Untreated" |
                        model_NICE_data$`Medication combination at follow-up`=="ACE inhibitors/ARBs" |
                        model_NICE_data$`Medication combination at follow-up`=="Calcium channel blockers" |
                        model_NICE_data$`Medication combination at follow-up`=="ACE inhibitors/ARBs + Diuretics" |
                        model_NICE_data$`Medication combination at follow-up`=="ACE inhibitors/ARBs + Calcium channel blockers" |
                        model_NICE_data$`Medication combination at follow-up`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics" |
                        model_NICE_data$`Medication combination at follow-up`=="ACE inhibitors/ARBs + Calcium channel blockers + Diuretics + B blockers" |
                        model_NICE_data$`Medication combination at follow-up`=="Calcium channel blockers + Diuretics",
]

model_NICE_data$`Medication combination` <- as.factor(model_NICE_data$`Medication combination`)
model_NICE_data$`Medication combination at follow-up` <- as.factor(model_NICE_data$`Medication combination at follow-up`)

#drop redundant factor levels
model_NICE_data$`Medication combination` <- droplevels(model_NICE_data$`Medication combination`)
model_NICE_data$`Medication combination at follow-up` <- droplevels(model_NICE_data$`Medication combination at follow-up`)

model_NICE_data$sys_change = model_NICE_data$sys_first_follow_up - model_NICE_data$sys_bp.0
model_NICE_data$dias_change = model_NICE_data$dias_first_follow_up - model_NICE_data$dias_bp.0

saveRDS(model_NICE_data,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_2/NICE_bp_change.rds")

########################################################################################

#make dummy variables indicating who started medications, who ended them, who increased dosage etc...
#make new variable to see who's started medications
model_NICE_data$`Started medication` <- as.numeric((model_NICE_data$`Medication combination` == "Untreated" & !(model_NICE_data$`Medication combination at follow-up`=="Untreated")))

#make new variable to see who stopped medications
model_NICE_data$`Ended medication` <- as.numeric((!(model_NICE_data$`Medication combination` == "Untreated") & model_NICE_data$`Medication combination at follow-up`=="Untreated"))

#make new variable to see who changed medications
model_NICE_data$`Changed medication` <- as.numeric(as.character(model_NICE_data$`Medication combination`)!= as.character(model_NICE_data$`Medication combination at follow-up`) & model_NICE_data$`Medication combination`!="Untreated" & model_NICE_data$`Medication combination at follow-up`!="Untreated")

                                  
#make new variable to see who was still untreated
model_NICE_data$`Stayed untreated` <- as.numeric(model_NICE_data$`Medication combination` == "Untreated" & model_NICE_data$`Medication combination at follow-up`=="Untreated")

library(dplyr)

#rename daily dosage
model_NICE_data <- model_NICE_data %>%
  rename(`Daily dose` = daily_dose.y)

#remove the columns filled with NA's (they were created when merging fu_1 and fu_2)
model_NICE_data[,215:230] <- NULL

#make follow-up daily dose column
model_NICE_data <- model_NICE_data %>%
  mutate(`Follow-up daily dose` = rowSums(select(., starts_with("dosage_follow_up_")),na.rm=TRUE))

#make variable to see who stayed on the same medications at the same dosage
model_NICE_data$`Stable medications` <- as.numeric(as.character(model_NICE_data$`Medication combination`) == as.character(model_NICE_data$`Medication combination at follow-up`) & model_NICE_data$`Medication combination`!="Untreated" 
                                                   & model_NICE_data$`Daily dose` == model_NICE_data$`Follow-up daily dose`)

#make column to see who increased their dosage but stayed on the same medications
model_NICE_data$`Increased dosage` <- ifelse(as.character(model_NICE_data$`Medication combination`) == model_NICE_data$`Medication combination at follow-up` & model_NICE_data$`Daily dose` < model_NICE_data$`Follow-up daily dose` ,1,0)

#make column to see who decreased their dosage but stayed on the same medications
model_NICE_data$`Decreased dosage` <- ifelse(as.character(model_NICE_data$`Medication combination`) == model_NICE_data$`Medication combination at follow-up` & model_NICE_data$`Daily dose` > model_NICE_data$`Follow-up daily dose` ,1,0)

#Make new column identifying which medication status people belong to.
model_NICE_data <- model_NICE_data %>%
  rename(med_stat = `Medication status`)

model_NICE_data$`Medication status` <- ifelse(model_NICE_data$`Started medication` == 1, "Started medication",
                                   ifelse(model_NICE_data$`Ended medication` == 1, "Ended medication",
                                          ifelse(model_NICE_data$`Changed medication` == 1, "Changed medications",
                                                 ifelse(model_NICE_data$`Stayed untreated` == 1, "Stayed untreated",
                                                        ifelse(model_NICE_data$`Increased dosage` == 1, "Increased dosage",
                                                               ifelse(model_NICE_data$`Stable medications` == 1, "Stable medications",
                                                                      "Decreased dosage"))))))

model_NICE_data$`Medication status` <- as.factor(model_NICE_data$`Medication status`)
model_NICE_data$`Medication status` <- relevel(model_NICE_data$`Medication status`,ref="Stable medications")

saveRDS(model_NICE_data,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_2/NICE_bp_change.rds")

####################################### FOCUS ON THOSE WHO CHANGED MEDS###############################################################
data <- readRDS("/rds/general/user/jt1019/home/Summer_project/baseline_models/model_2/NICE_bp_change.rds")

changed_meds <- data[data$`Medication status`=="Changed medications",]
saveRDS(changed_meds,"/rds/general/user/jt1019/home/Summer_project/baseline_models/model_2/NICE_changed_meds.rds")


