#Goal create table of treated vs untreated at baseline

data <- readRDS("/rds/general/user/jt1019/home/Summer_project/baseline_models/model_1/baseline_data_NICE.rds")

library(tableone)

vars <- c("Sex","Age","Ethnicity", "Index of Multiple Deprivation","BMI","Smoking status",
          "Weekly alcohol intake","Number of comorbidities")

table1 <- CreateTableOne(vars,strata="Medication status",data=data,test=T)

table1 <- print(table1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

write.csv(table1,"/rds/general/user/jt1019/home/Summer_project/baseline_models/table_1s/med_status.csv")

#Goal: Create Table 1s of medication combos at baseline (ACE inhibitors/ARBs, CCBs, etc)

table2 <- CreateTableOne(vars,strata="Medication combination",data=data,test=F)

table2 <- print(table2,quote=F,noSpaces=T,printToggle=F)

write.csv(table2,"/rds/general/user/jt1019/home/Summer_project/baseline_models/table_1s/med_combos.csv")

#Do another table 1 but categorising people into different age groups...
#40,50,50,60,60+
data$`Age group` <- cut(data$Age, breaks=c(45,55,65,Inf),labels=c("45-55","55-65","65+"), right = FALSE)

table3 <- CreateTableOne(vars,strata=c("Medication combination","Age group"),data=data,test=F)

table3 <- print(table3,quote=F,noSpaces=T,printToggle=F)

write.csv(table3,"/rds/general/user/jt1019/home/Summer_project/baseline_models/table_1s/med_combos_age_groups.csv")
