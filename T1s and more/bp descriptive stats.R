data <- readRDS("~/Documents/MSc/Thesis3/baseline_models/model_1/baseline_data_NICE.rds")


library(dplyr)

#make table for mean and sd of sys and dias bp for each group at baseline
#and find proportion of people with controlled hypertension in each group (<140/90)

table <- data %>%
  group_by(`Medication combination`) %>%
  summarize(mean_sys_bp = mean(sys_bp.0),sys_bp_sd = sd(sys_bp.0),
            mean_dias_bp = mean(dias_bp.0),dias_bp_sd = sd(dias_bp.0),
            n = n(),n_controlled = sum(sys_bp.0<140 & dias_bp.0<90))

table$n_controlled_percent <- (table$n_controlled / table$n)*100

write.csv(table,"~/Documents/MSc/Thesis3/baseline_models/model_1/bp_descriptive.csv")

