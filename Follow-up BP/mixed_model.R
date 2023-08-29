setwd("Documents/MSc/Thesis3/baseline_models/")
data <- readRDS("model_2/NICE_bp_change.rds")

data$`Age group` <- cut(data$Age, breaks=c(40,55,65,Inf),labels=c("40-55","55-65","65+")
                        ,right = FALSE)


MLR_data <- subset(data,select=c(eid,Sex,`Smoking status`, Ethnicity, BMI, `Age group`, `Number of comorbidities`, 
                                 `Weekly alcohol intake`, `Index of Multiple Deprivation`,`Medication status`, 
                                 sys_bp.0,dias_bp.0,sys_first_follow_up,
                                 dias_first_follow_up,`Daily dose`,`Follow-up daily dose`,
                                  `Medication combination`,`Medication combination at follow-up`))


baseline <- subset(MLR_data,select=-c(sys_first_follow_up,dias_first_follow_up,`Follow-up daily dose`,
                                      `Medication combination at follow-up`))

baseline$`Follow-up` <- 0

fu <- subset(MLR_data,select=-c(sys_bp.0,dias_bp.0,`Daily dose`,`Medication combination`))

fu$`Follow-up` <- 1
library(dplyr)
fu <- fu %>%
  rename(sys_bp.0 = sys_first_follow_up,dias_bp.0 = dias_first_follow_up,`Daily dose` = `Follow-up daily dose`,
         `Medication combination` = `Medication combination at follow-up`)

merged_df <- rbind(baseline,fu)

View(merged_df)

levels(merged_df$`Medication combination`) <- c("ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                                                  "ACE inhibitor/ARB + CCB + Diuretic",
                                                  "ACE inhibitor/ARB + Diuretic",
                                                  "CCB", "CCB + Diuretic","Untreated")
library(lme4)

library(afex)
merged_df$`Medication combination` <- relevel(merged_df$`Medication combination`,ref="Untreated")
#fit mixed model but controlling for random effect of eid (two measurements for each person)

mixed_sys_model <- lmer(sys_bp.0 ~ Sex + `Age group` + Ethnicity + BMI + `Smoking status` + 
                          `Weekly alcohol intake` + `Index of Multiple Deprivation` + 
                          `Number of comorbidities`  + `Medication combination`*`Follow-up` + 
                          (1|eid), data = merged_df,REML=FALSE)

mixed_dias_model <- lmer(dias_bp.0 ~ Sex + `Age group` + Ethnicity + BMI + `Smoking status` + 
                          `Weekly alcohol intake` + `Index of Multiple Deprivation` + 
                          `Number of comorbidities`  + `Medication combination`*`Follow-up` + 
                          (1|eid), data = merged_df,REML=FALSE)



#sys_p_values <- m1$tTable[,5]


library(gtsummary)

sys_tbl <- tbl_regression(mixed_sys_model,exponentiate=F) 
dias_tbl <- tbl_regression(mixed_dias_model,exponentiate=F) 

#merge the 2 tables together
merged_tbl <- tbl_merge(list(sys_tbl,dias_tbl),tab_spanner = c("**Systolic blood pressure (mmHg)**",
                                                                         "**Diastolic blood pressure (mmHg)**"))

gt::gtsave(as_gt(merged_tbl),"merged_MLR_table.png",expand=40)
#save as excel file
merged_tbl %>%
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., "mixed_model.xlsx")


#make treatment_combination combining peoples meds at baseline and follow-up into 1 column
merged_df <- merged_df %>%
  group_by(eid) %>%
  mutate(treatment_combination = paste(`Medication combination`, collapse = "-->"))

#count number of people in each category
# Compute frequency using table()
frequency_table <- table(merged_df$treatment_combination)

# Convert the table to a dataframe
result_dataframe <- data.frame(
  treatment_combinations = names(frequency_table),
  count = as.vector(frequency_table)
)
#divide each count by 2 as 2 measurements for each person
result_dataframe$count <- result_dataframe$count /2

write.csv(result_dataframe,"boxplot_treatmentcat-numbers.csv")
#prepare dataset for t-test analysis
t_test_data <- subset(merged_df,select=c(eid,sys_bp.0,dias_bp.0,`Follow-up`,treatment_combination))

#filter t_test_data to only keep treatment combinations with over 100 people
t_test_data <- t_test_data %>%
  group_by(treatment_combination) %>%
  filter(n()>=200)


#split data by follow_up 

fu_0 <- t_test_data[t_test_data$`Follow-up`==0,]
fu_1 <- t_test_data[t_test_data$`Follow-up`==1,]

fu_0 <- fu_0 %>%
  rename(`Baseline systolic blood pressure` = sys_bp.0,
         `Baseline diastolic blood pressure` = dias_bp.0)

fu_1 <- fu_1 %>%
  rename(`Follow-up systolic blood pressure` = sys_bp.0,
         `Follow-up diastolic blood pressure` = dias_bp.0)

test <- cbind(fu_0,fu_1,by="eid")

test$eid...6 <- NULL
test$treatment_combination...10 <- NULL
test$by <- NULL
test$`Follow-up...4` <- NULL
test$`Follow-up...9` <- NULL

# Split the dataframe by treatment combination
split_data <- split(test, test$treatment_combination...5)

# Perform paired t-test for systolic bp, for each treatment combination
sys_t_test_results <- lapply(split_data, function(subset) {
  t_test_result <- t.test(subset$`Baseline systolic blood pressure`, subset$`Follow-up systolic blood pressure`, 
                          paired = TRUE)
  return(t_test_result)
})

dias_t_test_results <- lapply(split_data, function(subset) {
  t_test_result <- t.test(subset$`Baseline diastolic blood pressure`, subset$`Follow-up diastolic blood pressure`, 
                          paired = TRUE)
  return(t_test_result)
})

# Convert the list of t-test results to a data frame
sys_t_test_results_df <- do.call(rbind, lapply(names(sys_t_test_results), function(name) {
  result <- sys_t_test_results[[name]]
  data.frame(
    treatment_combination = name,
    p_value = result$p.value,
    mean_difference = result$estimate[1],
    t_statistic = result$statistic
  )
}))

# Convert the list of t-test results to a data frame
dias_t_test_results_df <- do.call(rbind, lapply(names(dias_t_test_results), function(name) {
  result <- dias_t_test_results[[name]]
  data.frame(
    treatment_combination = name,
    p_value = result$p.value,
    mean_difference = result$estimate[1],
    t_statistic = result$statistic
  )
}))

#add new columns to adjust the p-values by the number of tests (20)
sys_t_test_results_df$p_adjusted <- p.adjust(sys_t_test_results_df$p_value,method="bonferroni",n=20)
dias_t_test_results_df$p_adjusted <- p.adjust(dias_t_test_results_df$p_value,method="bonferroni",n=20)


#visualise bps for each treatment combination category

#filter merged_df to only keep treatment combinations with over 100 people (2 measurements)
large_n <- merged_df %>%
  group_by(treatment_combination) %>%
  filter(n()>=200)

library(ggplot2)


#calcualte mean systolic blood pressure at each point for each group
mean_sys_values <- large_n %>%
  group_by(treatment_combination, `Follow-up`) %>%
  summarize(mean_sys_bp = mean(sys_bp.0, na.rm = TRUE))

#calcualte mean diastolic blood pressure at each point for each group
mean_dias_values <- large_n %>%
  group_by(treatment_combination, `Follow-up`) %>%
  summarize(mean_dias_bp = mean(dias_bp.0, na.rm = TRUE))


#for systolic
library(dplyr)
library(ggplot2)

large_n %>%
  ggplot(aes(as.factor(`Follow-up`), sys_bp.0, col = treatment_combination)) +
  geom_boxplot(aes(group = interaction(treatment_combination, `Follow-up`))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  ggtitle("Change in systolic blood pressure from baseline\n to follow-up for different medication categories") +
  facet_wrap(~ treatment_combination, ncol = 1) +
  geom_text(data = mean_sys_values, aes(x = as.factor(`Follow-up`), y = mean_sys_bp, label = round(mean_sys_bp, 1)),
            nudge_x = 0, nudge_y = 0, size = 2, color = "black", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 8),
        strip.text = element_text(size = 6.5, face = "bold")) +
  labs(y = "Systolic blood pressure (mmHg)", x = "Follow-up", colour = "Medication\nchange") + 
theme(legend.margin=margin(-10, 0, 0, 0))+ 
  guides(color = guide_legend(nrow = 2))

#for diastolic

large_n %>%
  ggplot(aes(as.factor(`Follow-up`), dias_bp.0, col = treatment_combination)) +
  geom_boxplot(aes(group = interaction(treatment_combination, `Follow-up`))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  ggtitle("Changes in diastolic blood pressure from baseline\n to follow-up for different medication categories") +
  facet_wrap(~ treatment_combination, ncol = 5) +
  geom_text(data = mean_dias_values, aes(x = as.factor(`Follow-up`), y = mean_dias_bp, label = round(mean_dias_bp, 1)),
            nudge_x = 0, nudge_y = 0, size = 2, color = "black", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size = 8),
        strip.text = element_text(size = 6.5, face = "bold")) +
  labs(y = "Diastolic blood pressure (mmHg)", x = "Follow-up", colour = "Medication\nchange")

#make individual boxplots
# Unique treatment combinations
unique_combinations <- unique(large_n$treatment_combination)

# Define a color palette with different colors for each combination
color_palette <- rainbow(length(unique_combinations))

# Loop through each combination and create individual box plots
individual_box_plots_list <- list()

for (i in seq_along(unique_combinations)) {
  combination <- unique_combinations[i]
  filtered_data <- large_n[large_n$treatment_combination == combination, ]
  
  mean_values <- mean_dias_values[mean_dias_values$treatment_combination == combination, ]
  
  individual_box_plot <- ggplot(filtered_data, aes(as.factor(`Follow-up`), sys_bp.0)) +
    geom_boxplot(fill = color_palette[i], color = "black") +  # Use a different color for each combination
    geom_text(data = mean_values, aes(x = as.factor(`Follow-up`), y = mean_dias_bp,
                                      label = round(mean_dias_bp, 1)),
              nudge_x = 0, nudge_y = 0, size = 3, color = "black", fontface = "bold") +
    theme_bw() +
    ggtitle(paste("Change in systolic blood pressure for", combination)) +
    labs(y = "Systolic blood pressure (mmHg)", x = "Follow-up")
  
  individual_box_plots_list[[combination]] <- individual_box_plot
}

# Display the individual box plots
for (combination in unique_combinations) {
  print(individual_box_plots_list[[combination]])
}
