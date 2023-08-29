setwd("Summer_project/Data/")

selfrep <- readRDS("self_rep_vs_icd/output_final_selfrep.rds")

icd <- readRDS("self_rep_vs_icd/output_final_icd.rds")

library(dplyr)

#only keep prevalent cases
selfrep <- selfrep %>%
  filter(prevalent_case==1)

icd <- icd %>%
  filter(prevalent_case==1)

selfrep_eids <- selfrep$eid

icd_eids <- icd$eid

#check how many self-reported hypertension cases are also reported through icd9/icd10
summary(selfrep_eids %in% icd_eids)

#only keep eids of people that had only self-reported hypertension (n=39,902 people overall)
selfrep_only <- selfrep[!(selfrep_eids %in% icd_eids),]

#load the dataframe of all prevalent cases used for our analysis (n=73,735)
prevalent_base <- readRDS("prevalent_baseline_analysis/prevalent_base.rds")

#how many of these 39,902 people are contained in the n=73,735?

summary(selfrep_only$eid %in% prevalent_base$eid)
#11,500 out of 73,735 prevalent cases had only self-reported their hypertension at baseline (not clinically diagnosed)

#let's look at how many of these people had antihypertensive prescriptions before they were clinically diagnosed

selfrep_only <- selfrep_only[selfrep_only$eid %in% prevalent_base$eid,]

meds_before_diag <- readRDS("prevalent_baseline_analysis/meds_before_prevalent_diagnosis.rds")

summary(selfrep_only$eid %in% meds_before_diag$eid)
#7,873 of the 22,740 people with meds before diagnosis had only self-reported their hypertension 

#how many rows does these people account for?
test <- meds_before_diag[meds_before_diag$eid %in% selfrep_only$eid,]
nrow(test)/nrow(meds_before_diag)
#64% of the meds people have before diagnosis is for those who only self-reported.

#for people that only self-reported their hypertension at baseline, for those that are untreated, is there a 
#difference in their blood pressure measurements at baseline?

nomeds <- readRDS("baseline_analysis/prevalent_base_nomeds.rds")

#subset people on no meds at baseline to only include those that only self-reported their hypertension
nomeds_selfrep <- nomeds[nomeds$eid %in% selfrep_only$eid,]

nomeds_icd <- nomeds[!(nomeds$eid %in% selfrep_only$eid),]

#produce table 1s
library(tableone)

vars <- c("case","sex.0.0","smoking_status.0.0","pack_years.0.0", "ethnicity.0.0", "bmi.0.0","age_recr.0.0",
          "sys_bp.0", "dias_bp.0")

nomeds_selfrep_t1 <- CreateTableOne(vars,strata="sex.0.0",data=nomeds_selfrep,includeNA=TRUE,test=F)

nomeds_selfrep_t1 <- print(nomeds_selfrep_t1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(nomeds_selfrep_t1, file = "self_rep_vs_icd/nomeds_selfrep_t1_baseline.csv")

nomeds_icd_t1 <- CreateTableOne(vars,strata="sex.0.0",data=nomeds_icd,includeNA=TRUE,test=F)

nomeds_icd_t1 <- print(nomeds_icd_t1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(nomeds_icd_t1, file = "self_rep_vs_icd/nomeds_icd_t1_baseline.csv")

saveRDS(nomeds_selfrep,"self_rep_vs_icd/nomeds_selfrep.rds")
saveRDS(nomeds_icd,"self_rep_vs_icd/nomeds_icd.rds")

#now let's see how many people with recent meds (n=34,463) had only self-reported their hypertension

recentmeds <- readRDS("prevalent_baseline_analysis/recentmedsfinal.rds")

summary(selfrep_only$eid %in% recentmeds$eid)
#7,787 of these only self-reported people were on antihypertensive meds at enrolment

recentmeds_selfrep <- recentmeds[recentmeds$eid %in% selfrep_only$eid,]

#keep one row for each person for table 1s
recentmeds_selfrep_unique <- recentmeds_selfrep[!duplicated(recentmeds_selfrep$eid),]

#same for icd
recentmeds_icd <- recentmeds[!(recentmeds$eid %in% selfrep_only$eid),]

recentmeds_icd_unique <- recentmeds_icd[!duplicated(recentmeds_icd$eid),]


recentmeds_selfrep_t1 <- CreateTableOne(vars,strata="sex.0.0",data=recentmeds_selfrep_unique,includeNA=TRUE,test=F)

recentmeds_selfrep_t1 <- print(recentmeds_selfrep_t1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(recentmeds_selfrep_t1, file = "self_rep_vs_icd/recentmeds_selfrep_t1_baseline.csv")


recentmeds_icd_t1 <- CreateTableOne(vars,strata="sex.0.0",data=recentmeds_icd_unique,includeNA=TRUE,test=F)

recentmeds_icd_t1 <- print(recentmeds_icd_t1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(recentmeds_icd_t1, file = "self_rep_vs_icd/recentmeds_icd_t1_baseline.csv")

saveRDS(recentmeds_selfrep,"self_rep_vs_icd/recentmeds_selfrep.rds")
saveRDS(recentmeds_icd,"self_rep_vs_icd/recentmeds_icd.rds")

#lastly, let's look at those with old medications (>6 months before baseline)

oldmeds <- readRDS("prevalent_baseline_analysis/oldmeds.rds")

summary(selfrep_only$eid %in% oldmeds$eid)
#745 people


oldmeds_selfrep <- oldmeds[oldmeds$eid %in% selfrep_only$eid,]

oldmeds_selfrep_unique <- oldmeds_selfrep[!duplicated(oldmeds_selfrep$eid),]

oldmeds_icd <- oldmeds[!(oldmeds$eid %in% selfrep_only$eid),]

oldmeds_icd_unique <- oldmeds_icd[!duplicated(oldmeds_icd$eid),]


oldmeds_selfrep_t1 <- CreateTableOne(vars,strata="sex.0.0",data=oldmeds_selfrep_unique,includeNA=TRUE,test=F)

oldmeds_selfrep_t1 <- print(oldmeds_selfrep_t1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(oldmeds_selfrep_t1, file = "self_rep_vs_icd/oldmeds_selfrep_t1_baseline.csv")

oldmeds_icd_t1 <- CreateTableOne(vars,strata="sex.0.0",data=oldmeds_icd_unique,includeNA=TRUE,test=F)

oldmeds_icd_t1 <- print(oldmeds_icd_t1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(oldmeds_icd_t1, file = "self_rep_vs_icd/oldmeds_icd_t1_baseline.csv")

saveRDS(oldmeds_selfrep,"self_rep_vs_icd/oldmeds_selfrep.rds")
saveRDS(oldmeds_icd,"self_rep_vs_icd/oldmeds_icd.rds")

#save unique dataframes so we can merge them together with bp info
saveRDS(recentmeds_selfrep_unique,"self_rep_vs_icd/recentmeds_selfrep_unique.rds")
saveRDS(recentmeds_icd_unique,"self_rep_vs_icd/recentmeds_icd_unique.rds")
saveRDS(oldmeds_selfrep_unique,"self_rep_vs_icd/oldmeds_selfrep_unique.rds")
saveRDS(oldmeds_icd_unique,"self_rep_vs_icd/oldmeds_icd_unique.rds")

#save unique dataframes as csvs so we can merge them together with bp info
write.csv(recentmeds_selfrep_unique,"self_rep_vs_icd/recentmeds_selfrep_unique.csv")
write.csv(recentmeds_icd_unique,"self_rep_vs_icd/recentmeds_icd_unique.csv")
write.csv(oldmeds_selfrep_unique,"self_rep_vs_icd/oldmeds_selfrep_unique.csv")
write.csv(oldmeds_icd_unique,"self_rep_vs_icd/oldmeds_icd_unique.csv")

write.csv(nomeds_icd,"self_rep_vs_icd/nomeds_icd.csv")
write.csv(nomeds_selfrep,"self_rep_vs_icd/nomeds_selfrep.csv")

merged_bp <- read.csv("self_rep_vs_icd/merged_bp.csv")

View(merged_bp)
merged_bp$Group <- as.factor(merged_bp$Group)

library(ggplot2)
ggplot(merged_bp, aes(x = sys_bp.0, fill = Group)) +
  geom_density(position = "identity", alpha = 0.2, bins = 50,na.rm=TRUE)

# Calculate the means for each group
mean_df <- aggregate(sys_bp.0 ~ Group, data = merged_bp, FUN = mean, na.rm = TRUE)

ggplot(merged_bp, aes(x = sys_bp.0, fill = Group)) +
  geom_histogram(position = "identity", alpha = 0.2, bins = 50, na.rm = TRUE) +
  geom_vline(data = mean_df, aes(xintercept = sys_bp.0,color=Group), linetype = "solid", size = 0.5) +
  labs(x = "sys_bp.0", y = "Frequency", title = "Histogram of sys_bp.0 by Group with Mean Lines") +
  theme_minimal()


library(dplyr)
library(forcats)

#make new df for nomeds, recentmeds and oldmeds
merged_bp2 <- merged_bp %>%
  mutate(Group = fct_collapse(Group,
                              "nomeds" = c("nomeds_icd", "nomeds_selfrep"),
                              "oldmeds" = c("oldmeds_icd", "oldmeds_selfrep"),
                              "recentmeds" = c("recentmeds_icd", "recentmeds_selfrep")
  ))

ggplot(merged_bp2, aes(x = sys_bp.0, fill = Group)) +
  geom_density(position = "identity", alpha = 0.2,na.rm=TRUE)

# Calculate the systolic means for each group
mean_df2 <- aggregate(sys_bp.0 ~ Group, data = merged_bp2, FUN = mean, na.rm = TRUE)

ggplot(merged_bp2, aes(x = sys_bp.0, fill = Group)) +
  geom_density(position = "identity", alpha = 0.2, na.rm = TRUE) +
  geom_vline(data = mean_df2, aes(xintercept = sys_bp.0,color=Group), linetype = "solid", size = 0.5) +
  labs(x = "sys_bp.0", y = "Density", title = "Histogram of sys_bp.0 by Group with Mean Lines") +
  theme_minimal()

# Calculate the diastolic means for each group
mean_dias_df2 <- aggregate(dias_bp.0 ~ Group, data = merged_bp2, FUN = mean, na.rm = TRUE)

ggplot(merged_bp2, aes(x = dias_bp.0, fill = Group)) +
  geom_density(position = "identity", alpha = 0.2, na.rm = TRUE) +
  geom_vline(data = mean_dias_df2, aes(xintercept = dias_bp.0,color=Group), linetype = "solid", size = 0.5) +
  labs(x = "dias_bp.0", y = "Density", title = "Histogram of dias_bp.0 by Group with Mean Lines") +
  theme_minimal()

library(psych)

#check for similar variances
describeBy(merged_bp2$sys_bp.0,group=merged_bp2$Group)
describeBy(merged_bp2$dias_bp.0,group=merged_bp2$Group)

anova <- aov(sys_bp.0 ~ Group,data=merged_bp2)
summary(anova)

anova_dias <- aov(dias_bp.0 ~ Group,data=merged_bp2)
summary(anova_dias)

#Tukey post-hoc test

library(multcomp)

# Tukey HSD test:
post_sys_test <- glht(anova,
                  linfct = mcp(Group = "Tukey"))

post_dias_test <- glht(anova_dias,
                      linfct = mcp(Group = "Tukey"))

summary(post_sys_test)

summary(post_dias_test)

##########################################################################################
#finally, let's do plots comparing the self_reported vs icd people for each group

nomeds_icd <- subset(nomeds_icd,select=c(sys_bp.0,dias_bp.0))
nomeds_selfrep <- subset(nomeds_selfrep,select=c(sys_bp.0,dias_bp.0))

nomeds_icd$Group <- "nomeds_icd"
nomeds_selfrep$Group <- "nomeds_selfrep"

combined_data <- rbind(nomeds_icd, nomeds_selfrep)

ggplot(combined_data, aes(x = sys_bp.0, fill = Group)) +
  geom_density(position = "identity", alpha = 0.2,na.rm=TRUE)

ggplot(combined_data, aes(x = dias_bp.0, fill = Group)) +
  geom_density(position = "identity", alpha = 0.2,na.rm=TRUE)



#repeat the same for recentmeds
recentmeds_icd_unique <- subset(recentmeds_icd_unique,select=c(sys_bp.0,dias_bp.0))
recentmeds_selfrep_unique <- subset(recentmeds_selfrep_unique,select=c(sys_bp.0,dias_bp.0))

recentmeds_icd_unique$Group <- "recentmeds_icd"
recentmeds_selfrep_unique$Group <- "recentmeds_selfrep"

combined_data <- rbind(recentmeds_icd_unique, recentmeds_selfrep_unique)

ggplot(combined_data, aes(x = sys_bp.0, fill = Group)) +
  geom_density(position = "identity", alpha = 0.2,na.rm=TRUE)

ggplot(combined_data, aes(x = dias_bp.0, fill = Group)) +
  geom_density(position = "identity", alpha = 0.2,na.rm=TRUE)

#repeat the same for oldmeds
oldmeds_icd_unique <- subset(oldmeds_icd_unique,select=c(sys_bp.0,dias_bp.0))
oldmeds_selfrep_unique <- subset(oldmeds_selfrep_unique,select=c(sys_bp.0,dias_bp.0))

oldmeds_icd_unique$Group <- "oldmeds_icd"
oldmeds_selfrep_unique$Group <- "oldmeds_selfrep"

combined_data <- rbind(oldmeds_icd_unique, oldmeds_selfrep_unique)

ggplot(combined_data, aes(x = sys_bp.0, fill = Group)) +
  geom_density(position = "identity", alpha = 0.2,na.rm=TRUE)

ggplot(combined_data, aes(x = dias_bp.0, fill = Group)) +
  geom_density(position = "identity", alpha = 0.2,na.rm=TRUE)

#make forest plots of mean and CI for each group

#produce summary stats to find mean and CI for each group
describeBy(merged_bp2$sys_bp.0,group=merged_bp2$Group)
describeBy(merged_bp2$dias_bp.0,group=merged_bp2$Group)

forest_data <- data.frame(Index = c(1,2,3),
    Group = c("nomeds", "oldmeds", "recentmeds"),
    sys_bp_mean = c(145.1, 140.99, 144.4),
    sys_bp_sd = c(20.05, 19.86, 18.37),
    sys_bp_ci_low = c(144.86, 140.60, 144.20),
    sys_bp_ci_high = c(145.34, 141.38, 144.60),
    sys_bp_ci = c("144.86, 145.34","140.60, 141.38","144.20, 144.60"),
    dias_bp_mean = c(85.79, 83.80, 83.75),
    dias_bp_sd = c(10.94, 10.76, 10.19),
    dias_bp_ci_low = c(85.65, 83.58, 83.68),
    dias_bp_ci_high = c(85.93, 84.02, 83.92),
    dias_bp_ci = c("85.65, 85.93","83.58, 84.02","83.68, 83.92")
  )
#produce forestplot for sys bp.

## Plot forest plot
sys_bp_plot <- ggplot(forest_data, aes(y = Index, x = sys_bp_mean)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = sys_bp_ci_low, xmax = sys_bp_ci_high), height = 0.25) +
  scale_y_continuous(name = "", breaks=1:3, labels = forest_data$Grou[], trans = "reverse") +
  xlab("Mean systolic blood pressure (95% CI)") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))

sys_bp_plot

#the code below adds a table to the plot
library(gridExtra)
## Create the table-base pallete
table_base <- ggplot(forest_data, aes(y=Group)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=12), 
        axis.text.x = element_text(color="white", hjust = -3, size = 25), ## This is used to help with alignment
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())

## OR point estimate table
tab1 <- table_base + 
  labs(title = "space") +
  geom_text(aes(y = rev(Index), x = 1, label = sprintf("%0.1f", round(sys_bp_mean, digits = 1))), size = 4) + ## decimal places
  ggtitle("Mean")

## 95% CI table
tab2 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = sys_bp_ci ), size = 4) + 
  ggtitle("95% CI")

## Merge tables with plot
lay <-  matrix(c(1,1,1,1,1,1,1,1,1,1,2,3,3), nrow = 1)
grid.arrange(sys_bp_plot, tab1, tab2, layout_matrix = lay)


#repeat the same code but for diastolic blood pressure

## Plot forest plot
dias_bp_plot <- ggplot(forest_data, aes(y = Index, x = dias_bp_mean)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = dias_bp_ci_low, xmax = dias_bp_ci_high), height = 0.25) +
  scale_y_continuous(name = "", breaks=1:3, labels = forest_data$Grou[], trans = "reverse") +
  xlab("Mean diastolic blood pressure (95% CI)") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))

#the code below adds a table to the plot
library(gridExtra)
## Create the table-base pallete
table_base <- ggplot(forest_data, aes(y=Group)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=12), 
        axis.text.x = element_text(color="white", hjust = -3, size = 25), ## This is used to help with alignment
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())

## OR point estimate table
tab1 <- table_base + 
  labs(title = "space") +
  geom_text(aes(y = rev(Index), x = 1, label = sprintf("%0.1f", round(dias_bp_mean, digits = 1))), size = 4) + ## decimal places
  ggtitle("Mean")

## 95% CI table
tab2 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = dias_bp_ci ), size = 4) + 
  ggtitle("95% CI")

## Merge tables with plot
lay <-  matrix(c(1,1,1,1,1,1,1,1,1,1,2,3,3), nrow = 1)
grid.arrange(dias_bp_plot, tab1, tab2, layout_matrix = lay)
