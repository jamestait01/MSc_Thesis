setwd("~/Documents/MSc/Thesis2/baseline_models/")
data <- readRDS("model_3a/final_data.rds")

data$binary_hospitalisation <- ifelse(data$n_hospitalisations_unique==0,0,1)
data$binary_cardiac <- ifelse(data$n_cardiac_unique==0,0,1)

data$`Age group` <- cut(data$Age, breaks=c(40,55,65,Inf),labels=c("40-55","55-65","65+")
                        ,right = FALSE)

library(dplyr)
data <- data %>%
  rename(`Baseline systolic blood pressure` = sys_bp.0,
         `Baseline diastolic blood pressure` = dias_bp.0)


#now let's look at those who stayed on the same medications...

stable_meds <- data[data$`Medication status`=="Stable medications",]

#relevel medication combination 
stable_meds$`Medication combination` <- relevel(stable_meds$`Medication combination`,ref="ACE inhibitors/ARBs")

#remove untreated factor level 
stable_meds$`Medication combination` <- droplevels(stable_meds$`Medication combination`)

levels(stable_meds$`Medication combination`) <- c("ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                                                  "ACE inhibitor/ARB + CCB + Diuretic",
                                                  "ACE inhibitor/ARB + Diuretic",
                                                  "CCB", "CCB + Diuretic")

med_cardiac <- glm(binary_cardiac ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                     `Index of Multiple Deprivation` + `Number of comorbidities` +
                     `Medication combination`,
                family=binomial(link='logit'),data=stable_meds)



#repeat the model but also including baseline blood pressure

med_cardiac_bp <- glm(binary_cardiac ~ Sex + `Age group` + BMI + `Smoking status` + 
                                       `Weekly alcohol intake` + `Index of Multiple Deprivation` + 
                                       `Number of comorbidities` + `Medication combination`+
                                       `Baseline systolic blood pressure` + `Baseline diastolic blood pressure`,
                         family=binomial(link='logit'),data=stable_meds)

#library(gtsummary)

#med_cardiac_tbl <- med_cardiac %>%
  #tbl_regression(exponentiate=T) %>%
  #bold_labels()


#med_cardiac_bp_tbl <- med_cardiac_bp %>%
  #tbl_regression(exponentiate=T) %>%
  #bold_labels()

#merge the 2 tables with and without blood pressure together
#merged_tbl <- tbl_merge(list(med_cardiac_tbl,med_cardiac_bp_tbl),
                        #tab_spanner = c("**Model 1: Without blood pressure**","**Model 2: With blood pressure**"))

#gt::gtsave(as_gt(merged_tbl),"stable_meds_cardiac_logistic_table.png",expand=40) 

#try matthew's code
library(ggplot2)
library(ggforce)
library(dplyr)

#make list of categorical variables
categorical_variables <- c("Sex","`Age group`","`Smoking status`","`Weekly alcohol intake`","`Number of comorbidities`",
                           "`Medication combination`")

#make list of reference levels for each categorical variable included in model
ref_levels <- c("Female","40-55","Never","Never","0","ACE inhibitor/ARB")

#make function to extract coefficients and 95% CI.s from model
makeORTable <- function(mod, ref_levels = NULL, dp = 3, categorical_variables = NULL) {
  mod_exp <- (mod$family$family == "binomial")
  
  if (class(mod)[1] == "gam") {
    tab <- as.data.frame(summary.gam(mod)$p.table)
    tab$Lower = tab$Estimate - 1.96 * tab$`Std. Error`
    tab$Upper = tab$Estimate + 1.96 * tab$`Std. Error`
    tab <- tab %>% dplyr::select(Estimate, Lower, Upper, `Pr(>|z|)`)
    tab$Level <- rownames(tab)
    tab <- tab %>% dplyr::select(Level, everything())
    colnames(tab) <- c("Level", "OR", "Lower", "Upper", "P_value")
    
    if (!is.null(categorical_variables)) {
      for (i in seq_along(categorical_variables)) {
        ref_level_name <- ifelse(is.null(ref_levels), 
                                 paste0(categorical_variables[i], " [reference]"),
                                 paste0(ref_levels[i], " [reference]"))
        tab <- rbind(tab, c(ref_level_name, 1, NA_real_, NA_real_, NA_real_))
      }
    }
    
    tab[, 2] <- round(exp(as.numeric(tab[, 2])), dp)
    tab[, 3] <- round(exp(as.numeric(tab[, 3])), dp)
    tab[, 4] <- round(exp(as.numeric(tab[, 4])), dp)
    tab[, 5] <- round(as.numeric(tab[, 5]), 5)
  } else {
    tab <- jtools::summ(mod, exp = mod_exp, ORs = mod_exp)
    
    if (mod_exp) {
      tab <- tab$coeftable %>% as.data.frame() %>% dplyr::select(1, 2, 3, 5)
    } else {
      tab <- tab$coeftable %>% as.data.frame() %>% dplyr::select(1, 2, 4)
      tab$Lower = tab$Est. - qnorm(p = 0.975, mean = 0, sd = 1) * tab$S.E.
      tab$Upper = tab$Est. + qnorm(p = 0.975, mean = 0, sd = 1) * tab$S.E.
      tab <- tab[, c(1, 4, 5, 3)]
    }
    
    tab$Level <- rownames(tab)
    tab <- tab %>% dplyr::select(Level, everything())
    colnames(tab) <- c("Level", "OR", "Lower", "Upper", "P_value")
    
    if (!is.null(categorical_variables)) {
      for (i in seq_along(categorical_variables)) {
        ref_level_name <- ifelse(is.null(ref_levels), 
                                 paste0(categorical_variables[i], " [reference]"),
                                 paste0(ref_levels[i], " [reference]"))
        tab <- rbind(tab, c(ref_level_name, 1, NA_real_, NA_real_, NA_real_))
      }
    }
    
    tab[, 2] <- round(as.numeric(tab[, 2]), dp)
    tab[, 3] <- round(as.numeric(tab[, 3]), dp)
    tab[, 4] <- round(as.numeric(tab[, 4]), dp)
    tab[, 5] <- round(as.numeric(tab[, 5]), 5)
  }
  
  rownames(tab) <- 1:nrow(tab)
  return(tab)
}

cardiac_tbl <- makeORTable(med_cardiac,ref_levels=ref_levels, categorical_variables=categorical_variables)

cardiac_bp_tbl <- makeORTable(med_cardiac_bp,ref_levels=ref_levels, categorical_variables=categorical_variables)

#remove intercept row
cardiac_tbl <- cardiac_tbl[-1,]
cardiac_bp_tbl <- cardiac_bp_tbl[-1,]
#rename appropriate variables
cardiac_tbl$Level <- c("Male","55-65","65+","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                             "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                             "Index of Multiple Deprivation","1-2",">=3","ACE inhibitor/ARB + CCB",
                             "ACE inhibitor/ARB + CCB + Diuretic","ACE inhibitor/ARB + Diuretic","CCB","CCB + Diuretic",
                             "Female [reference]","40-55 [reference]","Never [reference]",
                             "Never [reference]","0 [reference]","ACE inhibitor/ARB [reference]")

cardiac_bp_tbl$Level <- c("Male","55-65","65+","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                                     "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                                     "Index of Multiple Deprivation","1-2",">=3","ACE inhibitor/ARB + CCB",
                                     "ACE inhibitor/ARB + CCB + Diuretic","ACE inhibitor/ARB + Diuretic","CCB","CCB + Diuretic",
                                     "Baseline systolic blood pressure (mmHg)","Baseline diastolic blood pressure (mmHg)",
                                     "Female [reference]","40-55 [reference]","Never [reference]",
                                     "Never [reference]","0 [reference]","ACE inhibitor/ARB [reference]")


predictors <- c("Sex","`Age group`", "BMI","`Smoking status`", "`Weekly alcohol intake`", 
                "`Index of Multiple Deprivation`", "`Number of comorbidities`",
                "`Medication combination`")

#add predictor names to table
cardiac_tbl$predictor <- c("Sex","Age group","Age group", "BMI","Smoking status","Smoking status","Smoking status",
                                 "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                                 "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                                 "Number of comorbidities","Number of comorbidities","Medication combination",
                                 "Medication combination","Medication combination","Medication combination",
                                 "Medication combination",
                                 "Sex","Age group","Smoking status","Weekly alcohol intake","Number of comorbidities","Medication combination")

cardiac_bp_tbl$predictor <- c("Sex","Age group","Age group", "BMI","Smoking status","Smoking status","Smoking status",
                                         "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                                         "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                                         "Number of comorbidities","Number of comorbidities","Medication combination",
                                         "Medication combination","Medication combination","Medication combination",
                                         "Medication combination", "Baseline systolic blood pressure (mmHg)",
                                         "Baseline diastolic blood pressure (mmHg)",
                                         "Sex","Age group","Smoking status","Weekly alcohol intake",
                                         "Number of comorbidities","Medication combination")


# Combine the two OR tables
combined_stable_meds_table <- bind_rows(
  mutate(cardiac_tbl, model_type = "Without blood pressure"),
  mutate(cardiac_bp_tbl, model_type = "With blood pressure"))

combined_stable_meds_table <- combined_stable_meds_table[-c(12,39),]

# Create the forest plot with facets for each predictor and color by model type
ggplot(data = combined_stable_meds_table, aes(x = Level, y = OR, color = model_type)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(width = 0.5), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  scale_y_log10() +
  labs(col = "Model", x = "", y = "Odds Ratio") +
  theme_bw() + #get rid of grey background 
  theme(strip.background =element_rect(fill="white"),legend.position="bottom")+
  theme(strip.text = element_text(colour = 'black',face="bold")) +
  facet_col(~ predictor, scales = "free_y", space = "free") +
  scale_color_manual(values = c("Without blood pressure" = "blue", "With blood pressure" = "red")) +
  labs(x="Characteristic") + 
  theme(legend.margin=margin(-10, 0, 0, 0)) + #move legend label closer to the x-axis for less white space
  guides(color = guide_legend(nrow = 2))
