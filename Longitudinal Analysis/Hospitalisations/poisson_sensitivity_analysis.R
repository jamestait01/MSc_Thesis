setwd("~/Documents/MSc/Thesis2/baseline_models/")

data <- readRDS("model_3a/final_data.rds")

library(dplyr)
data <- data %>%
  rename(`Baseline systolic blood pressure` = sys_bp.0,
         `Baseline diastolic blood pressure` = dias_bp.0)

data$`Age group` <- cut(data$Age, breaks=c(40,55,65,Inf),labels=c("40-55","55-65","65+")
                        ,right = FALSE)

data$`Medication status` <- relevel(data$`Medication status`,ref="Stayed untreated")

#rename medication combination factor levels 
levels(data$`Medication combination`) <- c("Untreated","ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                                           "ACE inhibitor/ARB + CCB + Diuretic",
                                           "ACE inhibitor/ARB + Diuretic",
                                           "CCB", "CCB + Diuretic")

levels(data$`Last medication combination`) <- c("Untreated","ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                                                "ACE inhibitor/ARB + CCB + Diuretic",
                                                "ACE inhibitor/ARB + Diuretic",
                                                "CCB", "CCB + Diuretic")

men <- data[data$Sex=="Male",]

female <-  data[data$Sex=="Female",]


men_hosp <- glm(n_hospitalisations_unique ~ `Age group` + BMI + `Number of comorbidities` +  `Smoking status` +
                       `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                       `Medication status`,family="poisson",data=men)

men_cardiac <- glm(n_cardiac_unique ~ `Age group` + BMI + `Number of comorbidities` +  `Smoking status` +
                         `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                         `Medication status`,family="poisson",data=men)



female_hosp <- glm(n_hospitalisations_unique ~ `Age group` + BMI + `Number of comorbidities` +  `Smoking status` +
                  `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                  `Medication status`,family="poisson",data=female)

female_cardiac <- glm(n_cardiac_unique ~ `Age group` + BMI + `Number of comorbidities` +  `Smoking status` +
                     `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                     `Medication status`,family="poisson",data=female)


#visualise results

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_models(men_hosp,female_hosp,m.labels = c("Male","Female"),vline.color="black",
            title="Incidence rate ratio of hospitalisations for different categories",
            legend.title="Gender",prefix.labels=c("varname"),
            axis.lim=c(0.5,3)) + scale_y_continuous(limits = c(0.5,3))

plot_models(men_cardiac,female_cardiac,m.labels = c("Male","Female"),vline.color="black",
            title="Incidence rate ratio of cardiac hospitalisations for different categories",
            legend.title="Gender",prefix.labels=c("varname"),
            axis.lim=c(0.5,3)) + scale_y_continuous(limits = c(0.5,3))

#repeat the same but for people with stable medications

men_meds <- men[men$`Medication status`=="Stable medications",]
men_meds$`Medication combination` <- relevel(men_meds$`Medication combination`,ref="ACE inhibitors/ARBs")
men_meds$`Medication combination` <- droplevels(men_meds$`Medication combination`)


female_meds <- men[female$`Medication status`=="Stable medications",]

female_meds$`Medication combination` <- relevel(female_meds$`Medication combination`,ref="ACE inhibitors/ARBs")
female_meds$`Medication combination` <- droplevels(female_meds$`Medication combination`)

men_meds_hosp <- glm(n_hospitalisations_unique ~ `Age group` + BMI + `Number of comorbidities` +  `Smoking status` +
                  `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                  `Medication combination`,family="poisson",data=men_meds)

men_meds_cardiac <- glm(n_cardiac_unique ~ `Age group` + BMI + `Number of comorbidities` +  `Smoking status` +
                     `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                     `Medication combination`,family="poisson",data=men_meds)



female_meds_hosp <- glm(n_hospitalisations_unique ~ `Age group` + BMI + `Number of comorbidities` +  `Smoking status` +
                     `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                     `Medication combination`,family="poisson",data=female_meds)

female_meds_cardiac <- glm(n_cardiac_unique ~ `Age group` + BMI + `Number of comorbidities` +  `Smoking status` +
                        `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                        `Medication combination`,family="poisson",data=female_meds)

plot_models(men_meds_hosp,female_meds_hosp,m.labels = c("Male","Female"),vline.color="black",
            title="Incidence rate ratio of hospitalisations for people on different medications",
            legend.title="Gender",prefix.labels=c("varname"),
            axis.lim=c(0.5,3)) + scale_y_continuous(limits = c(0.5,3))


plot_models(men_meds_cardiac,female_meds_cardiac,m.labels = c("Male","Female"),vline.color="black",
            title="Incidence rate ratio of cardiac hospitalisations for people on different medications",
            legend.title="Gender",prefix.labels=c("varname"),
            axis.lim=c(0.5,3)) + scale_y_continuous(limits = c(0.5,3))

#repeat the above but for different age groups
age_cat1 <- data[data$`Age group`=="40-55",]
age_cat2 <- data[data$`Age group`=="55-65",]
age_cat3 <- data[data$`Age group`=="65+",]


age_cat1_hosp <- glm(n_hospitalisations_unique ~ `Sex` + BMI + `Number of comorbidities` +  `Smoking status` +
                  `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                  `Medication status`,family="poisson",data=age_cat1)

age_cat1_cardiac <- glm(n_cardiac_unique ~ `Age group` + BMI + `Number of comorbidities` +  `Smoking status` +
                     `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                     `Medication status`,family="poisson",data=men)

age_cat2_hosp <- glm(n_hospitalisations_unique ~ `Age group` + BMI + `Number of comorbidities` +  `Smoking status` +
                       `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                       `Medication status`,family="poisson",data=men)

age_cat2_cardiac <- glm(n_cardiac_unique ~ `Age group` + BMI + `Number of comorbidities` +  `Smoking status` +
                          `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                          `Medication status`,family="poisson",data=men)

age_cat3_hosp <- glm(n_hospitalisations_unique ~ `Age group` + BMI + `Number of comorbidities` +  `Smoking status` +
                       `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                       `Medication status`,family="poisson",data=men)

age_cat3_cardiac <- glm(n_cardiac_unique ~ `Age group` + BMI + `Number of comorbidities` +  `Smoking status` +
                          `Weekly alcohol intake` + `Index of Multiple Deprivation` +
                          `Medication status`,family="poisson",data=men)