setwd("/rds/general/user/jt1019/home/Summer_project/")
icd10_data <- readRDS("Data/comorbidities/icd10_data.rds")

#tranpose the data so we have 1 column with all icd codes, needed to run the package 


library(tidyverse)

icd_transposed <- icd10_data %>%
  pivot_longer(cols = starts_with("icd10_diag"), names_to = NULL, values_to = "icd10_code") %>%
  filter(!is.na(icd10_code))

saveRDS(icd_transposed,"Data/comorbidities/icd10_transposed.rds")