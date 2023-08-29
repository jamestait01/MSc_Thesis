meds <- readRDS("Data/old_data/cases_meds.rds")

med_list <- read.csv("preprocessing/prescriptions/medications_list_cleaned.csv",header=T)

library(parallel)

n_cores <- 7
cl=makeCluster(n_cores)

# Split the meds dataset into smaller subsets
meds_split <- split(meds, 1:n_cores)

# Makes function that takes in 2 dataframes and compares the 2 to see where there are matches
match_medicines <- function(meds, med_list) {
  # Creates new column to store matched medicines
  meds$matched_medicine <- NA
  
  # Iterate over rows in meds
  for (i in seq_len(nrow(meds))) {
    drug_name <- meds$drug_name[i]
    
    # Iterate over rows in med_list
    for (j in seq_len(nrow(med_list))) {
      medicine <- med_list$Medicine[j]
      
      # Check if the medicine name in med_list is found in the drug name in meds
      if (grepl(medicine, drug_name)) {
        meds$matched_medicine[i] <- medicine
        break  # Exit the inner loop once a match is found
      }
    }
  }
  
  return(meds)
}

# Export the med_list dataframe to the workers
clusterExport(cl,c("meds_split", "med_list"))

# Perform parallel processing on each subset
results <- parLapply(cl, meds_split, function(x) match_medicines(x, med_list))

# Stop the cluster
stopCluster(cl)

# Combine the results
meds_combined <- do.call(rbind, results)

# Save the combined result
saveRDS(meds_combined, "Data/cases_meds_final2.rds")

