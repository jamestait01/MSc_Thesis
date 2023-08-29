setwd("/rds/general/project/hda-22-23/live/Summer_projects/jt1019")

file_path <- "/rds/general/project/hda-22-23/live/Summer_projects/jt1019/gp_data/gp_scripts.txt"
small_file_path <- "/rds/general/project/hda-22-23/live/Summer_projects/jt1019/gp_scripts_small.txt"

nomeds <- readRDS("Data/cases_no_meds.rds")

#vector of eids to scan for in the gp scripts data
eids <- nomeds$eid

##############################################################################################################
#find total no. of rows

# Count the number of fields (columns) in each line
field_counts <- count.fields(file_path, sep = "\t")

# Calculate the total number of rows based on the number of lines with consistent field counts
total_rows <- sum(field_counts > 0)

#there are 56,216,351 total rows

######################################attempts parallelisation on small dataset to test code###########################
library(parallel)
no_cores <- 7
cl <- makeCluster(7)

# Export necessary variables and functions to the cluster workers

clusterExport(cl, "eids")
clusterExport(cl, "small_file_path")

# Create a function to process a chunk and return matched eids
process_chunk <- function(chunk_number) {
  matched_eids <- c()  # Initialize an empty vector to store matched eids
  
  # Open the file connection for the chunk
  con <- file(small_file_path, open = "r")
  header <- readLines(con, n = 1)  # Read the header line
  col_names <- unlist(strsplit(header, "\t"))  # Split the header by tabs
  
  eid_col <- which(col_names == "eid")  # Identify the column index of "eid"
  
  if (chunk_number <= num_full_chunks) {
    skip_lines <- (chunk_number - 1) * chunk_size  # Calculate the number of lines to skip
    seek(con, skip_lines)  # Skip lines to start reading from the chunk
    
    # Read the lines in the chunk
    lines <- readLines(con, n = chunk_size, warn = FALSE)
  } else {
    # Read the remaining lines in the partial chunk
    skip_lines <- num_full_chunks * chunk_size  # Calculate the number of lines to skip
    seek(con, skip_lines)  # Skip lines to start reading from the partial chunk
    
    # Read the lines in the partial chunk
    lines <- readLines(con, n = partial_chunk_size, warn = FALSE)
  }
  
  # Process each line in the chunk or partial chunk
  for (line in lines) {
    values <- strsplit(line, "\t")[[1]]
    eid <- values[eid_col]
    if (eid %in% eids) {
      matched_eids <- c(matched_eids, eid)
    }
  }
  
  # Close the file connection
  close(con)
  
  # Save the matched eids for this chunk
  write.table(matched_eids, paste0("matched_eids", chunk_number, ".txt"), col.names = FALSE, row.names = FALSE, quote = FALSE)
  
  return(matched_eids)
}

clusterExport(cl,"process_chunk")
# Calculate the number of full chunks
total_rows <- 1000000  # Total number of rows in the file
chunk_size <- 300000 #how many lines to process in each chunk
num_full_chunks <- floor(total_rows / chunk_size)  # Number of full chunks

# Calculate the size of the final partial chunk
partial_chunk_size <- total_rows %% chunk_size  # Size of the final partial chunk

clusterExport(cl,c("total_rows","chunk_size","num_full_chunks","partial_chunk_size"))

# Parallelize the processing of full chunks and the final partial chunk using parLapply
matched_eids_list <- parLapply(cl, 1:(num_full_chunks + 1), process_chunk)

# Close the parallel cluster
stopCluster(cl)

# Save the matched eids for each chunk or the final partial chunk
for (i in 1:(num_full_chunks + 1)) {
  write.table(matched_eids_list[[i]], paste0("matched_eids", i, ".txt"), col.names = FALSE, row.names = FALSE, quote = FALSE)
}

#this code works, but it took 25 minutes to run for the small dataset with 1 million rows. This means it's going to take
#about 1 day to run on the entire dataset of 56 million rows.

####################################### attempts parallelisation on entire dataset##########################################

library(parallel)
no_cores <- 7
cl <- makeCluster(7)

# Export necessary variables and functions to the cluster workers

clusterExport(cl, "eids")
clusterExport(cl, "file_path")

# Create a function to process a chunk and return matched eids
process_chunk <- function(chunk_number) {
  matched_eids <- c()  # Initialize an empty vector to store matched eids
  
  # Open the file connection for the chunk
  con <- file(file_path, open = "r")
  header <- readLines(con, n = 1)  # Read the header line
  col_names <- unlist(strsplit(header, "\t"))  # Split the header by tabs
  
  eid_col <- which(col_names == "eid")  # Identify the column index of "eid"
  
  if (chunk_number <= num_full_chunks) {
    skip_lines <- (chunk_number - 1) * chunk_size  # Calculate the number of lines to skip
    seek(con, skip_lines)  # Skip lines to start reading from the chunk
    
    # Read the lines in the chunk
    lines <- readLines(con, n = chunk_size, warn = FALSE)
  } else {
    # Read the remaining lines in the partial chunk
    skip_lines <- num_full_chunks * chunk_size  # Calculate the number of lines to skip
    seek(con, skip_lines)  # Skip lines to start reading from the partial chunk
    
    # Read the lines in the partial chunk
    lines <- readLines(con, n = partial_chunk_size, warn = FALSE)
  }
  
  # Process each line in the chunk or partial chunk
  for (line in lines) {
    values <- strsplit(line, "\t")[[1]]
    eid <- values[eid_col]
    if (eid %in% eids) {
      matched_eids <- c(matched_eids, eid)
    }
  }
  
  # Close the file connection
  close(con)
  
  # Save the matched eids for this chunk
  write.table(matched_eids, paste0("matched_eids", chunk_number, ".txt"), col.names = FALSE, row.names = FALSE, quote = FALSE)
  
  return(matched_eids)
}

clusterExport(cl,"process_chunk")
# Calculate the number of full chunks
total_rows <- 56216351  # Total number of rows in the file
chunk_size <- 10000000 #how many lines to process in each chunk
num_full_chunks <- floor(total_rows / chunk_size)  # Number of full chunks

# Calculate the size of the final partial chunk
partial_chunk_size <- total_rows %% chunk_size  # Size of the final partial chunk

clusterExport(cl,c("total_rows","chunk_size","num_full_chunks","partial_chunk_size"))

# Parallelize the processing of full chunks and the final partial chunk using parLapply
matched_eids_list <- parLapply(cl, 1:(num_full_chunks + 1), process_chunk)

# Close the parallel cluster
stopCluster(cl)

# Save the matched eids for each chunk or the final partial chunk
for (i in 1:(num_full_chunks + 1)) {
  write.table(matched_eids_list[[i]], paste0("matched_eids", i, ".txt"), col.names = FALSE, row.names = FALSE, quote = FALSE)
}
