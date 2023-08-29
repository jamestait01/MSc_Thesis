setwd("/rds/general/project/hda-22-23/live/Summer_projects/jt1019")
#create list of file paths
filelist = list.files(pattern = "matched_eids")

#assuming tab separated values with a header    
datalist = lapply(filelist, function(x)read.table(x, header=F)) 

#assuming the same header/columns for all files
datafr = do.call("rbind", datalist) 

datafr_unique <- unique(datafr$V1)
#there are 6134 people with medications for other conditions but not hypertension -> assume they are not on hypertensive
#treatment

unknown_meds <- readRDS("Data/cases_no_meds.rds")

unknown_meds_confirmed <- unknown_meds[!unknown_meds$eid %in% datafr_unique, ]

saveRDS(no_meds_confirmed,"Data/no_meds_confirmed.rds")

saveRDS(unknown_meds_confirmed,"Data/unknown_meds_confirmed.rds")
