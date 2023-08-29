gp_scripts_small.R contains the script used to extract a small portion of the gp_scripts dataset
gp_scripts_small.txt is this small dataset that is used to test the code.
bnf_codes.txt contains a file with all the bnf codes present in the gp_scripts dataset.

exploratory_pres_analysis.R contains the initial preprocessing of the datasets, resulting in cases_no_meds.rds, cases_meds.rds and cases_unique_meds.rds.

meds_unique_cleaning.R contains the code used to clean the list of medications in cases_unique_meds.rds down to only treatments used for hypertension. It includes the creation of a csv file containing the list of unique medications, which is then manually checked to see which meds are not used for hypertension, before matches the non-hypertensive medications in medications_list_cleaned.csv, and removing them from the cases_unique_meds.rds file, before saving it.
It also cleans the issue_dates, the dosages and the units. 

unknown_meds_verification.R is a script that's used to check if any of the eids present in cases_no_meds.rds are present in the original gp_scripts folder. This is a smaller script that is tested on the gp_scripts_small.txt to see if the code runs.

unknown_meds_eid_matching_big.R is a script used to check if any of the eids present in cases_no_meds are present in the original gp_scripts file. The script processes the file using 24 parallel chunks as an array job using the .sh script. The output is 24 .txt files of matched eids, one for each chunk.

matching_nomed_eids.R is a script that combines the 24 .txt files and identifies unique eids present. It then subsets the cases_no_med.rds file into a no_meds_confirmed.rds dataset using these identified eids. The other eids present in cases_no_meds.rds are renamed as unknown_meds_confirmed.rds.

meds_full_cleaning.R is a script that cleans the cases_meds.rds file in the same way cases_unique_meds_final.rds is cleaned. cases_meds.rds contains all of the duplicate medications as well, which contains more information at the cost of a larger dataset.

