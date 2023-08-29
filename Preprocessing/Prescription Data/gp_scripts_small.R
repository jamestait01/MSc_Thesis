smaller_file_path <- "/rds/general/project/hda-22-23/live/Summer_projects/jt1019/gp_scripts_small.txt"  
num_lines <- 1000000  # Specify the desired number of lines for the smaller file

# Read the desired number of lines from the original file
lines <- readLines(file_path, n = num_lines)

# Write the lines to the smaller file
writeLines(lines, smaller_file_path)
