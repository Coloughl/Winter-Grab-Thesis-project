#Code for changing EEMs and ABS data from .dat to .csv
#Connor O'Loughlin 9/3/24


getwd()


#Making input directories for ABS and EEMs data
ABS_input_dir <- "Connor/WinterGrab2/10Jul24/Output/ABS"
ABS_output_dir <- "Connor/WinterGrab2/10Jul24/Output/ABS_New"  


#Making output directories for ABS and EEMs data
EEMs_input_dir <- "Connor/WinterGrab2/10Jul24/Output/EEMs"
EEMs_output_dir <- "Connor/WinterGrab2/10Jul24/Output/EEMs_New"


# List all files ending in "_ABS.dat" or "_EEMs.dat"
ABS_files <- list.files(path = ABS_input_dir, pattern = "_ABS\\.dat$", full.names = TRUE)
EEMs_files <- list.files(path = EEMs_input_dir, pattern = "_EEMs\\.dat$", full.names = TRUE)



# Loop through each file
for (file in EEMs_files) {
  # Construct the output filename
  output_file <- file.path(EEMs_output_dir ,gsub("\\.dat$", ".csv", basename(file)))
  
  # Read the file
  data <- read.table(file, header = TRUE, sep = "\t")
  
  # Write to CSV
  write.csv(data, output_file, row.names = FALSE)
  
  # Optionally, print progress
  cat("Processed:", file, "->", output_file, "\n")
}

# Loop through each file
for (file in ABS_files) {
  # Construct the output filename
  output_file <- file.path(ABS_output_dir ,gsub("\\.dat$", ".csv", basename(file)))
  
  # Read the file
  data <- read.table(file, header = TRUE, sep = "\t")
  
  # Write to CSV
  write.csv(data, output_file, row.names = FALSE)
  
  # Optionally, print progress
  cat("Processed:", file, "->", output_file, "\n")
}

