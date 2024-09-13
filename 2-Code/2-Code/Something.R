#Trimming EEMs and ABS data for EEM_Correction.R
#Connor O'Loughlin


getwd()


ABS_input_dir <- "Connor/WinterGrab2/10Jul24/Output/ABS_New"
ABS_output_dir <- "Connor/WinterGrab2/10Jul24/Output/ABS_Corrected"

ABS_files <- list.files(path = ABS_input_dir, pattern = "_ABS\\.csv$", full.names = TRUE)




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