#EEMs file alteration code ----
#Connor O'Loughlin
#Used ChatGPT-4o to help develop code and troubleshoot
#Last update: 5/1/25 by CCO

library(tidyverse)
library(dplyr)


getwd()
setwd("C:/Users/ccolo/OneDrive/Documents/GitHub/Winter-Grab-Thesis-project/1-Data/")

#Grabbing files ----

#Making input directories for ABS and EEMs data

ABS_input_dir <- "Practice_3"

ABS_output_dir <- "Practice_3/ABS_corrected"  


#Making output directories for ABS and EEMs data
EEMs_input_dir <- "Practice_3"
EEMs_output_dir <- "Practice_3/EEMs_corrected"
BEEMs_output_dir <- "Practice_3/EEMs_corrected"


# List all files ending in "_ABS.dat" or "_EEMs.dat"

#Grabbing all ABS files and making a list variable
ABS_files <- list.files(path = ABS_input_dir, pattern = "\\(01\\) - Abs Spectra Graphs\\.dat$", full.names = TRUE)
print(ABS_files)

#Grabbing all EEMs files and making a list variable, then removing any blank files from the list
EEMs_files <- list.files(path = EEMs_input_dir, pattern = "\\(01\\) - Sample - Blank Waterfall Plot\\.dat$", full.names = TRUE)
print(EEMs_files)


#Checking file names and looking for missing data ----
#Making sure that there is a EEMs file for every ABS file
ABS_names <- basename(ABS_files) %>%
  gsub(" \\(01\\) - Abs Spectra Graphs\\.dat$", "", .)

# Extract sample names from EEMs file paths
EEMs_names <- basename(EEMs_files) %>%
  gsub(" \\(01\\) - Sample - Blank Waterfall Plot\\.dat$", "", .)

# Check to make sure data matches (should be 0)
ABS_names %>%
  setdiff(EEMs_names) %>%
  print()

EEMs_names %>%
  setdiff(ABS_names) %>%
  print()

#Check to see if the names are exactly the same (nessecary for EEMs pipeline)
if (setequal(ABS_names, EEMs_names)) {
  print("✅ Sample names match between ABS and EEMs.")
} else {
  print("❌ Sample names do NOT match.")
}


#This line is what grabs the blank file
BEEMs_files <- list.files(path = EEMs_input_dir, pattern = "\\(01\\) - Waterfall Plot Blank\\.dat$", full.names = TRUE)
print(BEEMs_files)



#Removing the necessary rows and columns from each file then saving to a new folder ----

for (file in ABS_files) {
  # Read the space-separated .dat file, skipping the first 3 lines
  data <- read.table(file, skip = 3, sep = "\t", header = FALSE, fill = TRUE)
  
  # Remove columns 2 through 9 and column 11. Leaves only OD column
  data_modified <- data[-c(2,3), -c(2:9,11)]
  
  # Create output file name with .csv extension
  sample_name <- gsub(" \\(01\\) - Abs Spectra Graphs\\.dat$", "", basename(file))
  new_file_name <- paste0(sample_name, ".csv")
  new_file <- file.path(ABS_output_dir, new_file_name)
  
  # Write cleaned data to CSV
  write.table(data_modified, file = new_file, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  print(paste("Processed and saved:", new_file))
}



for (file in EEMs_files) {
  # Read the CSV with headers
  data <- read.table(file, sep = "\t", header = FALSE)
  
  # Remove rows 2 and 3 (if needed)
  data <- data[-2, ]
  
  # Remove rows 239 to 250
  data <- data[-c(244:252), ]  # Adjust as needed
  
  data <- data[-c(2,3), ]
  
  # Convert entire data frame to character (to allow blanking A1)
  data[] <- lapply(data, as.character)
  
  # Blank out cell A1
  data[1, 1] <- ""
  
  # Build output path
  sample_name <- gsub("\\(01\\) - Sample - Blank Waterfall Plot\\.dat$", "", basename(file))
  new_file_name <- paste0(sample_name, ".csv")
  new_file <- file.path(EEMs_output_dir, new_file_name)
  
  # Write line by line to fully control formatting
  writeLines(
    apply(data, 1, function(row) paste(row, collapse = ",")),
    con = new_file
  )
  
  print(paste("Processed and saved:", new_file))
}




run_date <- "9Apr25"
blank_file_name <- paste0("blank", run_date, ".csv")
blank_file_path <- file.path(BEEMs_output_dir, blank_file_name)


for (file in BEEMs_files) {
  data <- read.table(file, sep = "\t", header = FALSE)
  
  # Remove rows 239 to 250
  #data_modified <- data[-c(239:250), ]
  
  
  data[] <- lapply(data, as.character)
  
  # Blank out cell A1
  data[1, 1] <- ""
  
  #sample_name <- gsub("\\(01\\) - Waterfall Plot Blank\\.dat$", "", basename(file))
  #new_file_name <- paste0(blank_file_name, ".csv")
  blank_file_path <- file.path(BEEMs_output_dir, blank_file_name)
  
  
  # Define the new file path in the output directory
  #new_file <- file.path(BEEMs_output_dir ,new_file_name)  # 'basename' keeps the original filename
  
  # Save to the new directory with the same filename (no '_modified' added)
  #
  writeLines(
    apply(data, 1, function(row) paste(row, collapse = ",")),
    con = blank_file_path)
  
  #write.csv(data_modified, file = new_file, row.names = FALSE, col.names = TRUE)
  
  print(paste("Processed and saved:", blank_file_name))
}





#Experimental code -----


# Loop through each file
#for (file in EEMs_files) {
  # Construct the output filename
  #output_file <- file.path(EEMs_output_dir ,gsub("\\.dat$", ".csv", basename(file)))
  
  # Read the file
  #data <- read.table(file, header = TRUE, sep = "\t")
  
  # Write to CSV
  #write.csv(data, output_file, row.names = FALSE)
  
  # Optionally, print progress
  #cat("Processed:", file, "->", output_file, "\n")
#}


# Loop through each file
#for (file in BEEMs_files) {
  # Construct the output filename
 # output_file <- file.path(BEEMs_output_dir ,gsub("\\.dat$", ".csv", basename(file)))
  
  # Read the file
  #data <- read.table(file, header = TRUE, sep = "\t")
  
  # Write to CSV
  #write.csv(data, output_file, row.names = FALSE)
  
  # Optionally, print progress
  #cat("Processed:", file, "->", output_file, "\n")
#}

# Loop through each file
#for (file in ABS_files) {
  # Construct the output filename
 # output_file <- file.path(ABS_output_dir ,gsub("\\.dat$", ".csv", basename(file)))
  
  # Read the file
  #data <- read.table(file, header = TRUE, sep = "\t")
#  
  # Write to CSV
 # write.csv(data, output_file, row.names = FALSE)
  
  # Optionally, print progress
  #cat("Processed:", file, "->", output_file, "\n")
#}




# All of this stuff is not done yet 
ABS_files <- list.files(path = ABS_input_dir, pattern = "Abs Spectra Graphs\\.dat$", full.names = TRUE)
EEMs_files <- list.files(path = EEMs_input_dir, pattern = "Waterfall Plot Sample\\.dat$", full.names = TRUE)
BEEMs_files <- list.files(path = EEMs_input_dir, pattern = "Waterfall Plot Blank\\.dat$", full.names = TRUE)

# Loop through each ABS file
for (file in ABS_files) {
  # Read the ABS file
  abs_data <- read.table(abs_file, header = TRUE, sep = "\t")
  
  # Extract the wavelength column (assuming it's named "Wavelength" - adjust if needed)
  abs_wavelengths <- abs_data$Wavelength
}


for (eem_file in EEMs_files) {
  # Read the EEMs file
  eem_data <- read.table(eem_file, header = TRUE, sep = "\t")
  
  # Extract the wavelength column from EEMs (assuming it's named "Wavelength" - adjust if needed)
  eem_wavelengths <- eem_data$Wavelength
  
  # Check if all ABS wavelengths are within the range of EEMs wavelengths
  if (all(abs_wavelengths >= min(eem_wavelengths) & abs_wavelengths <= max(eem_wavelengths))) {
    cat("Wavelengths in", abs_file, "are within the range of", eem_file, "\n")
  } else {
    cat("Warning: Wavelengths in", abs_file, "are out of range in", eem_file, "\n")
  }
}



