#EEMs file alteration code ----
#Created by: Connor O'Loughlin
#Used ChatGPT-4o to help develop code and troubleshoot
#Last update: 5/2/25 by CCO

#The objective of this pipeline is to format and trim EEMs and ABS files to be analyzed
#This pipeline was designed for and by the Vick-Majors microbial ecology and biogeochemistry lab
#Files should be exported from a Horiba Aqualog using the HJY export process
#Use this pipeline BEFORE attempting the EEMs Analysis pipeline

library(tidyverse)
library(dplyr)


getwd()
setwd("C:/Users/ccolo/OneDrive/Documents/GitHub/Winter-Grab-Thesis-project/1-Data/CDOM/Winter grab/WinterGrab/")

#Grabbing files ----

#Making input directories for ABS and EEMs data

ABS_input_dir <- "27Jun24"

ABS_output_dir <- "27Jun24/ABS_corrected"  


#Making output directories for ABS and EEMs data
EEMs_input_dir <- "27Jun24"
EEMs_output_dir <- "27Jun24/EEMs_corrected"
BEEMs_output_dir <- "27Jun24/EEMs_corrected"


# List all EEMs and ABS files ending in .dat
#These lines are very specific to each file type so lumping all the files into one folder is fine
#Files will be separated into different folders in the loops afterward 

#Grabbing all ABS files and making a list variable
ABS_files <- list.files(path = ABS_input_dir, pattern = "\\(01\\) - Abs Spectra Graphs\\.dat$", full.names = TRUE)
print(ABS_files)

#Grabbing all EEMs files and making a list variable, then removing any blank files from the list
EEMs_files <- list.files(path = EEMs_input_dir, pattern = "\\(01\\) - Sample - Blank Waterfall Plot\\.dat$", full.names = TRUE)
print(EEMs_files)

#This line is what grabs the blank file
BEEMs_files <- list.files(path = EEMs_input_dir, pattern = "\\(01\\) - Waterfall Plot Blank\\.dat$", full.names = TRUE)
print(BEEMs_files)

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

#Check to see if the names are exactly the same (necessary for EEMs pipeline)
if (setequal(ABS_names, EEMs_names)) {
  print("✅ Sample names match between ABS and EEMs.")
} else {
  print("❌ Sample names do NOT match.")
}






#Removing the necessary rows and columns from each file then saving to a new folder ----

for (file in ABS_files) {
  # Read the space-separated .dat file, skipping the first 3 lines
  data <- read.table(file, skip = 3, sep = "\t", header = FALSE, fill = TRUE)
  
  # Remove columns 2 through 9 and column 11. Leaves only OD column
  data_modified <- data[-c(2,3), -c(2:9,11)]
  
  
  #Removing the ABS file "pattern" and just leaving us with the sample name
  #Then rewriting as a .csv
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
  
  # Remove rows 2
  #Removes an extraneous row containing no actual data.
  data <- data[-2, ]
  
  # Remove rows 239 to 250
  #This is going to have to be adjusted for each method
  #This fits the upper bounds of the EEMs wavelength range into the ABS range
  data <- data[-c(238:252), ]  # Adjust as needed
  
  
  #This is line of code removes rows 2 and 3
  #May need to adjust based upon your method
  #This is to fit the lower bounds of the EEMs wavelength range into the ABS range.
  #data <- data[-c(2,3), ] #adjust as needed
  
  #Reads in the first row as charcaters instead of numeric
  #DO NOT change this. It is required for the rest of the transformations to work
  #It also makes sure that the first row of data are formatted correctly when ultimately exported
  data[] <- lapply(data, as.character)
  
  
  # Blank out cell A1
  data[1, 1] <- ""
  
  # Build output path
  #Removing the EEMs file "pattern" and just leaving us with the sample name
  #Then rewriting as a .csv
  sample_name <- gsub(" \\(01\\) - Sample - Blank Waterfall Plot\\.dat$", "", basename(file))
  new_file_name <- paste0(sample_name, ".csv")
  new_file <- file.path(EEMs_output_dir, new_file_name)
  
  # Write line by line to fully control formatting
  #DO NOT change this. 
  writeLines(
    apply(data, 1, function(row) paste(row, collapse = ",")),
    con = new_file
  )
  
  print(paste("Processed and saved:", new_file))
}



#These three lines of code are setup establish a file and naming scheme for the blank EEMs file
run_date <- "9Apr25"
blank_file_name <- paste0("blank", run_date, ".csv")
blank_file_path <- file.path(BEEMs_output_dir, blank_file_name)


for (file in BEEMs_files) {
  data <- read.table(file, sep = "\t", header = FALSE)
  
  #This may not be necessary for blank EEMs data
  #Since we only use it for normalization
  # Remove rows 239 to 250
  #data_modified <- data[-c(239:250), ]
  
  
  #Reads in the first row as charcters instead of numeric
  #DO NOT change this. It is required for the rest of the transformations to work
  #It also makes sure that the first row of data are formatted correctly when ultimately exported
  data[] <- lapply(data, as.character)
  
  # Blank out cell A1
  #This is done to replicate the blank space in the example data provided by the staRdom package
  # Users -> ccolo (or whatever your user profile is) -> Appdata -> Local -> R -> win-library -> 4.4 -> staRdom -> extdata
  data[1, 1] <- ""
  
  #Renames the file as a blank instead of a sample
  blank_file_path <- file.path(BEEMs_output_dir, blank_file_name)
  
  
  # Save to the new directory with the same filename (no '_modified' added)
  #DO NOT change this. 
  writeLines(
    apply(data, 1, function(row) paste(row, collapse = ",")),
    con = blank_file_path)
  
  print(paste("Processed and saved:", blank_file_name))
}

#Experimental code -----


#The following lines of code are in construction
#Ultimately it will notify you if your EEMs wavelength range is larger than your ABS wavelength range

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



