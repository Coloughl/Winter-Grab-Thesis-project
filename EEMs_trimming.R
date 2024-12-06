#EEMs file alteration code



getwd()
setwd("C:/Users/ccolo/OneDrive/Documents/GitHub/Winter-Grab-Thesis-project/1-Data/EEMs_Storage/")

#Making input directories for ABS and EEMs data
ABS_input_dir <- "4Nov24/ABS"
ABS_output_dir <- "4Nov24/Corrected"  


#Making output directories for ABS and EEMs data
EEMs_input_dir <- "4Nov24/EEMs"
EEMs_output_dir <- "4Nov24/Corrected"

#Making output directories for ABS and EEMs data
BEEMs_input_dir <- "4Nov24/EEMs"
BEEMs_output_dir <- "4Nov24/Corrected"


# List all files ending in "_ABS.dat" or "_EEMs.dat"
ABS_files <- list.files(path = ABS_input_dir, pattern = "Abs Spectra Graphs\\.dat$", full.names = TRUE)
EEMs_files <- list.files(path = EEMs_input_dir, pattern = "Waterfall Plot Sample\\.dat$", full.names = TRUE)
BEEMs_files <- list.files(path = BEEMs_input_dir, pattern = "Waterfall Plot Blank\\.dat$", full.names = TRUE)



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
for (file in BEEMs_files) {
  # Construct the output filename
  output_file <- file.path(BEEMs_output_dir ,gsub("\\.dat$", ".csv", basename(file)))
  
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








EEMs_folderpath <- "4Nov24"
ABS_folderpath <- "4Nov24"

ABS_files <- list.files(path = ABS_input_dir, pattern = "Abs Spectra Graphs\\.dat$", full.names = TRUE)
EEMs_files <- list.files(path = EEMs_input_dir, pattern = "Waterfall Plot Sample\\.dat$", full.names = TRUE)
BEEMs_files <- list.files(path = EEMs_input_dir, pattern = "Waterfall Plot Blank\\.dat$", full.names = TRUE)
View(ABS_files)

for (file in ABS_files) {
  # Read the .dat file (adjust 'sep' as needed, e.g., sep = "\t" for tab-delimited)
  data <- read.table(file, sep = "\t", header = FALSE, fill = TRUE) # adjust 'sep' and 'header' as needed
  
  # Modify the data: Remove the 11th column
  data_modified <- data[-c(1,2,3),-c(2,3,4,5,6,7,8,9,11)]
  
  colnames(data_modified) <- NULL
  
  
  # Define the new file path in the output directory
  new_file_name <-gsub(" \\(01\\) - Abs Spectra Graphs", "", basename(file))
  
  new_file <- file.path(ABS_output_dir ,gsub("\\.dat$", ".csv", new_file_name))
  
  
  # Save to the new directory with the same filename (no overwriting)
  write.csv(data_modified, file = new_file, row.names = FALSE, col.names = TRUE) # adjust 'sep' as needed
  
  print(paste("Processed and saved:", new_file))
}


for (file in EEMs_files) {
  # Read the .dat file (adjust 'sep' as needed)
  data <- read.table(file, sep = "\t", header = TRUE)
  
  # Remove rows 239 to 250
  data_modified <- data[-c(239:250), ]
  
  new_file_name <-gsub(" \\(01\\) - Waterfall Plot Sample", "", basename(file))
  
  new_file <- file.path(EEMs_output_dir ,gsub("\\.dat$", ".csv", new_file_name))

  
  # Save to the new directory with the same filename (no '_modified' added)
  write.csv(data_modified, file = new_file, row.names = FALSE, col.names = TRUE)
  
  print(paste("Processed and saved:", new_file))
}

rundate <- "4Nov24"
blank_file_name <- paste0("Blank", rundate)

for (file in BEEMs_files) {
  data <- read.table(file, sep = "\t", header = TRUE)
  
  # Remove rows 239 to 250
  data_modified <- data[-c(239:250), ]
  
  new_file_name <- blank_file_name
  
  # Define the new file path in the output directory
  new_file <- file.path(BEEMs_output_dir ,gsub("\\.dat$", ".csv", new_file_name))  # 'basename' keeps the original filename
  
  # Save to the new directory with the same filename (no '_modified' added)
  write.csv(data_modified, file = new_file, row.names = FALSE, col.names = TRUE)
  
  print(paste("Processed and saved:", new_file))
}




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



