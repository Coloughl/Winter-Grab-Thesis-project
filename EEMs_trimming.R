#EEMs file alteration code

getwd()


getwd()
setwd("C:/Users/ccolo/OneDrive/Documents/GitHub/Winter-Grab-Thesis-project/staRdom_Practice")

#Making input directories for ABS and EEMs data
ABS_input_dir <- "ABS"
ABS_output_dir <- "ABS"  


#Making output directories for ABS and EEMs data
EEMs_input_dir <- "EEMs"
EEMs_output_dir <- "EEMs"

#Making output directories for ABS and EEMs data
BEEMs_input_dir <- "EEMs"
BEEMs_output_dir <- "EEMs"


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








EEMs_folderpath <- "EEMS"
ABS_folderpath <- "ABS"

EEMs_Files <- list.files(path = EEMs_folderpath, full.names = TRUE)
ABS_Files <- list.files (path = ABS_folderpath, full.names = TRUE)
View(ABS_Files)


for (file in ABS_Files) {
  data <- read.csv(file)
  
  # Remove columns as needed
  #data_modified <- select(data, -column1, -column2)
  data_modified <- data[ -c(1,2), -c(2, 3, 4, 5, 6, 7, 8, 9, 11)]
  
  # Save to a new file (with "_modified" added to the original filename)
  new_file <- gsub(".csv", "_modified.csv", file)
  write.csv(data_modified, file = new_file, row.names = FALSE)
  
  print(paste("Processed and saved:", new_file))
}


for (file in EEMs_Files) {
  data <- read.csv(file)
  
  # Remove rows as needed
  data_modified <- data[ -c(239:250), ]
  
  # Save to a new file (with "_modified" added to the original filename)
  new_file <- gsub(".csv", "_modified.csv", file)
  write.csv(data_modified, file = new_file, row.names = FALSE)
  
  print(paste("Processed and saved:", new_file))
}


