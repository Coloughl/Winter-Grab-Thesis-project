#EEMs file alteration code



getwd()
setwd("C:/Users/ccolo/OneDrive/Documents/GitHub/Winter-Grab-Thesis-project/1-Data/Practice/")

#Grabbing files ----

#Making input directories for ABS and EEMs data

ABS_input_dir <- "ABS"

ABS_output_dir <- "ABS_corrected"  


#Making output directories for ABS and EEMs data
EEMs_input_dir <- "EEMs"
EEMs_output_dir <- "EEMs_corrected"
BEEMs_output_dir <- "EEMs_corrected"


# List all files ending in "_ABS.dat" or "_EEMs.dat"

#Grabbing all ABS files and making a list variable
ABS_files <- list.files(path = ABS_input_dir, pattern = ".csv$", full.names = TRUE)

#Grabbing all EEMs files and making a list variable, then removing any blank files from the list
EEMs_files <- list.files(path = EEMs_input_dir, pattern = ".csv$", full.names = TRUE)
EEMs_files <- EEMs_files[!grepl("blank", basename(EEMs_files), ignore.case = TRUE)]

#This line is what grabs the blank file
BEEMs_files <- list.files(path = EEMs_input_dir, pattern = "blank.*\\csv$", full.names = TRUE)


EEMs_folderpath <- "4Nov24"
ABS_folderpath <- "4Nov24"

ABS_files <- list.files(path = ABS_input_dir, pattern = "Abs Spectra Graphs\\.dat$", full.names = TRUE)
EEMs_files <- list.files(path = EEMs_input_dir, pattern = "Waterfall Plot Sample\\.dat$", full.names = TRUE)
BEEMs_files <- list.files(path = EEMs_input_dir, pattern = "Waterfall Plot Blank\\.dat$", full.names = TRUE)
View(ABS_files)


#Removing the nessecary rows and columns from each file then saving to a new folder ----


for (file in ABS_files) {
  # Read the .dat file (adjust 'sep' as needed, e.g., sep = "\t" for tab-delimited)
  data <- read.csv(file, header = FALSE)
  
  
  # Modify the data: Remove the 11th column
  data_modified <- data[-c(1,2,3),-c(2,3,4,5,6,7,8,9,11)]
  
  colnames(data_modified) <- NULL
  
  
  # Define the new file path in the output directory
  new_file_name <- basename(file)

  new_file <- file.path(ABS_output_dir , new_file_name)
  
  
  # Save to the new directory with the same filename (no overwriting)
  write.csv(data_modified, file = new_file, row.names = FALSE, col.names = TRUE, quote = FALSE) # adjust 'sep' as needed
  
  print(paste("Processed and saved:", new_file))
}





for (file in EEMs_files) {
  # Read the CSV with headers
  data <- read.csv(file, header = FALSE, stringsAsFactors = FALSE)
  
  # Remove rows 2 and 3 (if needed)
  data <- data[-2, ]
  
  # Remove rows 239 to 250
  data <- data[-c(243:252), ]  # Adjust as needed
  
  data <- data[-2, ]
  
  # Convert entire data frame to character (to allow blanking A1)
  data[] <- lapply(data, as.character)
  
  # Blank out cell A1
  data[1, 1] <- ""
  
  # Build output path
  new_file_name <- basename(file)
  new_file <- file.path(EEMs_output_dir, new_file_name)
  
  # Write line by line to fully control formatting
  writeLines(
    apply(data, 1, function(row) paste(row, collapse = ",")),
    con = new_file
  )
  
  print(paste("Processed and saved:", new_file))
}



for (file in BEEMs_files) {
  data <- read.csv(file, header = FALSE, stringsAsFactors = FALSE)
  
  
  # Remove rows 239 to 250
  data_modified <- data[-c(239:250), ]
  
  
  data[] <- lapply(data, as.character)
  
  # Blank out cell A1
  data[1, 1] <- ""
  
  new_file_name <- basename(file)
  
  # Define the new file path in the output directory
  new_file <- file.path(BEEMs_output_dir ,new_file_name)  # 'basename' keeps the original filename
  
  # Save to the new directory with the same filename (no '_modified' added)
  #
  writeLines(
    apply(data, 1, function(row) paste(row, collapse = ",")),
    con = new_file)
  
  #write.csv(data_modified, file = new_file, row.names = FALSE, col.names = TRUE)
  
  print(paste("Processed and saved:", new_file))
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



