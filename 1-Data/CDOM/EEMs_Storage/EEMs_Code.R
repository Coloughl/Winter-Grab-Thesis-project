
library("fewsdom")
library("readxl")


getwd()
setwd("C:/Users/ccolo/OneDrive/Documents/GitHub/Winter-Grab-Thesis-project/1-Data/EEMs_Storage/")

run_date <- "Corrected"
project <- "T0/"
prjpath <- paste("C:/Users/ccolo/OneDrive/Documents/GitHub/Winter-Grab-Thesis-project/1-Data/EEMs_Storage/",
                 project, run_date, sep="")


metadata <- read_xlsx(file.path(prjpath, "metadata_table.xlsx"))

run_eems(prjpath = prjpath, meta_name = "metadata_table.xlsx")
run_eems
data_process[[1]]
data_process[[2]]
abs_preprocess
