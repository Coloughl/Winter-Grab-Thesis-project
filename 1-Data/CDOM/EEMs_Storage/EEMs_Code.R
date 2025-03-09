
library("fewsdom")
library("readxl")


getwd()
setwd("C:/Users/ccolo/OneDrive/Documents/GitHub/Winter-Grab-Thesis-Project/9-Presentations/GSC 2025/1-Data/WG2_CDOM/")

run_date <- "Corrected"
project <- "26Jun24/"
prjpath <- paste("C:/Users/ccolo/OneDrive/Documents/GitHub/Winter-Grab-Thesis-project/9-Presentations/GSC 2025/1-Data/WG2_CDOM/",
                 project, run_date, sep="")


metadata <- read_xlsx(file.path(prjpath, "metadata_table.xlsx"))

run_eems(prjpath = prjpath, meta_name = "metadata_table.xlsx")
run_eems
data_process[[1]]
data_process[[2]]
abs_preprocess
