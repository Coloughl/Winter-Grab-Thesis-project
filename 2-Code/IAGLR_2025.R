#Foreword -----
#This code was developed for the analysis and visualization of WG2 data for IAGLR 2025
#Chatgpt was used to help write and troubleshoot code
#Made by Connor O'Louglin
#Last updated: 5/13/25


#Loading in packages
library(readxl)
library(dplyr)
library(stringr)

#Fetching and setting Working directory
getwd()
setwd("C:/Users/ccolo/OneDrive/Documents/GitHub/Winter-Grab-Thesis-project/1-Data/CDOM/Winter grab")

#Creating function to help merge EEMs data
parse_sample <- function(df){
  df %>%
    mutate(
      integration_time = str_extract(sample, "(?:01s|1s|01)$") %>%
        replace_na("01s"),
      sample_trimmed   = str_remove(sample, "(?:01s|1s|01)$"),
      date             = str_extract(sample_trimmed, "(?:\\d{1,2})?[A-Za-z]{3}\\d{2}$"),
      location         = str_remove(sample_trimmed, "(?:\\d{1,2})?[A-Za-z]{3}\\d{2}$")
    ) %>%
    select(-sample_trimmed)
}


#Loading in Data ----

Indices <- read_excel("WinterGrab_indices.xlsx") %>% parse_sample()
Parms   <- read_excel("WinterGrab_slope_parms.xlsx") %>% parse_sample()

EEMs <- full_join(Indices, Parms, by = c("sample","integration_time","date","location"),
                  suffix = c("", "_parm")) %>%
  # if Parms adds any truly distinct columns that ended up with _parm, keep them,
  # but drop the duplicated ones from Parms
  select(-ends_with("_parm"))

