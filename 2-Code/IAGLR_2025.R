#Foreword -----
#This code was developed for the analysis and visualization of WG2 data for IAGLR 2025
#Chatgpt was used to help write and troubleshoot code
#Made by Connor O'Louglin
#Last updated: 5/13/25


#Loading in packages
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

#Fetching and setting Working directory
getwd()
setwd("C:/Users/ccolo/OneDrive/Documents/GitHub/Winter-Grab-Thesis-project/")

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

Indices <- read_excel("WinterGrab_indices.xlsx") %>% parse_sample()
Parms   <- read_excel("WinterGrab_slope_parms.xlsx") %>% parse_sample()

EEMs <- full_join(Indices, Parms, by = c("sample","integration_time","date","location"),
                  suffix = c("", "_parm")) %>%
  # if Parms adds any truly distinct columns that ended up with _parm, keep them,
  # but drop the duplicated ones from Parms
  select(-ends_with("_parm"))

write.csv(EEMs, file = "EEMs.csv")

WG_EEMs <- EEMs %>% 
  left_join(Sites,
            by = "Station",
            relationship = "many-to-one")

EEMs_seasoned <- WG_EEMs %>%
  mutate(
    # 1) Month abbreviation (e.g. "Feb", "May", etc.)
    month_abbrev = str_extract(date, "[A-Za-z]{3}"),
    
    # 2) Year: last two digits, coerced to 2000-series
    year2        = str_extract(date, "\\d{2}$"),
    year         = as.integer(paste0("20", year2)),
    
    # 3) Season based on month_abbrev
    season = case_when(
      month_abbrev %in% c("Feb", "Mar")    ~ "Winter",
      month_abbrev == "May"                ~ "Spring",
      month_abbrev %in% c("Jun", "Aug")    ~ "Summer",
      TRUE                                 ~ NA_character_
    )
  ) %>%
  # (optional) drop helpers if you only want date, year, season
  select(-month_abbrev, -year2)


Summer_BP <- read.csv("1-Data/BActerial_Production/Aug_24_BP.csv")%>% 
  mutate(Year = "2024",
         Season = "Summer")

Spring_BP <- read.csv("1-Data/BActerial_Production/May_24_BP.csv")%>% 
  mutate(Year = "2024",
         Season = "Spring")

Winter_24_BP <- read.csv("1-Data/BActerial_Production/Feb_24_BP.csv") %>% 
  mutate(Year = "2024",
         Season = "Winter")

Winter_25_BP <- read_excel("1-Data/BActerial_Production/Feb_25_BP.xlsx") %>% 
  mutate(Month = "February",
         Year = "2025",
         Season = "Winter",
         Leu = `Leu C/L/d`,
         `Leu C/L/d` = NULL,
         TdR = `TdR C/L/d`,
         `TdR C/L/d` = NULL,
         Leu.TdR = `Leu:TdR`,
         `Leu:TdR` = NULL)


WG_BP_long <- bind_rows(
  Winter_25_BP,
  Winter_24_BP,
  Spring_BP,
  Summer_BP
)

# clean up ordering of columns if you like
WG_BP_long <- WG_BP_long %>%
  select(Sample, Season, Year, Month, everything()) %>% 
  mutate(Station = Sample,
         Sample = NULL)

write.csv(WG_BP_long, file = "WG_BP_long.csv")




#Loading in Data ----
EEMs <- read.csv("1-Data/CDOM/Winter grab/EEMs.csv")

Sites <- read_excel("1-Data/WG2_Sites.xlsx")

BP <- read.csv("1-Data/Bacterial_Production/WG_BP_long.csv")

WG_EEMs <- EEMs %>% 
  left_join(Sites,
            by = "Station",
            relationship = "many-to-one")

EEMs_seasoned <- WG_EEMs %>%
  mutate(
    # 1) Month abbreviation (e.g. "Feb", "May", etc.)
    month_abbrev = str_extract(date, "[A-Za-z]{3}"),
    
    # 2) Year: last two digits, coerced to 2000-series
    year2        = str_extract(date, "\\d{2}$"),
    year         = as.integer(paste0("20", year2)),
    
    # 3) Season based on month_abbrev
    season = case_when(
      month_abbrev %in% c("Feb", "Mar")    ~ "Winter",
      month_abbrev == "May"                ~ "Spring",
      month_abbrev %in% c("Jun", "Aug")    ~ "Summer",
      TRUE                                 ~ NA_character_
    )
  ) %>%
  # (optional) drop helpers if you only want date, year, season
  select(-month_abbrev, -year2)


WG_BP <- BP %>% 
  left_join(Sites,
            by = "Station",
            relationship = "many-to-one")

