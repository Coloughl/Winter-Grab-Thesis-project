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
library(tidyverse)

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

DINP <- read_excel("1-Data/Nutrients/DissolvedNP.xlsx")

NH4 <- read_excel("1-Data/Nutrients/NH4.xlsx")

NOx <- read_excel("1-Data/Nutrients/NOx.xlsx")

SRP <- read_excel("1-Data/Nutrients/SRP.xlsx")

TOC <- read.csv("1-Data/DOC_TN/WG2_24_TOC_TN.csv")


#Filtering DOC and TN data
DOC <- TOC %>% 
  filter(`NPOC.LOD.flag` != ">RANGE",
         `NPOC.LOD.flag` != "<LOD")
DOC <- DOC %>%
  arrange(Station, date) %>%       # optional: pick which order if you care
  distinct(Station, date, .keep_all = TRUE) %>% 
  select(-Lake) %>% 
  select(-TN)

TN <- TOC %>% 
  filter(`TN.LOD.flag` != ">RANGE",
         `TN.LOD.flag` != "<LOD") %>% 
  mutate(Month = NULL,
         Lake = NULL,
         NPOC = NULL,
         NPOC.LOD.flag = NULL,
         TN.LOD.flag = NULL,
         Notes = NULL)

TN <- TN %>%
  group_by(Station, date) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE),
            .groups = "drop")


#Merging EEMs data with Sites
WG_EEMs <- EEMs %>% 
  left_join(Sites,
            by = "Station",
            relationship = "many-to-one")

#Making Seasons and Years
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




#Merging all carbon data into one file
Carbon <- EEMs_seasoned %>% 
  left_join(DOC, by = c("Station", "date"),
            relationship = "many-to-one") %>% 
  left_join(TN, by = c("Station", "date"),
            relationship = "many-to-one") %>% 
  mutate(SUVA254 = (a254/NPOC))

Carbon_filtered <- Carbon %>%
  group_by(Station, date) %>%
  filter(
    # if there is ANY 1 s run in this Station+date group, keep only those:
    if (any(integration_time == "1s")) {
      integration_time == "1s"
    } else {
      # otherwise keep whatever you have (i.e. only "01s")
      TRUE
    }
  ) %>%
  ungroup()




WG_BP <- BP %>% 
  left_join(Sites,
            by = "Station",
            relationship = "many-to-one") %>% 
  mutate(Season = as.factor(Season)) 
  WG_BP <- WG_BP[-27,]
WG_BP$Season <- factor(WG_BP$Season, levels = c("Summer", "Spring", "Winter"))
WG_BP$Year <- factor(WG_BP$Year, levels = c("2025", "2024"))







ggplot(WG_BP, aes(x = TdR_nM, y = Leu_nM, fill = factor(Year), shape = factor(Season))) +
  labs(x = expression(paste(, "nmol Thymidine L"^-1, "d"^-1,)), 
       y = expression(paste(, "nmol Leucine L"^-1, "d"^-1,)),
       fill = "Year", shape = "Season") +
  scale_fill_manual(values = c("2025" = "#87CEDA",  # Lighter, vibrant blue
                               "2024" = "#E50245")) +
  scale_shape_manual(values = c("Summer" = 21, "Spring" = 22, "Winter" = 24)) +
  geom_point(size = 5, alpha = 0.7) +  # Black outline for points, fill color by Season
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkgray", linewidth = 3)+
  facet_wrap(~ Lake.x)
  xlim(0, 0.5) +  # Set x-axis limit to 0.3
  ylim(0, 15)

# Legend for graph above ^^^^^
  theme(axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 22, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(size = 22, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = c(0.1, 0.8),
        axis.ticks.length = unit(-0.35, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 28)) 
  
  
  
aov(`Leu.TdR` ~ Lake.x + Year + Season, data = WG_BP)
t.test(`Leu.TdR` ~ Year, data = WG_BP)  



