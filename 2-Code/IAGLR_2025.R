#Foreword -----
#This code was developed for the analysis and visualization of WG2 data for IAGLR 2025
#Chatgpt was used to help write and troubleshoot code
#Made by Connor O'Louglin
#Last updated: 5/27/25


#Loading in packages
library(reshape2)
library(corrplot)
library(GGally)
library(rlang)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
library(car)
library(patchwork)
library(lmodel2)
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
DINP$Season <- factor(DINP$Season, levels = c("Summer", "Spring", "Winter"))
DINP$Year <- factor(DINP$Year, levels = c("2025", "2024"))

NH4 <- read_excel("1-Data/Nutrients/NH4.xlsx")
NH4$Season <- factor(NH4$Season, levels = c("Summer", "Spring", "Winter"))
NH4$Year <- factor(NH4$Year, levels = c("2025", "2024"))

NOx <- read_excel("1-Data/Nutrients/NOx.xlsx")
NOx$Season <- factor(NOx$Season, levels = c("Summer", "Spring", "Winter"))
NOx$Year <- factor(NOx$Year, levels = c("2025", "2024"))

SRP <- read_excel("1-Data/Nutrients/SRP.xlsx")
SRP$Season <- factor(SRP$Season, levels = c("Summer", "Spring", "Winter"))
SRP$Year <- factor(SRP$Year, levels = c("2025", "2024"))

TOC <- read.csv("1-Data/DOC_TN/WG2_24_TOC_TN.csv")

Chla <-read_excel("1-Data/Chla/Chla.xlsx")
Chla$Season <- factor(Chla$Season, levels = c("Summer", "Spring", "Winter"))
Chla$Year <- factor(Chla$Year, levels = c("2025", "2024"))

#Filtering DOC and TN data----
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


#Merging EEMs data with Sites ----
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
    Year         = as.integer(paste0("20", year2)),
    
    # 3) Season based on month_abbrev
    Season = case_when(
      month_abbrev %in% c("Feb", "Mar")    ~ "Winter",
      month_abbrev == "May"                ~ "Spring",
      month_abbrev %in% c("Jun", "Aug")    ~ "Summer",
      TRUE                                 ~ NA_character_
    )
  ) %>%
  # (optional) drop helpers if you only want date, year, season
  select(-month_abbrev, -year2)


EEMs_seasoned <- EEMs_seasoned %>%
  mutate(
    # 1) Remove any leading “Lake ” if present
    Lake = str_remove(Lake, "^Lake\\s*"),
    # 2) Trim whitespace (in case there were leading/trailing spaces)
    Lake = str_trim(Lake),
    # 3) Re-add the prefix so every entry reads “Lake XYZ”
    Lake = paste("Lake", Lake)
  ) %>%
  mutate(
    Lake = as.character(Lake), 
    Lake = if_else(
      is.na(Lake) | Lake == "Lake NA",
      "Lake Erie",
      Lake
    ),
    Lake = factor(Lake)      
  ) %>%
  mutate(
    Year = as.character(Year),  
    Year = if_else(
      is.na(Year) & Season %in% c("Spring","Summer"),
      "2024",
      Year
    ),
    Year = factor(Year, levels = c("2024","2025"))
  ) %>%
  mutate(
    Lake = case_when(
      Lake %in% c("Lake St. Clair", "Lake St Clair") ~ "Lake St. Clair",
      TRUE                                          ~ Lake
    )
  )




#Merging all carbon data into one file ----
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
Carbon_filtered$Season <- factor(Carbon_filtered$Season, levels = c("Summer", "Spring", "Winter"))
Carbon_filtered$Year <- factor(Carbon_filtered$Year, levels = c("2025", "2024"))


#Merging BP data in Carbon data ----
WG_BP <- BP %>% 
  left_join(Sites,
            by = "Station",
            relationship = "many-to-one") %>% 
  mutate(Season = as.factor(Season)) 
  WG_BP <- WG_BP[-27,]
WG_BP$Season <- factor(WG_BP$Season, levels = c("Summer", "Spring", "Winter"))
WG_BP$Year <- factor(WG_BP$Year, levels = c("2025", "2024"))


carbon_to_merge <- Carbon_filtered %>% 
  select(-PI_name, -Lake)
carbon_to_merge$Season <- factor(carbon_to_merge$Season, levels = c("Summer", "Spring", "Winter"))
carbon_to_merge$Year <- factor(carbon_to_merge$Year, levels = c("2025", "2024"))

BP <- WG_BP %>% 
  left_join(carbon_to_merge, by = c("Station", "Year","Season")) %>% 
  

#Prepping nutrient and Chla data ----
NH4_summary <- NH4 %>%
  group_by(Station, Season, Year) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>% 
  distinct(Station, Season, Year, .keep_all = TRUE)

NOx_summary <- NOx %>%
  group_by(Station, Season, Year) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )%>% 
  distinct(Station, Season, Year, .keep_all = TRUE)

SRP_summary <- SRP %>%
  group_by(Station, Season, Year) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )%>% 
  distinct(Station, Season, Year, .keep_all = TRUE)


DINP_summary <- DINP %>%
  group_by(Station, Season, Year) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )%>% 
  distinct(Station, Season, Year, .keep_all = TRUE)

Chla <- Chla %>%
  mutate(
    Season = factor(Season,
                    levels = levels(carbon_to_merge$Season)),
    Year   = factor(as.character(Year),
                    levels = levels(carbon_to_merge$Year))
  )
Chla <- Chla %>%
  # if it’s a factor:
  mutate(`Chla ug/L` = as.character(`Chla ug/L`)) %>%
  # then numeric:
  mutate(`Chla ug/L` = as.numeric(`Chla ug/L`))


Chla_summary <- Chla %>%
  group_by(Station, Season, Year) %>%
  summarise(
    Chla = mean(`Chla ug/L`, na.rm = TRUE),
    .groups = "drop"
  )



#Making master data frame ----
merged_data <- carbon_to_merge %>%
  left_join(WG_BP, by = c("Station", "Season","Year"), relationship = "many-to-one") %>%  
  left_join(NH4_summary, by = c("Station", "Season","Year"), relationship = "many-to-one") %>% 
  left_join(NOx_summary, by = c("Station", "Season","Year"), relationship = "many-to-one") %>% 
  left_join(SRP_summary, by = c("Station", "Season","Year"), relationship = "many-to-one") %>% 
  left_join(DINP_summary, by = c("Station", "Season","Year"), relationship = "many-to-one") %>%  
  left_join(Chla_summary, by = c("Station", "Season","Year"), relationship = "many-to-one")

master_clean <- merged_data %>%
  select(-ends_with(".y"))

# 3. (Optional) Rename the remaining ".x" suffixed columns to remove the ".x"
names(master_clean) <- sub("\\.x$", "", names(master_clean))

master_clean <- master_clean %>%
  mutate(
    # 1) Remove any leading “Lake ” if present
    Lake = str_remove(Lake, "^Lake\\s*"),
    # 2) Trim whitespace (in case there were leading/trailing spaces)
    Lake = str_trim(Lake),
    # 3) Re-add the prefix so every entry reads “Lake XYZ”
    Lake = paste("Lake", Lake)
  )

master_clean <- master_clean %>%
  mutate(
    Lake = as.character(Lake),  # if it’s a factor, turn it into character first
    Lake = if_else(
      is.na(Lake) | Lake == "Lake NA",
      "Lake Erie",
      Lake
    ),
    Lake = factor(Lake)         # back to factor if you want
  )

master_clean <- master_clean %>%
  mutate(
    Year = as.character(Year),  
    Year = if_else(
      is.na(Year) & Season %in% c("Spring","Summer"),
      "2024",
      Year
    ),
    Year = factor(Year, levels = c("2024","2025"))
  )
master_clean <- master_clean %>%
  mutate(
    Lake = case_when(
      Lake %in% c("Lake St. Clair", "Lake St Clair") ~ "Lake St. Clair",
      TRUE                                          ~ Lake
    )
  )


#Plotting all vars against Leu:TdR-----
out_dir <- "C:/Users/ccolo/OneDrive/Documents/GitHub/Winter-Grab-Thesis-project/8-Figures/IAGLR"

if (!dir.exists("plots")) dir.create("plots")

# 3) get all numeric predictors except the response
for (var in numeric_vars) {
  # 1) turn the string into a symbol for aes()
  var_sym <- sym(var)
  
  # 2) sanitize the variable name for the filename
  safe_var <- gsub("[^A-Za-z0-9]", "_", var)
  fname    <- file.path(out_dir, paste0("TdR_", safe_var, ".png"))
  
  # 3) open a PNG device (6"x4" at 300dpi)
  png(
    filename = fname,
    width    = 6,     
    height   = 4,     
    units    = "in",  
    res      = 300    
  )
  
  # 4) draw the plot (you can filter out NA if you like)
  print(
    ggplot(master_clean, aes(x = !!var_sym, y = TdR_nM)) +
      geom_point(size = 2, na.rm = TRUE) +
      labs(
        title = paste("TdR_nM vs", var),
        x     = var,
        y     = "Thymidine nM"
      ) +
      theme_classic(base_size = 14)
  )
  
  # 5) close the device
  dev.off()
}




# BP Plots ----
z_cutoff <- 3

clean_BP <- BP%>%
  # 1. compute the z‐score for Leu.TdR
  mutate(z_Leu = (Leu_nM - mean(Leu_nM, na.rm = TRUE)) /
           sd(Leu_nM, na.rm = TRUE),
         z_TdR = (TdR_nM - mean(TdR_nM, na.rm = TRUE)) / 
           sd(TdR_nM, na.rm = TRUE)) %>% 
  filter(abs(z_Leu) <= z_cutoff) %>% 
  filter(abs(z_TdR) <= z_cutoff) %>% 
  select(-z_Leu) %>% 
  select(-z_TdR) %>% 
  rename(Lake = Lake.x) %>%  
  mutate(Lake = paste("Lake", Lake))


ggplot(clean_BP, aes(x = TdR_nM, y = Leu_nM, fill = factor(Year), shape = factor(Season))) +
  labs(x = expression(paste(, "nmol Thymidine L"^-1, "d"^-1,)), 
       y = expression(paste(, "nmol Leucine L"^-1, "d"^-1,)),
       fill = "Year", shape = "Season") +
  scale_fill_manual(values = c("2025" = "#87CEDA",  # Lighter, vibrant blue
                               "2024" = "#E50245")) +
  scale_shape_manual(values = c("Summer" = 21, "Spring" = 22, "Winter" = 24)) +
  guides(
    fill = guide_legend(
      override.aes = list(shape = 21, color = "black")
    )
  ) +
  geom_point(size = 8, alpha = 0.7) +  # Black outline for points, fill color by Season
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkgray", linewidth = 3)+
  facet_wrap(~ Lake) +
  theme_classic(base_size = 14) +
  theme(
    # axis text
    axis.title.x     = element_text(size = 24),
    axis.title.y     = element_text(size = 24),
    axis.text.x      = element_text(size = 22, margin = margin(t = 6, r = 0, b = 6, l = 0)),
    axis.text.y      = element_text(size = 22, margin = margin(t = 0, r = 6, b = 0, l = 6)),
    
    # legend
    legend.title     = element_text(size = 28),
    legend.text      = element_text(size = 20),
    
    # facet text
    strip.text.x     = element_text(size = 24, face = "bold"),
    
    # box around each panel
    panel.border     = element_rect(color = "black", fill = NA, size = 1),
    
    # spacing between panels
    panel.spacing    = unit(0.5, "cm")
  )


#EEMs plots. Additional data manipulation ----
clean_EEMs <- Carbon_filtered %>%
  mutate(z_hix = (hix - mean(hix, na.rm = TRUE)) /
           sd(hix, na.rm = TRUE),
         z_bix = (bix - mean(bix, na.rm = TRUE)) / 
           sd(bix, na.rm = TRUE)) %>% 
  filter(abs(z_hix) <= z_cutoff) %>% 
  filter(abs(z_bix) <= z_cutoff) %>% 
  select(-z_hix) %>% 
  select(-z_bix) 

hix_summary <- clean_EEMs %>%
  mutate(SeasonYear = paste(Season, Year)) %>%
  group_by(Lake, SeasonYear) %>%
  summarise(
    n        = sum(!is.na(hix)),
    mean_hix = mean(hix, na.rm = TRUE),
    se_hix   = if_else(n > 1,
                     sd(hix, na.rm = TRUE) / sqrt(n),
                     0),
    .groups  = "drop"
  ) %>%
  mutate(
    SeasonYear = factor(SeasonYear,
                        levels = c("Winter 2024","Spring 2024","Summer 2024","Winter 2025")
    )
  )

bix_summary <- clean_EEMs %>%
  mutate(SeasonYear = paste(Season, Year)) %>%
  group_by(Lake, SeasonYear) %>%
  summarise(
    n       = sum(!is.na(bix)),
    mean_b  = mean(bix, na.rm = TRUE),
    se_b   = if_else(n > 1,
                       sd(bix, na.rm = TRUE) / sqrt(n),
                       0),
    .groups = "drop"
  ) %>%
  mutate(
    SeasonYear = factor(SeasonYear,
                        levels = c("Winter 2024","Spring 2024","Summer 2024","Winter 2025")
    )
  )




#Setting offsets for bar charts and color pallette.

offset_h <- 0.05 * max(hix_summary$mean_hix, na.rm = TRUE)

offset_b <- 0.05 * max(bix_summary$mean_b, na.rm = TRUE)

season_palette <- c(
  "Winter 2024" = "#87CEDA",
  "Spring 2024" = "#B6798F",
  "Summer 2024" = "#E50245",
  "Winter 2025" = "#6BDACF"
)

#Making theme for plots
common_theme <- theme_classic(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 45),
    axis.text.y  = element_text(size = 45),
    axis.title   = element_text(size = 45),
    legend.title = element_text(size = 28),
    legend.text  = element_text(size = 20),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    panel.background    = element_rect(fill = NA, colour = NA),
    plot.background     = element_rect(fill = NA, colour = NA),
    legend.background   = element_rect(fill = NA, colour = NA),
    legend.key          = element_rect(fill = NA, colour = NA)
  )

#HIX plot
hix_plot <- ggplot(hix_summary, aes(Lake, mean_hix, fill = SeasonYear)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = mean_hix - se_hix, ymax = mean_hix + se_hix),
                width = 0.2, position = position_dodge(0.8)) +
  geom_text(aes(label = paste0("n=", n),
                y     = mean_hix + se_hix + offset_h),
            position = position_dodge(0.8), vjust = 0, size = 8, fontface = "bold") +
  scale_fill_manual(values = season_palette) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = "Mean HIX (±SE)", fill = "Season & Year") +
  common_theme +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

hix_plot


# BIX 
bix_plot <- ggplot(bix_summary, aes(Lake, mean_b, fill = SeasonYear)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = mean_b - se_b, ymax = mean_b + se_b),
                width = 0.2, position = position_dodge(0.8)) +
  geom_text(aes(label = paste0("n=", n),
                y     = mean_b + se_b + offset_b),
            position = position_dodge(0.8), vjust = 0, size = 8, fontface = "bold") +
  scale_fill_manual(values = season_palette) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  coord_cartesian(clip = "off") +
  labs(x = "Lake", y = "Mean BIX (±SE)") +
  common_theme +
  theme(legend.position = "none")
bix_plot


transparent_theme <- theme(
  panel.background    = element_rect(fill = NA, colour = NA),
  plot.background     = element_rect(fill = NA, colour = NA),
  legend.background   = element_rect(fill = NA, colour = NA),
  legend.key          = element_rect(fill = NA, colour = NA)
)


hix_plot <- hix_plot + transparent_theme +
  guides(fill = guide_legend(
    nrow        = 1,
    byrow       = TRUE,
    keywidth    = unit(2,   "lines"),
    keyheight   = unit(2,   "lines"),
    label.position = "bottom",
    title.position = "top",
    title.hjust = 0.5
  )) +
  theme(
    legend.position    = "top",           
    legend.box         = "horizontal",       
    legend.box.just    = "center",           
    legend.spacing.x   = unit(1.5, "cm"),    
    legend.spacing.y   = unit(0.5, "cm"),    
    legend.title       = element_text(size = 28),
    legend.text        = element_text(size = 24)
  )

bix_plot <- bix_plot + transparent_theme

hix_plot <- hix_plot +
  labs(tag = "A") +
  theme(
    plot.tag           = element_text(size = 36, face = "bold"),
    plot.tag.position  = c(0.02, 0.98)
  )

bix_plot <- bix_plot +
  labs(tag = "B") +
  theme(
    plot.margin        = margin(t = 20, r = 5, b = 5, l = 5, unit = "pt"),
    plot.tag           = element_text(size = 36, face = "bold"),
    plot.tag.position  = c(0.02, 1.05)
  )



combined <- hix_plot /
  plot_spacer() /
  bix_plot +
  plot_layout(heights = c(1, 0.1, 1))


combined

SUVA_summary <- Carbon_filtered %>%
  mutate(SeasonYear = paste(Season, Year)) %>%
  group_by(Lake, SeasonYear) %>%
  mutate(
    SeasonYear = factor(SeasonYear,
                        levels = c("Winter 2024","Spring 2024","Summer 2024")
    )
  )
suva_plot <- SUVA_summary %>% 
  filter(!is.na(SeasonYear)) %>% 
  ggplot(aes(x = SeasonYear, y = SUVA254, fill = SeasonYear)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, color = "black") +
  scale_fill_manual(values = season_palette, drop = TRUE) +
  labs(x = NULL, y = "SUVA254 (L·mg⁻¹·m⁻¹)") +
  facet_wrap(~ Lake) +
  theme(
    legend.position   = "none",
    axis.title.x      = element_text(size = 24),
    axis.title.y      = element_text(size = 24),
    axis.text.x       = element_text(
      size    = 22,
      angle   = 45,
      hjust   = 1,
      margin  = margin(0.5,0.5,0.5,0.5, "cm")
    ),
    axis.text.y       = element_text(
      size    = 22,
      margin  = margin(0.5,0.5,0.5,0.5, "cm")
    ),
    strip.text        = element_text(size = 28, face = "bold"),   # <- bigger facet labels
    panel.grid.major  = element_blank(), 
    panel.grid.minor  = element_blank(),
    panel.background  = element_blank(), 
    axis.line         = element_line(colour = "black"),
    axis.ticks.length = unit(-0.35, "cm")
  )

suva_plot

#DOC Plot ----
clean_DOC <- Carbon_filtered %>%
  mutate(z_doc = (NPOC - mean(NPOC, na.rm = TRUE)) /
           sd(NPOC, na.rm = TRUE)) %>% 
  filter(abs(z_doc) <= z_cutoff) %>% 
  select(-z_doc)  

DOC_summary <- clean_DOC %>%
  mutate(SeasonYear = paste(Season, Year)) %>%
  group_by(Lake, SeasonYear) %>%
  summarise(
    n        = sum(!is.na(NPOC)),
    mean_DOC = mean(NPOC, na.rm = TRUE),
    se_DOC   = if_else(n > 1,
                       sd(NPOC, na.rm = TRUE) / sqrt(n),
                       0),
    .groups  = "drop"
  ) %>%
  mutate(
    SeasonYear = factor(SeasonYear,
                        levels = c("Winter 2024","Spring 2024","Summer 2024","Winter 2025")
    )
  )

uncommon_theme <- theme_classic(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 45),
    axis.text.y  = element_text(size = 45),
    axis.title   = element_text(size = 45),
    legend.title = element_text(size = 30),
    legend.text  = element_text(size = 24),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    panel.background    = element_rect(fill = NA, colour = NA),
    plot.background     = element_rect(fill = NA, colour = NA),
    legend.background   = element_rect(fill = NA, colour = NA),
    legend.key          = element_rect(fill = NA, colour = NA)
  )


offset_doc <- 0.05 * max(DOC_summary$mean_DOC, na.rm = TRUE)

season_palette <- c(
  "Winter 2024" = "#87CEDA",
  "Spring 2024" = "#B6798F",
  "Summer 2024" = "#E50245",
  "Winter 2025" = "#6BDACF"
)


doc_plot <- ggplot(DOC_summary, aes(Lake, mean_DOC, fill = SeasonYear)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = mean_DOC - se_DOC, ymax = mean_DOC + se_DOC),
                width = 0.2, position = position_dodge(0.8)) +
  geom_text(aes(label = paste0("n=", n),
                y     = mean_DOC + se_DOC + offset_doc),
            position = position_dodge(0.8), vjust = 0, size = 16, fontface = "bold") +
  scale_fill_manual(values = season_palette) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  coord_cartesian(clip = "off") +
  labs(x = "Lake", y = expression(paste("Mean DOC Concentration (", "mg L"^-1, ")")), fill = "Season & Year") +
  uncommon_theme 

doc_plot
# ignore this. For Type II linear regressions ----
plot(log(`Leu.TdR`) ~ log(`NH4_ug/L`) ,data = master_clean)
mod3 <- lmodel2(log(`Leu.TdR`) ~ log(`NH4_ug/L`), data = master_clean,
                nperm = 0)
print(mod3)

plot(log(`Leu.TdR`) ~ log(NPOC), data = master_clean)
mod <- lmodel2(log(`Leu.TdR`) ~ log(NPOC), data = master_clean,
                nperm = 0)
print(mod)

sma_row <- mod$regression.results[
  mod$regression.results$Method == "SMA", ]
ci_row  <- mod$confidence.intervals[
  mod$confidence.intervals$Method  == "SMA", ]

sma_slope     <- sma_row$Slope
sma_intercept <- sma_row$Intercept
slope_lwr     <- ci_row$`2.5%-Slope`
slope_upr     <- ci_row$`97.5%-Slope`
int_lwr       <- ci_row$`2.5%-Intercept`
int_upr       <- ci_row$`97.5%-Intercept`
r_val         <- mod$r



annotation_text <- sprintf(
  "SMA slope = %.2f [%.2f, %.2f]\nPearson r = %.2f",
  sma_slope, slope_lwr, slope_upr, r_val
)


x_pos <- max(sma_df$logNPOC)
y_pos <- max(sma_df$upr)
 

x_log_seq <- seq(
  from = min(log(master_clean$NPOC), na.rm = TRUE),
  to   = max(log(master_clean$NPOC), na.rm = TRUE),
  length.out = 100
)

sma_df <- data.frame(
  logNPOC = x_log_seq,
  fit      = sma_intercept + sma_slope   * x_log_seq,
  lwr      = int_lwr       + slope_lwr   * x_log_seq,
  upr      = int_upr       + slope_upr   * x_log_seq
)

# 3) Plot points + SMA ribbon + SMA line
ggplot(master_clean, aes(x = log(NPOC), y = log(`Leu.TdR`))) +
  geom_point(size = 2) +
  geom_ribbon(
    data = sma_df,
    inherit.aes  = FALSE,
    aes(x = logNPOC, ymin = lwr, ymax = upr),
    fill  = "steelblue", alpha = 0.3
  ) +
  geom_line(
    data = sma_df,
    inherit.aes  = FALSE,
    aes(x = logNPOC, y = fit),
    color = "steelblue", size = 1
  ) +

  annotate(
    "text",
    x     = x_pos,
    y     = y_pos,
    label = annotation_text,
    hjust = 1,    
    vjust = 1,    
    size  = 4
  ) +
  labs(
    x = expression(Log[10]*"(NPOC, mg L"^{-1}*")"),
    y = expression(Log[10]*"(Leu:TdR)")
  ) +
  theme_classic(base_size = 14)





#Use this one
plot(log(`Leu.TdR`) ~ log(`P ug/L`) ,data = master_clean)
mod2 <- lmodel2(log(`Leu.TdR`) ~ log(`P ug/L`), data = master_clean,
                nperm = 0)
print(mod2)


sma_row <- mod2$regression.results[
  mod$regression.results$Method == "SMA", ]
ci_row  <- mod2$confidence.intervals[
  mod$confidence.intervals$Method  == "SMA", ]

sma_slope     <- sma_row$Slope
sma_intercept <- sma_row$Intercept
slope_lwr     <- ci_row$`2.5%-Slope`
slope_upr     <- ci_row$`97.5%-Slope`
int_lwr       <- ci_row$`2.5%-Intercept`
int_upr       <- ci_row$`97.5%-Intercept`
r_val         <- mod2$r



annotation_text <- sprintf(
  "SMA slope = %.2f [%.2f, %.2f]\nPearson r = %.2f",
  sma_slope, slope_lwr, slope_upr, r_val
)


sma_df <- data.frame(
  logP = x_log_seq,
  fit      = sma_intercept + sma_slope   * x_log_seq,
  lwr      = int_lwr       + slope_lwr   * x_log_seq,
  upr      = int_upr       + slope_upr   * x_log_seq
)

x_pos <- max(sma_df$logNP)
y_pos <- max(sma_df$upr)


x_log_seq <- seq(
  from = min(log(master_clean$`P ug/L`), na.rm = TRUE),
  to   = max(log(master_clean$`P ug/L`), na.rm = TRUE),
  length.out = 100
)



# 3) Plot points + SMA ribbon + SMA line
ggplot(master_clean, aes(x = log(`P ug/L`), y = log(`Leu.TdR`))) +
  geom_point(size = 2) +
  geom_ribbon(
    data = sma_df,
    inherit.aes  = FALSE,
    aes(x = logP, ymin = lwr, ymax = upr),
    fill  = "steelblue", alpha = 0.3
  ) +
  geom_line(
    data = sma_df,
    inherit.aes  = FALSE,
    aes(x = logP, y = fit),
    color = "steelblue", size = 1
  ) +
  
  annotate(
    "text",
    x     = x_pos,
    y     = y_pos,
    label = annotation_text,
    hjust = 1,    
    vjust = 1,    
    size  = 4
  ) +
  labs(
    x = expression(Log[10]*"(P, μg L"^{-1}*")"),
    y = expression(Log[10]*"(Leu:TdR)")
  ) +
  theme_classic(base_size = 14)


clean_Chla <- master_clean %>% 
  mutate(z_Chla = (Chla- mean(Chla, na.rm = TRUE)) /
           sd(Chla, na.rm = TRUE)) %>% 
  filter(abs(z_Chla) <= z_cutoff) %>% 
  select(-z_Chla) 

ggplot(clean_Chla, aes(x = Leu_nM , y = Chla)) +
  geom_point()
ggplot(clean_Chla, aes(x = TdR_nM , y = Chla)) +
  geom_point()



clean_TN <- clean_BP %>% 
  mutate(z_TN = (TN- mean(TN, na.rm = TRUE)) /
           sd(TN, na.rm = TRUE)) %>% 
  filter(abs(z_TN) <= z_cutoff) %>% 
  select(-z_TN) 

clean_NPOC <- clean_BP %>% 
  mutate(z_NPOC = (NPOC- mean(NPOC, na.rm = TRUE)) /
           sd(NPOC, na.rm = TRUE)) %>% 
  filter(abs(z_NPOC) <= z_cutoff) %>% 
  select(-z_NPOC) 


clean_P <- master_clean %>% 
  mutate(z_P = (`P ug/L` - mean(`P ug/L`, na.rm = TRUE)) /
           sd(`P ug/L`, na.rm = TRUE)) %>% 
  filter(abs(z_P) <= z_cutoff) %>% 
  select(-z_P) 