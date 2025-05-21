install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(readxl)

getwd()
setwd("C:/Users/ccolo/OneDrive/Documents/GitHub/Winter-Grab-Thesis-Project/9-Presentations/GSC 2025/1-Data")

#Winter 2022 Data
WG1_sites <- read.csv("Winter Grab 1 site coordinatess.csv", 
                           stringsAsFactors = TRUE, fileEncoding = "UTF-8") %>% 
  rename(Station = Site.name) %>% 
  mutate(Lake = recode(Lake, "Keweenaw Waterway (Superior)" = "Lake Superior")) %>% 
  filter(!grepl("Upper St. Lawrence River", Lake),
         !grepl("Lake St Clair", Lake))


WG1_DOC <- read.csv("WinterGrab_DOC_DOM_concentration.csv")
WG1_DOC_noice <- WG1_DOC %>%
  filter(!grepl("Ice", Depth_m))

WG1_DOCF <- WG1_DOC_noice %>% 
  mutate(Sample_name = NULL,
         Rep = NULL) %>%
  unite(names, Analysis, Unit) %>% 
  pivot_wider(names_from = names, values_from = Value) %>% 
  mutate(Station = as.factor(Station)) %>% 
  filter(Depth_m >= 0 & Depth_m <= 1)
  
  
DOC_2022 <- WG1_DOCF %>%
  dplyr::select(PI_name, Station, Year, DOC_mg.L) %>%
  inner_join(WG1_sites, by = "Station")

fDOM_2022 <- WG1_DOCF %>% 
  dplyr::select(PI_name, Station, Year, HIX_, FI_, everything()[25]) %>% 
  inner_join(WG1_sites, by = "Station") %>% 
  mutate(SUVA254 = .[[6]],
         HIX = HIX_,
         FI = FI_)


WG1_Bact <- read.csv("WinterGrab_bacterialproduction_rate.csv")
WG1_BactF <- WG1_Bact %>% 
  mutate(Sample_Name = NULL,
         Rep = NULL) %>%
  unite(names, Analysis, Unit) %>% 
  pivot_wider(names_from = names, values_from = Value) %>% 
  mutate(Depth_m = as.factor(Depth_m),
         Station = as.factor(Station))

Bact_2022 <- inner_join(WG1_BactF, WG1_sites, by = "Station") %>% 
  mutate(Leu_nM = BP_Leucine_Incorporation_nmol_leucine_L_day,
         TdR_nM = BP_Thymidine_Incorporation_nmol_thymidine_L_day,
         BP_Leucine_Incorporation_nmol_leucine_L_day = NULL,
         BP_Thymidine_Incorporation_nmol_thymidine_L_day = NULL,
        Leu = .[[7]],
        TdR = .[[6]])
      


WG1_Ice <- read.csv("WinterGrab_snow_ice_properties.csv")
WG1_IceF <- WG1_Ice %>% 
  mutate(Sample_name = NULL,
         Analysis = NULL,
         Ice_thick_m = na_if(Ice_thick_m, "no data"),
         Snow_thick_m = na_if(Snow_thick_m, "no data"),
         Ice_m = as.numeric(Ice_thick_m),
         Snow_cm = as.numeric(Snow_thick_m),
         "Snow+Ice_m" = Ice_m + Snow_cm,
         snow_cm = Snow_cm * 100)

Ice_2022 <- inner_join(WG1_IceF, WG1_sites, by = "Station")


#Winter 2024 and 2025 Data
WG2_Sites <-read_xlsx("Wg2_Site_GSC_Poster.xlsx")

WG2_DOC <- read.csv("WG2_24_TOC_TN.csv")
DOC_2024 <- WG2_DOC %>% 
  filter(`NPOC.LOD.flag` != ">RANGE",
         `NPOC.LOD.flag` != "<LOD",
         Month == "February") %>% 
  mutate(DOC_mg.L = NPOC,
         NPOC = NULL,
         TN = NULL,
         Station = Sample.ID)

WG2_fDOM <- read_xlsx("WG2_CDOM/WG2_fDOM.xlsx") %>% 
  mutate(Year = "2024",
         Year = as.integer(Year))

WG2_fDOM[WG2_fDOM == "na"] <- NA
str(WG2_fDOM)

WG2_fDOM <- mutate(WG2_fDOM,
       FI = as.double(FI),
       BIX = as.double(BIX))


WG2_Bact <- read.csv("Feb_24_BP.csv") %>% 
  mutate(Station = Sample)



WG2_Snow <- read_xlsx("WG2_Snow_data.xlsx")

WG2_Ice <- inner_join(WG2_Snow, WG2_Sites, by = "Station") %>% 
  mutate("Snow+Ice_m" = (snow_cm/100) + Ice_m)




#Combining Data ----
WG_DOC <- bind_rows(DOC_2022,DOC_2024)

WG_Bact <- bind_rows(Bact_2022, WG2_Bact)

WG_fDOM <- bind_rows(WG2_fDOM, fDOM_2022)

WG_Ice_Snow <- bind_rows(WG2_Ice, Ice_2022)


#Calculating the mean, sd, and sem for ice thickness
mean_thickness <- WG_Ice_Snow %>%
  group_by(Lake, Year) %>%
  summarise(
    mean_ice_thickness = mean(`Snow+Ice_m`, na.rm = TRUE))

ice_sd <- WG_Ice_Snow %>%
  group_by(Lake, Year) %>%
  summarise(sd_ice_thickness = sd(`Snow+Ice_m`, na.rm = TRUE))

# Count the number of observations in each group
lake_count <- WG_Ice_Snow %>%
  group_by(Lake, Year) %>%
  summarise(count = n())

# Merge the mean, standard deviation, and count data together
sem_thickness <-mean_thickness %>%
  left_join(ice_sd, by = c("Lake", "Year")) %>%
  left_join(lake_count, by = c("Lake", "Year")) %>%
  mutate(sem_ice_thickness = sd_ice_thickness / sqrt(count))



mean_DOC <- WG_DOC %>%
  group_by(Lake, Year) %>%
  summarise(
    mean_DOC = mean(`DOC_mg.L`, na.rm = TRUE))

DOC_sd <- WG_DOC %>%
  group_by(Lake, Year) %>%
  summarise(sd_DOC = sd(`DOC_mg.L`, na.rm = TRUE))

# Count the number of observations in each group
lake_count_D <- WG_DOC %>%
  group_by(Lake, Year) %>%
  summarise(count = n())

# Merge the mean, standard deviation, and count data together
sem_DOC <-mean_DOC %>%
  left_join(DOC_sd, by = c("Lake", "Year")) %>%
  left_join(lake_count_D, by = c("Lake", "Year")) %>%
  mutate(sem_DOC = sd_DOC / sqrt(count))


#Adding a new row for Lake St. Clair to make the DOC bars in the barchart the same size
new_DOC <- tibble(
  Lake = "Lake St. Clair",
  Year = 2022,
  mean_DOC = NA,
  sd_DOC = NA,
  sem_DOC = NA,
  count = NA
)
sem_DOC <- bind_rows(sem_DOC, new_DOC)


new_fDOM <- tibble(
  Lake ="Lake St. Clair",
  Year = 2022)

WG_fDOM <- bind_rows(WG_fDOM, new_fDOM)

new_Ice <- tibble(
  Lake ="Lake St Clair",
  Year = 2022)

sem_thickness <- bind_rows(sem_thickness, new_Ice) %>% 
  mutate(across(everything(), ~na_if(., 0)))


#Plots ----


#Barchart of DOC concentrations in the lakes by year
ggplot(sem_DOC, aes(x = Lake, y = mean_DOC, fill = factor(Year))) + 
  geom_bar(stat = "identity", position = position_dodge2(padding = 0.1), width = .5) +  
  scale_fill_manual(values = c("2022" = "#87CEDA", "2024" = "#E50245")) + 
  labs(x = NULL,
       y = expression(paste("Average DOC Concentration (", "mg L"^-1, ")")), 
       fill = "Year") + 
  theme_minimal() +  
  theme(axis.title.x = element_text(size = 24),  
        axis.title.y = element_text(size = 24),  
        axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 20),
        legend.text = element_text(size = 24),
        legend.position = c(0.8, 0.8),
        legend.title = element_text(size = 28)) +
  geom_errorbar(aes(ymin = mean_DOC - sem_DOC,
                    ymax = mean_DOC + sem_DOC), width = 0.1, position = position_dodge(0.5))

ggplot(sem_DOC, aes(x = Lake, y = mean_DOC, fill = factor(Year))) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = .6) +  # Adjust dodge width and bar width
  scale_fill_manual(values = c("2022" = "#87CEDA", "2024" = "#E50245")) + 
  labs(x = NULL,
       y = expression(paste("Average DOC Concentration (", "mg L"^-1, ")")), 
       fill = "Year") + 
  theme_minimal() +  
  theme(axis.title.x = element_text(size = 24),  
        axis.title.y = element_text(size = 24),  
        axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 20),
        legend.text = element_text(size = 24),
        legend.position = c(0.8, 0.8),
        legend.title = element_text(size = 28)) +
  geom_errorbar(aes(ymin = mean_DOC - sem_DOC,
                    ymax = mean_DOC + sem_DOC), 
                width = 0.4, size = 1,position = position_dodge(0.6))   # Same dodge width for error bars
  geom_text(aes(label = paste("n = ", count)), vjust = -3, size = 6, position = position_dodge(0.6))




#Average Ice and Snow thickness on each lake for each year.
ggplot(sem_thickness, aes(x = Lake, y = mean_ice_thickness, fill = factor(Year))) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = .6) +  # Adjust dodge width and bar width
  scale_fill_manual(values = c("2022" = "#87CEDA", "2024" = "#E50245", "2025" = "#6D8EAA")) + 
  labs(x = NULL,
       y = "Average Ice and Snow Thickness (m)", 
       fill = "Year") + 
  theme_minimal() +  
  theme(axis.title.x = element_text(size = 24),  
        axis.title.y = element_text(size = 24),  
        axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 20),
        legend.text = element_text(size = 24),
        legend.position = c(0.8, 0.8),
        legend.title = element_text(size = 28)) +
  geom_errorbar(aes(ymin = mean_ice_thickness - sem_ice_thickness,
                    ymax = mean_ice_thickness + sem_ice_thickness), 
                width = 0.4, size = 1,position = position_dodge(0.6))   # Same dodge width for error bars
  geom_text(aes(label = paste("n = ", count)), vjust = -3, size = 6, position = position_dodge(0.6))  # Add 'n = ' text




#BP Scatterplot
ggplot(WG_Bact, aes(x = TdR_nM, y = Leu_nM, fill = factor(Year))) +
  labs(x = expression(paste(, "nmol Thymidine L"^-1, "d"^-1,)), 
  y = expression(paste(, "nmol Leucine L"^-1, "d"^-1,)),
  fill = "Year") +
  scale_fill_manual(values = c("2022" = "#87CEDA",  # Lighter, vibrant blue
                               "2024" = "#E50245")) +
  geom_point(size = 18, shape = 21, alpha = 0.7) +  # Black outline for points, fill color by Season
  theme(axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 22, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(size = 22, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = c(0.1, 0.9),
        axis.ticks.length = unit(-0.35, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 28)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkgray", linewidth = 3) +
  xlim(0, 0.5) +  # Set x-axis limit to 0.3
  ylim(0, 15)



#SUVA Boxplot
ggplot(WG_fDOM) +
  geom_boxplot(aes(Lake, SUVA254, fill = factor(Year))) +
  scale_fill_manual(values = c("2022" = "#87CEDA","2024" = "#E50245")) +
  labs(x = NULL,
       y = "SUVA254",
       fill = "Year") +
  theme(axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 22, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(size = 22, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), angle = 45, hjust = 1,),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = c(0.1, 0.9),
        axis.ticks.length = unit(-0.35, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 28)) 

#HIX boxplot
ggplot(WG_fDOM) +
  geom_boxplot(aes(Lake, HIX, fill = factor(Year))) +
  scale_fill_manual(values = c("2022" = "#87CEDA","2024" = "#E50245")) +
  labs(x = NULL,
       y = "HIX",
       fill = "Year") +
  theme(axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 22, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(size = 22, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), angle = 45, hjust = 1,),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = c(0.1, 0.9),
        axis.ticks.length = unit(-0.35, "cm"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 28))











ggplot(WG_fDOM) +
  geom_boxplot(aes(Lake, BIX, fill = factor(Year))) +
  scale_fill_manual(values = c("2022" = "skyblue", "2024" = "lightpink")) +
  labs(x = "Lake",
       y = "Biological Index (BIX)")


ggplot(WG_fDOM) +
  geom_boxplot(aes(Lake, FI, fill = factor(Year))) +
  scale_fill_manual(values = c("2022" = "skyblue", "2024" = "lightpink")) +
  labs(x = "Lake",
       y = "Flourescence Index (FI)")

hix_summary <- master_clean %>%
  mutate(SeasonYear = paste(Season, Year)) %>%
  group_by(Lake, SeasonYear) %>%
  summarise(
    n        = sum(!is.na(hix)),
    mean_hix = mean(hix, na.rm = TRUE),
    se_hix   = sd(hix,   na.rm = TRUE) / sqrt(n),
    .groups  = "drop"
  ) %>%
  mutate(
    SeasonYear = factor(SeasonYear,
      levels = c("Winter 2024","Spring 2024","Summer 2024","Winter 2025")
    )
  )

# 2) Plot bars + error bars + n labels
ggplot(hix_summary, aes(x = Lake, y = mean_hix, fill = SeasonYear)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width    = 0.7,
    color    = "black"
  ) +
  geom_errorbar(
    aes(ymin = mean_hix - se_hix, ymax = mean_hix + se_hix),
    width    = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  geom_text(
    aes(label = n,
        y     = mean_hix + se_hix + 1),    # adjust +1 to move labels just above the error bar
    position = position_dodge(width = 0.8),
    vjust    = 0,
    size     = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "Winter 2024" = "#87CEDA",
    "Spring 2024" = "#B6798F",
    "Summer 2024" = "#E50245",
    "Winter 2025" = "#6BDACF"
  )) +
  labs(
    x    = "Lake",
    y    = "Mean HIX (±SE)",
    fill = "Season & Year"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 12),
    axis.title    = element_text(size = 16),
    legend.title  = element_text(size = 14),
    legend.text   = element_text(size = 12),
    panel.border  = element_rect(colour = "black", fill = NA, size = 0.5),
    panel.spacing = unit(0.6, "cm")
  )




BPAOV <- aov((Leu_nM/TdR_nM) ~ as.factor(Lake) * as.factor(Year), data = WG_Bact)
summary(BPAOV)

DOCAOV <- aov(DOC_mg.L ~ as.factor(Lake) * as.factor(Year), data = WG_DOC)
summary(DOCAOV)

DOC_Tukey <- TukeyHSD(DOCAOV)
summary(DOC_Tukey)
