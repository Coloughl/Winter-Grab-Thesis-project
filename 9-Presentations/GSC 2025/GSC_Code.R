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


mean_thickness <- WG_Ice_Snow %>%
  group_by(Lake, Year) %>%
  summarise(
    mean_ice_thickness = mean(`Snow+Ice_m`, na.rm = TRUE))



#Plots ----


#Boxplot of DOC concentrations in the lakes by year
ggplot(WG_DOC) + 
  geom_boxplot(aes(x = Lake, y = DOC_mg.L, fill = factor(Year)),
               position = position_dodge(width = 0.8)) +  # Map 'Year' to 'fill' and position side by side
  scale_fill_manual(name = "Year", 
                    values = c("2022" = "skyblue", 
                               "2024" = "lightpink")) +  # Colors for each year
  theme(legend.position = "right") +
  labs(x = "Lake",  # Custom label for x-axis
       y = "DOC Concentration (mg/L)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title.x = element_text(size = 24),  # Set x-axis title size to 24
        axis.title.y = element_text(size = 24),  # Set y-axis title size to 24
        axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 24))# Place the legend on the right side




#Average Ice and Snow thickness on each lake for each year.
ggplot(mean_thickness, aes(x = Lake, y = mean_ice_thickness, fill = factor(Year))) + 
  geom_bar(stat = "identity", position = "dodge") +  # 'dodge' places bars side by side
  scale_fill_manual(values = c("2022" = "skyblue", "2024" = "lightpink", "2025" = "#C4A3D4")) +  # Custom colors for years
  labs(x = "Lake", 
       y = "Average Ice and Snow Thickness (m)", 
       fill = "Year") +  # Labels and title
  theme_minimal() +  # Minimal theme
  theme(axis.title.x = element_text(size = 24),  # Set x-axis title size to 24
        axis.title.y = element_text(size = 24),  # Set y-axis title size to 24
        axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 24))


ggplot(WG_Bact, aes(x = TdR_nM, y = Leu_nM, fill = factor(Year))) +
  labs(x = "TdR (nmol TdR/L/d)", 
  y = "Leu (nmol Leu/L/d)",
  fill = "Year") +
  scale_fill_manual(values = c("2022" = "skyblue", "2024" = "lightpink")) +
  geom_point(size = 14, shape = 22, color = "black", alpha = 0.6) +  # Black outline for points, fill color by Season
  theme(legend.position = "right",  # Hide legend
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 20, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(size = 20, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(-0.25, "cm")) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkgray", linewidth = 2)


ggplot(WG_Bact, aes(x = TdR_nM, y = Leu_nM, fill = factor(Year))) +
  labs(x = "TdR (nmol TdR/L/d)", 
       y = "Leu (nmol Leu/L/d)",
       fill = "Year") +
  scale_fill_manual(values = c("2022" = "skyblue", "2024" = "lightpink")) +
  geom_point(size = 10, shape = 22, color = "black", alpha = 0.6) +  # Black outline for points, fill color by Year
  theme(legend.position = "right",  # Show legend on the right
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(size = 16, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(-0.25, "cm")) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkgray", linewidth = 2) +
  xlim(0, 0.3) +  # Set x-axis limit to 0.3
  ylim(0, 5)



ggplot(WG_fDOM) +
  geom_boxplot(aes(Lake, SUVA254, fill = factor(Year))) +
  scale_fill_manual(values = c("2022" = "skyblue", "2024" = "lightpink")) +
  labs(x = "Lake",
       y = "Specific Ultra-violet absorbance @ 254 nm (SUVA254)",
       fill = "Year")

ggplot(WG_fDOM) +
  geom_boxplot(aes(Lake, BIX, fill = factor(Year))) +
  scale_fill_manual(values = c("2022" = "skyblue", "2024" = "lightpink")) +
  labs(x = "Lake",
       y = "Biological Index (BIX)")

ggplot(WG_fDOM) +
  geom_boxplot(aes(Lake, HIX, fill = factor(Year))) +
  scale_fill_manual(values = c("2022" = "skyblue", "2024" = "lightpink")) +
  labs(x = "Lake",
       y = "Humification Index (HIX)")

ggplot(WG_fDOM) +
  geom_boxplot(aes(Lake, FI, fill = factor(Year))) +
  scale_fill_manual(values = c("2022" = "skyblue", "2024" = "lightpink")) +
  labs(x = "Lake",
       y = "Flourescence Index (FI)")






BPAOV <- aov((Leu_nM/TdR_nM) ~ as.factor(Lake) + as.factor(Year), data = WG_Bact)
summary(BPAOV)

DOCAOV <- aov(DOC_mg.L ~ as.factor(Lake) * as.factor(Year), data = WG_DOC)
summary(DOCAOV)

DOC_Tukey <- TukeyHSD(DOCAOV)
summary(DOC_Tukey)
