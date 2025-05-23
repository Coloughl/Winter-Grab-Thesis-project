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
library(car)
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
Carbon$Season <- factor(WG_BP$Season, levels = c("Summer", "Spring", "Winter"))
WG_BP$Year <- factor(WG_BP$Year, levels = c("2025", "2024"))


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


#MLM ----
library(glmmTMB)
library(nlme)
library(lme4)
library(MuMIn)
library(lattice)
library(emmeans)

master_scaled <- master_clean %>%
  mutate(across(c(Chla, SUVA254, Nox.N.ug.L, `P ug/L`, TN, NPOC, `NH4_ug/L`,bix, hix ),
                ~ scale(.)[,1]))

master_scaled <- master_scaled %>%
  rename_with(~ make.names(.), everything())


preds <- c("Chla", "SUVA254", "Nox.N.ug.L", "P.ug.L", "TN", "NPOC", "NH4_ug.L", "hix", "bix")

uni_results <- map_df(preds, function(var) {
  # build formula safely now that var is syntactic
  f  <- reformulate(c(var, "(1|Season)", "(1|Lake)"), response = "Leu.TdR")
  m  <- lmer(f, data = master_scaled, REML = FALSE)
  ss <- summary(m)$coefficients
  tibble(
    predictor = var,
    AIC       = AIC(m),
    estimate  = ss[var, "Estimate"],
    se        = ss[var, "Std. Error"],
    t.value   = ss[var, "t value"]
  )
})

print(uni_results)

full_ml <- lmer(
  Leu.TdR ~ Chla + `P.ug.L` + NPOC + hix + Nox.N.ug.L + SUVA254 +  
    (1|Lake) + (1|Season),
  data   = master_scaled,
  REML   = FALSE
)
summary(full_ml)
AIC(full_ml)

full_ml2 <- lmer(
  Leu.TdR ~ Chla:Season + `P.ug.L` + NPOC + hix + Nox.N.ug.L +  
    (1|Lake) + (1|Season),
  data   = master_scaled,
  REML   = FALSE
)
summary(full_ml2)
AIC(full_ml2)

drop1(full_ml, test = "Chisq")

vars <- c("Leu.TdR", "Chla", "P.ug.L", "NH4_ug.L", "Lake", "Season", "NPOC")


master_mod <- master_scaled %>%
  select(all_of(vars)) %>%
  drop_na()


nrow(master_mod)  

full_ml2 <- lmer(
  Leu.TdR ~ Chla + `P.ug.L` + NH4_ug.L + (1|Lake) + (1|Season),
  data = master_mod,
  REML = FALSE,
  na.action = na.omit
)

drop1(full_ml2, test = "Chisq")

m2 <- update(full_ml2, . ~ . - `P ug/L`)

summary(m2)


m_season <- lmer(
  Leu.TdR ~ Chla*Season + P.ug.L + NH4_ug.L + NPOC + ,
  data   = master_mod,
  REML   = FALSE
)
AIC(m_season)
summary(m_season)

AIC(full_ml2, m_season)


final_mod <- update(m_season, REML = TRUE)
summary(final_mod)
r.squaredGLMM(final_mod)


slopes <- emtrends(final_mod, ~ Season, var = "Chla")
summary(slopes)

emmip(final_mod, Season ~ Chla, CIs = TRUE) +
  labs(x = "Chla (µg L⁻¹)",
       y = "Predicted Leu.TdR (nmol L⁻¹ d⁻¹)",
       colour = "Season") +
  theme_minimal(base_size = 14)


newdata <- expand_grid(
  Chla      = seq(min(master_mod$Chla), max(master_mod$Chla), length.out = 50),
  Season    = factor(c("Summer","Spring"), levels = levels(master_mod$Season)),
  `P.ug.L`  = mean(master_mod$P.ug.L, na.rm=TRUE),
  NH4_ug.L  = mean(master_mod$NH4_ug.L, na.rm=TRUE),
  NPOC      = mean(master_mod$NPOC, na.rm=TRUE),
  hix       = mean(master_mod$hix, na.rm=TRUE)
)


newdata$pred <- predict(final_mod, newdata, re.form = NA)

ggplot(newdata, aes(x = Chla, y = pred, colour = Season)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = pred - 1.96 * predict(final_mod, newdata, re.form = NA, se.fit = TRUE)$se.fit,
                  ymax = pred + 1.96 * predict(final_mod, newdata, re.form = NA, se.fit = TRUE)$se.fit),
              alpha = 0.2) +
  labs(x = "Chlorophyll-a (µg L⁻¹)",
       y = "Predicted Leu:TdR",
       colour = "Season") +
  theme_classic(base_size = 14) +
  theme(
    # axis text
    axis.title.x     = element_text(size = 24),
    axis.title.y     = element_text(size = 24),
    axis.text.x      = element_text(size = 22, margin = margin(t = 6, r = 0, b = 6, l = 0)),
    axis.text.y      = element_text(size = 22, margin = margin(t = 0, r = 6, b = 0, l = 6)),
    
    # legend
    legend.title     = element_text(size = 28),
    legend.text      = element_text(size = 20))




#Plots ----
plot_data <- merged_data %>%
  filter(!is.na(Lake.x))


ggplot(WG_BP, aes(x = TdR_nM, y = Leu_nM, fill = factor(Year), shape = factor(Season))) +
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
  facet_wrap(~ Lake.x) +
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

mod <- aov(`Leu.TdR` ~ Lake.x + Year + Season, data = WG_BP)
summary(mod)


Anova(mod, type = 2)

t.test(`Leu.TdR` ~ Year, data = WG_BP)  


#EEMs and DOC plots. Additional data manipulation ----
hix_summary <- EEMs_seasoned %>%
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

bix_summary <- EEMs_seasoned %>%
  mutate(SeasonYear = paste(Season, Year)) %>%
  group_by(Lake, SeasonYear) %>%
  summarise(
    n       = sum(!is.na(bix)),
    mean_b  = mean(bix, na.rm = TRUE),
    se_b    = sd(bix,   na.rm = TRUE)/sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    SeasonYear = factor(SeasonYear,
                        levels = c("Winter 2024","Spring 2024","Summer 2024","Winter 2025")
    )
  )



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
    Lake = as.character(Lake),  # if it’s a factor, turn it into character first
    Lake = if_else(
      is.na(Lake) | Lake == "Lake NA",
      "Lake Erie",
      Lake
    ),
    Lake = factor(Lake)         # back to factor if you want
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


offset <- 0.05 * max(hix_summary$mean_hix, na.rm = TRUE)

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
    aes(
      label = paste0("n=", n),
      y     = mean_hix + se_hix + offset
    ),
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
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 22),
    axis.text.y   = element_text(size = 22),
    axis.title    = element_text(size = 24),
    legend.title  = element_text(size = 28),
    legend.text   = element_text(size = 20),
    panel.border  = element_rect(colour = "black", fill = NA, size = 0.5),
    panel.spacing = unit(0.6, "cm")
  )


hix_mod <- aov(hix ~ Year*Season, clean_EEMs)
summary(hix_mod)


ggplot(bix_summary, aes(Lake, mean_b, fill = SeasonYear)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = mean_b - se_b, ymax = mean_b + se_b),
                width = 0.2, position = position_dodge(0.8)) +
  geom_text(aes(label = paste0("n=",n),
                y     = mean_b + se_b + 0.05*max(mean_b)),
            position = position_dodge(0.8), vjust = 0, size = 4) +
  scale_fill_manual(values = c(
    "Winter 2024" = "#87CEDA",
    "Spring 2024" = "#B6798F",
    "Summer 2024" = "#E50245",
    "Winter 2025" = "#6BDACF"
  )) +
  labs(x = NULL, y = "Mean BIX (±SE)", fill = "Season & Year") +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y   = element_text(size = 12),
    axis.title    = element_text(size = 14),
    legend.position = "none",             # hide second legend
    panel.border  = element_rect(color = "black", fill = NA),
    panel.spacing = unit(0.5, "cm")
  )

# 5) Combine with patchwork
hix_plot + bix_plot + 
  plot_annotation(
    title = "Mean HIX and BIX by Lake, Season & Year",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )




EEMs_seasoned$date <- as.Date(EEMs_seasoned$date, format = "%d-%b-%y")
# If any dates parsed before 1970, bump them into 2000s:
EEMs_seasoned$date <- ifelse(
  EEMs_seasoned$date < as.Date("1960-01-01"),
  EEMs_seasoned$date + years(100),
  EEMs_seasoned$date)   # adjust format if needed
EEMs_seasoned$Year <- as.integer(format(EEMs_seasoned$date, "%Y"))
