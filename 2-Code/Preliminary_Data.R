#Code made for manipulating and displaying Winter Grab 2 preliminary data
#Connor O'Loughlin
#Used Chatgpt for help with errors and general coding troubleshooting
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
library(readxl)
install.packages("openxlsx")
library(openxlsx)


getwd()
setwd("C:/Users/ccolo/OneDrive/Documents/GitHub/Winter-Grab-Thesis-project/1-Data")


convert_to_molar_concentration <- function(tdn_mg_per_L) {
  # Molar mass of nitrogen (N) in g/mol
  molar_mass_nitrogen <- 14.01
  
  # Convert mg/L to g/L
  tdn_g_per_L <- tdn_mg_per_L / 1000
  
  # Calculate molar concentration (mol/L)
  molar_concentration <- tdn_g_per_L / molar_mass_nitrogen
  
  return(molar_concentration)
}

#Read in data ----
Aug_BP <- read.csv("Bacterial_Production/Aug_24_BP.csv",
               stringsAsFactors = TRUE)
Feb_BP <- read.csv("Bacterial_Production/Feb_24_BP.csv",
                   stringsAsFactors = TRUE)
May_BP <- read.csv("Bacterial_Production/May_24_BP.csv",
                   stringsAsFactors = TRUE)
TOC <- read.csv("DOC_TN/WG2_24_TOC_TN.csv",
                stringsAsFactors = TRUE)


#Reading in all the sheets from the inventory (very inefficiently)
Cell_Counts <- read_excel("Winter_Grab_2024_Inventory.xlsx", sheet = "Cell Counts")
Zoop_Trophic <- read_excel("Winter_Grab_2024_Inventory.xlsx", sheet = "Zoop Trophic")
DOC_EEMs <- read_excel("Winter_Grab_2024_Inventory.xlsx", sheet = "DOC&EEMs")
Chloride <- read_excel("Winter_Grab_2024_Inventory.xlsx", sheet = "Chloride")
Zoop_Biomass <- read_excel("Winter_Grab_2024_Inventory.xlsx", sheet = "Zoop Biomass")
Water_Isotopes<- read_excel("Winter_Grab_2024_Inventory.xlsx", sheet = "Water Isotopes")
Micro_Nano <- read_excel("Winter_Grab_2024_Inventory.xlsx", sheet = "Micro and nano phytoP")
BP_all <- read_excel("Winter_Grab_2024_Inventory.xlsx", sheet = "BP")
N_P <- read_excel("Winter_Grab_2024_Inventory.xlsx", sheet = "Total N&P")
Cyanotoxins <- read_excel("Winter_Grab_2024_Inventory.xlsx", sheet = "Cyanotoxins")
chl_a <- read_excel("Winter_Grab_2024_Inventory.xlsx", sheet = "Chl-a")
Phycocyanin <- read_excel("Winter_Grab_2024_Inventory.xlsx", sheet = "Phycocyanin")
CN_PP <- read_excel("Winter_Grab_2024_Inventory.xlsx", sheet = "CN and PP")
Nutrients <- read_excel("Winter_Grab_2024_Inventory.xlsx", sheet = "Nutrients")
DNA <- read.xlsx("Winter_Grab_2024_Inventory.xlsx", sheet = "DNA")






##Messing with the data tables to make graphing easier ----

#Merging the Bacterial Production data
BP <- bind_rows(Aug_BP, Feb_BP, May_BP)


#Setting the factor levels for the variable Month
##This is so we can display the graphs in chronological order
TOC$Month <- factor(TOC$Month, 
                    levels = c("January", "February", "March",
                               "April", "May", 
                               "June", "July", 
                               "August", "September", "October", 
                               "November", "December"))

#Removing data that exceeds the detection limit
DOC <- TOC %>% 
  filter(`NPOC.LOD.flag` != ">RANGE",
         `NPOC.LOD.flag` != "<LOD")

TN <- TOC %>% 
  filter(`TN.LOD.flag` != ">RANGE",
         `TN.LOD.flag` != "<LOD")

  
TOC_filtered <- TOC %>%
  filter(`NPOC.LOD.flag` != ">RANGE",  # Remove rows where NPOC flag is ">RANGE"
           `NPOC.LOD.flag` != "<LOD",  # Remove rows where NPOC flag is "<LOD"
           `TN.LOD.flag` != ">RANGE",  # Remove rows where TN flag is ">RANGE"
           `TN.LOD.flag` != "<LOD") %>%  # Remove rows where TN flag is "<LOD"
  mutate(C_N = `NPOC` / `TN`)  # Create new C:N column by dividing NPOC by TN  
  
#Creating data frames for each month so we can display them separately
DOC_feb <- DOC %>% filter(Month == "February")
DOC_mar <- DOC %>% filter(Month == "March")
DOC_may <- DOC %>% filter(Month == "May")
DOC_jul <- DOC %>% filter(Month == "July")
DOC_aug <- DOC %>% filter(Month == "August")
  
TN_feb <- TN %>% filter(Month == "February")
TN_mar <- TN %>% filter(Month == "March")
TN_may <- TN %>% filter(Month == "May")
TN_jul <- TN %>% filter(Month == "July")
TN_aug <- TN %>% filter(Month == "August")


TN$TDN_M <- convert_to_molar_concentration(TN$TN)

##Getting counts for each analyte----

#Cell Counts
Cell_counts <- table(Cell_Counts$Month)
print(Cell_counts)

#DNA
DNA_counts <- table(DNA$Month)
print(DNA_counts)

#Chl-a
Chla_counts <- table(chl_a$Month)
print(Chla_counts)

#Chloride
Chloride_counts <- table(Chloride$Month)
print(Chloride_counts)

#CN & PP
CN_PP_counts <- table(CN_PP$Month)/2
print(CN_PP_counts)

#BP
BP_counts <- table(BP_all$Month)
print(BP_counts)

#DOC & EEMs (2 per site)
DOC_EEMs_counts <- table(DOC_EEMs$Month)/2
print(DOC_EEMs_counts)

#Micro and Nano Phytoplankton
Mirco_nano_counts <- table(Micro_Nano$Month)
print(Mirco_nano_counts)

#Total N & P
N_P_counts <- table(N_P$Month)/2
print(N_P_counts)

#Nutrients
Nutrients_counts <- table(Nutrients$Month)/2
print(Nutrients_counts) #Mar is 4.5 since one site has triplicate samples

#Phycocyanin
Phycocyanin_counts <- table(Phycocyanin$Month)
print(Phycocyanin_counts)

#Water Isotopes
Water_counts <- table(Water_Isotopes$Month)/2
print(Water_counts)

#Zoop Biomass
Zoop_bio_counts <- table(Zoop_Biomass$Month)
print(Zoop_bio_counts)

#Zoop Trophic
Zoop_counts <- table(Zoop_Trophic$Month)
print(Zoop_counts)

#Cyanotoxins
Cyano_counts <- table(Cyanotoxins$Month)/2
print(Cyano_counts)


#Visual Displays----
##Stacked bar chart of C:N ----
ggplot(TOC_filtered, aes(x = Sample.ID, y = C_N, fill = Month)) +
  geom_bar(stat = "identity") +           # Create a bar plot
  facet_grid(~ Lake, scales = "free") +  # Facet by both Month and Lake with independent x and y axes
  theme_minimal() +                       # Use a minimal theme
  labs(title = "Bar plot of C:N by Site, Lake, and Month",
       x = "Site",
       y = "C:N") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) 


##DOC plots ----
##Multi-Facted displat of scatterplots showing DOC conc in lakes
ggplot(DOC, aes(x = Sample.ID, y = NPOC, color = Lake)) +
  geom_point() +  # Create scatterplot
  facet_wrap(~ Month, scales = "free", ncol = 3) +  # Facet by Month
  theme_minimal() +
  geom_text(aes(label = Sample.ID), vjust = -1, hjust = .6, size = 3) +# Use a minimal theme
  labs(title = "Scatterplot of TOC by Site, Lake, and Month",
       x = "Site",
       y = "TOC (mg C/L)") +  # Label the y-axis as TOC
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis title
    plot.margin = margin(10, 10, 80, 10),  # Increase bottom margin for more space
    strip.text = element_text(size = 10),  # Increase size of facet labels (Month)
    panel.spacing = unit(1, "lines")  # Increase spacing between facets
  )

##Scatterplot of all the sites
ggplot(DOC, aes(x = Sample.ID, y = NPOC, color = Lake, shape = Month)) +  # Map Month to shape
  geom_point() +  # Scatter plot of DOC values
  theme_minimal() +  # Use a minimal theme
  labs(title = "Scatterplot of NPOC by Site and Lake with Month as Shape",
       x = "Site",
       y = "NPOC (mg/L)") +  # Label the y-axis as NPOC
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.title.x = element_blank()  # Optionally, remove x-axis title
  )

## Create scatterplot for February only
ggplot(DOC_feb, aes(x = Sample.ID, y = NPOC, color = Lake)) +
  geom_point() +  # Create scatterplot
  facet_wrap(~ Month, scales = "free", ncol = 1) +  # Facet by Month (only February will be shown)
  theme_minimal() +
  geom_text(aes(label = Sample.ID), vjust = -1, hjust = .6, size = 3) +  # Add labels to points
  labs(title = "Scatterplot of TOC by Site and Lake during the month of February",
       x = "Site",
       y = "TOC (mg C/L)") +  # Label the y-axis as TOC
  scale_y_continuous(limits = c(0, 8)) + 
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis title
    plot.margin = margin(10, 10, 10, 10),  # Increase bottom margin for more space
    strip.text = element_text(size = 10),  # Increase size of facet labels (Month)
    panel.spacing = unit(1, "lines"),  # Increase spacing between facets
    legend.position = "bottom",  # Place the legend at the bottom
    legend.box = "horizontal",  # Arrange legend items horizontally 
  )

## Scatterplot for March only 
ggplot(DOC_mar, aes(x = Sample.ID, y = NPOC, color = Lake)) +
  geom_point() +  # Create scatterplot
  facet_wrap(~ Month, scales = "free", ncol = 1) +  # Facet by Month (only February will be shown)
  theme_minimal() +
  geom_text(aes(label = Sample.ID), vjust = -1, hjust = .6, size = 3) +  # Add labels to points
  labs(title = "Scatterplot of TOC by Site and Lake during the month of March",
       x = "Site",
       y = "TOC (mg C/L)") +  # Label the y-axis as TOC
  scale_y_continuous(limits = c(0, 8)) + 
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis title
    plot.margin = margin(10, 10, 10, 10),  # Increase bottom margin for more space
    strip.text = element_text(size = 10),  # Increase size of facet labels (Month)
    panel.spacing = unit(1, "lines"),  # Increase spacing between facets
    legend.position = "bottom",  # Place the legend at the bottom
    legend.box = "horizontal",  # Arrange legend items horizontally 
  )

##Scatterplot for May only 
ggplot(DOC_may, aes(x = Sample.ID, y = NPOC, color = Lake)) +
  geom_point() +  # Create scatterplot
  facet_wrap(~ Month, scales = "free", ncol = 1) +  # Facet by Month (only February will be shown)
  theme_minimal() +
  geom_text(aes(label = Sample.ID), vjust = -1, hjust = .6, size = 3) +  # Add labels to points
  labs(title = "Scatterplot of TOC by Site and Lake during the month of May",
       x = "Site",
       y = "TOC (mg C/L)") +  # Label the y-axis as TOC
  scale_y_continuous(limits = c(0, 8)) + 
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis title
    plot.margin = margin(10, 10, 10, 10),  # Increase bottom margin for more space
    strip.text = element_text(size = 10),  # Increase size of facet labels (Month)
    panel.spacing = unit(1, "lines"),  # Increase spacing between facets
    legend.position = "bottom",  # Place the legend at the bottom
    legend.box = "horizontal",  # Arrange legend items horizontally 
  )

##Scatter plot for July only 
ggplot(DOC_jul, aes(x = Sample.ID, y = NPOC, color = Lake)) +
  geom_point() +  # Create scatterplot
  facet_wrap(~ Month, scales = "free", ncol = 1) +  # Facet by Month (only February will be shown)
  theme_minimal() +
  geom_text(aes(label = Sample.ID), vjust = -1, hjust = .6, size = 3) +  # Add labels to points
  labs(title = "Scatterplot of TOC by Site and Lake during the month of July",
       x = "Site",
       y = "TOC (mg C/L)") +  # Label the y-axis as TOC
  scale_y_continuous(limits = c(0, 8)) + 
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis title
    plot.margin = margin(10, 10, 10, 10),  # Increase bottom margin for more space
    strip.text = element_text(size = 10),  # Increase size of facet labels (Month)
    panel.spacing = unit(1, "lines"),  # Increase spacing between facets
    legend.position = "bottom",  # Place the legend at the bottom
    legend.box = "horizontal",  # Arrange legend items horizontally 
  )


##Scatterplot for August only 
ggplot(DOC_aug, aes(x = Sample.ID, y = NPOC, color = Lake)) +
  geom_point() +  # Create scatterplot
  facet_wrap(~ Month, scales = "free", ncol = 1) +  # Facet by Month (only February will be shown)
  theme_minimal() +
  geom_text(aes(label = Sample.ID), vjust = -1, hjust = .6, size = 3) +  # Add labels to points
  labs(title = "Scatterplot of TOC by Site and Lake during the month of August",
       x = "Site",
       y = "TOC (mg C/L)") +  # Label the y-axis as TOC
  scale_y_continuous(limits = c(0, 8)) + 
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis title
    plot.margin = margin(10, 10, 10, 10),  # Increase bottom margin for more space
    strip.text = element_text(size = 10),  # Increase size of facet labels (Month)
    panel.spacing = unit(1, "lines"),  # Increase spacing between facets
    legend.position = "bottom",  # Place the legend at the bottom
    legend.box = "horizontal",  # Arrange legend items horizontally 
  )

##Graphical displays for TN ----
##Multi-Facted displat of scatterplots showing N conc in lakes
ggplot(TN, aes(x = Sample.ID, y = TN, color = Lake)) +
  geom_point() +  # Create scatterplot
  facet_wrap(~ Month, scales = "free", ncol = 3) +  # Facet by Month
  theme_minimal() +
  geom_text(aes(label = Sample.ID), vjust = -1, hjust = .6, size = 3) +# Use a minimal theme
  labs(title = "Scatterplot of TN by Site, Lake, and Month",
       x = "Site",
       y = "TN (mg N/L)") +  # Label the y-axis as TOC
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis title
    plot.margin = margin(10, 10, 80, 10),  # Increase bottom margin for more space
    strip.text = element_text(size = 10),  # Increase size of facet labels (Month)
    panel.spacing = unit(1, "lines")  # Increase spacing between facets
  )

##Scatterplot of all the sites
ggplot(TN, aes(x = Sample.ID, y = TN, color = Lake, shape = Month)) +  # Map Month to shape
  geom_point() +  # Scatter plot of DOC values
  theme_minimal() +  # Use a minimal theme
  labs(title = "Scatterplot of TN by Site and Lake with Month as Shape",
       x = "Site",
       y = "TN (mg N/L)") +  # Label the y-axis as NPOC
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.title.x = element_blank()  # Optionally, remove x-axis title
  )

## Create scatterplot for February only
ggplot(TN_feb, aes(x = Sample.ID, y = TN, color = Lake)) +
  geom_point() +  # Create scatterplot
  facet_wrap(~ Month, scales = "free", ncol = 1) +  # Facet by Month (only February will be shown)
  theme_minimal() +
  geom_text(aes(label = Sample.ID), vjust = -1, hjust = .6, size = 3) +  # Add labels to points
  labs(title = "Scatterplot of TN by Site and Lake during the month of February",
       x = "Site",
       y = "TN (mg N/L)") +  # Label the y-axis as TOC
  scale_y_continuous(limits = c(0, 8)) + 
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis title
    plot.margin = margin(10, 10, 10, 10),  # Increase bottom margin for more space
    strip.text = element_text(size = 10),  # Increase size of facet labels (Month)
    panel.spacing = unit(1, "lines"),  # Increase spacing between facets
    legend.position = "bottom",  # Place the legend at the bottom
    legend.box = "horizontal",  # Arrange legend items horizontally 
  )

## Scatterplot for March only 
ggplot(TN_mar, aes(x = Sample.ID, y = TN, color = Lake)) +
  geom_point() +  # Create scatterplot
  facet_wrap(~ Month, scales = "free", ncol = 1) +  # Facet by Month (only February will be shown)
  theme_minimal() +
  geom_text(aes(label = Sample.ID), vjust = -1, hjust = .6, size = 3) +  # Add labels to points
  labs(title = "Scatterplot of TN by Site and Lake during the month of March",
       x = "Site",
       y = "TN (mg N/L)") +  # Label the y-axis as TOC
  scale_y_continuous(limits = c(0, 8)) + 
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis title
    plot.margin = margin(10, 10, 10, 10),  # Increase bottom margin for more space
    strip.text = element_text(size = 10),  # Increase size of facet labels (Month)
    panel.spacing = unit(1, "lines"),  # Increase spacing between facets
    legend.position = "bottom",  # Place the legend at the bottom
    legend.box = "horizontal",  # Arrange legend items horizontally 
  )

##Scatterplot for May only 
ggplot(TN_may, aes(x = Sample.ID, y = TN, color = Lake)) +
  geom_point() +  # Create scatterplot
  facet_wrap(~ Month, scales = "free", ncol = 1) +  # Facet by Month (only February will be shown)
  theme_minimal() +
  geom_text(aes(label = Sample.ID), vjust = -1, hjust = .6, size = 3) +  # Add labels to points
  labs(title = "Scatterplot of TN by Site and Lake during the month of May",
       x = "Site",
       y = "TN (mg N/L)") +  # Label the y-axis as TOC
  scale_y_continuous(limits = c(0, 8)) + 
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis title
    plot.margin = margin(10, 10, 10, 10),  # Increase bottom margin for more space
    strip.text = element_text(size = 10),  # Increase size of facet labels (Month)
    panel.spacing = unit(1, "lines"),  # Increase spacing between facets
    legend.position = "bottom",  # Place the legend at the bottom
    legend.box = "horizontal",  # Arrange legend items horizontally 
  )

##Scatter plot for July only 
ggplot(TN_jul, aes(x = Sample.ID, y = TN, color = Lake)) +
  geom_point() +  # Create scatterplot
  facet_wrap(~ Month, scales = "free", ncol = 1) +  # Facet by Month (only February will be shown)
  theme_minimal() +
  geom_text(aes(label = Sample.ID), vjust = -1, hjust = .6, size = 3) +  # Add labels to points
  labs(title = "Scatterplot of TN by Site and Lake during the month of July",
       x = "Site",
       y = "TN (mg N/L)") +  # Label the y-axis as TOC
  scale_y_continuous(limits = c(0, 8)) + 
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis title
    plot.margin = margin(10, 10, 10, 10),  # Increase bottom margin for more space
    strip.text = element_text(size = 10),  # Increase size of facet labels (Month)
    panel.spacing = unit(1, "lines"),  # Increase spacing between facets
    legend.position = "bottom",  # Place the legend at the bottom
    legend.box = "horizontal",  # Arrange legend items horizontally 
  )


##Scatterplot for August only 
ggplot(TN_aug, aes(x = Sample.ID, y = TN, color = Lake)) +
  geom_point() +  # Create scatterplot
  facet_wrap(~ Month, scales = "free", ncol = 1) +  # Facet by Month (only February will be shown)
  theme_minimal() +
  geom_text(aes(label = Sample.ID), vjust = -1, hjust = .6, size = 3) +  # Add labels to points
  labs(title = "Scatterplot of TN by Site and Lake during the month of August",
       x = "Site",
       y = "TN (mg N/L)") +  # Label the y-axis as TOC
  scale_y_continuous(limits = c(0, 8)) + 
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis title
    plot.margin = margin(10, 10, 10, 10),  # Increase bottom margin for more space
    strip.text = element_text(size = 10),  # Increase size of facet labels (Month)
    panel.spacing = unit(1, "lines"),  # Increase spacing between facets
    legend.position = "bottom",  # Place the legend at the bottom
    legend.box = "horizontal",  # Arrange legend items horizontally 
  )

ggplot(TN, aes(x = Sample.ID, y = TN, fill = Month)) +
  geom_bar(stat = "identity") +           # Create a bar plot
  facet_grid(~ Lake, scales = "free") +  # Facet by both Month and Lake with independent x and y axes
  theme_minimal() +                       # Use a minimal theme
  labs(title = "Bar plot of C:N by Site, Lake, and Month",
       x = "Site",
       y = "TN mg N/L") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) 


##Stacked bar chart of Leu:TdR ----
ggplot(BP, aes(x = Sample, y = Leu.TdR, fill = Month)) +
  geom_bar(stat = "identity") +           # Create a bar plot
  facet_grid(~ Lake, scales = "free") +  # Facet by both Month and Lake with independent x and y axes
  theme_minimal() +                       # Use a minimal theme
  labs(title = "Bar plot of Leu:TdR by Site, Lake, and Month",
       x = "Site",
       y = "Leu:TdR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) 
#Linear Regression models ----
##Linear regression of NPOC ----
ggplot(DOC, aes(x = Sample.ID, y = NPOC, color = Lake)) +
  geom_point() +  # Scatter plot of DOC values
  theme_minimal() +  # Use a minimal theme
  labs(title = "Linear Regression of DOC by Site and Lake",
       x = "Site",
       y = "DOC (mg/L)") +  # Label the y-axis as DOC
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.title.x = element_blank()  # Optionally, remove x-axis title
  )
