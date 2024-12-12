#Code made for manipulating and displaying Winter Grab 2 preliminary data
#Connor O'Loughlin
#Used Chatgpt for help with errors and general coding troubleshooting
library(dplyr)
library(ggplot2)
library(readxl)
library(openxlsx)


getwd()
setwd("C:/Users/ccolo/OneDrive/Documents/GitHub/Winter-Grab-Thesis-project/1-Data")

molar_concentration <- function(TN) {
  molar_mass_nitrogen <- 14.01
  
  tdn_g_per_L <- TN / 1000  # Convert to grams per liter
  
  molar <- tdn_g_per_L / molar_mass_nitrogen  # Calculate molar concentration
  
  return(molar)  # Return the molar concentration
}


TOC_filtered$TDN_M <- molar_concentration(TOC_filtered$TN)  # Apply 

molar_concentration_C <- function(TN) {
  molar_mass_nitrogen <- 14.01
  
  tdn_g_per_L <- TN / 1000  # Convert to grams per liter
  
  molar <- tdn_g_per_L / molar_mass_nitrogen  # Calculate molar concentration
  
  return(molar)  # Return the molar concentration
}




TOC_filtered$TN_M <- molar_concentration(TOC_filtered$TN)  # Apply 


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


get_season <- function(month) {
  if(month %in% c('February', 'March')) {
    return('Winter')
  } else if(month == 'May') {
    return('Spring')
  } else if(month %in% c('July', 'August')) {
    return('Summer')
  } else {
    return('Other')  # You can add other seasons as needed
  }
}

#Merging the Bacterial Production data
BP <- bind_rows(Aug_BP, Feb_BP, May_BP)


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
  mutate(C_N = `DOC_M` / `TN_M`)  # Create new C:N column by dividing NPOC by TN  

TOC_filtered$C.N <- TOC_filtered$DOC_M / TOC_filtered$TN_M


# Apply the function to create the 'Season' column based on the 'Month' column
DOC$Season <- sapply(DOC$Month, get_season)
TN$Season <- sapply(TN$Month, get_season)
TOC_filtered$Season <- sapply(TOC_filtered$Month, get_season)
BP$Season <- sapply(BP$Month, get_season)  

#Creating data frames for each month so we can display them separately
DOC_Winter <- DOC %>% filter(Season == "Winter")
DOC_Spring <- DOC %>% filter(Season == "Spring")
DOC_Summer <- DOC %>% filter(Season == "Summer")

  
TN_Winter <- TN %>% filter(Season == "Winter")
TN_Spring <- TN %>% filter(Season == "Spring")
TN_Summer <- TN %>% filter(Season == "Summer")

BP_Winter <- BP %>% filter(Season == "Winter")
BP_Spring <- BP %>% filter(Season == "Spring")
BP_Summer <- BP %>% filter(Season == "Summer")


#Setting the factor levels for the variable Month
##This is so we can display the graphs in chronological order
BP$Season <- factor(BP$Season, 
                   levels = c("Summer", "Spring", "Winter"))
DOC$Season <- factor(DOC$Season, 
                    levels = c("Summer", "Spring", "Winter"))
TN$Season <- factor(TN$Season, 
                    levels = c("Summer", "Spring", "Winter"))
TOC_filtered$Season <- factor(TOC_filtered$Season, 
                    levels = c("Summer", "Spring", "Winter"))



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
##Bar chart of C:N ----
ggplot(TOC_filtered, aes(x = Sample.ID, y = C_N, fill = Season)) +
  geom_bar(stat = "identity", position = "Dodge") +           # Create a bar plot
  theme_minimal() +                       # Use a minimal theme
  labs(x = "Site",
       y = "C:N") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))+
  geom_hline(yintercept = 6.625, 
             linetype = "dashed", 
             color = "red", 
             size = 1 ,
             aes(color = "Redfield's Ratio")) +  # Add the line and label it in the legend
  scale_color_manual(name = "Legend", 
                     values = c("Redfield's Ratio" = "red"))  # Custom color for the line in the legend


ggplot(TOC_filtered, aes(x = TN_M, y = DOC_M, fill = Season)) +
  xlab("TDN (M)") + 
  ylab("DOC (M)") +
  geom_point(size = 10, shape = 22, color = "black") +  # Black outline for points, fill color by Season
  theme(legend.position = "right",  # Hide legend
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(size = 16, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(-0.25, "cm")) +
  geom_abline(intercept = 0, slope = 6.625, linetype = "dashed", color = "darkgray", linewidth = 2)

#Barcahrt
ggplot(TOC_filtered, aes(x = Sample.ID, y = C_N, fill = Lake)) +
  geom_bar(stat = "identity", position = "dodge") +  # Create a side-by-side bar plot
  facet_wrap(~ Season) +                             # Facet by Season
  theme_minimal() +                                  # Use a minimal theme
  labs(title = "Bar plot of C:N by Site, Season, and Lake",
       x = "Site",
       y = "C:N") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))  # Adjust x-axis text
##DOC plots ----
##Multi-Facted display of scatterplots showing DOC conc in lakes
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

#Barchart of DOC
ggplot(DOC, aes(x = Sample.ID, y = NPOC, fill = Season)) +
  geom_bar(stat = "identity", position = "Dodge", width = 1) +           # Create a bar plot
  theme_minimal() +                       # Use a minimal theme
  labs(x = "Site",
       y = "DOC mg C/L") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))

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

##Graphical displays for TDN ----
##Multi-Facted displat of scatterplots showing N conc in lakes
ggplot(TN, aes(x = Sample.ID, y = TN, color = Lake)) +
  geom_point() +  # Create scatterplot
  facet_wrap(~ Month, scales = "free", ncol = 3) +  # Facet by Month
  theme_minimal() +
  geom_text(aes(label = Sample.ID), vjust = -1, hjust = .6, size = 3) +# Use a minimal theme
  labs(x = "Site",
       y = "TDN (mg N/L)") +  # Label the y-axis as TOC
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis title
    plot.margin = margin(10, 10, 80, 10),  # Increase bottom margin for more space
    strip.text = element_text(size = 10),  # Increase size of facet labels (Month)
    panel.spacing = unit(1, "lines")  # Increase spacing between facets
  )

#Barchart for TN
ggplot(TN, aes(x = Sample.ID, y = TN, fill = Season)) +
  geom_bar(stat = "identity", position = "Dodge") +           # Create a bar plot
  theme_minimal() +                       # Use a minimal theme
  labs(x = "Site",
       y = "TDN mg N/L") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))

#Stackec Barchart for TN
ggplot(TN, aes(x = Sample.ID, y = TN, fill = Month)) +
  geom_bar(stat = "identity") +           # Create a bar plot
  facet_grid(~ Lake, scales = "free") +  # Facet by both Month and Lake with independent x and y axes
  theme_minimal() +                       # Use a minimal theme
  labs(title = "Bar plot of TN by Site, Lake, and Month",
       x = "Site",
       y = "TN mg N/L") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))

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

ggplot(BP, aes(x = Sample, y = Leu.TdR, fill = Season)) +
  geom_bar(stat = "identity", position = "Dodge") +           # Create a bar plot
  theme_minimal() +                       # Use a minimal theme
  labs(x = "Site",
       y = "Leu:TdR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))


p11 <- ggplot(BP, aes(x = TdR_nM, y = Leu_nM, fill = Season)) +
  xlab("TdR (nmol TdR/L/d)") + 
  ylab("Leu (nmol Leu/L/d)") +
  geom_point(size = 10, shape = 22, color = "black") +  # Black outline for points, fill color by Season
  theme(legend.position = "right",  # Hide legend
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(size = 16, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(-0.25, "cm")) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkgray", linewidth = 2)  # Custom color palette for each season
p11
