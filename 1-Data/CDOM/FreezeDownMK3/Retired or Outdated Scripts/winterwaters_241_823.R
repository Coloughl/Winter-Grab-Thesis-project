#Modified version of Maci's code from the winter waters project

library(eemR)
library(dplyr)
ls("package:eemR")

#Giant pile of help commands
?eemR
?eem_remove_scattering
#read in uncorrected EEMS
#file <- system.file("//cloud/project", 
#                   package = "eemR")
eems <- eem_read("//cloud/project/FDE3_19DEC23/EEMSCombined", import_function = "aqualog")
summary(eems)
plot(eems, which = 1)

#Read in and subtract Blank
blank <- eem_read("//cloud/project/FDE3_19DEC23/Blank", import_function = "aqualog")
eems <- eem_remove_blank(eems, blank)
summary(eems)

#remove Rayleigh scattering
##order 1 scattering, 10 nm width
eems <- eem_remove_scattering(eems, "rayleigh", 1, 20) 
##order 2 scattering
eems <- eem_remove_scattering(eems, "rayleigh", 2, 20) #%>%
#eem_remove_scattering("raman", 2, 10)
plot(eems, which = 1)

#remove Raman scattering
##order 1 scattering
eems <- eem_remove_scattering(eems, "raman", 1, 20)
##order 1 scattering
eems <- eem_remove_scattering(eems, "raman", 2, 20)
plot(eems, which = 1)

#read in absorbance data
absorbance <- read.csv("Compiled_Absorbance_19_Dec_23.csv",header=TRUE)
head(absorbance)

#run inner filter effect correction
eems <- eem_inner_filter_effect(
  eem = eems,
  absorbance = absorbance,
  pathlength = 1
)
plot(eems, xlab = "Excitation", which = 1, horizontal = TRUE)


eems <- eem_raman_normalisation(eems, blank)
summary(eems)

#coblepeaks
eem_coble_peaks(eems, verbose = TRUE)

#calculate fluorescence indices #will get warning messages that data was interpolated
eem_biological_index(eems, verbose = TRUE)
FreezeDown_eems_bio_index <- eem_biological_index(eems, verbose = TRUE)
FreezeDown_eems_fluor_index <- eem_fluorescence_index(eems, verbose = TRUE)
FreezeDown_eems_humi_index <- eem_humification_index(eems, scale = FALSE, verbose = TRUE)

View(FreezeDown_eems_bio_index)
View(FreezeDown_eems_fluor_index)
View(FreezeDown_eems_humi_index)

#export fluroescence indices as .csv file
write.table(FreezeDown_eems_bio_index, file = "FreezeDown_eems_bio_index.csv", append = FALSE, quote = TRUE, sep = ",")
write.table(FreezeDown_eems_fluor_index, file = "FreezeDown_eems_fluor_index.csv", append = FALSE, quote = TRUE, sep = ",")
write.table(FreezeDown_eems_humi_index, file = "FreezeDown_eems_humi_index.csv", append = FALSE, quote = TRUE, sep = ",")

#export corrected eems into matlab
eem_export_matlab("FD_E3_19DEC23.mat", eems)

####################################
####################################
####################################
#In this section, I will be splitting by T0 and T1 time points to generate two
#separate plots/data sets









