library(dplyr)
library(stringr)

# Plot data of all plots in ForestREplot database
load("I:/DATA/input/forestREplot/version3.1/plot.data_forestREplot_V3.1.RData")
# Vegetation data of all plots
load("I:/DATA/input/forestREplot/version3.1/veg.data_forestREplot_V3.1.RData")

# EU_010b, all the plots have the same coordination.
eu10b <- plot_data[grep("EU_010b", plot_data$plotID), ] # herb layer (H)

# Plot with wrong latitude and longitude.
# EU_082: latitude and longitude swapped (all 281 plots) -> corrected in
# CIT/01_CleanData.R, saved as I:/DATA/output/forestREplot/EU_PlotData_v3.1.RData
