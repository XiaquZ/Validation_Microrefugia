library(dplyr)
library(stringr)

# Plot data of all plots in ForestREplot database
load("I:/DATA/input/forestREplot/version3.1/plot.data_forestREplot_V3.1.RData")
# Vegetation data of all plots
load("I:/DATA/input/forestREplot/version3.1/veg.data_forestREplot_V3.1.RData")

# EU_010b, all the plots have the same coordination.
eu10b <- plot_data[grep("EU_010b", plot_data$plotID), ] # herb layer (H)
