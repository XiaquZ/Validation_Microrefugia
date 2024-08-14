# Load data
library(dplyr)
library(tidyverse)

# Load data
macro <- readRDS("I:/DATA/output/MinTmicroMI_macro_diff.RDS")
str(macro)

# Extract the plot information that is needed for max temp.
plotinfo <- macro[, c(1, 5:8)]
head(plotinfo)

# Load CIT data of all plots during the summer.
load("I:/DATA/output/CommunityInferredTemp/CIT_Allsurveys_maxTSummer.RData")
colnames(plot_maxt_summ)[1] <- "plotID"
plotinfo$baselineCIT <- plot_maxt_summ$baseline[
    match(plotinfo$plotID, plot_maxt_summ$plotID)
]
