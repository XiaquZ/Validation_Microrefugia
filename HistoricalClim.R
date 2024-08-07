# Load data
load("I:/DATA/output/preparation/CleanHerbL.RData")
load("I:/DATA/input/forestREplot/version3/plot_data.RData")
min(plot_data$year_baseline_survey) #1933
max(plot_data$year_baseline_survey) #2013
plot1960s <- plot_data[plot_data$year_baseline_survey > 1960, ] #4326
min(plot_data$year_resurvey_R1) #1969
max(plot_data$year_resurvey_R1) #2023
