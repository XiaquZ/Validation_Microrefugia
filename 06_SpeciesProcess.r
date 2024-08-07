library(dplyr)
library(tidyverse)

load("I:/DATA/input/forestREplot/version3/plot_data.RData")
# Load data
load("I:/DATA/output/preparation/CleanHerbL.RData")
# ---- 8. SUBDIVISION CIT ----

# Extract all plot sample ID's and species names of the herb layer
species_proce <- spe_herb[, c("sample", "species_name")]
# Select the site number EU_080, EU_079, 078, 076.
list_plots <- c(
    "EU_080_", "EU_079_",
    "EU_078_", "EU_076_"
)
species_proce <- species_proce[grepl(
    paste(list_plots, collapse = "|"),
    species_proce$sample
), ]

# Split the sample name to keep the survey type of each plot.
split_parts <- strsplit(species_proce$sample, "_")
species_proce$first_three <- sapply(
    split_parts,
    function(x) paste(x[1], x[2], x[3], sep = "_")
)
species_proce$last_part <- sapply(split_parts, function(x) x[4])
species_proce <- species_proce[, -1]
head(species_proce)
species_proce <- species_proce[, c(2, 1, 3)]
head(species_proce)
colnames(species_proce)[1] <- "plotID"
colnames(species_proce)[3] <- "surveyTyp"

# Change the order of the columns to a more logical order
col_order <- c("plotID", "surveyTyp", "species_name")
species_proce <- species_proce[, col_order]
unique(species_proce$surveyTyp)
unique(species_proce$plotID)
list_values <- c(
    "R1", "R2"
)
species_proce <- subset(
    species_proce,
    grepl(
        paste0(list_values, collapse = "|"),
        surveyTyp
    )
)
# Every species present in the resurvey 1 gets a score of 1,
# and each species in the resurvey 2 a score of 2.
species_proce$score <- ifelse(species_proce$surveyTyp == "R1", 1, 2)

# The CIT data is now in long format,
# so first it needs to be converted to a wide format data frame
# to separate the baseline surveys
# and resurveys into separate columns for further calculations
species_proce <- spread(species_proce, surveyTyp, score)
anyNA(species_proce$R1)
# All species that are not present in a certain survey get a score of 0
species_proce$R1[is.na(species_proce$R1)] <- 0
species_proce$R2[is.na(species_proce$R2)] <- 0

# Calculate species process score by substracting R1 from R2.
species_proce$process <- species_proce$R2 - species_proce$R1

# Add the species abundance data and the temperature data
# from Climplant of both surveys to the data frame.
load("I:/DATA/output/preparation/Replot_MmaxT_Summer.RData")

# For maximum temperature during the summer.
list_plots <- c(
    "EU_080_", "EU_079_",
    "EU_078_", "EU_076_"
)
species_summ <- herb_maxTSum[grepl(
    paste(list_plots, collapse = "|"),
    herb_maxTSum$sample
), ]

species_summ <- species_summ[, -3]

# Split the sample name to keep the survey type of each plot.
split_parts <- strsplit(species_summ$sample, "_")
species_summ$first_three <- sapply(
    split_parts,
    function(x) paste(x[1], x[2], x[3], sep = "_")
)
species_summ$last_part <- sapply(split_parts, function(x) x[4])
species_summ <- species_summ[, -1]
head(species_summ)
match("first_three", names(species_summ)) # get the column number
species_summ <- species_summ[, c(5, 6, 1, 2, 4, 3)]
head(species_summ)
colnames(species_summ)[1] <- "plotID"
colnames(species_summ)[2] <- "surveyTyp"

# Only keep R1 and R2.
list_values <- c(
    "R1", "R2"
)
species_summ <- subset(
    species_summ,
    grepl(
        paste0(list_values, collapse = "|"),
        surveyTyp
    )
)

# Convert data into wide format to category speices into baseline and resurvey.
species_summ <- pivot_wider(
    species_summ,
    names_from = surveyTyp,
    values_from = abundance
)

# Give the columns more clear names
colnames(species_summ)[5:6] <- c("R1_abundance", "R2_abundance")
head(species_summ)

# Join the species scores with the abundance data and the
# species temperature data from ClimPlant based on the plotIDs and species names
out_1 <- anti_join(species_summ, species_proce,
    by = c("plotID", "species_name")
)
out_2 <- anti_join(species_proce, species_summ,
    by = c("plotID", "species_name")
)
output <- left_join(species_summ, species_proce,
    by = c("plotID", "species_name")
)
## All the species in the forest Replot cannot all find their temperature values
## in the ClimPlants. Species do not have temp values are in out_2.
unique(output$process)
# Split the species per plot into different data frames based on their sprocess
sp_disappear <- output[grep("-1", output$process), ]
sp_new <- output[grep("2", output$process), ]
sp_remain <- subset(output, process == "1")

#### Now the CITs of the three different processes can be calculated ####

# For the disappearing species (only present in the R1 survey)
cit_disappear <- sp_disappear %>%
    group_by(plotID) %>%
    summarise(
        CIT_disappear = weighted.mean(
            mean_maxTSumm, R1_abundance,
            na.rm = T
        )
    )

# For the newly occurring species (only present in the resurvey 2)
# calculate the CIT based on the abundance of the resurvey 2.
cit_new <- sp_new %>%
    group_by(plotID) %>%
    summarise(
        CIT_new = weighted.mean(
            mean_maxTSumm, R2_abundance,
            na.rm = T
        )
    )

# For the remaining species (present in both R1 and R2 resurvey)
sp_remain <- sp_remain[!is.na(sp_remain$R2_abundance), ]
## remove plots without abundance data for R2.

cit_remain <- sp_remain %>%
    group_by(plotID) %>%
    summarise(
        CIT_remain = weighted.mean(
            mean_maxTSumm, R2_abundance,
            na.rm = T
        )
    )


#### CITs species process will be compared  to the general CIT ####

# By visually comparing the CITs based on the processes (as density plots)
# and the general CIT for the baseline survey and the resurvey (mean as vertical lines)

# Extract the CIT of R1 and R2.
load("I:/DATA/output/CommunityInferredTemp/CIT_Allsurveys_maxTSummer.RData")

plot_maxt_summ <- plot_maxt_summ[, c(1:4)]
# For maximum temperature during the summer.
list_plots <- c(
    "EU_080_", "EU_079_",
    "EU_078_", "EU_076_"
)
plot_maxt_summ <- plot_maxt_summ[grepl(
    paste(list_plots, collapse = "|"),
    plot_maxt_summ$sample
), ]
colnames(plot_maxt_summ)[1] <- "plotID"
head(plot_maxt_summ)
# Join the CITs of the three processes to combine them in one figure
cit_all <- full_join(plot_maxt_summ, cit_disappear, by = "plotID")
cit_all <- full_join(cit_all, cit_new, by = "plotID")
cit_all <- full_join(cit_all, cit_remain, by = "plotID")
cit_all <- cit_all[, -2]
colnames(cit_all)[4:6] <- c("species_loss", "species_gain", "Change_abundance")
head(cit_all)
cit_all_nona <- cit_all[complete.cases(cit_all), ]
# Convert to wide data format to plot multiple density plots.
cit_all02 <- pivot_longer(
    cit_all,
    cols = c("species_loss", "species_gain", "Change_abundance"),
    names_to = "process",
    values_to = "CIT"
)
colnames(cit_all02) <- c("plotID", "CIT_R1", "CIT_R2", "process", "processCIT")
head(cit_all02)
hist(cit_all$R2)
save(cit_all,
    file = "I:/DATA/output/SpeciesChanges/CIT_maxtSummer_SpeciesProcess.RData"
)
load("I:/DATA/output/SpeciesChanges/CIT_maxtSummer_SpeciesProcess.RData")
