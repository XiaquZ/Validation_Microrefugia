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
# Check the year of each survey.
plot_data <- plot_data[grepl(
    paste(list_plots, collapse = "|"),
    plot_data$plotID
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

# All species that are not present in a certain survey get a score of 0
scoring_species_processes["B"][is.na(scoring_species_processes["B"])] <- 0
scoring_species_processes["R1"][is.na(scoring_species_processes["R1"])] <- 0
scoring_species_processes["R2"][is.na(scoring_species_processes["R2"])] <- 0
scoring_species_processes["R3"][is.na(scoring_species_processes["R3"])] <- 0
scoring_species_processes["R4"][is.na(scoring_species_processes["R4"])] <- 0
scoring_species_processes["R5"][is.na(scoring_species_processes["R5"])] <- 0

# Join the scoring data frame with the CIT data frame (based on plotID) to calculate the score of the different processes
# based on the latest resurvey for which there is CIT data
# It does not matter which CIT we use for this, because we just want to know for which resurvey there is CIT data and this is the same for the three CITs
scoring_species_processes <- left_join(scoring_species_processes, CIT_maxTempGS, by = "plotID")

# Calculate the species score for the different processes per plot by subtracting the score of the baseline survey with the score of the latest resurvey of that plot
# This is again done with a nested ifelse statement
# Explanation code: if there is a CIT value for resurvey 5 (is.na()==FALSE), then subtract score baseline survey from score resurvey 5.
# If not (is.na()==TRUE), then see if there is a value for resurvey 4.
# If this is the case, then subtract score baseline survey from score resurvey 4. If not, see if there is a value for resurvey 3
# and continue to do this until all species per plot have a score based on the process they went through between the baseline survey and the latest resurvey
scoring_species_processes$species_process_score <- ifelse(is.na(scoring_species_processes$R5_CIT_maxTempGS) == F,
    scoring_species_processes$B - scoring_species_processes$R5,
    ifelse(is.na(scoring_species_processes$R4_CIT_maxTempGS) == F,
        scoring_species_processes$B - scoring_species_processes$R4,
        ifelse(is.na(scoring_species_processes$R3_CIT_maxTempGS) == F,
            scoring_species_processes$B - scoring_species_processes$R3,
            ifelse(is.na(scoring_species_processes$R2_CIT_maxTempGS) == F,
                scoring_species_processes$B - scoring_species_processes$R2,
                scoring_species_processes$B - scoring_species_processes$R1
            )
        )
    )
)
