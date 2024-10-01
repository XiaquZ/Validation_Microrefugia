library(dplyr)
library(tidyverse)

# Load data
load("I:/DATA/output/forestREplot/EU_TreeShrubL.RData")
load("I:/DATA/output/MicroClimPlant_CIT/MicroClimPlantCIT_MIs.RData")
load("I:/DATA/input/forestREplot/version3.1/plot.data_forestREplot_V3.1.RData")

# Data that only contain plots with first survey after 1950
n_after1950 <- micro_cit[(micro_cit$first_year >= 1950), ]
save(n_after1950, file = "I:/DATA/output/MicroClimPlant_REplotV3.1_plots1950s.RData")

# Check the forest managment type/formaer land use of selected plots.
manag_df <- plot_data$former_landuse[match(n_after1950$plotID, plot_data$plotID)]
unique(manag_df)

# Add the canopy cover of first survey
unique(n_after1950$survey_time) # [1] "baseline" "R1"
unique(n_after1950$resurvey_time) # [1] "R1" "R3" "R2" "R4"
y <- n_after1950 %>%
    mutate(
        cover_first_survey = if_else(
            survey_time == "baseline",
            treeshrub$baseline[match(plotID, treeshrub$sample)],
            treeshrub$R1[match(plotID, treeshrub$sample)]
        )
    )

# Add the canopy cover of resurvey
y <- y %>%
    mutate(
        cover_resurvey = ifelse(
            resurvey_time == "R1",
            treeshrub$R1[match(plotID, treeshrub$sample)],
            ifelse(
                resurvey_time == "R2",
                treeshrub$R2[match(plotID, treeshrub$sample)],
                ifelse(
                    resurvey_time == "R3",
                    treeshrub$R3[match(plotID, treeshrub$sample)],
                    treeshrub$R4[match(plotID, treeshrub$sample)]
                )
            )
        )
    )
plots_micro <- y

# Calculate canopy cover change
plots_micro <- plots_micro |>
    mutate(
        delta_cover = plots_micro$cover_resurvey - plots_micro$cover_first_survey
    )
hist(plots_micro$delta_cover)
head(plots_micro)
cor.test(plots_micro$delta_cover, plots_micro$deltaCIT)
save(plots_micro, file = 'I:/DATA/output/MicroClimPlant_REplotV3.1_1950s.RData')

# Check the distribution of survey year
hist(plots_micro$first_year)
max(plots_micro$first_year) #2013
min(plots_micro$first_year) #1950
hist(plots_micro$resurvey_year)
max(plots_micro$resurvey_year) #2023
min(plots_micro$resurvey_year) #1987
