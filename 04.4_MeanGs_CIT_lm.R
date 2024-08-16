# Load data
library(dplyr)
library(tidyverse)
library(ggeffects)

# Load data
macro <- readRDS("I:/DATA/output/MinTmicroMI_macro_diff.RDS")

str(macro)

# Extract the plot information that is needed for max temp.
match("macro_diff", names(macro))
plotinfo <- macro[, c(1, 5:8, 15)]
head(plotinfo)

# Load CIT data of all plots during the summer.
load("I:/DATA/output/CommunityInferredTemp/CIT_Allsurveys_meanTempGs.RData")
anyNA(plot_meangs$baseline) # T
head(plot_meangs)
colnames(plot_meangs)[1] <- "plotID"

# Add the baseline CIT of each plot to the plots with macro_diff.
plotinfo$baselineCIT <- plot_meangs$baseline[
    match(plotinfo$plotID, plot_meangs$plotID)
]
anyNA(plotinfo$baselineCIT) # F

# Add the most recent resurvey CIT of each plot.
plot_meangs <- plot_meangs %>%
    mutate(
        Resurvey_cit = case_when(
            !is.na(R5) ~ R5,
            !is.na(R4) ~ R4,
            !is.na(R3) ~ R3,
            !is.na(R2) ~ R2,
            TRUE ~ R1
        )
    )

plotinfo$ResurveyCIT <- plot_meangs$Resurvey_cit[
    match(plotinfo$plotID, plot_meangs$plotID)
]
anyNA(plotinfo$ResurveyCIT) # F
head(plotinfo)

# Check if the resurvey year is correct in the plot info.
load("I:/DATA/input/forestREplot/version3/plot_data.RData")
# Load the plot_data to get the survey year.
head(plot_data)
plot_data <- plot_data[, c(1, 9:14)]
plot_data <- as_tibble(plot_data)
head(plot_data)

plot_meangs <- merge(plot_meangs, plot_data, by = "plotID")
head(plot_meangs)

# Add the year of recent survey to a new column.
plot_meangs <- plot_meangs |>
    mutate(
        Resuvey_year = case_when(
            Resurvey_cit == R5 ~ year_resurvey_R5,
            Resurvey_cit == R4 ~ year_resurvey_R4,
            Resurvey_cit == R3 ~ year_resurvey_R3,
            Resurvey_cit == R2 ~ year_resurvey_R2,
            TRUE ~ year_resurvey_R1
        )
    ) |>
    as_tibble()
head(plot_meangs)

# Remove columns that won't be used.
match("year_resurvey_R1", names(plot_meangs))
plot_meangs <- plot_meangs[, -c(3:7, 10:14)]
head(plot_meangs)
plot_meangs <- subset(plot_meangs, plotID %in% plotinfo$plotID)
head(plot_meangs)
## Identical to the plot info.

# Calculate delta CIT and delta year between baseline and recent survey.
plotinfo <- plotinfo |>
    mutate(
        deltaCIT = plotinfo$ResurveyCIT - plotinfo$baselineCIT,
        deltaYr = plotinfo$ResuveyYr - plotinfo$year_baseline_survey
    )

# Check for na values in delta cit.
sum(is.na(plotinfo$deltaCIT)) # 0
sum(is.na(plotinfo$deltaYr)) # 0
head(plotinfo)

# Calculate CIT change per year.
plotinfo$CITperYr <- plotinfo$deltaCIT / plotinfo$deltaYr

# Add the microrefugia index.
plotinfo$MI_wm <- macro$MI_wm[
    match(plotinfo$plotID, macro$plotID)
]

plotinfo$MI_FV <- macro$MI_FV[
    match(plotinfo$plotID, macro$plotID)
]

plotinfo$MI_BV <- macro$MI_BV[
    match(plotinfo$plotID, macro$plotID)
]

# Test the relationship between delta cit and macroclimate change.
lm_macro <- lm(deltaCIT ~ macro_diff, data = plotinfo)
summary(lm_macro) # Not significant

# Forward velocity versus CIT change per year.
lm_fv <- lm(CITperYr ~ MI_FV, data = plotinfo)
summary(lm_fv)
gpred_fv <- ggpredict(lm_fv)
plot(gpred_fv)

# Backward velocity versus CIT change per year
lm_bv <- lm(CITperYr ~ MI_BV, data = plotinfo)
summary(lm_bv)
gpred_bv <- ggpredict(lm_bv)
plot(gpred_bv) # Not significant

# Warming magnitude.
lm_wm <- lm(deltaCIT ~ MI_wm, data = plotinfo)
summary(lm_wm) # Weak significant.
gpred_wm <- ggpredict(lm_wm)
plot(gpred_wm)

# Try with interaction.
lm_macro_fv <- lm(deltaCIT ~ macro_diff * MI_FV, data = plotinfo)
summary(lm_macro_fv)
gpred_macrofv <- ggpredict(lm_macro_fv, c("MI_FV","macro_diff [0.4, 1.0, 1.6]"))
plot(gpred_macrofv)

lm_macro_wm <- lm(deltaCIT ~ macro_diff * MI_wm, data = plotinfo)
summary(lm_macro_wm)
gpred_macro_wm <- ggpredict(lm_macro_wm, c("MI_wm","macro_diff [0.4, 1.0, 1.6]"))
plot(gpred_macro_wm)

lm_macro_bv <- lm(deltaCIT ~ macro_diff * MI_BV, data = plotinfo)
summary(lm_macro_bv)
gpred_macro_bv <- ggpredict(lm_macro_bv, c("MI_BV","macro_diff [0.4, 1.0, 1.6]"))
plot(gpred_macro_bv)
