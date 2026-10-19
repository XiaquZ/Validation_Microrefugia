# Load data
library(dplyr)
library(tidyverse)
library(ggeffects)

# Load data
macro <- readRDS("I:/DATA/output/MinTmicroMI_macro_diff.RDS")

str(macro)

# Extract the plot information that is needed for max temp.
match("macro_diff", names(macro))
plotinfo <- macro[, c(1, 5:8, 15:18)]
head(plotinfo)

# Load CIT data of all plots during the summer.
load("I:/DATA/output/CommunityInferredTemp/CIT_Allsurveys_MAT.RData")
anyNA(plot_mat$baseline) # T
head(plot_mat)
colnames(plot_mat)[1] <- "plotID"

# Add the baseline CIT of each plot to the plots with macro_diff.
plotinfo$baselineCIT <- plot_mat$baseline[
    match(plotinfo$plotID, plot_mat$plotID)
]
anyNA(plotinfo$baselineCIT) # F

# Add canopy cover change to the plot info.
plotinfo$CanopyChange <- macro$canopyChange[
    match(plotinfo$plotID, macro$plotID)
]

# Get the most recent resurvey CIT of each plot.
plot_mat <- plot_mat %>%
    mutate(
        Resurvey_cit = case_when(
            !is.na(R5) ~ R5,
            !is.na(R4) ~ R4,
            !is.na(R3) ~ R3,
            !is.na(R2) ~ R2,
            TRUE ~ R1
        )
    )

plotinfo$ResurveyCIT <- plot_mat$Resurvey_cit[
    match(plotinfo$plotID, plot_mat$plotID)
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

plot_mat <- merge(plot_mat, plot_data, by = "plotID")
head(plot_mat)

# Add the year of recent survey to a new column.
plot_mat <- plot_mat |>
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
head(plot_mat)

# Remove columns that won't be used.
match("year_resurvey_R1", names(plot_mat))
plot_mat <- plot_mat[, -c(3:7, 10:14)]
head(plot_mat)
plot_mat <- subset(plot_mat, plotID %in% plotinfo$plotID)
head(plot_mat)
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
hist(plotinfo$deltaCIT)
# Calculate CIT change per year.
plotinfo$CITperYr <- plotinfo$deltaCIT / plotinfo$deltaYr

# Test the relationship between delta cit and macroclimate change.
lm_macro <- lm(deltaCIT ~ macro_diff, data = plotinfo)
summary(lm_macro) # Not significant
save(lm_macro, file = "I:/DATA/output/lm_rdata/lm_macro_diff_cit.RData")
pred_macro <- predict_response(
    lm_macro, "macro_diff",
    margin = "mean_mode"
)
pred_macro

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

#### fit model with 4-way-interaction ####
fit4 <- lm(deltaCIT ~ macro_diff * MI_wm * MI_FV * MI_BV, data = plotinfo)

# adjusted predictions for all 4 interaction terms
pr <- predict_response(fit4, c("MI_wm", "macro_diff [0.4, 1.0, 1.6]"))

# Try with interaction between macro_diff and other variables.
# Macro diff with forward velocity MI.
lm_macro_fv <- lm(deltaCIT ~ macro_diff * MI_FV, data = plotinfo)
summary(lm_macro_fv)
save(lm_macro_fv, file = "I:/DATA/output/lm_rdata/lm_macro_fv.RData")
pred <- predict_response(
    lm_macro_fv, c("MI_FV", "macro_diff [0.4, 1.0, 1.6]"),
    margin = "mean_mode"
)
pred
plot(pred)

# Macro diff with warming magnitude MI.
lm_macro_wm <- lm(deltaCIT ~ macro_diff * MI_wm, data = plotinfo)
summary(lm_macro_wm)
save(lm_macro_wm, file = "I:/DATA/output/lm_rdata/lm_macro_wm.RData")

gpred_macro_wm <- ggpredict(
    lm_macro_wm, c("MI_wm", "macro_diff [0.4, 1.0, 1.6]")
)
plot(gpred_macro_wm)

# Macroclimate difference with backward vel MI.
lm_macro_bv <- lm(deltaCIT ~ macro_diff * MI_BV, data = plotinfo)
summary(lm_macro_bv) # Not significant.
save(lm_macro_bv, file = "I:/DATA/output/lm_rdata/lm_macro_bv.RData")

gpred_macro_bv <- ggpredict(
    lm_macro_bv, c("MI_BV", "macro_diff [0.4, 1.0, 1.6]")
)
plot(gpred_macro_bv)

# load package
library(sjPlot)
library(sjmisc)
library(sjlabelled)

tab_model(lm_macro, lm_macro_fv, lm_macro_bv, lm_macro_wm,
    title = "Table 1. Regression between CIT, macroclimate and microrefugia indices ",
    transform = NULL,
    file = "I:/DATA/output/tmp/lm_MEB.html"
)

# Generate spatial point vector to add in the map.
library(sf)
library(mapview)
library(terra)

offset <- rast("E:/Output/ForestOffset/mean/mean_ForestTempMaxTOffset_V3.tif")

colSums(is.na(plotinfo))
plotinfo_nona <- na.omit(plotinfo)

plots_xy <- plotinfo_nona[, c(1:3)]

# Convert data frame to sf object
eu_plots_sf <- st_as_sf(
    x = plots_xy,
    coords = c("longitude", "latitude"),
    crs = "+proj=longlat +datum=WGS84"
)

mapview(eu_plots_sf)

eu_plots_st <- st_transform(eu_plots_sf, crs = st_crs(offset))
mapview(eu_plots_st)

# Extract the microclimate values.
eu_plots_st <- vect(eu_plots_st)
writeVector(
    eu_plots_st, filename = "I:/DATA/output/resurvey_plots.shp", overwrite = TRUE
    )
