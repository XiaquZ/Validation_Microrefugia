# Load data
library(dplyr)
library(tidyverse)
library(ggeffects)

# Load data
macro <- readRDS("I:/DATA/output/MinTmicroMI_macro_diff.RDS")
str(macro)

# Load microclimate cit data from 02.4_
load("I:/DATA/output/CommunityInferredTemp/CIT_microTemp_AllSurvey.RData")
anyNA(plot_micro$baseline) # T
head(plot_micro)
colnames(plot_micro)[1] <- "plotID"

# Load the plot_data
load("I:/DATA/input/forestREplot/version3.1/plot.data_forestREplot_V3.1.RData")
head(plot_data)
plotinfo <- plot_data[, c(1:3, 9:14)]

# Add canopy cover change to the plot info.
load("I:/DATA/output/forestREplot/EU_TreeShrubL.RData")
vegtreesh_totalC <- veg_treeshrub |>
    group_by(sample) |>
    summarise(total_cover = sum(abundance))
head(vegtreesh_totalC)
anyNA(vegtreesh_totalC$total_cover)
# Get the canopy cover change per plot.
# Baseline

treeshrub <- vegtreesh_totalC[grep("_B", vegtreesh_totalC$sample), ]
treeshrub$sample <- str_replace(treeshrub$sample, "_B", "") # get plotid.
colnames(treeshrub)[2] <- "baseline"
head(treeshrub) #4438 plots

# R1
r1 <- vegtreesh_totalC[grep("_R1", vegtreesh_totalC$sample), ] # 584 obs
colnames(r1)[2] <- "R1"
r1$sample <- str_replace(r1$sample, "_R1", "") # get plotid.
head(r1) #4359
# Merge survey data.
treeshrub <- merge(treeshrub, r1, by = "sample", all = T)
head(treeshrub)#4502 
anyNA(treeshrub$baseline) # some plots do not have abundance for baseline.

# Check which plots have a second resurvey R2.
r2 <- vegtreesh_totalC[grep("_R2", vegtreesh_totalC$sample), ]
colnames(r2)[2] <- "R2"
r2$sample <- str_replace(r2$sample, "_R2", "") # get plotid.
head(r2)

# merge r2 data to the plot data.
treeshrub <- merge(treeshrub, r2, by = "sample", all = TRUE)
head(treeshrub)

# Check which plots have a resurvey R3.
r3 <- vegtreesh_totalC[grep("_R3", vegtreesh_totalC$sample), ] # 18 obs
colnames(r3)[2] <- "R3"
r3$sample <- str_replace(r3$sample, "_R3", "") # get plotid.
# merge r3 data to the plot data.
treeshrub <- merge(treeshrub, r3, by = "sample", all = TRUE)
head(treeshrub)

# Check which plots have a resurvey R4.
r4 <- vegtreesh_totalC[grep("_R4", vegtreesh_totalC$sample), ]
r4$sample <- str_replace(r4$sample, "_R4", "") # get plotid.
# merge r4 data to the plot data.
treeshrub <- merge(treeshrub, r4, by = "sample", all = TRUE)
head(treeshrub)

# Check which plots have a resurvey R5.
r5 <- vegtreesh_totalC[grep("_R5", vegtreesh_totalC$sample), ]
colnames(r5)[2] <- "R5"
r5$sample <- str_replace(r5$sample, "_R5", "") # get plotid.
# merge r5 data to the plot data.
treeshrub <- merge(treeshrub, r5, by = "sample", all = TRUE)
head(treeshrub)

# Save tree and shrub canopy cover data.
save(treeshrub,
    file = "I:/DATA/output/forestREplot/TreeShrubCanopyCover_v3.1.RData"
)

# plotinfo$CanopyChange <- macro$canopyChange[
#     match(plotinfo$plotID, macro$plotID)
# ]
anyNA(plot_micro$baseline) # T

# Get the first survey data, if baseline is na, then use R1, R2..
x <- plot_micro %>%
    mutate(
        first_cit = case_when(
            !is.na(baseline) ~ baseline,
            !is.na(R1) ~ R1,
            !is.na(R2) ~ R2,
            !is.na(R3) ~ R3,
            TRUE ~ R4
        ),
        survey_time = case_when(
            !is.na(baseline) ~ "baseline",
            !is.na(R1) ~ "R1",
            !is.na(R2) ~ "R2",
            !is.na(R3) ~ "R3",
            TRUE ~ "R4"
        )
    )
unique(x$survey_time) # [1] "baseline" "R1"       "R2"
anyNA(x$first_cit) # F
anyNA(x$survey_time) # F

# Get the most recent resurvey CIT of each plot.
x <- x %>%
    mutate(
        Resurvey_cit = case_when(
            !is.na(R5) ~ R5,
            !is.na(R4) ~ R4,
            !is.na(R3) ~ R3,
            !is.na(R2) ~ R2,
            !is.na(R1) ~ R1,
            TRUE ~ NA
        ),
        resurvey_time = case_when(
            !is.na(R5) ~ "R5",
            !is.na(R4) ~ "R4",
            !is.na(R3) ~ "R3",
            !is.na(R2) ~ "R2",
            !is.na(R1) ~ "R1",
            TRUE ~ NA
        )
    )
anyNA(x$Resurvey_cit) # T
anyNA(x$resurvey_time)

test <- x[is.na(x$Resurvey_cit), ] # These plots only got baseline cit.
test02 <- x[!is.na(x$Resurvey_cit), ]
anyNA(test02$Resurvey_cit)
x <- test02
# Remove rows when the first survey equal to the resurvey_time
s <- subset(x, survey_time == resurvey_time)
y <- subset(x, survey_time != resurvey_time)
head(y)

# Remove the cit columns that do not need anymore
unique(y$survey_time)
unique(y$resurvey_time)

# Add the survey year.
y <- y %>%
    mutate(
        first_year = if_else(
            survey_time == "baseline",
            plotinfo$year_baseline_survey[match(plotID, plotinfo$plotID)],
            plotinfo$year_resurvey_R1[match(plotID, plotinfo$plotID)]
        )
    )
unique(y$first_year)

# Check the output data, if first_year is correct
test <- y[y$survey_time == "R1", ]
subinfo <- plotinfo[grep("EU_010b_110", plotinfo$plotID), ] # correct

# Add the resurvey year.
unique(y$resurvey_time) # [1] "R1" "R4" "R3" "R2"
y <- y %>%
    mutate(
        resurvey_year = ifelse(
            resurvey_time == "R1",
            plotinfo$year_resurvey_R1[match(plotID, plotinfo$plotID)],
            ifelse(
                resurvey_time == "R2",
                plotinfo$year_resurvey_R2[match(plotID, plotinfo$plotID)],
                ifelse(
                    resurvey_time == "R3",
                    plotinfo$year_resurvey_R3[match(plotID, plotinfo$plotID)],
                    plotinfo$year_resurvey_R4[match(plotID, plotinfo$plotID)]
                )
            )
        )
    )
unique(y$resurvey_year) # includes NA
anyNA(y$Resurvey_cit) # F

# Which rows contain na in resurvey_year
test <- y[is.na(y$resurvey_year), ]
subinfo <- plot_micro[grep("EU_083_22", plot_micro$plotID), ]
temp02 <- plotinfo[grep("EU_083_22", plotinfo$plotID), ]
## Use R1 instead of R2 for this plot.
test <- y |>
    mutate(resurvey_time = ifelse(
        plotID == "EU_083_22",
        gsub("R2", "R1", y$resurvey_time),
        resurvey_time
    ))


temp02 <- test %>%
    mutate(
        resurvey_year = ifelse(
            resurvey_time == "R1",
            plotinfo$year_resurvey_R1[match(plotID, plotinfo$plotID)],
            ifelse(
                resurvey_time == "R2",
                plotinfo$year_resurvey_R2[match(plotID, plotinfo$plotID)],
                ifelse(
                    resurvey_time == "R3",
                    plotinfo$year_resurvey_R3[match(plotID, plotinfo$plotID)],
                    plotinfo$year_resurvey_R4[match(plotID, plotinfo$plotID)]
                )
            )
        )
    )
anyNA(temp02$resurvey_year) #F
y <- temp02

# Change the resurvey cit also to R1
y <- y %>%
    mutate(
        Resurvey_cit = case_when(
            !is.na(R5) ~ R5,
            !is.na(R4) ~ R4,
            !is.na(R3) ~ R3,
            !is.na(R2) ~ R2,
            !is.na(R1) ~ R1,
            TRUE ~ NA
        )
    )
anyNA(y$resurvey_year) #F
anyNA(y$resurvey_time) #F
anyNA(y$Resurvey_cit) #F
unique(y$resurvey_time) #[1] "R1" "R4" "R3" "R2"
temp <- y[y$resurvey_time == "R3", ]
subinfo <- plotinfo[grep("EU_009b_1", plotinfo$plotID), ] # correct
micro_cit <- y
save(micro_cit, file = "I:/DATA/output/tmp/micro_cit.RData")
load("I:/DATA/output/tmp/micro_cit.RData")

# Clean the year data in Sup_CleanReplotYearData.R.
unique(micro_cit$first_year)
head(micro_cit)

# Define the list of values to search for
list_values <- c(
    "1976 \\(T, S 1975\\)", "1956-57",
    "1955 and 57", "1963\\?",
    "1998/99"
)

# Note that Some characters such as (, ), ?, and - have special meanings
# in regular expressions.
# To match them literally, you need to escape them with a double backslash \\.
micro_cit02 <- micro_cit %>%
    filter(grepl(paste(list_values, collapse = "|"), first_year))

# Replace the "1976 \\(T, S 1975\\)" with "1976".
micro_cit$first_year <- gsub(
    "1976 \\(T, S 1975\\)", "1976",
    micro_cit$first_year
)

# Replace the "1956-57" with "1957".
micro_cit$first_year <- gsub(
    "1956-57", "1957",
    micro_cit$first_year
)

# Replace the "1955 and 57" with "1956".
micro_cit$first_year <- gsub(
    "1955 and 57", "1956",
    micro_cit$first_year
)

# Replace the "1963\\?" with "1963".
micro_cit$first_year <- gsub(
    "1963\\?", "1963",
    micro_cit$first_year
)

# Replace the "1998/99" with "1999".
micro_cit$first_year <- gsub(
    "1998/99", "1999",
    micro_cit$first_year
)

# Check the baseline year again.
baseline <- unique(micro_cit$first_year)
baseline ##Nomal

# Check if the resurvey year is correct in the plot info.
unique(micro_cit$resurvey_year)
# Replace the "2019-2020" with "2020".
micro_cit$resurvey_year <- gsub(
    "2019-2020", "2020",
    micro_cit$resurvey_year
)
unique(micro_cit$resurvey_year)

# Remove columns that won't be used.
head(micro_cit)
micro_cit <- micro_cit[, -c(2:7)]
head(micro_cit)

# Calculate delta CIT and delta year between baseline and recent survey.
str(micro_cit)
micro_cit <- as_tibble(micro_cit)
micro_cit$resurvey_year <- as.numeric(micro_cit$resurvey_year)
micro_cit$first_year <- as.numeric(micro_cit$first_year)

micro_cit <- micro_cit |>
    mutate(
        deltaCIT = micro_cit$Resurvey_cit - micro_cit$first_cit,
        deltaYr = micro_cit$resurvey_year - micro_cit$first_year
    )

# Check for na values in delta cit.
anyNA(micro_cit$deltaYr)
anyNA(micro_cit$deltaCIT)
head(micro_cit)
hist(micro_cit$deltaCIT)
hist(micro_cit$deltaYr)

# Calculate CIT change per year.
micro_cit$CITperYr <- micro_cit$deltaCIT / micro_cit$deltaYr

# Add the xy information.
head(plot_data)
micro_cit$x <- plot_data$longitude[match(micro_cit$plotID, plot_data$plotID)]
micro_cit$y <- plot_data$latitude[match(micro_cit$plotID, plot_data$plotID)]
head(micro_cit)
save(micro_cit, file = "I:/DATA/output/tmp/micro_cit.RData")
## Go to the 05_extracMicro.R

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
    eu_plots_st,
    filename = "I:/DATA/output/resurvey_plots.shp", overwrite = TRUE
)
