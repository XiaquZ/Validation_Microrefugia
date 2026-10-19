# Load data
library(dplyr)
library(tidyverse)
library(ggeffects)

# Plot data
load("I:/DATA/input/forestREplot/version3/plot_data.RData")

head(veg_data)
min(plot_data$year_baseline_survey) # 1933
max(plot_data$year_baseline_survey) # 2013
min(plot_data$year_resurvey_R1) # 1969
max(plot_data$year_resurvey_R1) # 2023
anyNA(plot1960s$year_baseline_survey) # F
anyNA(plot1960s$year_resurvey_R1) # F
str(plot_data)

# Only select plots with data in "H" and "T" layers.
# Data cleaning already done in 01_CleanData.R.
# Load the cleaned data.
load("I:/DATA/output/preparation/EU_TreeL.RData")

# Calculate the change of total tree cover between surveys.
# Remove plots with large change in the total cover.
# Get plot id and start with the baseline survey.
treecover <- vegtree_totalC[grep("_B", vegtree_totalC$sample), ]
head(treecover)
treecover$sample <- str_replace(treecover$sample, "_B", "")
## get plotid.
colnames(treecover)[2] <- "baseline_canopyCover"
head(treecover)

# Check which plots have first resurvey R1.
r1 <- vegtree_totalC[grep("_R1", vegtree_totalC$sample), ] # 4072
head(r1)
colnames(r1)[2] <- "R1_canopyCover"
r1$sample <- str_replace(r1$sample, "_R1", "") # get plotid.
head(r1)
# merge r1 data to the plot data.
plot_treecover <- merge(treecover, r1, by = "sample", all = TRUE)
## Some plots do not have baseline tree cover data but R1.
head(plot_treecover)
anyNA(plot_treecover$baseline_canopyCover)
anyNA(plot_treecover$R1_canopyCover)


# Check which plots have a second resurvey R2.
r2 <- vegtree_totalC[grep("_R2", vegtree_totalC$sample), ] # 910 obs
colnames(r2)[2] <- "R2_canopyCover"
r2$sample <- str_replace(r2$sample, "_R2", "") # get plotid.
head(r2)
# merge r2 data to the r1 data.
plot_treecover <- merge(plot_treecover, r2, by = "sample", all = TRUE)
head(plot_treecover)

# Check which plots have a resurvey R3.
r3 <- vegtree_totalC[grep("_R3", vegtree_totalC$sample), ] # 713 obs
head(r3)
colnames(r3)[2] <- "R3_canopyCover"
r3$sample <- str_replace(r3$sample, "_R3", "") # get plotid.
head(r3)
# merge r2 data to the r1 data.
plot_treecover <- merge(plot_treecover, r3, by = "sample", all = TRUE)
head(plot_treecover)

# Check which plots have a resurvey R4.
r4 <- vegtree_totalC[grep("_R4", vegtree_totalC$sample), ] # 493 obs
head(r4)
colnames(r4)[2] <- "R4_canopyCover"
r4$sample <- str_replace(r4$sample, "_R4", "") # get plotid.
head(r4)
# merge data.
plot_treecover <- merge(plot_treecover, r4, by = "sample", all = TRUE)
head(plot_treecover)

# Check which plots have a resurvey R5.
r5 <- vegtree_totalC[grep("_R5", vegtree_totalC$sample), ] # 12 obs
head(r5)
colnames(r5)[2] <- "R5_canopyCover"
r5$sample <- str_replace(r5$sample, "_R5", "") # get plotid.
head(r4)
# merge data.
plot_treecover <- merge(plot_treecover, r5, by = "sample", all = TRUE)
head(plot_treecover)

# Split the tree cover data into time1 and time2.
# Make a category column of time1.
plot_treecover <- plot_treecover %>%
  mutate(
    time1 = case_when(
      !is.na(baseline_canopyCover) ~ baseline_canopyCover,
      !is.na(R1_canopyCover) ~ R1_canopyCover,
      !is.na(R2_canopyCover) ~ R2_canopyCover,
      !is.na(R3_canopyCover) ~ R3_canopyCover,
      !is.na(R4_canopyCover) ~ R4_canopyCover,
      !is.na(R5_canopyCover) ~ R5_canopyCover,
      TRUE ~ "Unknown"
    )
  )
# For the tree cover data of time2,
# choose the cover data from the recent survey.
plot_treecover <- plot_treecover %>%
  mutate(
    time2 = case_when(
      !is.na(R5_canopyCover) ~ R5_canopyCover,
      !is.na(R4_canopyCover) ~ R4_canopyCover,
      !is.na(R3_canopyCover) ~ R3_canopyCover,
      !is.na(R2_canopyCover) ~ R2canopyCover,
      TRUE ~ R1_canopyCover
    )
  )

str(plot_treecover)
plot_treecover$time1 <- as.numeric(plot_treecover$time1)
plot_treecover$canopyChange <- plot_treecover$time2 - plot_treecover$time1
head(plot_treecover)
hist(plot_treecover$canopyChange)

# Select plots that with canopy cover changes less than 25 %.
plot_treecover25 <- plot_treecover[
  abs(plot_treecover$canopyChange) <= 25 &
    !is.na(plot_treecover$canopyChange),
]
# Remove plots that with only one tree cover data from baseline to R5.
plot_treecover25 <- plot_treecover25[
  rowSums(!is.na(plot_treecover25[, 2:7])) > 1,
] # 2501 plots can be included.

plot(plot_treecover25$canopyChange)

# get the CIT of plots.
# Load herb layer species cit data
load("I:/DATA/output/CommunityInferredTemp/CIT_Allsurveys_minTWinSpr.RData")

plot_tc02 <- plot_treecover25[, c(1, 10)] # 2501
plot_tc02 <- merge(plot_tc02, plot_mint, by = "sample") # 2499
## There are two plots do not have CIT from plot_mint. No ClimPlant data.

# If R1 to R5 are all NA, then remove this row.
plot_tc02 <- plot_tc02 |>
  filter(!if_all(R1:R5, is.na))
head(plot_tc02) # 2490

# Remove plots that with only one survey data from baseline to R5.
plot_tc02 <- plot_tc02[
  rowSums(!is.na(plot_tc02[, 3:8])) > 1,
] # 2483
anyNA(plot_tc02$baseline) # F

# Use most recent survey as resurvey.
# Create a column with cit of most recent survey.
plot_tc02 <- plot_tc02 %>%
  mutate(
    Resurvey_cit = case_when(
      !is.na(R5) ~ R5,
      !is.na(R4) ~ R4,
      !is.na(R3) ~ R3,
      !is.na(R2) ~ R2,
      TRUE ~ R1
    )
  )

anyNA(plot_tc02$Resurvey_cit)

# Add the baseline survey year
head(plot_data)
plotinfo <- plot_data[, c(1:3, 9:14)]
head(plotinfo)
colnames(plot_tc02)[1] <- "plotID"
plot_tc02 <- merge(plot_tc02, plotinfo, by = "plotID")
head(plot_tc02)
str(plot_tc02)

# Add the year of recent survey to a new column.
plot_tc02 <- plot_tc02 %>%
  mutate(
    ResuveyYr = case_when(
      Resurvey_cit == R5 ~ year_resurvey_R5,
      Resurvey_cit == R4 ~ year_resurvey_R4,
      Resurvey_cit == R3 ~ year_resurvey_R3,
      Resurvey_cit == R2 ~ year_resurvey_R2,
      TRUE ~ year_resurvey_R1
    )
  )

head(plot_tc02)

# Check for NA values in ResuveyYr
sum(is.na(plot_tc02$ResuveyYr)) # 0
sum(is.na(plot_tc02$year_baseline_survey)) # 0

# View rows with NA to understand why they failed
plot_tc02[is.na(plot_tc02$ResuveyYr), ]

# Remove columns that are not going to be used.
plot_tc02 <- plot_tc02[, -c(4:8, 13:17)]

# Check the formatting of year.
unique(plot_tc02$year_baseline_survey)

# Use mutate and recode for a cleaner syntax.
plot_tc02 <- plot_tc02 %>%
  mutate(year_baseline_survey = recode(
    year_baseline_survey,
    "1976 (T, S 1975)" = "1976",
    "1956-57" = "1957",
    "1955 and 57" = "1956",
    "1998/99" = "1999",
    "1963?" = "1963"
  ))

unique(plot_tc02$year_baseline_survey)
unique(plot_tc02$year_baseline_survey)
sum(is.na(plot_tc02$ResuveyYr)) # 0
str(plot_tc02)

# Inspect the year data.
# Assuming 'year_column' is your column name
problematic_yr <- plot_tc02$ResuveyYr[is.na(as.numeric(plot_tc02$ResuveyYr))]
problematic_yr01 <- plot_tc02$year_baseline_survey[
  is.na(as.numeric(plot_tc02$year_baseline_survey))
]
print(problematic_yr)
print(problematic_yr01)

# Convert the year data
plot_tc02$ResuveyYr <- gsub("2019-2020", "2020", plot_tc02$ResuveyYr)

# Convert the survey years to numeric.
cols <- c("ResuveyYr", "year_baseline_survey")
plot_tc02[cols] <- lapply(plot_tc02[cols], as.numeric)

# Check NA
sum(is.na(plot_tc02$ResuveyYr))
sum(is.na(plot_tc02$year_baseline_survey))

# Calculate delta CIT and delta year between baseline and recent survey.
plot_tc02 <- plot_tc02 |>
  mutate(
    deltaCIT = plot_tc02$Resurvey_cit - plot_tc02$baseline,
    deltaYr = plot_tc02$ResuveyYr - plot_tc02$year_baseline_survey
  )

# Check for na values in delta cit.
sum(is.na(plot_tc02$deltaCIT)) # 0
sum(is.na(plot_tc02$deltaYr))
head(plot_tc02)

# Calculate CIT change per year.
plot_tc02$CITperYr <- plot_tc02$deltaCIT / plot_tc02$deltaYr

#### Extract microclimate data. ####
library(terra)
library(sf)
library(mapview)

offset <- rast("E:/Output/ForestOffset/mean/mean_ForestTempMinOffset_V3.tif")

# Check for duplicate xy. Keep one and remove the rest.
plot_tc02 <- plot_tc02[!duplicated(plot_tc02[c(5, 6)]), ]
head(plot_tc02)

# Convert data frame to sf object
plots_sf <- st_as_sf(
  x = plot_tc02[, c(1, 5:6)],
  coords = c("longitude", "latitude"),
  crs = "+proj=longlat +datum=WGS84"
)
mapview(plots_sf)
## Plot 082 have wrong xy, should be the opposite.

# Switch the xy of plot 082.
i <- which(str_starts(plot_tc02$plotID, "^EU_082_"))
x <- plot_tc02$latitude[i]
y <- plot_tc02$longitude[i]
plot_tc02$latitude[i] <- y
plot_tc02$longitude[i] <- x

# Convert data frame to sf object
plots_sf <- st_as_sf(
  x = plot_tc02[, c(1, 5:6)],
  coords = c("longitude", "latitude"),
  crs = "+proj=longlat +datum=WGS84"
)
mapview(plots_sf)

plots_st <- st_transform(plots_sf, crs = st_crs(offset))
mapview(plots_st)

# Extract the microclimate values.
plots_st <- vect(plots_st)
plotsmicro <- extract(offset, plots_st, method = "simple", bind = T)
plotsmicro <- as.data.frame(plotsmicro)
head(plotsmicro)
hist(plotsmicro$mean)

# merge micro with CIT changes
micro_cit <- merge(plot_tc02, plotsmicro, by = "plotID")
head(micro_cit)
cor.test(micro_cit$mean, micro_cit$CITperYr)
plot(micro_cit$mean, micro_cit$CITperYr)
plot(micro_cit$canopyChange, micro_cit$mean)

# Save data.
save(micro_cit,
  file = "I:/DATA/output/extraMicro/MinToffset_CITperYr_allPlots.RData"
)

# linear trend + confidence interval
library(ggpubr)
library(ggplot2)

gplot_df <- micro_cit[sample(nrow(micro_cit), 1000), ]

ggplot(gplot_df, aes(x = mean, y = CITperYr)) +
  geom_point() +
  ggtitle("Minimum temperature offset and CIT change per year") +
  labs(y = "CIT change per year (MinT)", x = "Microclimate offset (MinT)") +
  geom_smooth(method = lm, color = "#482EDB", fill = "#69b3a2", se = TRUE) +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01) +
  theme_bw() +
  guides(color = "none")

# View rows with large CIT.
largecit <- micro_cit[(micro_cit$CITperYr > 0.4), ]
largecit <- micro_cit[(micro_cit$CITperYr < -0.2), ]

# Extract the MI of velocity
# Convert data frame to sf object
velocity <- rast("E:/Output/MicrorefugiaIndex/MI_bvocc_75km_25m_add0.tif")

plots_sf <- st_as_sf(
  x = macro[, c(1, 5:6)],
  coords = c("longitude", "latitude"),
  crs = "+proj=longlat +datum=WGS84"
)
mapview(plots_sf)

plots_st <- st_transform(plots_sf, crs = st_crs(velocity))
mapview(plots_st)

# Extract the microclimate values.
plots_st <- vect(plots_st)
plotsmicro <- extract(velocity, plots_st, method = "simple", bind = T)
plotsmicro <- as.data.frame(plotsmicro)
head(plotsmicro)
colnames(plotsmicro)[2] <- "Backward_velocity"
hist(plotsmicro$Backward_velocity)

# merge micro with CIT changes
micro_cit02 <- merge(micro_cit, plotsmicro, by = "plotID")
head(micro_cit02)
cor.test(micro_cit02$Backward_velocity, micro_cit02$CITperYr)
plot(micro_cit02$Backward_velocity, micro_cit02$CITperYr)

# Add macro data
macro <- readRDS("I:/DATA/output/macro_diff.RDS")

# Add the MI of velocity to the data.
macro$MI_BV <- plotsmicro$Backward_velocity[
  match(macro$plotID, plotsmicro$plotID)
]
head(macro)
macro$MI_BV <- round(macro$MI_BV, digits = 1)

# Test the relationship between deltaCIT and macro_diff first.
lm_macro <- lm(deltaCIT ~ macro_diff, data = macro)
summary(lm_macro)

# Create the interaction and make the ggeffects plot.
str(macro)
colnames(macro)[16] <- "MI_FV"
lm_bv <- lm(deltaCIT ~ macro_diff * MI_BV, data = macro)
lm_fv <- lm(deltaCIT ~ macro_diff * MI_FV, data = macro)
summary(lm_bv)
hist(macro$MI_BV)
p_vel <- ggpredict(lm_bv, c("macro_diff", "MI_BV [0.1, 0.5, 0.9]"))
plot(p_vel)

#### Add the MI of warming magnitude.####
wm <- rast("E:/Output/MicrorefugiaIndex/MI_WarmingMagnitude.tif")
plots_sf <- st_as_sf(
  x = macro[, c(1, 5:6)],
  coords = c("longitude", "latitude"),
  crs = "+proj=longlat +datum=WGS84"
)
mapview(plots_sf)
# Transform the crs
plots_st <- st_transform(plots_sf, crs = st_crs(wm))
mapview(plots_st)

# Extract the microclimate values.
plots_st <- vect(plots_st)
plotsmicro <- extract(wm, plots_st, method = "simple", bind = T)
plotsmicro <- as_tibble(plotsmicro)
head(plotsmicro)
colnames(plotsmicro)[2] <- "MI_wm"
hist(plotsmicro$MI_wm)

# Add a new column of wm
macro$MI_wm <- plotsmicro$MI_wm[match(macro$plotID, plotsmicro$plotID)]
head(macro)
macro$MI_wm <- round(macro$MI_wm, digits = 1)
hist(macro$MI_wm)

lm_wm <- lm(deltaCIT ~ macro_diff * MI_wm, data = macro)
summary(lm_wm)
p_wm <- ggpredict(lm_wm, c("macro_diff", "MI_wm [0.3, 0.6, 0.9 ]"))
plot(p_wm)

lm_all <- lm(deltaCIT ~ macro_diff * MI_BV * MI_wm, data = macro)
summary(lm_all)
saveRDS(macro, file = "I:/DATA/output/MinTmicroMI_macro_diff.RDS")

library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(lm_bv, lm_fv, lm_wm,
  title = "Table 1.  ",
  file = "I:/DATA/output/tmp/lm_macro.html"
)
