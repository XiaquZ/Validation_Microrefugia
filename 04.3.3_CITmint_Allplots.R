# Load data
library(dplyr)
library(tidyverse)

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
plot_treecover$time1 <- ifelse(
  is.na(plot_treecover$baseline_canopyCover) == FALSE, 
  plot_treecover$baseline_canopyCover,
  ifelse(
    is.na(plot_treecover$R1_canopyCover) == FALSE,
    plot_treecover$R1_canopyCover,
    ifelse(
      is.na(plot_treecover$R2_canopyCover) == FALSE, 
      plot_treecover$R2_canopyCover,
      ifelse(
        is.na(plot_treecover$R3_canopyCover) == FALSE,
        plot_treecover$R3_canopyCover,
        ifelse(
          is.na(plot_treecover$R4_canopyCover) == FALSE,
          plot_treecover$R4_canopyCover,
          ifelse(
            is.na(plot_treecover$R5_canopyCover) == FALSE,
            plot_treecover$R5_canopyCover,
            "Unknown"
          )
        )
      )
    )

  )
)
# For the tree cover data of time2,
# choose the cover data from the recent survey.
plot_treecover$time2 <-  ifelse(
  is.na(plot_treecover$R5_canopyCover) == FALSE, 
  plot_treecover$R5_canopyCover,
  ifelse(
    is.na(plot_treecover$R4_canopyCover) == FALSE,
    plot_treecover$R4_canopyCover,
    ifelse(
      is.na(plot_treecover$R3_canopyCover) == FALSE, 
      plot_treecover$R3_canopyCover,
      ifelse(
        is.na(plot_treecover$R2_canopyCover) == FALSE,
        plot_treecover$R2_canopyCover,
        plot_treecover$R1_canopyCover
          )
        )
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
  !is.na(plot_treecover$canopyChange)
,]
# Remove plots that with only one tree cover data from baseline to R5.
plot_treecover25 <- plot_treecover25[
  rowSums(!is.na(plot_treecover25[,2:7])) > 1, 
  ] #2501 plots can be included.
save(plot_treecover25, file = "I:/DATA/output/temp/TreeCoverChangeLess25.RData")



load("I:/DATA/output/preparation/CleanHerbL.RData")
head(spe_herb)

# get the CIT of plots.
# load minimum temp data of each species.
load("I:/DATA/output/CommunityInferredTemp/cit_minT_WinSpr.RData")

#### Add ClimPlants mean Temp data to the forestREplot data. ####
# For the minimum temperature during spring
species_winspr <- right_join(spe_herb, minT_species, by = "species_name")
head(species_winspr)
head(spe_herb)
## There are some species in forestREplot do not have temp data in climPlant.

# Calculate the CIT
cit_mint_b2000s <- species_b2000s |>
  group_by(sample) |>
  summarise(
    cit_mint_b2000s = weighted.mean(Mean_minTDec2May, abundance)
  )
head(cit_mint_b2000s)





# Check which plots have a second resurvey R2.
r2 <- cit_mint_b2000s[grep("_R2", cit_mint_b2000s$sample), ] # 48
colnames(r2)[2] <- "R2"
r2$sample <- str_replace(r2$sample, "_R2", "") # get plotid.
head(r2)

# merge r2 data to the r1 data.
plot_mint <- merge(plot_mint, r2, by = "sample", all = TRUE) # 110
head(plot_mint)
# check how many plots only have r2 cit data
onlyr2 <- plot_mint[is.na(plot_mint$baseline) & is.na(plot_mint$R1), ] # 078_30
## EU_078_30_B has species Spiraea douglasii. No R1 for this plot.
## In R2, it has Sorbus aucuparia.
spiraea <- minT_species[grep("Spiraea douglasii", minT_species$species_name), ]
## Not included in ClimPlant.
sorbus <- minT_species[grep("Sorbus aucuparia", minT_species$species_name), ]
## Included in ClimPlant

# Check which plots have a resurvey later than r2.
r3 <- cit_mint_b2000s[grep("_R3", cit_mint_b2000s$sample), ] # 0
r4 <- cit_mint_b2000s[grep("_R4", cit_mint_b2000s$sample), ] # 0
r5 <- cit_mint_b2000s[grep("_R5", cit_mint_b2000s$sample), ] # 0
anyNA(plot_mint$baseline)

# Note that this is from baseline to R2.
# Remove plots EU_078_30_B, which only has R2.
plot_mint <- plot_mint[-grep("EU_078_30", plot_mint$sample), ]
# Remove plots that only has baseline but don't have resurveys.
plot_mint <- plot_mint[
  !with(plot_mint, is.na(plot_mint$R1) &
    is.na(plot_mint$R2)),
]

#### Calculate the deltaCIT and the deltaYear ####
# First, add each survey year and xy to the plots.
colnames(plot_mint)[1] <- "plotID"
match("year_baseline_survey", names(plot_data)) # 9
match("year_resurvey_R1", names(plot_data)) # 10
plot_info <- plot_data[, c(1:3, 9:11)]
plot_mint02 <- merge(plot_mint, plot_info,
  by = "plotID",
)
plot_mint02 <- plot_mint02[, c(1, 5, 6, 2, 7, 3, 8, 4, 9)]
head(plot_mint02)
# Calculate the delta CIT.
plot_mint02$deltaCIT1 <- plot_mint02$R1 - plot_mint02$baseline
plot_mint02$deltaCIT2 <- plot_mint02$R2 - plot_mint02$R1
sum(plot_mint02$deltaCIT1 > 0, na.rm = T) # 46
sum(plot_mint02$deltaCIT2 > 0, na.rm = T) # 30
plot_mint02$deltaCITbr2 <- plot_mint02$R2 - plot_mint02$baseline
# Check if there are plots only contain baseline info.
onlybase <- plot_mint02[
  is.na(plot_mint02$year_resurvey_R1) &
    is.na(plot_mint02$year_resurvey_R2),
]
## All the plots has at least one resurvey.

# Calculate delta year.
colsnum <- c(
  "year_baseline_survey",
  "year_resurvey_R1",
  "year_resurvey_R2"
)
plot_mint02[colsnum] <- sapply(plot_mint02[colsnum], as.numeric)
plot_mint02$deltayear1 <- plot_mint02$year_resurvey_R1 -
  plot_mint02$year_baseline_survey
head(plot_mint02)

plot_mint02$deltayear2 <- plot_mint02$year_resurvey_R2 -
  plot_mint02$year_resurvey_R1
head(plot_mint02)

plot_mint02$deltayear_br2 <- plot_mint02$year_resurvey_R2 -
  plot_mint02$year_baseline_survey
head(plot_mint02)

# CIT changes per year
plot_mint02$cit_yr1 <- plot_mint02$deltaCIT1 / plot_mint02$deltayear1
plot_mint02$cit_yr2 <- plot_mint02$deltaCIT2 / plot_mint02$deltayear2
plot_mint02$cit_yrbr2 <- plot_mint02$deltaCITbr2 / plot_mint02$deltayear_br2
hist(plot_mint02$cit_yr1)
hist(plot_mint02$cit_yrbr2)

#### Conclusions: So for minT of winter and spring ####
# we can only used the CIT from baseline to the R2 surveys.
# For these three sites, there are no resurveys later than R2.

#### Check if there are any plots with R1 later than 2000 can be added####
# So the R1 become the baseline survey here.
class(plot_data$year_resurvey_R1)
plot2000s_r1 <- plot_data %>%
  mutate(year_resurvey_R1 = as.numeric(year_resurvey_R1)) %>%
  filter(year_resurvey_R1 >= 2000)
plot2000s_r1 <- plot2000s_r1[, -c(6:8, 15:16)]
plot2000s_r1 <- plot2000s_r1[, -6]
# If R2 to R5 are all NA, then remove this row.
plot2000s_r1 <- plot2000s_r1 |>
  filter(!if_all(year_resurvey_R2:year_resurvey_R5, is.na))

head(plot2000s_r1)

#### Calculate the changes of CIT in each plot. ####
load("I:/DATA/output/CommunityInferredTemp/CIT_Allsurveys_minTWinSpr.RData")
# Select the cit of the selected plots and remove the baseline.
plot_mint <- plot_mint[, -2]
colnames(plot_mint)[1] <- "plotID"
head(plot_mint)
plot2000s_r1 <- merge(plot2000s_r1, plot_mint, by = "plotID")

# Use the R1 as baseline survey and the most recent survey as resurvey.
resurvey_cit <- plot2000s_r1[, c(1, 6:10)]
head(resurvey_cit)
str(resurvey_cit)
anyNA(resurvey_cit$year_resurvey_R1) # F
# Convert the year to numeric
cols <- colnames(resurvey_cit)[3:6]
resurvey_cit[cols] <- sapply(resurvey_cit[cols], as.numeric)
str(resurvey_cit)

# Create a column that contain the most recent survey year.
resurvey_cit$final_survey <- ifelse(
  rowSums(!is.na(resurvey_cit[, 2:6])) > 1,
  pmax(resurvey_cit$year_resurvey_R2,
    resurvey_cit$year_resurvey_R3,
    resurvey_cit$year_resurvey_R4,
    resurvey_cit$year_resurvey_R5,
    na.rm = TRUE
  ),
  resurvey_cit$year_resurvey_R2
)
head(resurvey_cit)

# Calculate the delta year.
match("R1", names(plot2000s_r1))
resurvey_cit$cit_r1 <- plot2000s_r1[, 11]
head(resurvey_cit)
anyNA(resurvey_cit$final_survey)
match("cit_recent", names(resurvey_cit))
resurvey_cit <- resurvey_cit[, -9]

# Make a category column of resurvey.
resurvey_cit$recent <- ifelse(
  resurvey_cit$final_survey == resurvey_cit$year_resurvey_R2, "R2",
  ifelse(
    resurvey_cit$final_survey == resurvey_cit$year_resurvey_R3, "R3",
    ifelse(
      resurvey_cit$final_survey == resurvey_cit$year_resurvey_R4, "R4",
      "R5"
    )
  )
)
unique(resurvey_cit$recent)
## Why there are NA when it should be "R4"?

# Calculate the delta year between R1 and recent one.
resurvey_cit$deltayr <- resurvey_cit$final_survey -
  resurvey_cit$year_resurvey_R1
head(resurvey_cit)
# Check the data with NA in the recent column after the ifelse.
x <- which(is.na(resurvey_cit$recent), arr.ind = TRUE)
resurvey_cit[x, ] # all the data contain NA in the recent are belong to R4.
resurvey_cit <- resurvey_cit %>%
  mutate(recent = replace_na(recent, "R4"))
head(resurvey_cit)
unique(resurvey_cit$recent) # Corrected the NA to "R4".

# Match the plot minimum temp cit values to the resurvey
# first extract the resurvey cit and convert to long format with plot ID.
str(resurvey_cit)
match("R2", names(plot2000s_r1))
cit_r2r5 <- plot2000s_r1[, c(1, 12:15)]
cit_r2r5_long <- pivot_longer(
  cit_r2r5,
  cols = -1,
  names_to = "surveyTyp"
)
colnames(resurvey_cit)[10] <- "surveyTyp"
head(resurvey_cit)
resurvey_cit <- merge(resurvey_cit, cit_r2r5_long, by = c("plotID", "surveyTyp"))
colnames(resurvey_cit)[11] <- "cit_recent"
head(resurvey_cit)

# Calculate the CIT changes per year.
resurvey_cit$deltaCIT <- resurvey_cit$cit_recent - resurvey_cit$cit_r1
head(resurvey_cit)
resurvey_cit$CITperYR <- resurvey_cit$deltaCIT / resurvey_cit$deltayr
hist(resurvey_cit$CITperYR)

#### Extract microclimate data. ####
library(terra)
library(sf)
library(mapview)
offset <- rast("E:/Output/ForestOffset/mean/mean_ForestTempMinOffset_V3.tif")
load("I:/DATA/input/forestREplot/version3/plot_data.RData")

head(plot_data)
plots_xy <- plot_data[, c(1:3)]
head(plots_xy)
resurvey_citxy <- merge(resurvey_cit, plots_xy, by = "plotID")
resurvey_citxy <- resurvey_citxy[, c(1, 13:15)]
# Check for duplicate xy. Keep one and remove the rest.
resurvey_citxy02 <- resurvey_citxy[!duplicated(resurvey_citxy[c(3, 4)]), ]
head(resurvey_citxy02)

# Convert data frame to sf object
plots_sf <- st_as_sf(
  x = resurvey_citxy[, c(1, 3:4)],
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
micro_cit <- merge(resurvey_citxy, plotsmicro, by = "plotID")
head(micro_cit)
cor.test(micro_cit$mean, micro_cit$CITperYR)
plot(micro_cit$mean, micro_cit$CITperYR)

unique(resurvey_cit$surveyTyp)
colnames(resurvey_cit)[2] <- "RecentSurveyTyp"
head(resurvey_cit)
resurvey_final <- merge(resurvey_citxy, resurvey_cit, by = "plotID")
head(resurvey_final)
resurvey_final <- resurvey_final[, -16]
colnames(resurvey_final)[2] <- "CITperYr"

save(resurvey_final,
  file = "I:/DATA/output/temp/R1toR5_CITperYr_2000s_mint.RData"
)

# Make density plots
gplot_cit <- micro_cit[, c(16:18)]
# Melt
melt_cit <- pivot_longer(gplot_cit, cols = names(gplot_cit))
colnames(melt_cit) <- c("Survey", "CITperYr")
head(melt_cit)
library(ggplot2)
# With transparency (right)
ggplot(melt_cit, aes(x = CITperYr, color = Survey, fill = Survey)) +
  geom_density(adjust = 1.5, alpha = .4)
mean(gplot_cit$cit_yr1) # 0.00752557
mean(gplot_cit$cit_yr2, na.rm = TRUE) # 0.05656997
mean(gplot_cit$cit_yrbr2, na.rm = T) # 0.0226431
