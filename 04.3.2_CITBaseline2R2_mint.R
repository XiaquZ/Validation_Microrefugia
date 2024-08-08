# Load data
library(dplyr)
library(tidyverse)

load("I:/DATA/output/preparation/CleanHerbL.RData")
load("I:/DATA/input/forestREplot/version3/plot_data.RData")

min(plot_data$year_baseline_survey) # 1933
max(plot_data$year_baseline_survey) # 2013
min(plot_data$year_resurvey_R1) # 1969
max(plot_data$year_resurvey_R1) # 2023
anyNA(plot1960s$year_baseline_survey) # F
anyNA(plot1960s$year_resurvey_R1) # F
str(plot_data)
# check the year later than 2000.
class(plot_data$year_baseline_survey)
plot2000s_b <- plot_data %>%
  mutate(year_baseline_survey = as.numeric(year_baseline_survey)) %>%
  filter(year_baseline_survey >= 2000)

unique(plot2000s_b$plotID)
## note that EU_080 has plots with baseline older than 2000.

# match("year_resurvey_R3", names(plot2000s_b))
# plot2000s_b <- plot2000s_b[, -c(12:16)]
# plot2000s_b <- plot2000s_b[, -c(6:8)]
# anyNA(plot2000s_b$year_baseline_survey) # F
# anyNA(plot2000s_b$year_resurvey_R1) # F
# anyNA(plot2000s_b$year_resurvey_R2) # T

# get the CIT of plot 83.
# load minimum temp data of each species.
load("I:/DATA/output/CommunityInferredTemp/cit_minT_WinSpr.RData")

#### Add ClimPlants mean Temp data to the forestREplot data. ####
# For the minimum temperature during spring
species_winspr <- right_join(spe_herb, minT_species, by = "species_name")
head(species_winspr)
head(spe_herb)

## grab the data of plot 078 079 083
list_plots <- plot2000s_b$plotID
species_b2000s <- species_winspr[grepl(
  paste(list_plots, collapse = "|"),
  species_winspr$sample
), ]

species_b2000s02 <- spe_herb[grepl(
  paste(list_plots, collapse = "|"),
  spe_herb$sample
), ]
## This output did not merge with ClimPlant
## By comparing species_b2000s02 with species_b2000s
## We can see how many species did not have temp data in Climplant.
## Which might cause the missing CIT of one of the survey
## as calculated below.

# Calculate the CIT
cit_mint_b2000s <- species_b2000s |>
  group_by(sample) |>
  summarise(
    cit_mint_b2000s = weighted.mean(Mean_minTDec2May, abundance)
  )
head(cit_mint_b2000s)

# Get plot id and start with the baseline CIT.
cit_b2000s02 <- cit_mint_b2000s[grep("_B", cit_mint_b2000s$sample), ]
## 109 plots has baseline later than 2000 from the 3 sites.
cit_b2000s02$sample <- str_replace(cit_b2000s02$sample, "_B", "")
## get plotid.
colnames(cit_b2000s02)[2] <- "baseline"
head(cit_b2000s02)

# Check which plots have first resurvey R1.
r1 <- cit_mint_b2000s[grep("_R1", cit_mint_b2000s$sample), ] # 105
colnames(r1)[2] <- "R1"
r1$sample <- str_replace(r1$sample, "_R1", "") # get plotid.
head(r1)

# merge r1 data to the plot data.
anyNA(cit_b2000s02$baseline)
plot_mint <- merge(cit_b2000s02, r1, by = "sample", all = TRUE)
head(plot_mint)
## 190 obs
sum(is.na(plot_mint$baseline)) # 0 plots without baseline but have R1.
sum(is.na(plot_mint$R1)) # 4 plots without R1 but have bseline.
nobaseline <- plot_mint[is.na(plot_mint$R1), ] # 079 does not have R1.


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
plot_mint <- plot_mint[-grep("EU_078_30", plot_mint$sample),]
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
plot_mint02$cit_yr1 <- plot_mint02$deltaCIT1/plot_mint02$deltayear1
plot_mint02$cit_yr2 <- plot_mint02$deltaCIT2/plot_mint02$deltayear2
plot_mint02$cit_yrbr2 <- plot_mint02$deltaCITbr2/plot_mint02$deltayear_br2
hist(plot_mint02$cit_yr1)
hist(plot_mint02$cit_yrbr2)

#### Conclusions: So for minT of winter and spring ####
# we can only used the CIT from baseline to the R2 surveys.
# For these three sites, there are no resurveys later than R2.

# 
plot2000s_R1 <- plot_data %>%
  mutate(year_baseline_survey = as.numeric(year_baseline_survey)) %>%
  filter(year_baseline_survey >= 2000)

unique(plot2000s_b$plotID)





#### Extract microclimate data. ####
library(terra)
library(sf)
library(mapview)
offset <- rast("E:/Output/ForestOffset/mean/mean_ForestTempMinOffset_V3.tif")

plots_xy <- plot_mint02[, c(1:3)]

# Convert data frame to sf object
plots_sf <- st_as_sf(
    x = plots_xy,
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
hist(plotsmicro$mean)

# merge micro with CIT changes
micro_cit <- merge(plot_mint02, plotsmicro, by = "plotID")
cor.test(micro_cit$mean, micro_cit$cit_yr1)
plot(micro_cit$mean, micro_cit$cit_yr1)

# Make density plots
gplot_cit <- micro_cit[ , c(16:18)]
#Melt
melt_cit <- pivot_longer(gplot_cit, cols = names(gplot_cit))
colnames(melt_cit) <- c("Survey", "CITperYr")
head(melt_cit)
library(ggplot2)
# With transparency (right)
ggplot(melt_cit, aes(x=CITperYr, color = Survey, fill = Survey)) +
    geom_density(adjust=1.5, alpha=.4) 
mean(gplot_cit$cit_yr1) #0.00752557
mean(gplot_cit$cit_yr2, na.rm = TRUE) # 0.05656997
mean(gplot_cit$cit_yrbr2, na.rm = T) #0.0226431
