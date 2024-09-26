#### ForestREplot ####
library(dplyr)

# Plot data of all plots in ForestREplot database
load("I:/DATA/input/forestREplot/version3.1/plot.data_forestREplot_V3.1.RData")
# Vegetation data of all plots
load("I:/DATA/input/forestREplot/version3.1/veg.data_forestREplot_V3.1.RData")

# Insepect forestREplot data.
head(plot_data)
head(veg_data)

# ---- 3. FILTER AND CLEAN DATA ----

# Select plots and vegetation data from Europe (and excluding that of North America)
veg_EU <- veg_data[grep("EU_", veg_data$sample), ]
plot_EU <- plot_data[grep("EU_", plot_data$plotID), ]
head(veg_EU)
head(plot_EU)
inspNA <- plot_EU[!is.na(plot_EU$n_quadrat_B), ] # all the columns are NA.
inspNA <- plot_EU[!is.na(plot_EU$location_add), ] # 90 pbs.
inspNA <- plot_EU[!is.na(plot_EU$n_quadrat_R1), ] # 0 obs.

# 0 obs. all plots has at least 1 resurvey.
inspNA <- plot_EU[!is.na(plot_EU$year_resurvey_R2), ] # 1486 obs
inspNA <- plot_EU[!is.na(plot_EU$year_resurvey_R3), ] # 921 obs
inspNA <- plot_EU[!is.na(plot_EU$year_resurvey_R4), ] # 629 obs
inspNA <- plot_EU[!is.na(plot_EU$year_resurvey_R5), ] # 86 obs
inspNA <- plot_EU[!is.na(plot_EU$year_afforestation), ] # 282 obs

# Check the data
unique(plot_EU$former_landuse)
unique(veg_EU$layer)
# "T"  "S"  "H"  "B"  "L"  "SH"
## Remove the "B" "L" and "SH".
class(veg_EU$layer)

# Slect the layers that are not sure about. create a vector.
list_values <- c("L", "SH")
## 'L' stands for lichen,
## and 'SH' stands for no separation between shrub and herb layer was made.

veg_lyr <- subset(veg_EU, grepl(paste0(list_values, collapse = "|"), layer))

# Select the herb layer (for CIT), shrub layer
# and tree layer (for the calculation of forest canopy changes later on).
vegherb <- veg_EU[grep("H", veg_EU$layer), ] # herb layer (H)

# Tree and shrub layer.
list_values <- c("T", "S")
veg_treeshrub <- subset(
  veg_EU, grepl(paste0(list_values, collapse = "|"), layer)
)

hist(vegherb$abundance)
hist(veg_treeshrub$abundance)

# Select the sites with abnormal abundance values.
herb_abun <- subset(vegherb, vegherb$abundance > 5000)
unique(herb_abun$abundance) # 9999

treeshrub_abun <- subset(veg_treeshrub, veg_treeshrub$abundance > 5000)
unique(treeshrub_abun$abundance) # 9999 42737
plotinfo <- treeshrub_abun[grep("42737", treeshrub_abun$abundance), ]
## EU_064_20_B

# Exclude abnormal abundance values (values of 9999, 42737)
vegherb <- subset(vegherb, vegherb$abundance < 5000)
hist(vegherb$abundance) # all the abundance values between 0-100
max(vegherb$abundance) # 100

veg_treeshrub <- subset(veg_treeshrub, veg_treeshrub$abundance < 5000)
hist(veg_treeshrub$abundance) # all the abundance values between 0-100
max(veg_treeshrub$abundance) # 100

# Check for abnormal total cover of herb layer per plot sample by
# grouping per plot sample and calculating the sum per plot sample
library(dplyr)
vegherb_totalC <- vegherb |>
  group_by(sample) |>
  summarise(total_cover = sum(abundance))

max(vegherb_totalC$total_cover) # 1980

vegtreesh_totalC <- veg_treeshrub |>
  group_by(sample) |>
  summarise(total_cover = sum(abundance))
max(vegtreesh_totalC$total_cover) # 312.75

# Add the total cover per plot to the vegetation data
# This is only needed for the herb layer
# since in the tree layer we only work with the total cover
# while in the herb layer we work with individual abundances.
vegherb <- left_join(vegherb, vegherb_totalC, by = "sample")
head(vegherb)

veg_treeshrub <- left_join(veg_treeshrub, vegtreesh_totalC, by = "sample")
head(veg_treeshrub)

# Save vegherb for the CIT calculation.
# Save the total tree cover data for later explanatory.
save(vegherb, file = "I:/DATA/output/forestREplot/EU_herbL.RData")
save(veg_treeshrub, file = "I:/DATA/output/forestREplot/EU_TreeShrubL.RData")
