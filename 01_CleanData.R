## ForestREplot

# Plot data of all plots in ForestREplot database
load("I:/DATA/input/forestREplot/version3/plot_data.RData")
# Vegetation data of all plots
load("I:/DATA/input/forestREplot/version3/veg_data.RData")

# Insepect forestREplot data.
head(plot_data)
head(veg_data)

## MI data

# MIs based on three climate offsets (mean annual temperature,
# minimmum temperature during spring and winter and maximum temperature during summer)
# and synthesis MIs (mean of the three MIs)
load("ClimateOffset_MIs_XiquZ_V3.RData")
MI_Offset_magnitude <- coordinates_plots

# ---- 3. FILTER AND CLEAN DATA ----

# Select plots and vegetation data from Europe (and excluding that of North America)
veg_EU <- veg_data[grep("EU", veg_data$sample), ]
plot_EU <- plot_data[grep("EU", plot_data$plotID), ]
head(veg_EU)
head(plot_EU)
inspNA <- plot_EU[!is.na(plot_EU$n_quadrat_B), ] #all the columns are NA.
inspNA <- plot_EU[!is.na(plot_EU$location_add), ] #90 pbs.
inspNA <- plot_EU[!is.na(plot_EU$n_quadrat_R1), ] #0 obs.
inspNA <- plot_EU[!is.na(plot_EU$n_quadrat_R1), ] 
# 0 obs. all plots has at least 1 resurvey.
inspNA <- plot_EU[!is.na(plot_EU$year_resurvey_R2), ] #1486 obs
inspNA <- plot_EU[!is.na(plot_EU$year_resurvey_R3), ] #921 obs
inspNA <- plot_EU[!is.na(plot_EU$year_resurvey_R4), ] #629 obs
inspNA <- plot_EU[!is.na(plot_EU$year_resurvey_R5), ] #86 obs
inspNA <- plot_EU[!is.na(plot_EU$year_afforestation), ] #282 obs

unique(plot_EU$former_landuse)
#  [1] "ancient"
#  [2] "not known"
#  [3] NA
#  [4] "recent"
#  [5] "common grazing, poss wood pasture"
#  [6] "yes"
#  [7] "grassland"
#  [8] "border grassland / ancient"
#  [9] "water (?) (1786), grassland (1900)"
# [10] "water (?)"
# [11] "water (1786), water/reed (1900)"
# [12] "grassland / bog"
# [13] "grassland (1786), water/reed (1900)"
# [14] "grassland (1786), open bog (1900)"
# [15] "wet grassland / open bog"
# [16] "forest until >1900"
# [17] "grassland / open bog"
# [18] "heathland"
# [19] "meadow"
# [20] "not known, probably ancient"
# [21] "Wood pasture"
# [22] "arable"
# [23] "other (fallow land)"
# [24] "other (pond)"
# [25] "successional forest on wet grassland"
# [26] "post-agricultural forest, single tree selection"
# [27] "other: open sand dune"
# [28] "ancient forest"
# [29] "Ancient"
# [30] "Grassland"
unique(veg_EU$layer)
# [1] "T"   "S"   "H"   "B"   "L"   "SH"  "TL2" "TL1" "SL"  "ML"  "HL"  "FL"
# [13] "s"   "k"   "b"   "m"   "b2"  "b1"
class(veg_EU$layer)
# Slect the layers that are not sure about. create a vector.
list_values <- c("L", "ML", "HL", "FL", "s", "k", "b", "m", "b2", "b1")

veg_lyr <- subset(veg_EU,grepl(paste0(list_values, collapse = "|"), layer))

# Select the herb layer (for the further analysis) and tree layer (for the calculation of forest canopy changes later on) of the plots
vegherb <- veg_EU[grep("H", veg_EU$layer), ] # herb layer (H)
vegtree <- veg_EU[grep("T", veg_EU$layer), ] # tree layer (T)
hist(vegherb$abundance)
hist(vegtree$abundance)
# Select the sites with abnormal abundance values.
herb_abun <- subset(vegherb, vegherb$abundance > 5000)
unique(herb_abun$abundance) #9999

tree_abun <- subset(vegtree, vegtree$abundance > 5000)
unique(tree_abun$abundance) #9999 42737

# Exclude abnormal abundance values (values of 9999)
vegherb <- subset(vegherb,vegherb$abundance < 5000)
hist(vegherb$abundance) #all the abundance values between 0-100
max(vegherb$abundance) #100

vegtree <- subset(vegtree, vegtree$abundance < 5000)
hist(vegtree$abundance) #all the abundance values between 0-100
max(vegtree$abundance) #100

# Check for abnormal total cover of herb layer per plot sample by 
# grouping per plot sample and calculating the sum per plot sample
library(dplyr)
vegherb_totalC <- vegherb |> 
  group_by(sample)|> 
  summarise(total_cover = sum(abundance))

max(vegherb_totalC$total_cover) #1980

vegtree_totalC <- vegtree |> 
  group_by(sample)|> 
  summarise(total_cover = sum(abundance))
max(vegtree_totalC$total_cover) #279.875

# Add the total cover per plot to the vegetation data
# This is only needed for the herb layer, since we in the tree layer we only work with the total cover while in the herb layer we work with individual abundances
vegherb <- left_join(vegherb, vegherb_totalC, by = "sample")
vegtree <- left_join(vegtree, vegtree_totalC, by = "sample")
#Save vegherb for the CIT calculation. Save the total tree cover data for later explanatory.
save(vegherb, file = "I:/DATA/input/forestREplot/version3/EU_herbL.RData")
save(vegtree_totalC, file = "I:/DATA/input/forestREplot/version3/EU_TreeL.RData")
