library(terra)
library(sf)
library(mapview)


load("I:/DATA/output/CITyearly/Base2Re2_maxt_Summer_1995s.RData")
load("I:/DATA/input/forestREplot/version3/plot_data.RData")
offset <- rast("E:/Output/ForestOffset/mean/mean_ForestTempMaxTOffset_V3.tif")

# xy <- data.frame(plot_data$plotID, plot_data$latitude, plot_data$longitude)
# head(xy)
# colnames(xy)[1] <- "plotID"
# colnames(xy)[2] <- "latitude"
# colnames(xy)[3] <- "longitude"
# head(xy)
# #add xy to the deltacit dataset.
# no_na1995s02 <- merge(no_na1995s, xy, by = "plotID")
# no_na1995s02 <- no_na1995s02[, c(1,14,15,2,3,4,5,6,7,8,9,10,11,12,13)]

plots_xy <- plot_summer02[, c(1:3)]

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
plotsmicro <- extract(offset, eu_plots_st, method = "simple", bind = T)
plotsmicro <- as.data.frame(plotsmicro)

hist(plotsmicro$mean)

# merge micro with CIT changes
micro_cit <- merge(plot_summer02, plotsmicro, by = "plotID")
hist(micro_cit$citPeryear1)
cor.test(micro_cit$mean, micro_cit$citPeryear1, method = "pearson")
plot(micro_cit$mean, micro_cit$citPeryear1)

# save data
save(micro_cit,
    file = "I:/DATA/output/extraMicro/micro_deltaCIT_maxtSummer_1995s.RData"
)
