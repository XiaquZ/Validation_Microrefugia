library(terra)
library(sf)
library(mapview)

load("I:/DATA/output/tmp/micro_cit.RData")
warmingm <- rast('H:/Output/WarmingMagnitude/WarmingMagnitude_SSP370.tif')

# xy <- data.frame(plot_data$plotID, plot_data$latitude, plot_data$longitude)
# head(xy)
# colnames(xy)[1] <- "plotID"
# colnames(xy)[2] <- "latitude"
# colnames(xy)[3] <- "longitude"
# head(xy)
# #add xy to the deltacit dataset.
# no_na1995s02 <- merge(no_na1995s, xy, by = "plotID")
# no_na1995s02 <- no_na1995s02[, c(1,14,15,2,3,4,5,6,7,8,9,10,11,12,13)]

plots_xy <- micro_cit[, c(1,11:12)]
head(plots_xy)

# Convert data frame to sf object
plots_sf <- st_as_sf(
    x = plots_xy,
    coords = c("x", "y"),
    crs = "+proj=longlat +datum=WGS84"
)

mapview(plots_sf)

eu_plots_st <- st_transform(plots_sf, crs = st_crs(warmingm))
mapview(eu_plots_st)

# Extract the microclimate values.
eu_plots_st <- vect(eu_plots_st)
plotsmicro <- extract(warmingm, eu_plots_st, method = "simple", bind = T)
plotsmicro <- as.data.frame(plotsmicro)

hist(plotsmicro$macroMAT2085)

# merge micro with CIT changes
micro_cit <- merge(micro_cit, plotsmicro, by = "plotID")
hist(micro_cit$citPeryear1)
anyNA(micro_cit$macroMAT2085)

# Extract Vomc data.
fvomc <- rast("H:/Output/VoCC/ForwardVoCC/25m/75kmSR/FVoCC_75kmSR_25m_2010-2085_SSP370.tif")
bvomc <- rast("H:/Output/VoCC/BackwardVoCC/25m/BVoMC_75kmSR_25m_2010-2085_SSP370.tif")

micro_fv <- extract(fvomc, eu_plots_st, method = "simple", bind = T)
micro_bv <- extract(bvomc, eu_plots_st, method = "simple", bind = T)

micro_fv <- as.data.frame(micro_fv)
micro_bv <- as.data.frame(micro_bv)
head(micro_fv)

# merge data
micro_cit <- merge(micro_cit, micro_fv, by = "plotID")
head(micro_cit)
colnames(micro_cit)[14] <- "fvomc"
hist(micro_cit$fvomc)

micro_cit <- merge(micro_cit, micro_bv, by = "plotID")
head(micro_cit)
colnames(micro_cit)[15] <- "bvomc"

# Test
plot(micro_cit$fvomc,micro_cit$CITperYr)
plot(micro_cit$bvomc, micro_cit$CITperYr)
cor.test(micro_cit$CITperYr, micro_cit$fvomc)
cor.test(micro_cit$CITperYr, micro_cit$bvomc)

# calculate decadal warming magnitude.
micro_cit$WarmingPerYr <- micro_cit$macroMAT2085/75
head(micro_cit)

#Test
cor.test(micro_cit$WarmingPerYr, micro_cit$CITperYr)
hist(micro_cit$WarmingPerYr)

# save data
save(micro_cit,
    file = "I:/DATA/output/ExtractMicroIndex/MicroClimPlantCIT_MIs.RData"
)

# Check if there any duplicate coordinations.
 load("I:/DATA/output/ExtractMicroIndex/MicroClimPlantCIT_MIs.RData")
n_occur <- unique(micro_cit$x)
head(micro_cit)
dup_df <- micro_cit[duplicated(micro_cit[, 11:12]),]
unique(dup_df$plotID)

# Remove these duplicate plots
plotls <- dup_df$plotID

micro_cit <- micro_cit[!(micro_cit$plotID %in% plotls), ]
save(micro_cit,
    file = "I:/DATA/output/ExtractMicroIndex/MicroClimPlantCIT_MIs.RData"
)
