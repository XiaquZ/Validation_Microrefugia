library(terra)
library(sf)
library(mapview)
library(dplyr)

# Load the data
load("I:/DATA/easyclimate/output/Mean_annualTemp_REplot1950-1970.RData")
hist <- mean_annual_tmean
load("I:/DATA/easyclimate/output/Mean_annualTemp_REplot2000-2020.RData")
pre <- mean_annual_tmean
head(hist)
head(pre)
load("I:/DATA/easyclimate/input/dailyTmax_1951-1970.RData")
head(tasmax_yrs)

# # Extract the offset of 2000-2020.
# offset_pre <- rast("I:/DATA/mean_annualOffset.tif")
# print(offset_pre)

# Load the microclimate CIT data
load("I:/DATA/output/MicroClimPlant_CIT/MicroClimP_REp3.1_1950s.RData")
plots_micro <- as_tibble(plots_micro)
load("I:/DATA/output/hist_micro/historicalMicroOffset.RData")

# Calculate the historical microclimate offset
# Extract the current offset values.
xy <- plots_micro[, c(1, 11:12)]
xy <- st_as_sf(
    x = xy,
    coords = c("x", "y"),
    crs = "+proj=longlat +datum=WGS84"
)
mapview(xy)

xy_trans <- st_transform(xy, crs = st_crs(offset_pre))
mapview(xy_trans)
xy_trans <- vect(xy_trans)

#### Predicting historical microclimate. ####
# Extract offset.
plot_offset <- extract(offset_pre, xy_trans, method = "simple", bind = T)
plot_offset <- as.data.frame(plot_offset)
micro_hist <- as_tibble(plot_offset)
micro_hist$x <- plots_micro$x[match(plots_micro$plotID, micro_hist$plotID)]
micro_hist$y <- plots_micro$y[match(plots_micro$plotID, micro_hist$plotID)]

# Add macroclimate pre to the plot based on the xy.
pre <- pre[, -1]
pre <- pre[, -3]
colnames(micro_hist)[3] <- "lon"
colnames(micro_hist)[4] <- "lat"
micro_current <- left_join(micro_hist, pre)
colnames(micro_current)[2] <- "offset_current"
colnames(micro_current)[5] <- "macroclimate_current"
plot(micro_current$macroclimate_current, micro_current$offset_current)

# Fit linear regression model between offset and macroclimate.
reg1 <- lm(offset_current ~ macroclimate_current, data = micro_current)
summary(reg1)

save(reg1, file = "I:/DATA/output/hist_micro/lm_offsetMacro_2000-2020.RData")
load("I:/DATA/output/hist_micro/lm_offsetMacro_2000-2020.RData")
# Slope value
slope1 <- reg1$coefficients[2]

# Add historical microclimate.
offset_current <- micro_hist
lm_df <- micro_current
hist <- hist[, -c(1, 4)]
lm_df <- left_join(lm_df, hist)
colnames(lm_df)[6] <- "macroclimate_hist"
plot(lm_df$macroclimate_current, lm_df$offset_current)
lm_df$deltaMacro <- lm_df$macroclimate_hist - lm_df$macroclimate_current
hist(lm_df$deltaMacro)
lm_df$offset_hist <- slope1 * lm_df$deltaMacro + lm_df$offset_current
hist(lm_df$offset_hist)
lm_df$micro_hist <- lm_df$macroclimate_hist + lm_df$offset_hist
hist(lm_df$micro_hist)

# Extract microclimate current
micro_current <- rast("D:/PhD/Data/Input/ForestClim_01.tif")

# Extract forestBIO1
bio1_current <- extract(micro_current, xy_trans, method = "simple", bind = T)
bio1_current <- as.data.frame(bio1_current)
bio1_current <- as_tibble(bio1_current)

# Create a tibble for warming magnitude.
warming <- lm_df[, c(1, 3:4, 9)]
head(warming)

# Save data
save(lm_df, file = "I:/DATA/output/hist_micro/historicalMicroOffset.RData")
save(warming, file = "I:/DATA/output/hist_micro/historical_warmingM.RData")
save(plots_micro,
    file = "I:/DATA/output/MicroClimPlant_CIT/MicroClimP_REp3.1_1950s.RData"
)

#### Historical microclimate for buffer zones ####
##################################################
# Load data
tmax_his <- rast(
    "I:/DATA/easyclimate/input/Tiles/1950-1970/Tmax/Tmax_70kmBuffer_1950-1970.tif"
)
tmin_his <- rast(
    "I:/DATA/easyclimate/input/Tiles/1950-1970/Tmin/Tmin_70kmBuffer_1950-1970.tif"
)

# Calculate macroclimate historical MAT.
s <- c(tmax_his, tmin_his)
tmat_hist <- mean(s, na.rm = TRUE)
plot(tmat_hist)

# Save the mean annual temp.
writeRaster(tmat_hist,
    filename = "I:/DATA/easyclimate/input/Tiles/1950-1970/Tmat_70kmBuffer_1950-1970.tif"
)

# Extract current offset data of buffer areas.
offset_pre <- rast("I:/DATA/mean_annualOffset.tif")
buffer <- vect("I:/DATA/output/REplotBuffer_shp/70kmBuffer_REplotAfter1950.shp")
plot(buffer)
offset_b <- crop(offset_pre, buffer, mask = TRUE)

# Fit linear regression model between offset and macroclimate.
reg1 <- lm(offset_b ~ macroclimate_current, data = micro_current)
summary(reg1)