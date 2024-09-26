library(terra)
library(sf)
library(mapview)
library(dplyr)
library(easyclimate)

# Load data
load("I:/DATA/output/ExtractMicroIndex/MicroClimPlantCIT_MIs.RData")
temp_crs <- rast("H:/Output/forestBIO1_convertedTO0_25m.tif")

# 'easyclimate' only has climate data available after 1950-
hist(micro_cit$first_year)
str(micro_cit)
n_after1950 <- micro_cit[(micro_cit$first_year >= 1950), ]
## 47 plots get first survey before 1950.
hist(n_after1950$first_year)
n_after1950 <- as_tibble(n_after1950)
head(n_after1950)

# Change the names of coordinations.
colnames(n_after1950)[11:12] <- c("lon", "lat")
head(n_after1950)

plots_xy <- n_after1950[, c(1, 11:12)]
head(plots_xy)


# Convert data frame to sf object
plots_sf <- st_as_sf(
    x = plots_xy,
    coords = c("lon", "lat"),
    crs = "+proj=longlat +datum=WGS84"
)
# View in the map
mapview(plots_sf)

# Create 70 km buffer of the point locations.
# Reproject
eu_plots_st <- st_transform(plots_sf, crs = st_crs(temp_crs))
mapview(eu_plots_st)
plot_vct <- vect(eu_plots_st)
crs(plot_vct)

# Create buffer
buff <- buffer(plot_vct, 70000)
plot(buff)
mapview(buff)

# Convert data.
buff <- st_as_sf(buff)
single_buf <- st_union(buff)
single_buf <- vect(single_buf)
crs(single_buf)
mapview(single_buf)

# Save the buffer as .shp file.
writeVector(single_buf,
    filename = "I:/DATA/output/Plot_shp/70kmBuffer_REplotAfter1950.shp",
    overwrite = TRUE
)

#### easyclimate data download. ####
lambert_xy <- st_coordinates(eu_plots_st)
wgs_xy <- st_coordinates(plots_sf)

save(wgs_xy, file = 'I:/DATA/output/wgs_xy_REplot3.1.RData')

load("I:/DATA/output/wgs_xy_REplot3.1.RData")
# Get daily minimum temperature 1950.
?get_daily_climate
tas_min <- get_daily_climate(
  wgs_xy,
  period = 1950,
  climatic_var = "Tmin",
  version = 4,
  check_connection = TRUE
)

tail(tas_min)
# Save data
save(tas_min, file = 'I:/DATA/easyclimate/dailyTmin_1950.RData')

# Get daily maximum temperature data of 1950.
tas_max <- get_daily_climate(
  wgs_xy,
  period = 1950,
  climatic_var = "Tmax",
  version = 4,
  check_connection = TRUE
)
head(tas_max)
tail(tas_max)
save(tas_max, file = 'I:/DATA/easyclimate/dailyTmax_1950.RData')

# Get daily minimum data for the whole period.
tasmin_yrs <- get_daily_climate(
  wgs_xy,
  period = 1951:1970,
  climatic_var = "Tmin",
  version = 4,
  check_connection = TRUE
)
head(tasmin_yrs)
tail(tasmin_yrs)
save(tasmin_yrs, file = 'I:/DATA/easyclimate/dailyTmin_1951-1970.RData')

# Get daily maximum data for the whole period.
tasmax_yrs <- get_daily_climate(
  wgs_xy,
  period = 1951:1970,
  climatic_var = "Tmax",
  version = 4,
  check_connection = TRUE
)
save(tasmax_yrs, file = 'I:/DATA/easyclimate/dailyTmax_1951-1970.RData')

