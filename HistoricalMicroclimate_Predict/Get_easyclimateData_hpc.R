library(terra)
library(easyclimate)

# Load the buffering polygons and the europe map.
buffer_wgs <- vect("I:/DATA/output/REplotBuffer_shp/70kmBuffer_REplotAfter1950_wgs84.shp")

plot(buffer_wgs)


#### Grid method: Split the polygons into smaller areas.####
v <- buffer_wgs

# Create grid cells.
size <- 1
r <- rast(v, res = size)

# Convert raster grid to vector polygons
grid_p <- as.polygons(r)
plot(grid_p)

# Intersect the original polygon with the grid to split it
split_polygons <- intersect(buffer_wgs, grid_p)
plot(split_polygons)
#writeVector(split_polygons, 'I:/DATA/easyclimate/output/split_polygons.shp', overwrite = TRUE)

# # Convert the SpatVector into a list of individual tiles (to avoid transferring the whole object)
# tiles_list <- lapply(1:nrow(split_polygons), function(i) split_polygons[i, ])

#### Forloop ####
# create a list to store the output.
result <- list()
for (i in 19:29) {
  # Extract one tile (polygon) at a time
  tile <- split_polygons[i, ]
  plot(tile)

  # Extract climate data for the current tile
  temp_max <- get_daily_climate(
    coords = tile,
    climatic_var = "Tmax",
    period = 1950:1970,
    output = "raster"
  )
  result[[i]] <- temp_max |> mean()
}

# Remove NULL elements from the result list
valid_rasters <- result[!sapply(result, is.null)]

# Save raster
for (i in 19:29) {
  writeRaster(result[[i]],
              filename = paste0('I:/DATA/easyclimate/output/Tiles/Tmax1950-1970_', i,'.tif'),
              overwrite = TRUE)
}

rastls <- list.files(path = "I:/DATA/easyclimate/output/Tiles",
                     pattern = ".tif",
                     full.names = TRUE)

# Build vrt file with all the tiles and then write it as a final tif.
v <- vrt(rastls, "test.vrt", overwrite = TRUE)
plot(v)