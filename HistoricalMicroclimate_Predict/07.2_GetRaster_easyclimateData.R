library(terra)
library(easyclimate)
library(foreach)
library(parallelly)
library(doParallel)

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

# Convert the SpatVector into a list of individual tiles (to avoid transferring the whole object)
tiles_list <- lapply(1:nrow(split_polygons), function(i) split_polygons[i, ])

#### Foreach loop ####
# Interate over each id to get the easyclimate data of each tile.
ncores = 3
cl <- makeClusterPSOCK(ncores, autoStop = TRUE)
registerDoParallel(cl)


# Parallel loop to iterate over each polygon and return a list of rasters
templs <- foreach(i = 1:3, 
                  .packages = c('terra', 'easyclimate')) %dopar% {
                    
                    # Load split_polygons on each worker
                    split_polygons <- vect('I:/DATA/easyclimate/output/split_polygons.shp')
                    
                    # Extract one tile (polygon) at a time
                    tile <- split_polygons[i, ]
                    
                    # Create a list
                    result <- list()
                    
                    # Extract climate data for the current tile
                    temp_max <- get_daily_climate(
                      coords = tile,
                      climatic_var = "Tmax",
                      period = 1950:1970,
                      output = "raster"
                    )
                    
                    result[[i]] <- temp_max |> mean()
                    # Return the raster for this tile
                    return(result)
                  }
stopCluster(cl)
  
#### Forloop ####
# create a list to store the output.
result <- list()

for (i in 1:5) {
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
names(result) <- "maximum temp average 1950-1970"
save(result, file = "I:/DATA/easyclimate/output/Tiles/Tile_4to10_tasmax_mean1950-1970.RData")

  writeRaster(temp_max, filename = paste0('I:/DATA/easyclimate/output/Tiles/Tmax1950-1970_', i,'.tif'), overwrite = TRUE)

