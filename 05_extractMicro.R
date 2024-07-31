library(terra)
install.packages("sf")
install.packages("mapview")
library(sf)
library(mapview)


load("I:/DATA/output/CITyearly/Base2Re2_maxt_Gs_1995s.RData")
load("I:/DATA/input/forestREplot/version3/plot_data.RData")
offset <- rast("E:/Input/ForestTemp_new/01_Offsets/mean_annualOffset.tif")

xy <- data.frame(plot_data$plotID, plot_data$latitude, plot_data$longitude)
head(xy)
colnames(xy)[1] <- "plotID"
colnames(xy)[2] <- "latitude"
colnames(xy)[3] <- "longitude"
head(xy)
unique(no_na1995s$plotID)
eu_076 <- xy[grep("EU_076_", xy$plotID), ]
eu_078 <- xy[grep("EU_078_", xy$plotID), ]


# Convert data frame to sf object
eu_082_sf <- st_as_sf(x = eu_082, 
                      coords = c("latitude", "longitude"),
                      crs = "+proj=longlat +datum=WGS84")

eu_076_sf <- st_as_sf(x = eu_076, 
                      coords = c("longitude", "latitude"),
                      crs = "+proj=longlat +datum=WGS84")


eu_076_st <- st_transform(eu_076_sf, crs = st_crs(offset))
eu_082_st <- st_transform(eu_082_sf, crs = st_crs(offset))

# interactive map:

mapview(eu_082_sf)
mapview(eu_010b_sf)
mapview(eu_008_st)
mapview(eu_082_st)

# Extract the microclimate values.
eu_008_st <- vect(eu_008_st)
e8 <- extract(offset,eu_008_st, method = "simple", bind = T )
e8_micro <- as.data.frame(e8)

eu_082_st <- vect(eu_082_st)
e82 <- extract(offset,eu_082_st, method = "simple", bind = T )
e82_micro <- as.data.frame(e82)
hist(e82_micro$mean_annualOffset)

# save data
save(e8_micro, file = "I:/DATA/output/extraMicro/EU_008_MAToffset.RData")
save(e82_micro, file = "I:/DATA/output/extraMicro/EU_082_MAToffset.RData")

#load data
load("I:/DATA/output/extraMicro/EU_082_MAToffset.RData")
load("I:/DATA/output/CITyearly/R1toR2_CITperYR_maxt_Gs_eu082.RData")

# merge micro with CIT changes
micro_cit <- merge(r1tor2_10yrs, e82_micro, by = "plotID")
cor.test(micro_cit$mean_annualOffset, micro_cit$CITperYR)
hist(micro_cit$CITperYR)
