library(terra)
library(sf)
library(mapview)
library(dplyr)
library(ggeffects)

# Load the data
load("I:/DATA/easyclimate/output/Mean_annualTemp_REplot1950-1970.RData")
hist <- mean_annual_tmean
load("I:/DATA/easyclimate/output/Mean_annualTemp_REplot2000-2020.RData")
pre <- mean_annual_tmean
head(hist)
head(pre)

# Extract the offset of 2000-2020.
offset_pre <- rast("I:/DATA/mean_annualOffset.tif")
print(offset_pre)

# Load the microclimate CIT data
load("I:/DATA/output/MicroClimPlant_CIT/MicroClimPlant_REplotV3.1_1950s.RData")
head(plots_micro)

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

# Calculate warming magnitude
warming$micro_current <- bio1_current$ForestClim_01[match(
    bio1_current$plotID, warming$plotID
)]
head(warming)

warming$hist_warming <- warming$micro_current - warming$micro_hist
hist(warming$hist_warming)
warm_max <- max(warming$hist_warming)
warm_min <- min(warming$hist_warming)

# Rescale  warming magnitude
quantile_ls <- quantile(warming$hist_warming, c(.05, .95), na.rm = T)
hist(warming$hist_warming)
warming <- warming |>
    mutate(
        MI_warming = ifelse(
            warming$hist_warming < quantile_ls[[1]],
            1,
            ifelse(warming$hist_warming > quantile_ls[[2]],
                0,
                (warm_max - warming$hist_warming) / (warm_max - warm_min)
            )
        )
    )
plot(warming$hist_warming, warming$MI_warming)

# Add warming MI to the plot CIT dataframe.
plots_micro$MI_warming <- warming$MI_warming
plots_micro$hist_warming <- warming$hist_warming
plots_micro$warming_decade <- plots_micro$hist_warming / 5
plots_micro$warmingPerYr <- plots_micro$hist_warming / 50

# Check the data
plot(plots_micro$warmingPerYr, plots_micro$CITperYr)
lm_warm <- lm()
hist(plots_micro$warmingPerYr)
str(plots_micro)

# Subset the MIs of warming based on the MI data distribution.
sub_plot <- plots_micro[
    plots_micro$MI_warming > 0.4 & plots_micro$MI_warming < 0.7,
]

# Subset canopy cover that were increased.
sub_plot_cover <- plots_micro[
    plots_micro$delta_cover > -20 & plots_micro$delta_cover < 20,
]
sub_plot_cover <- as_tibble(sub_plot_cover)
head(sub_plot_cover)
sub_plot_cover <- sub_plot_cover[rowSums(is.na(sub_plot_cover[,16:18]))!=3,]
anyNA(sub_plot_cover$delta_cover)

hist(sub_plot_cover$delta_cover)
hist(plots_micro$delta_cover)
# Inspect the subplot data.
# Warming per year with delta canopy cover.
plot(sub_plot$warmingPerYr, sub_plot$delta_cover)
lm_warmPYr_deltaCover <- lm(
    warmingPerYr ~ delta_cover,
    data = sub_plot
)
summary(lm_warmPYr_deltaCover) # p-value sig.
save(lm_warmPYr_deltaCover,
    file = "I:/DATA/output/lm_histmicro/lm_WarmPYr_deltaCover.RData"
)

# Warming mag with delta canopy cover
plot(sub_plot$hist_warming, sub_plot$delta_cover)
lm_warmM_deltaCover <- lm(
    hist_warming ~ delta_cover,
    data = sub_plot
)
summary(lm_warmM_deltaCover) # p-value sig.
save(lm_warmM_deltaCover,
    file = "I:/DATA/output/lm_histmicro/lm_WarmM_deltaCover.RData"
)

# MI of warming magnitude with delta canopy vover.
plot(sub_plot$MI_warming, sub_plot$delta_cover)
lm_MIwarm_deltaCover <- lm(
    MI_warming ~ delta_cover,
    data = sub_plot
)
summary(lm_MIwarm_deltaCover) # p-value sig.

# MI of Warming magnitude with delta cit.
plot(sub_plot$MI_warming, sub_plot$deltaCIT)
lm_MIwarm_deltacit <- lm(
    deltaCIT ~ MI_warming,
    data = sub_plot
)
summary(lm_MIwarm_deltacit) # p-value sig.
save(lm_MIwarm_deltacit,
    file = "I:/DATA/output/lm_histmicro/lm_MIwarm_deltaCIT.RData"
)

# MI of Warming magnitude with cit per year.
plot(sub_plot$MI_warming, sub_plot$CITperYr)
lm_MIwarm_citPerYr <- lm(
    CITperYr ~ MI_warming,
    data = sub_plot
)
summary(lm_MIwarm_citPerYr) # Not sig.

# Warming per year with delta cit change per year.
plot(sub_plot$warmingPerYr, sub_plot$CITperYr)
lm_warmPerYr_citPerYr <- lm(
    CITperYr ~ warmingPerYr,
    data = sub_plot
)
summary(lm_warmPerYr_citPerYr) # Not sig.

# Check the relationship between delta cit and cover.
plot(sub_plot$delta_cover, sub_plot$deltaCIT)
lm_cover_cit <- lm(
    deltaCIT ~ delta_cover,
    data = sub_plot
)
summary(lm_cover_cit)

hist(sub_plot$CITperYr)
save(lm_df, file = "I:/DATA/output/hist_micro/historicalMicroOffset.RData")
save(warming, file = "I:/DATA/output/hist_micro/historical_warmingM.RData")
save(plots_micro,
    file = "I:/DATA/output/MicroClimPlant_CIT/MicroClimP_REp3.1_1950s.RData"
)
load("I:/DATA/output/MicroClimPlant_CIT/MicroClimP_REp3.1_1950s.RData")