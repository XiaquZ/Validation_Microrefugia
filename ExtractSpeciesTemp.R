library(tidyr)
library(dplyr)
## ClimPlant (newest update: 24/01/2023)

# Maximum temperature during growing season (April-September)
# at 1000 random locations for all 1168 species
maxTempGS <- read.csv("I:/DATA/input/ClimPlant/12199628/MaxTempGrowingSeason.csv")
# Monthly minimum temperatures during spring (March to May)
# at 1000 random locations for all 1168 species
minTempMarch <- read.csv("I:/DATA/input/ClimPlant/12199628/MinTempMar.csv")
minTempApril <- read.csv("I:/DATA/input/ClimPlant/12199628/MinTempApr.csv")
minTempMay <- read.csv("I:/DATA/input/ClimPlant/12199628/MinTempMay.csv")
# Monthly minimum temperatures during winter (December to February)
# at 1000 random locations for all 1168 species
minTempDecember <- read.csv("I:/DATA/input/ClimPlant/12199628/MinTempDec.csv")
minTempJanuary <- read.csv("I:/DATA/input/ClimPlant/12199628/MinTempJan.csv")
minTempFebruary <- read.csv("I:/DATA/input/ClimPlant/12199628/MinTempFeb.csv")

# plotting the density of the temperature distribution of a species.
class(minTempApril)
AbAl <- minTempApril[1, ]
AbAl <- AbAl %>% pivot_longer(
    cols = !X,
    names_to = "site.nr"
)
hist(AbAl$value)
# ---- 4. EXTRACTING RELEVANT TEMPERATURE INFORMATION PER SPECIES ----

# # Calculate the mean temperature per species for the maximum
# temperature during the growing season
dim(maxTempGS)
mean_maxT_Gs <- data.frame(maxTempGS[, 1], rowMeans(maxTempGS[, 2:1001]))
colnames(mean_maxT_Gs) <- c("species_name", "mean_maxTGs")
head(mean_maxT_Gs)
save(mean_maxT_Gs, file = "I:/DATA/output/MeanMaxT_Gs.RData")
# Calculate the minimum temperature during spring (March to May)
# for the 1000 sampling points by calculating the average
# minimum temperature for these months.
## check if species are in the same row.
identical(minTempMarch[["X"]], minTempMay[["X"]]) # TRUE

minT_Spr <- data.frame(
    minTempMarch[, 1],
    (minTempMarch[, 2:1001] +
        minTempApril[, 2:1001] +
        minTempMay[, 2:1001]) / 3
)
colnames(minT_Spr)[1] <- "species"
str(minT_Spr)
# check species distribution.
AbAl_sp <- minT_Spr[1, ]
AbAl_sp <- AbAl_sp %>% pivot_longer(
    cols = !species,
    names_to = "site.nr"
)
hist(AbAl_sp$value)
save(minT_Spr, file = "I:/DATA/output/minT_MAR2MAY.RData")

# Calculate the mean temperature per species for the minimum temp spring.
head(minT_Spr[, 2])
mean_minTSpr <- data.frame(minT_Spr[, 1], rowMeans(minT_Spr[, 2:1001]))
colnames(mean_minTSpr) <- c("species_name", "mean_minTempSpring")
save(mean_minTSpr, file = "I:/DATA/output/MeanMinT_MAR2MAY.RData")

# Calculate the minimum temperature during winter
# (December to February) for the 1000 sampling points
# by calculating the average minimum temperature for these months
minT_Win <- data.frame(
    minTempDecember[, 1],
    (minTempDecember[, 2:1001] +
        minTempJanuary[, 2:1001] +
        minTempFebruary[, 2:1001]) / 3
)
colnames(minT_Win)[1] <- "species_name"
minT_Win[1:5, 1:5]
save(minT_Win, file = "I:/DATA/output/minT_Dec2Feb.RData")

# Calculate the mean temperature per species for minT during winter
mean_minTWin <- data.frame(minT_Win[, 1], rowMeans(minT_Win[, 2:1001]))
colnames(mean_minTWin) <- c("species_name", "mean_minTempWinter")
mean_minTWin[1:5, ]
save(mean_minTWin, file = "I:/DATA/output/MeanMinT_Dec2Feb.RData")

# Join the respective mean temperature per species with the forestREplot data,
# right join here (join based on ClimPlant).
# We are interested in the species present
# in the ClimPlant database.
# (no temperature data for species only present in forestREplot)

# Q: There are 1168 understory species in ClimPlant and 1410 in forestREplot,
# so ClimPlant covers 82,8% of forestREplot?
load("I:/DATA/output/minT_MAR2MAY.RData")
load("I:/DATA/output/MeanMinT_MAR2MAY.RData")
load("I:/DATA/output/minT_Dec2Feb.RData")
load("I:/DATA/output/MeanMinT_Dec2Feb.RData")
load("I:/DATA/input/forestREplot/version3/EU_herbL.RData")


# split the name of vegherb species with space and fix the 'x' in the species name.
spe_herb <- vegherb |>
    separate(species_name,
        c("genus", "species"),
        extra = "merge",
        fill = "right"
    )
## use 'extra=merge' to keep the extra elements in the second name.

# Replace NA with a specific string
spe_herb <- spe_herb |>
    mutate(species = ifelse(is.na(species), "UnknownSp", species))
spe_herb <- spe_herb[complete.cases(spe_herb[]), ]
spe_herb$species_name <- paste(spe_herb$genus, spe_herb$species)
spe_herb <- spe_herb[-c(2:3)]
spe_herb <- spe_herb[, c(1, 5, 2, 3, 4)]
head(spe_herb)
tail(spe_herb)

## Circaea intermedia  Prunus fruticans: hybride, add 'x' in the name.
# For Circaea intermedia
dup_sp <- grep("Circaea intermedia", spe_herb$species_name)
tadf <- spe_herb[dup_sp, ]
# spe_herb$species_name <- gsub(
#     "Circaea intermedia", "Circaea x intermedia",
#     spe_herb$species_name
# )
# dup_sp <- grep("Circaea x intermedia", spe_herb$species_name)
# tadf <- spe_herb[dup_sp, ]
# For Prunus fruticans
dup_sp <- grep("Prunus fruticans", spe_herb$species_name)
tadf <- spe_herb[dup_sp, ]
# spe_herb$species_name <- gsub(
#     "Prunus fruticans", "Prunus x fruticans",
#     spe_herb$species_name
# )
# dup_sp <- grep("Prunus x fruticans", spe_herb$species_name)
# tadf <- spe_herb[dup_sp, ]
save(spe_herb, file = "I:/DATA/input/forestREplot/version3/CleanHerbL.RData")

#### Match with ClimPlant ####
load("I:/DATA/input/forestREplot/version3/CleanHerbL.RData")
sp_list <- data.frame(unique(spe_herb$species_name)) # 1441 obs
colnames(sp_list)[1] <- "species_name"
head(sp_list)

# there are 1441 species from forestREplot H. 1168 in ClimPlants.
Re.climP <- right_join(sp_list, mean_minTSpr, by = "species_name")
# check hybride species
pf <- grep("Prunus", mean_minTWin$species_name)
pfdf <- mean_minTWin[pf, ] # No ClimPlant data for Prunus x fruticans.
ci <- grep("Circaea intermedia", mean_minTWin$species_name)
cidf <- mean_minTWin[ci, ] # ClimPlant includes Circaea intermedia.

## 1168 obs in merge data, there are some species not in ClimPlants.
clim_ugent <- (1441 - 1168) / 1441
## There are 0.189 species in foresREplot not in the ClimPlants.

#### Add ClimPlants mean Temp data to the forestREplot data. ####
# For the minimum temperature during spring
herb_spr <- right_join(spe_herb, mean_minTSpr, by = "species_name")
head(herb_spr)
# For the minimum temperature during winter
herb_win <- right_join(spe_herb, mean_minTWin, by = "species_name")
head(herb_win)
# For the maximum temperature during growing season
herb_maxTGs <- right_join(spe_herb, mean_maxT_Gs, by = "species_name")

# Save data
save(herb_spr,
    file = "I:/DATA/input/forestREplot/version3/HerbL_MminTspr.RData"
)
save(herb_win,
    file = "I:/DATA/input/forestREplot/version3/HerbL_MminTwin.RData"
)
save(herb_maxTGs,
    file = "I:/DATA/input/forestREplot/version3/HerbL_MmaxT_GS.RData"
)

# Displaying species response curves for illustration (one example here for minimum temperature during spring)
plot_min_spr <- data.frame(t(minT_Spr[, 2:1001]))
# ggplot takes columns so invert dataframe first
plot_min_spr
library(ggplot2)
# Example for the species response curve of Abies alba for the minimum temperature during spring
ggplot(plot_min_spr, aes(plot_min_spr[, 1])) +
    geom_density(color = "black", lwd = 1.1, fill = "darkgreen", alpha = 0.4) +
    theme_bw() +
    ylab("Frequency of occurence") +
    xlab("Minimum temperature during spring (°C)") +
    ggtitle("Abies alba") +
    theme(plot.title = element_text(hjust = 0, size = 20)) +
    geom_vline(aes(xintercept = mean(plot_min_spr[, 1])), linetype = "dashed", linewidth = 0.75) # adding mean as vertical dashed line
