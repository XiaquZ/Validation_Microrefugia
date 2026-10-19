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

# Monthly maximum temperatures during summer (June to August)
maxT_Jun <- read.csv("I:/DATA/input/ClimPlant/12199628/MaxTempJun.csv")
maxT_Jul <- read.csv("I:/DATA/input/ClimPlant/12199628/MaxTempJul.csv")
maxT_Aug <- read.csv("I:/DATA/input/ClimPlant/12199628/MaxTempAug.csv")

# Microclimate data from ClimPlant.
micro <- read.csv("I:/DATA/input/ClimPlant/12199628/Summaryfile_V2.0.csv")
head(micro)
# Extract Microclimate_YearMeanMean
micro_mean <- select(micro, X, Microclimate_YearMeanMean)

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
save(mean_maxT_Gs, file = "I:/DATA/output/preparation/MeanMaxT_Gs.RData")

# Calculate the minimum temperature during spring (March to May)
# for the 1000 sampling points by calculating the average
# minimum temperature for these months.
## check if species are in the same row.
identical(minTempMarch[["X"]], minTempMay[["X"]]) # TRUE

# For spring months 1000 plots.
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

# Calculating the average minimum temperature in winter months
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


# Calculating the average maximum temperature in growing season
maxT_summer <- data.frame(
    maxT_Aug[, 1],
    (maxT_Jun[, 2:1001] +
        maxT_Jul[, 2:1001] +
        maxT_Aug[, 2:1001]) / 3
)

colnames(maxT_summer)[1] <- "species_name"
save(maxT_summer, file = "I:/DATA/output/preparation/maxT_summer.RData")

# Calculate the mean temperature per species for max temp during summer.
mean_maxT_summer <- data.frame(
    maxT_summer[, 1],
    rowMeans(maxT_summer[, 2:1001])
)
colnames(mean_maxT_summer) <- c("species_name", "mean_maxTSumm")
save(mean_maxT_summer,
    file = "I:/DATA/output/preparation/MeanMaxT_summer.RData"
)
# Join the respective mean temperature per species with the forestREplot data,
# right join here (join based on ClimPlant).
# We are interested in the species present
# in the ClimPlant database.
# (no temperature data for species only present in forestREplot)

load("I:/DATA/output/minT_MAR2MAY.RData")
load("I:/DATA/output/MeanMinT_MAR2MAY.RData")
load("I:/DATA/output/minT_Dec2Feb.RData")
load("I:/DATA/output/MeanMinT_Dec2Feb.RData")
load("I:/DATA/input/forestREplot/version3/EU_herbL.RData")
# forestREplot Version 3.1
load("I:/DATA/output/forestREplot/EU_herbL.RData")

head(vegherb)
sp.lst <- unique(vegherb$species_name)
# split the name of vegherb species with space
# fix the 'x' in the hybrid species name.
sp_herb <- vegherb |>
    separate(species_name,
        c("genus", "species"),
        extra = "merge",
        fill = "right"
    )
## use 'extra=merge' to keep the extra elements in the second name.
head(sp_herb)
# Check species name
salix <- vegherb[grep("Salix", vegherb$species_name),]

# Replace NA with a specific string
sp_herb <- sp_herb |>
    mutate(species = ifelse(is.na(species), "UnknownSp", species))
sp_herb <- sp_herb[complete.cases(sp_herb[]), ]
sp_herb$species_name <- paste(sp_herb$genus, sp_herb$species)
head(sp_herb)
sp_herb <- sp_herb[-c(2:3)]
sp_herb <- sp_herb[, c(1, 5, 2, 3, 4)]
head(sp_herb)
tail(sp_herb)

## Check the hybrid species names.
# For Salix fraglis
dup_sp <- grep("Salix", sp_herb$species_name)
tadf <- sp_herb[dup_sp, ]
# spe_herb$species_name <- gsub(
#     "Circaea intermedia", "Circaea x intermedia",
#     spe_herb$species_name
# )
# dup_sp <- grep("Circaea x intermedia", spe_herb$species_name)
# tadf <- spe_herb[dup_sp, ]

# For Dryopteris tavelii.
dup_sp <- grep("Dryopteris tavelii", sp_herb$species_name)
tadf <- sp_herb[dup_sp, ]
# spe_herb$species_name <- gsub(
#     "Prunus fruticans", "Prunus x fruticans",
#     spe_herb$species_name
# )
# dup_sp <- grep("Prunus x fruticans", spe_herb$species_name)
# tadf <- spe_herb[dup_sp, ]

# For species with multiple names: Dryopteris dilatata/expansa
dup_sp <- grep("Dryopteris", sp_herb$species_name)
tadf <- sp_herb[dup_sp, ]
unique(tadf$species_name)

# Save final data
save(sp_herb, file = "I:/DATA/output/forestREplot/Cleaned_EU_HerbL.RData")

#### Match with ClimPlant ####
load("I:/DATA/output/forestREplot/Cleaned_EU_HerbL.RData")
load("I:/DATA/output/ClimPlants_micro_mean.RDS")
sp_list <- data.frame(unique(sp_herb$species_name)) # 1441 obs
colnames(sp_list)[1] <- "species_name"
head(sp_list)

# there are 1441 species from forestREplot H. 1168 in ClimPlants.
Re.climP <- right_join(sp_list, micro_mean, by = "species_name")

## 1168 obs in merge data, there are around 19% species not in ClimPlants.
clim_ugent <- (1441 - 1168) / 1441
## There are 0.189 species in foresREplot not in the ClimPlants.
# ClimPlant covers 81.1% forestREplot species.

#### Add ClimPlants mean Temp data to the forestREplot data. ####
# For the minimum temperature during spring
herb_spr <- right_join(spe_herb, mean_minTSpr, by = "species_name")
head(herb_spr)

# For the minimum temperature during winter
herb_win <- right_join(spe_herb, mean_minTWin, by = "species_name")
head(herb_win)

# For the maximum temperature during growing season
herb_maxTGs <- right_join(spe_herb, mean_maxT_Gs, by = "species_name")

# For the maximum temperature during summer
herb_maxTSum <- right_join(spe_herb, mean_maxT_summer, by = "species_name")

# For the microclimate mean annual temperature
herb_micro <- right_join(sp_herb, micro_mean, by = "species_name")
head(herb_micro)

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

save(herb_maxTSum,
    file = "I:/DATA/output/preparation/HerbL_MmaxT_Summer.RData"
)

save(herb_micro,
    file = "I:/DATA/output/forestREplot/EU_HerbL_micro_mean.RData"
)

# Displaying species response curves for illustration
# (one example here for minimum temperature during spring)
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
    geom_vline(aes(xintercept = mean(plot_min_spr[, 1])),
     linetype = "dashed", linewidth = 0.75) 
     # adding mean as vertical dashed line
