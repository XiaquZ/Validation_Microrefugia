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
# mean_maxTempGS <- data.frame(maxTempGS[, 1], rowMeans(maxTempGS[, 2:1001]))
# colnames(mean_maxTempGS) <- c("species_name", "mean_maxTempGS")

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

# Calculate the mean temperature per species for the minimum temp spring.
head(minT_Spr[, 2])
mean_minTSpr <- data.frame(minT_Spr[, 1], rowMeans(minT_Spr[, 2:1001]))
colnames(mean_minTSpr) <- c("species_name", "mean_minTempSpring")

# Calculate the minimum temperature during winter
# (December to February) for the 1000 sampling points
# by calculating the average minimum temperature for these months
minT_Win <- data.frame(
    minTempDecember[, 1],
    (minTempDecember[, 2:1001] +
        minTempJanuary[, 2:1001] +
        minTempFebruary[, 2:1001]) / 3
)

# Calculate the mean temperature per species for minT during winter
mean_minTWin <- data.frame(minT_Win[, 1], rowMeans(minT_Win[, 2:1001]))
colnames(mean_minTWin) <- c("species_name", "mean_minTempWinter")

# Join the respective mean temperature per species with the forestREplot data,
# right join here (join based on ClimPlant).
# We are interested in the species present
# in the ClimPlant database.
# (no temperature data for species only present in forestREplot)

# Q: There are 1168 understory species in ClimPlant and 1410 in forestREplot,
# so ClimPlant covers 82,8% of forestREplot?
load("I:/DATA/input/forestREplot/version3/EU_herbL.RData")
sp.list <- data.frame(unique(vegherb$species_name))
## This scripts can't exactly make the unique species name,
## some of the species names contain space in the biginning...

colnames(sp.list)[1] <- "species_name"
sp.list <- sp.list[order(sp.list$species_name), ]
sp.list <- data.frame(sp.list)
colnames(sp.list)[1] <- "species_name_ugent"
# split the name of species with space.
spe_list <- sp.list |>
    separate(species_name_ugent,
        c("genus", "species"),
        extra = "drop", fill = "right"
    )
# first, remove the name contains extra space.
spe_list <- spe_list[complete.cases(spe_list[ ,2]), ]
spe_list$species_name <- paste(spe_list$genus, spe_list$species)
spe_list <- spe_list[-c(1:2)]
head(spe_list)
# check the duplicate species name
ta <- table(as.matrix(spe_list))
ta <- ta[ta > 1] #26 duplicate species.

# there are 1300 species from forestREplot H. 1168 in ClimPlants.
Re.climP <- right_join(spe_list, mean_minTSpr, by = "species_name")
## 1194 obs in merge data, there are some species not in ClimPlants.
check.df <- data.frame(unique(spe_list$species_name))
# For the maximum temperature during growing season
vegdata_mean_maxTempGS <- right_join(vegdata_ForestREplot_herblayer, mean_maxTempGS, by = "species_name")
# For the minimum temperature during spring
vegdata_mean_minTempSpring <- right_join(vegdata_ForestREplot_herblayer, mean_minTempSpring, by = "species_name")
# For the minimum temperature during winter
vegdata_mean_minTempWinter <- right_join(vegdata_ForestREplot_herblayer, mean_minTempWinter, by = "species_name")

# Displaying species response curves for illustration (one example here for minimum temperature during spring)
plot_minTempSpring <- data.frame(t(minTempSpring[, 2:1001])) # ggplot takes columns so invert dataframe first

# Example for the species response curve of Abies alba for the minimum temperature during spring
ggplot(plot_minTempSpring, aes(plot_minTempSpring[, 1])) +
    geom_density(color = "black", lwd = 1.1, fill = "darkgreen", alpha = 0.4) +
    theme_bw() +
    ylab("Frequency of occurence") +
    xlab("Minimum temperature during spring (°C)") +
    ggtitle("Abies alba") +
    theme(plot.title = element_text(hjust = 0, size = 20)) +
    geom_vline(aes(xintercept = mean(plot_minTempSpring[, 1])), linetype = "dashed", linewidth = 0.75) # adding mean as vertical dashed line
