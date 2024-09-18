library(tidyr)
library(dplyr)
library(stringr)

# Load the climplant mean annual temperature.
micro <- read.csv("I:/DATA/input/ClimPlant/12199628/Summaryfile_V2.0.csv")
head(micro)

# Extract Microclimate_YearMeanMean
micro_mean <- select(micro, X, Microclimate_YearMeanMean)

# Change column name.
colnames(micro_mean)[1] <- "species_name"

# Save ClimPlant data.
save(micro_mean, file = "I:/DATA/output/ClimPlants_micro_mean.RDS")

# Calculate the mean annual temperature per species.
# mean_mtyr <- data.frame(mt_yr[, 1], rowMeans(mt_yr[, 2:1001]))
# colnames(mean_mtyr) <- c("species_name", "MAT")
# head(mean_mtyr)

#### Match ClimPlant temperature with foresREplot ####
# Go to scripts 02_ to check the data first before matching two data.
load("I:/DATA/output/ClimPlants_micro_mean.RDS")
load("I:/DATA/output/forestREplot/Cleaned_EU_HerbL.RData")
head(sp_herb)

# Check abundance data, if there are any abnormal values.
max(!is.na(sp_herb$abundance)) # 1
min(!is.na(sp_herb$abundance)) # 1

# Before merging forestREplot and ClimPlant:
# There are some NAs in the microclimate data, remove these species.
# And also remove the plots that contain these species, as it will
# have wrong conclusions on CIT.
anyNA(micro_mean$Microclimate_YearMeanMean) # False
anyNA(sp_herb$sample) # F
x <- merge(micro_mean, sp_herb, all = T)
anyNA(x$sample) # T

# Check which species in ClimPlant that are not in replot.
# remove these species that did not appear in replot.
s <- x[is.na(x$sample), ]
x <- x |> drop_na(sample)
anyNA(x$sample) # False

# Check species that do not have microclimate data.
y <- x[is.na(x$Microclimate_YearMeanMean), ]
anyNA(y$sample) # F

# Remove these species from data
species_micro <- x |> drop_na(Microclimate_YearMeanMean)
min(species_micro$Microclimate_YearMeanMean)
max(species_micro$Microclimate_YearMeanMean)

# Check the data
j <- species_micro[species_micro$Microclimate_YearMeanMean == '#N/A', ] # 7
species_micro <- as_tibble(species_micro)
head(species_micro)

# Remove these row.
species_micro <- species_micro[
    !grepl("#N/A", species_micro$Microclimate_YearMeanMean),
]
min(species_micro$Microclimate_YearMeanMean) # Not N/A anymore

# Re-arrange data
species_micro$Microclimate_YearMeanMean <- as.numeric(
    species_micro$Microclimate_YearMeanMean
)
head(species_micro)
species_micro <- species_micro[, c(3, 1, 2, 4, 5, 6)]

# Calculate the CIT base on the mean annual temp.
cit_micro <- species_micro |>
    group_by(sample) |>
    summarise(
        cit_micro = weighted.mean(Microclimate_YearMeanMean, abundance)
    )
head(cit_micro)

##### Convert CIT data into one plot per row.####
# Seperate baseline survey and resurvey into different columns.
# (baseline survey B vs resurvey R1 to R5)

# Get plot id and start with the baseline CIT.
plot_micro <- cit_micro[grep("_B", cit_micro$sample), ]
plot_micro$sample <- str_replace(plot_micro$sample, "_B", "") # get plotid.
colnames(plot_micro)[2] <- "baseline"
head(plot_micro)

# Check which plots have first resurvey R1.
r1 <- cit_micro[grep("_R1", cit_micro$sample), ] # 584 obs
colnames(r1)[2] <- "R1"
r1$sample <- str_replace(r1$sample, "_R1", "") # get plotid.
head(r1)

# Merge r1 data to the plot data.
plot_micro <- merge(plot_micro, r1, by = "sample", all = TRUE)

# Check data
sum(is.na(plot_micro$baseline)) # 13 plots without baseline but have R1.
sum(is.na(plot_micro$R1)) # 31 plots without R1 but have bseline.

# Check which plots have a second resurvey R2.
r2 <- cit_micro[grep("_R2", cit_micro$sample), ] # 1431 obs
colnames(r2)[2] <- "R2"
r2$sample <- str_replace(r2$sample, "_R2", "") # get plotid.
head(r2)

# merge r2 data to the plot data.
plot_micro <- merge(plot_micro, r2, by = "sample", all = TRUE)
head(plot_micro)

# Check which plots have a resurvey R3.
r3 <- cit_micro[grep("_R3", cit_micro$sample), ] # 18 obs
colnames(r3)[2] <- "R3"
r3$sample <- str_replace(r3$sample, "_R3", "") # get plotid.

# merge r3 data to the plot data.
plot_micro <- merge(plot_micro, r3, by = "sample", all = TRUE)

# Check which plots have a resurvey R4.
r4 <- cit_micro[grep("_R4", cit_micro$sample), ] # 629 obs
colnames(r4)[2] <- "R4"
r4$sample <- str_replace(r4$sample, "_R4", "") # get plotid.

# merge r4 data to the plot data.
plot_micro <- merge(plot_micro, r4, by = "sample", all = TRUE)
head(plot_micro)

# Check which plots have a resurvey R5.
r5 <- cit_micro[grep("_R5", cit_micro$sample), ] # 0 obs
colnames(r5)[2] <- "R5"
r5$sample <- str_replace(r5$sample, "_R5", "") # get plotid.
# merge r5 data to the plot data.
plot_micro <- merge(plot_micro, r5, by = "sample", all = TRUE)
head(plot_micro)

plot_micro <- plot_micro[rowSums(is.na(plot_micro[, 2:7])) >= 2, ]

save(plot_micro,
    file = "I:/DATA/output/CommunityInferredTemp/CIT_microTemp_AllSurvey.RData"
)
