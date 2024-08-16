library(tidyr)
library(dplyr)
library(stringr)

# Load the mean temperature during the growing season.
mt_gs <- read.csv("I:/DATA/input/ClimPlant/12199628/MeanTempGrowingSeason.csv")

# Calculate the mean temperature per species during the growing season
mean_mtgs <- data.frame(mt_gs[, 1], rowMeans(mt_gs[, 2:1001]))
colnames(mean_mtgs) <- c("species_name", "mean_gs")
head(mean_mtgs)

#### Match ClimPlant temperature with foresREplot ####
load("I:/DATA/output/preparation/CleanHerbL.RData")
head(spe_herb)

# Add ClimPlants mean temp growing season to the forestREplot.
species_gs <- right_join(spe_herb, mean_mtgs, by = "species_name")
head(species_gs)

# Check abundance data, if there are any abnormal values.
max(!is.na(species_gs$abundance)) # 1
min(!is.na(species_gs$abundance)) # 0

# Calculate the CIT base on the mean temp of growing season.
cit_meangs <- species_gs |>
    group_by(sample) |>
    summarise(
        cit_meangs = weighted.mean(mean_gs, abundance)
    )
head(cit_meangs)

##### Convert CIT data into one plot per row.####
# Seperate baseline survey and resurvey into different columns.
# (baseline survey B vs resurvey R1 to R5)

# Get plot id and start with the baseline CIT.
plot_meangs <- cit_meangs[grep("_B", cit_meangs$sample), ]
plot_meangs$sample <- str_replace(plot_meangs$sample, "_B", "") # get plotid.
colnames(plot_meangs)[2] <- "baseline"
head(plot_meangs)

# Check which plots have first resurvey R1.
r1 <- cit_meangs[grep("_R1", cit_meangs$sample), ] # 4969 obs
colnames(r1)[2] <- "R1"
r1$sample <- str_replace(r1$sample, "_R1", "") # get plotid.

# merge r1 data to the plot data.
plot_meangs <- merge(plot_meangs, r1, by = "sample", all = TRUE)
## 5000 obs, which means there are 5000-4987=13 plots don't have baseline?
sum(is.na(plot_meangs$baseline)) # 13 plots without baseline but have R1.
sum(is.na(plot_meangs$R1)) # 31 plots without R1 but have bseline.
nobaseline <- plot_meangs[is.na(plot_meangs$baseline), ]

# Check which plots have a second resurvey R2.
r2 <- cit_meangs[grep("_R2", cit_meangs$sample), ] # 1431 obs
colnames(r2)[2] <- "R2"
r2$sample <- str_replace(r2$sample, "_R2", "") # get plotid.
head(r2)

# merge r2 data to the plot data.
plot_meangs <- merge(plot_meangs, r2, by = "sample", all = TRUE)
head(plot_meangs)

# Check which plots have a resurvey R3.
r3 <- cit_meangs[grep("_R3", cit_meangs$sample), ] # 916 obs
colnames(r3)[2] <- "R3"
r3$sample <- str_replace(r3$sample, "_R3", "") # get plotid.

# merge r3 data to the plot data.
plot_meangs <- merge(plot_meangs, r3, by = "sample", all = TRUE)

# Check which plots have a resurvey R4.
r4 <- cit_meangs[grep("_R4", cit_meangs$sample), ] # 629 obs
colnames(r4)[2] <- "R4"
r4$sample <- str_replace(r4$sample, "_R4", "") # get plotid.
# merge r4 data to the plot data.
plot_meangs <- merge(plot_meangs, r4, by = "sample", all = TRUE)
head(plot_meangs)

# Check which plots have a resurvey R5.
r5 <- cit_meangs[grep("_R5", cit_meangs$sample), ] # 83 obs
colnames(r5)[2] <- "R5"
r5$sample <- str_replace(r5$sample, "_R5", "") # get plotid.
# merge r4 data to the plot data.
plot_meangs <- merge(plot_meangs, r5, by = "sample", all = TRUE)
head(plot_meangs)

save(plot_meangs,
    file = "I:/DATA/output/CommunityInferredTemp/CIT_Allsurveys_meanTempGs.RData"
)
