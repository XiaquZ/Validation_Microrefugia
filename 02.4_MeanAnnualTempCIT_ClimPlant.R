library(tidyr)
library(dplyr)
library(stringr)

# Load the climplant mean annual temperature.
mt_yr <- read.csv("I:/DATA/input/ClimPlant/12199628/MeanTempYear.csv")

# Calculate the mean annual temperature per species.
mean_mtyr <- data.frame(mt_yr[, 1], rowMeans(mt_yr[, 2:1001]))
colnames(mean_mtyr) <- c("species_name", "MAT")
head(mean_mtyr)

#### Match ClimPlant temperature with foresREplot ####
load("I:/DATA/output/preparation/CleanHerbL.RData")
head(spe_herb)

# Add ClimPlants annual mean temp to the forestREplot.
species_mat <- right_join(spe_herb, mean_mtyr, by = "species_name")
head(species_mat)

# Check abundance data, if there are any abnormal values.
max(!is.na(species_mat$abundance)) # 1
min(!is.na(species_mat$abundance)) # 0

# Calculate the CIT base on the mean annual temp.
cit_meanmat <- species_mat |>
    group_by(sample) |>
    summarise(
        cit_meanmat = weighted.mean(MAT, abundance)
    )
head(cit_meanmat)

##### Convert CIT data into one plot per row.####
# Seperate baseline survey and resurvey into different columns.
# (baseline survey B vs resurvey R1 to R5)

# Get plot id and start with the baseline CIT.
plot_mat <- cit_meanmat[grep("_B", cit_meanmat$sample), ]
plot_mat$sample <- str_replace(plot_mat$sample, "_B", "") # get plotid.
colnames(plot_mat)[2] <- "baseline"
head(plot_mat)

# Check which plots have first resurvey R1.
r1 <- cit_meanmat[grep("_R1", cit_meanmat$sample), ] # 4969 obs
colnames(r1)[2] <- "R1"
r1$sample <- str_replace(r1$sample, "_R1", "") # get plotid.

# merge r1 data to the plot data.
plot_mat <- merge(plot_mat, r1, by = "sample", all = TRUE)
## 5000 obs, which means there are 5000-4987=13 plots don't have baseline?
sum(is.na(plot_mat$baseline)) # 13 plots without baseline but have R1.
sum(is.na(plot_mat$R1)) # 31 plots without R1 but have bseline.
nobaseline <- plot_mat[is.na(plot_mat$baseline), ]

# Check which plots have a second resurvey R2.
r2 <- cit_meanmat[grep("_R2", cit_meanmat$sample), ] # 1431 obs
colnames(r2)[2] <- "R2"
r2$sample <- str_replace(r2$sample, "_R2", "") # get plotid.
head(r2)

# merge r2 data to the plot data.
plot_mat <- merge(plot_mat, r2, by = "sample", all = TRUE)
head(plot_mat)

# Check which plots have a resurvey R3.
r3 <- cit_meanmat[grep("_R3", cit_meanmat$sample), ] # 916 obs
colnames(r3)[2] <- "R3"

r3$sample <- str_replace(r3$sample, "_R3", "") # get plotid.

# merge r3 data to the plot data.
plot_mat <- merge(plot_mat, r3, by = "sample", all = TRUE)

# Check which plots have a resurvey R4.
r4 <- cit_meanmat[grep("_R4", cit_meanmat$sample), ] # 629 obs
colnames(r4)[2] <- "R4"
r4$sample <- str_replace(r4$sample, "_R4", "") # get plotid.

# merge r4 data to the plot data.
plot_mat <- merge(plot_mat, r4, by = "sample", all = TRUE)
head(plot_mat)

# Check which plots have a resurvey R5.
r5 <- cit_meanmat[grep("_R5", cit_meanmat$sample), ] # 83 obs
colnames(r5)[2] <- "R5"
r5$sample <- str_replace(r5$sample, "_R5", "") # get plotid.
# merge r5 data to the plot data.
plot_mat <- merge(plot_mat, r5, by = "sample", all = TRUE)
head(plot_mat)

save(plot_mat,
    file = "I:/DATA/output/CommunityInferredTemp/CIT_Allsurveys_MAT.RData"
)
