#### Community inferred temperature calculation ####

# Calculate CIT per plot sample by taking the weighted mean (abundance)
# of the respective mean temperature of the species present in that plot
library(tidyr)
library(dplyr)
library(stringr)
# Load the species temperature data
load("I:/DATA/output/preparation/Replot_MmaxT_Summer.RData")

# CIT based on the maximum temperature during summer
cit_maxt_summ <- herb_maxTSum |>
    group_by(sample) |>
    summarise(
        cit_maxt_summ = weighted.mean(mean_maxTSumm, abundance)
    )

##### Convert CIT data into one plot per row.####
# Seperate baseline survey and resurvey into different columns.
# (baseline survey B vs resurvey R1 to R5)

##### For the maximum temperature during summer.####

# Get plot id and start with the baseline CIT.
plot_maxt_summ <- cit_maxt_summ[grep("_B", cit_maxt_summ$sample), ]
## 4987 plots has baseline.
plot_maxt_summ$sample <- str_replace(plot_maxt_summ$sample, "_B", "")
## get plotid.
colnames(plot_maxt_summ)[2] <- "baseline"

# Check which plots have first resurvey R1.
r1 <- cit_maxt_summ[grep("_R1", cit_maxt_summ$sample), ] # 4969 obs
colnames(r1)[2] <- "R1"
r1$sample <- str_replace(r1$sample, "_R1", "") # get plotid.
head(r1)
# merge r1 data to the plot data.
plot_maxt_summ <- merge(plot_maxt_summ, r1, by = "sample", all = TRUE)
head(plot_maxt_summ)
## 5000 obs
onlyr1 <- plot_maxt_summ[is.na(plot_maxt_summ$baseline), ] #13 obs.
no_na <- plot_maxt_summ |>
    filter_at(
        vars(baseline, R1),
        all_vars(!is.na(.))
    )
# 4956 obs

# Check which plots have a second resurvey R2.
r2 <- cit_maxt_summ[grep("_R2", cit_maxt_summ$sample), ] # 1431 obs
colnames(r2)[2] <- "R2"
r2$sample <- str_replace(r2$sample, "_R2", "") # get plotid.
# merge r2 data to the plot data.
head(r2)
plot_maxt_summ <- merge(plot_maxt_summ, r2, by = "sample", all = TRUE)
## 5001 obs

onlyr2 <- plot_maxt_summ[
    is.na(plot_maxt_summ$baseline) &
        is.na(plot_maxt_summ$R1),
] # 1 obs, EU_078_30
no_na <- plot_maxt_summ |>
    filter_at(
        vars(baseline, R1, R2),
        all_vars(!is.na(.))
    )
## 1414 plots have all the three surveys' data.

# Check which plots have a resurvey R3.
r3 <- cit_maxt_summ[grep("_R3", cit_maxt_summ$sample), ] # 916 obs
colnames(r3)[2] <- "R3"
r3$sample <- str_replace(r3$sample, "_R3", "") # get plotid.
head(r3)
# merge r3 data to the plot data.
plot_maxt_summ <- merge(plot_maxt_summ, r3, by = "sample", all = TRUE)
## 5001 obs
head(plot_maxt_summ)
onlyr3 <- plot_maxt_summ[
    is.na(plot_maxt_summ$baseline) &
        is.na(plot_maxt_summ$R1) &
        is.na(plot_maxt_summ$R2),
] # 0obs

no_na <- plot_maxt_summ |>
    filter_at(
        vars(baseline, R1, R2, R3),
        all_vars(!is.na(.))
    )
## 898 plots have all the three surveys' data.

# Check which plots have a resurvey R4.
r4 <- cit_maxt_summ[grep("_R4", cit_maxt_summ$sample), ] # 629 obs
colnames(r4)[2] <- "R4"
r4$sample <- str_replace(r4$sample, "_R4", "") # get plotid.
head(r4)
# merge r4 data to the plot data.
plot_maxt_summ <- merge(plot_maxt_summ, r4, by = "sample", all = TRUE)
head(plot_maxt_summ)
## 5001 obs
onlyr4 <- plot_maxt_summ[
    is.na(plot_maxt_summ$baseline) &
        is.na(plot_maxt_summ$R1) &
        is.na(plot_maxt_summ$R2) &
        is.na(plot_maxt_summ$R3),
] # 0obs

no_na <- plot_maxt_summ |>
    filter_at(
        vars(baseline, R1, R2, R3, R4),
        all_vars(!is.na(.))
    )
## 609 obs with all survey data available.

# Check which plots have a resurvey R5.
r5 <- cit_maxt_summ[grep("_R5", cit_maxt_summ$sample), ] # 83 obs
colnames(r5)[2] <- "R5"
r5$sample <- str_replace(r5$sample, "_R5", "") # get plotid.
head(r5)
# merge r5 data to the plot data.
plot_maxt_summ <- merge(plot_maxt_summ, r5, by = "sample", all = TRUE)
## 5001 obs
onlyr5 <- plot_maxt_summ[
    is.na(plot_maxt_summ$baseline) &
        is.na(plot_maxt_summ$R1) &
        is.na(plot_maxt_summ$R2) &
        is.na(plot_maxt_summ$R3) &
        is.na(plot_maxt_summ$R4),
] # 0obs

no_na <- plot_maxt_summ |>
    filter_at(
        vars(baseline, R1, R2, R3, R4, R5),
        all_vars(!is.na(.))
    )
## 80 obs with all survey data available.


# Save data.
save(plot_maxt_summ,
    file = "I:/DATA/output/CommunityInferredTemp/CIT_Allsurveys_maxTSummer.RData"
)