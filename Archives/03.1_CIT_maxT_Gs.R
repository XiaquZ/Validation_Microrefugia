#### Community inferred temperature calculation ####

# Calculate CIT per plot sample by taking the weighted mean (abundance)
# of the respective mean temperature of the species present in that plot
library(tidyr)
library(dplyr)
library(stringr)
# Load the species temperature data
load("I:/DATA/output/preparation/Replot_MmaxT_GS.RData")
load("I:/DATA/output/preparation/Replot_MminTspr.RData")
load("I:/DATA/output/preparation/Replot_MmaxT_Summer.RData")
load("I:/DATA/output/preparation/Replot_MminTwin.RData")

# CIT based on the maximum temperature during the growing season
cit_maxt_gs <- herb_maxTGs |>
  group_by(sample) |>
  summarise(
    cit_maxt_gs = weighted.mean(mean_maxTGs, abundance)
  )
# CIT based on the minimum temperature during spring
cit_mint_spr <- herb_spr |>
  group_by(sample) |>
  summarise(
    cit_mint_spr = weighted.mean(mean_minTempSpring, abundance)
  )

# CIT based on the minimum temperature during winter
cit_mint_win <- herb_win |>
  group_by(sample) |>
  summarise(
    cit_mint_win = weighted.mean(mean_minTempWinter, abundance)
  )

# CIT based on the maximum temperature during summer
cit_maxt_sum <- herb_maxTSum |>
  group_by(sample) |>
  summarise(
    cit_maxt_sum = weighted.mean(mean_maxTSumm, abundance)
  )

# Save the CIT.
save(cit_maxt_gs,
  file = "I:/DATA/output/CommunityInferredTemp/cit_maxT_Gs.RData"
)
save(cit_maxt_sum,
  file = "I:/DATA/output/CommunityInferredTemp/cit_maxT_Summer.RData"
)
save(cit_mint_spr,
  file = "I:/DATA/output/CommunityInferredTemp/cit_minT_Spring.RData"
)
save(cit_mint_win,
  file = "I:/DATA/output/CommunityInferredTemp/cit_minT_Winter.RData"
)

##### Convert CIT data into one plot per row.####
# Seperate baseline survey and resurvey into different columns.
# (baseline survey B vs resurvey R1 to R5)

##### For the maximum temperature during the growing season.####

# Get plot id and start with the baseline CIT.
plot_maxt_gs <- cit_maxt_gs[grep("_B", cit_maxt_gs$sample), ]
## 4987 plots has baseline.
plot_maxt_gs$sample <- str_replace(plot_maxt_gs$sample, "_B", "") # get plotid.
colnames(plot_maxt_gs)[2] <- "baseline"

# Check which plots have first resurvey R1.
r1 <- cit_maxt_gs[grep("_R1", cit_maxt_gs$sample), ] # 4969 obs
colnames(r1)[2] <- "R1"
r1$sample <- str_replace(r1$sample, "_R1", "") # get plotid.
# merge r1 data to the plot data.
plot_maxt_gs <- merge(plot_maxt_gs, r1, by = "sample", all = TRUE)
## 5000 obs, which means there are 5000-4987=13 plots don't have baseline?
sum(is.na(plot_maxt_gs$baseline)) # 13 plots without baseline but have R1.
sum(is.na(plot_maxt_gs$R1)) # 31 plots without R1 but have bseline.
nobaseline <- plot_maxt_gs[is.na(plot_maxt_gs$baseline), ]

# Check which plots have a second resurvey R2.
r2 <- cit_maxt_gs[grep("_R2", cit_maxt_gs$sample), ] # 1431 obs
colnames(r2)[2] <- "R2"
r2$sample <- str_replace(r2$sample, "_R2", "") # get plotid.
# merge r2 data to the plot data.
plot_maxt_gs <- merge(plot_maxt_gs, r2, by = "sample", all = TRUE)
## 5001 obs
onlyr2 <- plot_maxt_gs[is.na(plot_maxt_gs$baseline) & is.na(plot_maxt_gs$R1), ]
no_na <- plot_maxt_gs |> filter_at(vars(baseline, R1, R2), all_vars(!is.na(.)))
## 1414 plots have all the three surveys' data.

# Check which plots have a resurvey R3.
r3 <- cit_maxt_gs[grep("_R3", cit_maxt_gs$sample), ] # 916 obs
colnames(r3)[2] <- "R3"
r3$sample <- str_replace(r3$sample, "_R3", "") # get plotid.
# merge r3 data to the plot data.
plot_maxt_gs <- merge(plot_maxt_gs, r3, by = "sample", all = TRUE)
## 5001 obs
onlyr3 <- plot_maxt_gs[
  is.na(plot_maxt_gs$baseline) &
    is.na(plot_maxt_gs$R1) &
    is.na(plot_maxt_gs$R2),
] # 0obs

no_na <- plot_maxt_gs |>
  filter_at(
    vars(baseline, R1, R2, R3),
    all_vars(!is.na(.))
  )
## 898 plots have all the three surveys' data.

# Check which plots have a resurvey R4.
r4 <- cit_maxt_gs[grep("_R4", cit_maxt_gs$sample), ] # 916 obs
colnames(r4)[2] <- "R4"
r4$sample <- str_replace(r4$sample, "_R4", "") # get plotid.
# merge r4 data to the plot data.
plot_maxt_gs <- merge(plot_maxt_gs, r4, by = "sample", all = TRUE)
## 5001 obs
onlyr4 <- plot_maxt_gs[
  is.na(plot_maxt_gs$baseline) &
    is.na(plot_maxt_gs$R1) &
    is.na(plot_maxt_gs$R2) &
    is.na(plot_maxt_gs$R3),
] # 0obs

no_na <- plot_maxt_gs |>
  filter_at(
    vars(baseline, R1, R2, R3, R4),
    all_vars(!is.na(.))
  )
## 609 obs with all survey data available.

# Check which plots have a resurvey R5.
r5 <- cit_maxt_gs[grep("_R5", cit_maxt_gs$sample), ] # 83 obs
colnames(r5)[2] <- "R5"
r5$sample <- str_replace(r5$sample, "_R5", "") # get plotid.
# merge r5 data to the plot data.
plot_maxt_gs <- merge(plot_maxt_gs, r5, by = "sample", all = TRUE)
## 5001 obs
onlyr5 <- plot_maxt_gs[
  is.na(plot_maxt_gs$baseline) &
    is.na(plot_maxt_gs$R1) &
    is.na(plot_maxt_gs$R2) &
    is.na(plot_maxt_gs$R3) &
    is.na(plot_maxt_gs$R4),
] # 0obs

no_na <- plot_maxt_gs |>
  filter_at(
    vars(baseline, R1, R2, R3, R4, R5),
    all_vars(!is.na(.))
  )
## 80 obs with all survey data available.

# Save data.
save(plot_maxt_summ,
  file = "I:/DATA/output/CommunityInferredTemp/CIT_Allsurveys_maxTSummer.RData"
)
