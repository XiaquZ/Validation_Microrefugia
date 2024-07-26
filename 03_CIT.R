#### Community inferred temperature calculation ####

# Calculate CIT per plot sample by taking the weighted mean (abundance)
# of the respective mean temperature of the species present in that plot
library(tidyr)
library(dplyr)
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
##### Convert CIT data into one plot per row.####
# Seperate baseline survey and resurvey into different columns.
# (baseline survey B vs resurvey R1 to R5)

##### For the maximum temperature during the growing season.####

# Get plot id and start with the baseline CIT.
plot_maxt_gs <- cit_maxt_gs[grep("_B", cit_maxt_gs$sample), ]
##4987 plots has baseline.
plot_maxt_gs$sample <- str_replace(plot_maxt_gs$sample, "_B", "") #get plotid.
colnames(plot_maxt_gs)[2] <- "baseline"

# Check which plots have first resurvey R1.
r1 <- cit_maxt_gs[grep("_R1", cit_maxt_gs$sample), ] #4969 obs
colnames(r1)[2] <- "R1"
r1$sample <- str_replace(r1$sample, "_R1", "") #get plotid.
# merge r1 data to the plot data.
plot_maxt_gs <- merge(plot_maxt_gs, r1, by = "sample", all = TRUE) 
##5000 obs, which means there are 5000-4987=13 plots don't have baseline?
sum(is.na(plot_maxt_gs$baseline)) #13 plots without baseline but have R1.
sum(is.na(plot_maxt_gs$R1)) #31 plots without R1 but have bseline.
nobaseline <- plot_maxt_gs[is.na(plot_maxt_gs$baseline), ]

# Check which plots have a second resurvey R2.






# Extract the survey type (baseline survey or resurvey 1 to 5) of the plot sample with a nested ifelse statement
# (e.g. if plot sample contains _R1 then give value R1 in new column)
CIT_maxTempGS$survey_type <- ifelse(grepl("_B", CIT_maxTempGS$sample), "B",
  ifelse(grepl("_R1", CIT_maxTempGS$sample), "R1",
    ifelse(grepl("_R2", CIT_maxTempGS$sample), "R2",
      ifelse(grepl("_R3", CIT_maxTempGS$sample), "R3",
        ifelse(grepl("_R4", CIT_maxTempGS$sample), "R4", "R5")
      )
    )
  )
)

# Plot sample column not needed anymore (since this is now split between plotID and survey type), so exclude it
CIT_maxTempGS <- CIT_maxTempGS[, -1]

# Change the order of the columns to a more logical order
col_order <- c("plotID", "survey_type", "CIT_maxTempGS")
CIT_maxTempGS <- CIT_maxTempGS[, col_order]

# The CIT data is now in long format, so first it needs to be converted to a wide format data frame
# to separate the baseline surveys and resurveys into separate columns, which makes it easier for later calculations
CIT_maxTempGS <- spread(CIT_maxTempGS, survey_type, CIT_maxTempGS)

# Order plotIDs numerically (same order as plot data, more logical to view/find data)
CIT_maxTempGS <- CIT_maxTempGS[str_order(CIT_maxTempGS$plotID, numeric = T), ]

# Give columns more clear names
colnames(CIT_maxTempGS) <- c(
  "plotID", "B_CIT_maxTempGS", "R1_CIT_maxTempGS",
  "R2_CIT_maxTempGS", "R3_CIT_maxTempGS", "R4_CIT_maxTempGS", "R5_CIT_maxTempGS"
)
