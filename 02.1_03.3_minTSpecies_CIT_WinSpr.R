# Load the data
load("I:/DATA/output/preparation/MeanMinT_Dec2Feb.RData")
load("I:/DATA/output/preparation/MeanMinT_MAR2MAY.RData")

# check species name to see if they are identical.
identical(mean_minTSpr["species_name"], mean_minTWin["species_name"])

# Calculate the mean temperature per species for minT during winter and spring.
minT_species <- merge(mean_minTWin, mean_minTSpr, by = "species_name")
minT_species$Mean_minTDec2May <- (
    minT_species$mean_minTempWinter
        + minT_species$mean_minTempSpring) / 2
minT_species <- minT_species[, -c(2, 3)]
#### Match ClimPlant temperature with foresREplot ####
load("I:/DATA/output/preparation/CleanHerbL.RData")
#### Add ClimPlants mean Temp data to the forestREplot data. ####
# For the minimum temperature during spring
species_winspr <- right_join(spe_herb, minT_species, by = "species_name")
head(species_winspr)
# CIT based on the maximum temperature during the growing season
cit_mint_winspr <- species_winspr |>
  group_by(sample) |>
  summarise(
    cit_mint_winspr = weighted.mean(Mean_minTDec2May, abundance)
  )
##### Convert CIT data into one plot per row.####
# Seperate baseline survey and resurvey into different columns.
# (baseline survey B vs resurvey R1 to R5)

##### For the maximum temperature during the growing season.####

# Get plot id and start with the baseline CIT.
plot_mint <- cit_mint_winspr[grep("_B", cit_mint_winspr$sample), ]
## 4987 plots has baseline.
plot_mint$sample <- str_replace(plot_mint$sample, "_B", "") # get plotid.
colnames(plot_mint)[2] <- "baseline"
head(plot_mint)
# Check which plots have first resurvey R1.
r1 <- cit_mint_winspr[grep("_R1", cit_mint_winspr$sample), ] # 4969 obs
colnames(r1)[2] <- "R1"
r1$sample <- str_replace(r1$sample, "_R1", "") # get plotid.
# merge r1 data to the plot data.
plot_mint <- merge(plot_mint, r1, by = "sample", all = TRUE)
## 5000 obs, which means there are 5000-4987=13 plots don't have baseline?
sum(is.na(plot_mint$baseline)) # 13 plots without baseline but have R1.
sum(is.na(plot_mint$R1)) # 31 plots without R1 but have bseline.
nobaseline <- plot_mint[is.na(plot_mint$baseline), ]

# Check which plots have a second resurvey R2.
r2 <- cit_mint_winspr[grep("_R2", cit_mint_winspr$sample), ] # 1431 obs
colnames(r2)[2] <- "R2"
r2$sample <- str_replace(r2$sample, "_R2", "") # get plotid.
head(r2)
# merge r2 data to the plot data.
plot_mint <- merge(plot_mint, r2, by = "sample", all = TRUE)
head(plot_mint)
## 5001 obs
onlyr2 <- plot_mint[is.na(plot_mint$baseline) & is.na(plot_mint$R1), ]
no_na <- plot_mint |> filter_at(vars(baseline, R1, R2), all_vars(!is.na(.)))
## 1414 plots have all the three surveys' data.

# Check which plots have a resurvey R3.
r3 <- cit_mint_winspr[grep("_R3", cit_mint_winspr$sample), ] # 916 obs
colnames(r3)[2] <- "R3"
r3$sample <- str_replace(r3$sample, "_R3", "") # get plotid.
# merge r3 data to the plot data.
plot_mint <- merge(plot_mint, r3, by = "sample", all = TRUE)
## 5001 obs
onlyr3 <- plot_mint[
  is.na(plot_mint$baseline) &
    is.na(plot_mint$R1) &
    is.na(plot_mint$R2),
] # 0obs

no_na <- plot_mint |>
  filter_at(
    vars(baseline, R1, R2, R3),
    all_vars(!is.na(.))
  )
## 898 plots have all the three surveys' data.

# Check which plots have a resurvey R4.
r4 <- cit_mint_winspr[grep("_R4", cit_mint_winspr$sample), ] # 916 obs
colnames(r4)[2] <- "R4"
r4$sample <- str_replace(r4$sample, "_R4", "") # get plotid.
# merge r4 data to the plot data.
plot_mint <- merge(plot_mint, r4, by = "sample", all = TRUE)
## 5001 obs
onlyr4 <- plot_mint[
  is.na(plot_mint$baseline) &
    is.na(plot_mint$R1) &
    is.na(plot_mint$R2) &
    is.na(plot_mint$R3),
] # 0obs

no_na <- plot_mint |>
  filter_at(
    vars(baseline, R1, R2, R3, R4),
    all_vars(!is.na(.))
  )
## 609 obs with all survey data available.

# Check which plots have a resurvey R5.
r5 <- cit_mint_winspr[grep("_R5", cit_mint_winspr$sample), ] # 83 obs
colnames(r5)[2] <- "R5"
r5$sample <- str_replace(r5$sample, "_R5", "") # get plotid.
# merge r5 data to the plot data.
plot_mint <- merge(plot_mint, r5, by = "sample", all = TRUE)
## 5001 obs

no_na <- plot_mint |>
  filter_at(
    vars(baseline, R1, R2, R3, R4, R5),
    all_vars(!is.na(.))
  )
## 80 obs with all survey data available.

# Save data.
save(plot_mint,
  file = "I:/DATA/output/CommunityInferredTemp/CIT_Allsurveys_minTWinSpr.RData"
)
