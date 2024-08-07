# Load data
library(dplyr)
library(tidyverse)

load("I:/DATA/output/preparation/CleanHerbL.RData")
load("I:/DATA/input/forestREplot/version3/plot_data.RData")

min(plot_data$year_baseline_survey) #1933
max(plot_data$year_baseline_survey) #2013
min(plot_data$year_resurvey_R1) #1969
max(plot_data$year_resurvey_R1) #2023
anyNA(plot1960s$year_baseline_survey) #F
anyNA(plot1960s$year_resurvey_R1) #F
str(plot_data)
#check the year later than 2000.
plot2000s_b <- plot_data %>%
  mutate(year_baseline_survey = as.numeric(year_baseline_survey)) %>%
  filter(year_baseline_survey >= 2000)

unique(plot2000s_b$plotID)
##078, 079, 083 has baseline surveys later than 2000.
match("year_resurvey_R3", names(plot2000s_b))
plot2000s_b <- plot2000s_b[, -c(12:16)]
plot2000s_b <- plot2000s_b[, -c(6:8)]
anyNA(plot2000s_b$year_baseline_survey) #F
anyNA(plot2000s_b$year_resurvey_R1) #F
anyNA(plot2000s_b$year_resurvey_R2) #T

# get the CIT of plot 83.
# load minimum temp data of each species.
load("I:/DATA/output/CommunityInferredTemp/cit_minT_WinSpr.RData")

#### Add ClimPlants mean Temp data to the forestREplot data. ####
# For the minimum temperature during spring
species_winspr <- right_join(spe_herb, minT_species, by = "species_name")
head(species_winspr)
head(spe_herb)

##grab the data of plot 078 079 083
list_plots <- c(
    "EU_083_", "EU_079_",
    "EU_078_"
)
species_b2000s<- species_winspr[grepl(
    paste(list_plots, collapse = "|"),
    species_winspr$sample
), ]

species_b2000s02<- spe_herb[grepl(
    paste(list_plots, collapse = "|"),
    spe_herb$sample
), ]
## This output did not merge with ClimPlant
## By comparing species_b2000s02 with species_b2000s
## We can see how many species did not have temp data in Climplant.
## Which might cause the missing CIT of one of the survey
## as calculated below.

# Calculate the CIT
cit_mint_b2000s <- species_b2000s |>
  group_by(sample) |>
  summarise(
    cit_mint_b2000s = weighted.mean(Mean_minTDec2May, abundance)
  )
head(cit_mint_b2000s)

# Get plot id and start with the baseline CIT.
cit_b2000s <- cit_mint_b2000s[grep("_B", cit_mint_b2000s$sample), ]
## 190 plots has baseline later than 2000 from the 3 sites.
cit_b2000s$sample <- str_replace(cit_b2000s$sample, "_B", "") # get plotid.
colnames(cit_b2000s)[2] <- "baseline"
head(cit_b2000s)

# Check which plots have first resurvey R1.
r1 <- cit_mint_b2000s[grep("_R1", cit_mint_b2000s$sample), ] # 177
colnames(r1)[2] <- "R1"
r1$sample <- str_replace(r1$sample, "_R1", "") # get plotid.
head(r1)

# merge r1 data to the plot data.
plot_mint <- merge(cit_b2000s, r1, by = "sample", all = TRUE)
head(plot_mint)
## 190 obs
sum(is.na(plot_mint$baseline)) # 0 plots without baseline but have R1.
sum(is.na(plot_mint$R1)) # 13 plots without R1 but have bseline.
nobaseline <- plot_mint[is.na(plot_mint$R1), ] # 079 does not have R1.


# Check which plots have a second resurvey R2.
r2 <- cit_mint_b2000s[grep("_R2", cit_mint_b2000s$sample), ] #119
colnames(r2)[2] <- "R2"
r2$sample <- str_replace(r2$sample, "_R2", "") # get plotid.
head(r2)

# merge r2 data to the r1 data.
plot_mint <- merge(plot_mint, r2, by = "sample", all = TRUE) #191
head(plot_mint)
# check how many plots only have r2 cit data 
onlyr2 <- plot_mint[is.na(plot_mint$baseline) & is.na(plot_mint$R1), ] #078_30
## EU_078_30_B has species Spiraea douglasii. No R1 for this plot.
## In R2, it has Sorbus aucuparia. 
spiraea <- minT_species[grep("Spiraea douglasii", minT_species$species_name), ]
## Not included in ClimPlant.
sorbus <- minT_species[grep("Sorbus aucuparia", minT_species$species_name), ]
## Included in ClimPlant

# Check which plots have a resurvey later than r2.
r3 <- cit_mint_b2000s[grep("_R3", cit_mint_b2000s$sample), ] # 0
r4 <- cit_mint_b2000s[grep("_R4", cit_mint_b2000s$sample), ] # 0
r5 <- cit_mint_b2000s[grep("_R5", cit_mint_b2000s$sample), ] # 0
anyNA(plot_mint$baseline)
save(plot_mint, file = "I:/DATA/output/temp/mint_baseline2000s.RData")
# Remove plots EU_078_30_B, which only has R2.
# merge data of R1 and R2 together as a new column call Resurvey.
# Now all the plot will have baseline and resurvey data.
# Note that this is from baseline to R1.
load("I:/DATA/output/temp/mint_baseline2000s.RData")

#### Conclusions: So for minT of winter and scpring ####
# we can only used the CIT from baseline to the R2 surveys.
# For these three sites, there are no resurveys later than R2.

#### Calculate the deltaCIT and the deltaYear ####
# First, add each survey year to the plots.
