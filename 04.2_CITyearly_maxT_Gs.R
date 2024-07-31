####Calculate the delta CIT of plots.####

# First, from baseline to R1.
no_na1990s$deltaCIT1 <- no_na1990s$R1 - no_na1990s$baseline
sum(no_na1990s$deltaCIT1 > 0)
## 313 plots has increased CIT,524 plots has decreased CIT.
# Then, calculate the year interval.
# baseline to resurvey1
no_na1990s$year_resurvey_R1 <- as.numeric(no_na1990s$year_resurvey_R1)
str(no_na1990s$year_resurvey_R1) # num
anyNA(no_na1990s$year_resurvey_R1) # FALSE
no_na1990s$year_baseline_survey <- as.numeric(no_na1990s$year_baseline_survey)
no_na1990s$deltayear1 <- no_na1990s$year_resurvey_R1 -
    no_na1990s$year_baseline_survey
# Add to the new dataframe.
cit_yearly_maxt_gs <- data.frame(
    no_na1990s$plotID,
    no_na1990s$deltaCIT1,
    no_na1990s$deltayear1
)
colnames(cit_yearly_maxt_gs)[1] <- "plotID"
colnames(cit_yearly_maxt_gs)[2] <- "deltaCIT_base2R1"
colnames(cit_yearly_maxt_gs)[3] <- "deltaYr_base2R1"
cit_yearly_maxt_gs$cit_perYbase2R1 <- cit_yearly_maxt_gs$deltaCIT_base2R1 /
    cit_yearly_maxt_gs$deltaYr_base2R1
sum(cit_yearly_maxt_gs$cit_perYbase2R1 > 0) # 817

# Second, resurvey 1 to resurvey 2.
no_na1990s$deltaCIT2 <- no_na1990s$R2 - no_na1990s$R1
sum(no_na1990s$deltaCIT2 > 0)
## 802 plots has increased their CIT from R1 to R2. 539 plots has decreased CIT.
no_na1990s$year_resurvey_R2 <- as.numeric(no_na1990s$year_resurvey_R2)
no_na1990s$deltayear2 <- no_na1990s$year_resurvey_R2 -
    no_na1990s$year_resurvey_R1
# Add to the new dataframe.
cit_yearly_maxt_gs$deltaCIT_R1R2 <- no_na1990s$deltaCIT2
cit_yearly_maxt_gs$deltaYr_R1R2 <- no_na1990s$deltayear2
cit_yearly_maxt_gs$cit_perYR1R2 <- cit_yearly_maxt_gs$deltaCIT_R1R2 /
    cit_yearly_maxt_gs$deltaYr_R1R2
hist(cit_yearly_maxt_gs$cit_perYbase2R1)

yearbase <- count(no_na1990s, no_na1990s$year_baseline_survey)
colnames(yearbase)[1] <- "baseline"
basesum <- sum(yearbase$n[yearbase$baseline > 1990]) # 570 plots
unique(no_na1990s$deltayear1)
# ' [1] 26 22  4 25  7 12 20 10 11 13 42 18 17'
unique(no_na1990s$deltayear2)
# ' [1]  8  6 11 13 12  5  3  7 10 18 15 14'

#### Set a fixed time interval, like 10 years, between baseline and R1.
yearbase <- count(no_na1990s, no_na1990s$deltayear1)
yearr1r2 <- count(no_na1990s, no_na1990s$deltayear2)
base2r1_10yrs <- no_na1990s |>
    group_by(deltayear1) |>
    filter(deltayear1 == "10") # 476 obs

unique(base2r1_10yrs$plotID) # 082, 066, 044
unique(base2r1_10yrs$year_baseline_survey) # [1] 1992 1988 1981
unique(base2r1_10yrs$year_resurvey_R1) # [1] 2002 1998 1991
# keep 1981-1991
base2r1_10yrs <- base2r1_10yrs |>
    group_by(year_baseline_survey) |>
    filter(year_baseline_survey == "1981") # 281 obs
unique(base2r1_10yrs$plotID) # all from site EU_082_

load("I:/DATA/input/forestREplot/version3/plot_data.RData")
plot_082 <- plot_data[grep("EU_082_", plot_data$plotID), ]
## More/less same location.
max(plot_082$latitude) - min(plot_082$latitude)
# vertical distance around 600 meters
max(plot_082$longitude) - min(plot_082$longitude) # horizontal around 72 meter.
# Try with time interval of 11 and 12 years.
base2r1_1112yrs <- no_na1990s |>
    group_by(deltayear1) |>
    filter(deltayear1 == "11" | deltayear1 == "12" | deltayear1 == "13") # 182 obs
unique(base2r1_1112yrs$year_baseline_survey) # 1993 1999 1998 2002 1994
unique(base2r1_1112yrs$year_resurvey_R1) # 2005 2010 2013 2011 2007
unique(base2r1_1112yrs$plotID) # 078, 076, 027, 079, 080

# for resurvey 1 to resurvey 2, within 10 years time interval.
r1tor2_10yrs <- no_na1990s |>
    group_by(deltayear2) |>
    filter(deltayear2 == "10") # 591 obs

unique(r1tor2_10yrs$plotID) # 082, 079, 076, 044
unique(r1tor2_10yrs$year_resurvey_R1) # [1] 2002 2010 2011 1991
unique(r1tor2_10yrs$year_resurvey_R2) # [1] 2012 2020 2021 2001
r1tor2_10yrs <- r1tor2_10yrs |>
    group_by(year_resurvey_R1) |>
    filter(year_resurvey_R1 == "1991") # 281 obs all from EU_082_
base2r1_10yrs <- subset(base2r1_10yrs,
    select = -c(R2, year_resurvey_R2, deltaCIT2, deltayear2)
)
r1tor2_10yrs <- subset(r1tor2_10yrs,
    select = -c(baseline, year_baseline_survey, deltayear1, deltaCIT1)
)
base2r1_10yrs$CITperYR <- base2r1_10yrs$deltaCIT1 / base2r1_10yrs$deltayear1
str(base2r1_10yrs)
r1tor2_10yrs$CITperYR <- r1tor2_10yrs$deltaCIT2 / r1tor2_10yrs$deltayear2
str(r1tor2_10yrs)
save(base2r1_10yrs,
    file = "I:/DATA/output/CITyearly/Base2Re1_CITperYR_maxt_Gs_eu082.RData"
)
save(r1tor2_10yrs,
    file = "I:/DATA/output/CITyearly/R1toR2_CITperYR_maxt_Gs_eu082.RData"
)
