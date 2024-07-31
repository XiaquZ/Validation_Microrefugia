library(dplyr)
####Calculate the delta CIT of plots.####
load("I:/DATA/output/CITyearly/Base2Re2_maxt_Gs_1995s.RData")
# First, from baseline to R1.
no_na1995s$deltaCIT1 <- no_na1995s$R1 - no_na1995s$baseline
sum(no_na1995s$deltaCIT1 > 0)
## 204 plots has increased CIT
# Then, calculate the year interval.
# baseline to resurvey1
no_na1995s$year_resurvey_R1 <- as.numeric(no_na1995s$year_resurvey_R1)
str(no_na1995s$year_resurvey_R1) # num
anyNA(no_na1995s$year_resurvey_R1) # FALSE
no_na1995s$year_baseline_survey <- as.numeric(no_na1995s$year_baseline_survey)
no_na1995s$deltayear1 <- no_na1995s$year_resurvey_R1 -
    no_na1995s$year_baseline_survey

# calculate delta CIT.
no_na1995s$cit_perYbase2R1 <- (no_na1995s$deltaCIT1)/
    (no_na1995s$deltayear1)
sum(no_na1995s$cit_perYbase2R1 > 0) # 204

# Second, resurvey 1 to resurvey 2.
no_na1995s$deltaCIT2 <- no_na1995s$R2 - no_na1995s$R1
sum(no_na1995s$deltaCIT2 > 0)
## 210 plots has increased their CIT from R1 to R2.

no_na1995s$year_resurvey_R2 <- as.numeric(no_na1995s$year_resurvey_R2)
no_na1995s$deltayear2 <- no_na1995s$year_resurvey_R2 -
    no_na1995s$year_resurvey_R1
# cit changes per year
no_na1995s$cit_perYR1R2 <- no_na1995s$deltaCIT2 /
    no_na1995s$deltayear2
hist(no_na1995s$cit_perYbase2R1)

unique(no_na1995s$deltayear1)
## 11 12 13
unique(no_na1995s$deltayear2)
## [1] 10  7 15

#### Set a fixed time interval, like 10 years, between baseline and R1.
yearbase <- count(no_na1995s, no_na1995s$deltayear1)
yearr1r2 <- count(no_na1995s, no_na1995s$deltayear2)

# base2r1_10yrs <- no_na1990s |>
#     group_by(deltayear1) |>
#     filter(deltayear1 == "10") 

unique(no_na1995s$plotID) # 080, 079, 078, 076
unique(no_na1995s$year_baseline_survey) # [1] 1999 1998 2002 1994
unique(no_na1995s$year_resurvey_R1) # [1] 2010 2013 2011 2007
save(no_na1995s, file = "deltaCIT_base2R2_maxtGs_1995s.RData")
# # keep 1981-1991
# base2r1_10yrs <- base2r1_10yrs |>
#     group_by(year_baseline_survey) |>
#     filter(year_baseline_survey == "1981") # 281 obs
# unique(base2r1_10yrs$plotID) # all from site EU_082_

# # Try with time interval of 11 and 12 years.
# base2r1_1112yrs <- no_na1990s |>
#     group_by(deltayear1) |>
#     filter(deltayear1 == "11" | deltayear1 == "12" | deltayear1 == "13")
# unique(base2r1_1112yrs$year_baseline_survey)
# unique(base2r1_1112yrs$year_resurvey_R1) 
# unique(base2r1_1112yrs$plotID) 

#### for resurvey 1 to resurvey 2, within 10 years time interval.
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

#save data.
save(base2r1_10yrs,
    file = "I:/DATA/output/CITyearly/Base2Re1_CITperYR_maxt_Gs_eu082.RData"
)
save(r1tor2_10yrs,
    file = "I:/DATA/output/CITyearly/R1toR2_CITperYR_maxt_Gs_eu082.RData"
)

