library(tidyverse)
library(dplyr)
load("I:/DATA/input/forestREplot/version3/plot_data.RData")
colnames(plot_data)
plot.lst <- unique(plot_data$plotID) # no duplicate plots.

# load plot CIT data of different survey time.
load("I:/DATA/output/CommunityInferredTemp/CIT_Allsurveys_maxTGs.RData")
colnames(plot_maxt_gs)[1] <- "plotID"
# Add the baseline year to the CIT data.
match("year_baseline_survey", names(plot_data))
baseyear <- plot_data[, c(1, 9)]
plot_maxt_gs02 <- merge(plot_maxt_gs, baseyear,
    by = "plotID",
)
plot_maxt_gs02 <- plot_maxt_gs02[, c(1, 2, 8, 3, 4, 5, 6, 7)]
# Add the resurvey year 1 to the CIT data.
match("year_resurvey_R1", names(plot_data)) # get the column number
re1 <- plot_data[, c(1, 10)]
plot_maxt_gs02 <- merge(plot_maxt_gs02, re1,
    by = "plotID",
)
plot_maxt_gs02 <- plot_maxt_gs02[, c(1, 2, 3, 4, 9, 5, 6, 7, 8)]
# Add the resurvey year R2 to the CIT data.
match("year_resurvey_R2", names(plot_data)) # get the column number
re2 <- plot_data[, c(1, 11)]
sum(!is.na(plot_data$year_resurvey_R2)) # 1486 obs
re2 <- re2[!is.na(re2$year_resurvey_R2), ]
plot_maxt_gs02 <- merge(plot_maxt_gs02, re2,
    by = "plotID",
)
match("year_resurvey_R2", names(plot_maxt_gs02)) # get the column number
plot_maxt_gs02 <- plot_maxt_gs02[, c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
baseline <- unique(plot_maxt_gs02$year_baseline_survey)

#### Check and clean the data. ####
# For the year of baseline.
baseline <- unique(plot_maxt_gs02$year_baseline_survey)
# Define the list of values to search for
list_values <- c(
    "1976 \\(T, S 1975\\)", "1956-57",
    "1955 and 57", "1963\\?",
    "1998/99"
)
# Note that Some characters such as (, ), ?, and - have special meanings
# in regular expressions.
# To match them literally, you need to escape them with a double backslash \\.
plot_maxt_gs03 <- plot_maxt_gs02 %>%
    filter(grepl(paste(list_values, collapse = "|"), year_baseline_survey))
# Replace the "1976 \\(T, S 1975\\)" with "1976".
plot_maxt_gs02$year_baseline_survey <- gsub(
    "1976 \\(T, S 1975\\)", "1976",
    plot_maxt_gs02$year_baseline_survey
)
# Replace the "1956-57" with "1957".
plot_maxt_gs02$year_baseline_survey <- gsub(
    "1956-57", "1957",
    plot_maxt_gs02$year_baseline_survey
)
# Replace the "1955 and 57" with "1956".
plot_maxt_gs02$year_baseline_survey <- gsub(
    "1955 and 57", "1956",
    plot_maxt_gs02$year_baseline_survey
)
# Replace the "1963\\?" with "1963".
plot_maxt_gs02$year_baseline_survey <- gsub(
    "1963\\?", "1963",
    plot_maxt_gs02$year_baseline_survey
)
# Replace the "1998/99" with "1999".
plot_maxt_gs02$year_baseline_survey <- gsub(
    "1998/99", "1999",
    plot_maxt_gs02$year_baseline_survey
)
# Check the baseline year again.
baseline <- unique(plot_maxt_gs02$year_baseline_survey)
plot(baseline)
## Finally looks normal now...

# Check how many baseline year are more recent.
list_values <- c(
    "1995", "1999", "1998", "2002",
    "2013", "2011", "2003"
)

plot_maxt_gs03 <- plot_maxt_gs02 %>%
    filter(grepl(paste(list_values, collapse = "|"), year_baseline_survey))
## Only 417 obs

# Now check the year of resurvey R1.
re1 <- unique(plot_maxt_gs02$year_resurvey_R1)
# Replace the "2019-2020" with "2020".
plot_maxt_gs02$year_resurvey_R1 <- gsub(
    "2019-2020", "2020",
    plot_maxt_gs02$year_resurvey_R1
)
re1 <- unique(plot_maxt_gs02$year_resurvey_R1)
no_na <- plot_maxt_gs02 |>
    filter_at(
        vars(
            baseline, year_baseline_survey,
            R1, year_resurvey_R1
        ),
        all_vars(!is.na(.))
    ) # 4956 obs

# Now check the year of resurvey R2.
re2 <- unique(plot_maxt_gs02$year_resurvey_R2)
# Replace the "2019-2020" with "2020".
plot_maxt_gs02$year_resurvey_R2 <- gsub(
    "1976 \\(T, S 1975\\)", "1976",
    plot_maxt_gs02$year_resurvey_R2
)
re1 <- unique(plot_maxt_gs02$year_resurvey_R1)
no_na <- plot_maxt_gs02 |>
    filter_at(
        vars(
            baseline, year_baseline_survey,
            R1, year_resurvey_R1,
            R2, year_resurvey_R2
        ),
        all_vars(!is.na(.))
    ) # 1413 obs
re2 <- unique(plot_maxt_gs02$year_resurvey_R2)

# save data
save(no_na, file = "I:/DATA/output/CITyearly/baseline2Re2.RData")
save(plot_maxt_gs02, file = "I:/DATA/output/CITyearly/FullBaseline2Re2.RData")

######## Calculate yearly CIT and choose the range of time interval.######
load("I:/DATA/output/CITyearly/FullBaseline2Re2.RData")
load("I:/DATA/input/forestREplot/version3/plot_data.RData")
load("I:/DATA/output/CITyearly/baseline2Re2.RData")

# check data
yearbase <- count(plot_maxt_gs02, plot_maxt_gs02$year_baseline_survey)
year_r1 <- count(plot_maxt_gs02, plot_maxt_gs02$year_resurvey_R1)
year_r2 <- count(plot_maxt_gs02, plot_maxt_gs02$year_resurvey_R2)
# sum the number of plots that created after 1995.
colnames(yearbase)[1] <- "baseline_year"
colnames(year_r1)[1] <- "re1_year"
colnames(year_r2)[1] <- "re2_year"
basesum <- sum(yearbase$n[yearbase$baseline_year > 1990]) # 630 plots
r1sum <- sum(year_r1$n[year_r1$re1_year > 1990]) # 1412
r2sum <- sum(year_r2$n[year_r2$re2_year > 1990]) # 1412

# select the plots with resurvey year 1 and resurvey year 2 later than 1990.
plots1990s <- plot_maxt_gs02[
    which(
        plot_maxt_gs02$year_resurvey_R1 > 1990 |
            plot_maxt_gs02$year_resurvey_R2 > 1990
    ),
]

no_na1990s <- plots1990s |>
    filter_at(
        vars(
            baseline, year_baseline_survey,
            R1, year_resurvey_R1,
            R2, year_resurvey_R2
        ),
        all_vars(!is.na(.))
    ) # 1341 obs
no_na1990s <- subset(no_na1990s, select = -c(R4, R5))
# save data
save(no_na1990s, file = "I:/DATA/output/CITyearly/NoNA_Base2Re2_1990s.RData")

#### Now, start calculating the yearly CIT. ####
# Check if the year of resurvey is more recent than the previous survey
no_na1990s$compare <- ifelse(
    no_na1990s$year_resurvey_R1 >
        no_na1990s$year_resurvey_R2, "R1 larger",
    ifelse(no_na1990s$year_resurvey_R1 <
        no_na1990s$year_resurvey_R2, "R2 larger",
    "None"
    )
)
no_na1990s <- subset(no_na1990s, select = -c(R3, compare))
# First, from baseline to R1.
no_na1990s$deltaCIT1 <- no_na1990s$R1 - no_na1990s$baseline
sum(no_na1990s$deltaCIT1 > 0)
## 817 plots has increased CIT,524 plots has decreased CIT.
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

unique(base2r1_10yrs$plotID) #082, 066, 044
unique(base2r1_10yrs$year_baseline_survey) # [1] 1992 1988 1981
unique(base2r1_10yrs$year_resurvey_R1) # [1] 2002 1998 1991
# keep 1981-1991
base2r1_10yrs <- base2r1_10yrs |>
    group_by(year_baseline_survey) |>
    filter(year_baseline_survey == "1981") # 281 obs
unique(base2r1_10yrs$plotID) # all from site EU_082_

load("I:/DATA/input/forestREplot/version3/plot_data.RData")
plot_082 <- plot_data[grep("EU_082_", plot_data$plotID), ]
##More/less same location.
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

unique(r1tor2_10yrs$plotID) #082, 079, 076, 044
unique(r1tor2_10yrs$year_resurvey_R1) # [1] 2002 2010 2011 1991
unique(r1tor2_10yrs$year_resurvey_R2) # [1] 2012 2020 2021 2001
r1tor2_10yrs <- r1tor2_10yrs |>
    group_by(year_resurvey_R1) |>
    filter(year_resurvey_R1 == "1991") # 281 obs all from EU_082_

save(cit_yearly_maxt_gs,
    file = "I:/DATA/output/CITyearly/CITperYr1990s_Baseline2Rs2_maxt_Gs.RData"
)
