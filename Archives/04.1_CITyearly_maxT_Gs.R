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
plots1990s <- plot_maxt_gs02[which(
    plot_maxt_gs02$year_baseline_survey > 1990 &
        plot_maxt_gs02$year_resurvey_R1 > 1990),
]
unique(plots1990s$plotID)
no_na1990s <- plots1990s |>
    filter_at(
        vars(
            baseline, year_baseline_survey,
            R1, year_resurvey_R1,
            R2, year_resurvey_R2
        ),
        all_vars(!is.na(.))
    ) # 1341 obs
no_na1990s <- subset(no_na1990s, select = -c(R3, R4, R5))
# save data
min(no_na1990s$year_resurvey_R2)
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
no_na1990s$compare <- ifelse(
    no_na1990s$year_baseline_survey >
        no_na1990s$year_resurvey_R1, "baseline larger",
    ifelse(no_na1990s$year_baseline_survey <
        no_na1990s$year_resurvey_R1, "R1 larger",
    "None"
    )
)
unique(no_na1990s$compare)
no_na1990s <- subset(no_na1990s, select = -c(compare))
no_na1990s <- no_na1990s[!grepl("EU_010b_", no_na1990s$plotID),]
no_na1995s <- no_na1990s[!grepl("1992|1993", no_na1990s$year_baseline_survey),]
unique(no_na1995s$plotID)
save(no_na1995s, file = "I:/DATA/output/CITyearly/Base2Re2_maxt_Gs_1995s.RData")
####Go to scripts 05_extractMicro.R to check the coordinators of the plots.####
####if the plots coordinators look normal, follow next scripts 04.2 .####