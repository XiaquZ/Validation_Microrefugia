library(tidyverse)
library(dplyr)

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
