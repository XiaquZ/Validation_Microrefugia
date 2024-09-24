library(dplyr)
library(lubridate)

# Load easyclimate data.
load("I:/DATA/easyclimate/dailyTmin_1950.RData")
load("I:/DATA/easyclimate/dailyTmax_1950.RData")

# Calculate the T mean by adding min and max together and average.
tmean <- merge(tas_min, tas_max, by = c("date", "lon", "lat", "ID_coords"))
tmean <- tmean |>
    mutate(
        Tmean = (tmean$Tmin + tmean$Tmax) / 2
    )
tmean <- tmean[, -c(5, 6)]
head(tmean)
tmean <- as_tibble(tmean)
tail(tmean)
# Convert character to .datetime object
tmean$date <- parse_date_time(tmean$date, "ymd")
head(tmean)
id_ls <- unique(tmean$ID_coords)
id_s <- sample(id_ls,1)
# Group daily data to monthly



test <- tmean |>
    mutate(
        date = floor_date(date, "month")
    ) |>
    group_by(date) |>
    summarise(
        n = n(),
        MonthlyTmean = sum(Tmean) / n
    )
?floor_date
