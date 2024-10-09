library(dplyr)
library(lubridate)

# Load easyclimate data.
load("I:/DATA/easyclimate/output/yearly_plot_tmean1950.RData")
load("I:/DATA/easyclimate/input/dailyTmax_1951-1970.RData")
load("I:/DATA/easyclimate/input/dailyTmin_1951-1970.RData")
head(tasmin_yrs)
head(tasmax_yrs)

# Calculate the T mean by adding min and max together and average.
tmean <- merge(tasmin_yrs, tasmax_yrs,
    by = c("date", "lon", "lat", "ID_coords")
)

# Get the mean temperature.
tmean <- tmean |>
    mutate(
        Tmean = (tmean$Tmin + tmean$Tmax) / 2
    )
head(tmean)
tmean <- as_tibble(tmean)
tmean <- tmean[, -c(5, 6)]
tail(tmean)

# Convert character to .datetime object
tmean$date <- parse_date_time(tmean$date, "ymd")
head(tmean)

# Group daily data to monthly
monthly_tmean <- tmean |>
    mutate(
        date = floor_date(date, "month")
    ) |>
    group_by(ID_coords, lon, lat, date) |>
    summarise(
        n = n(),
        MonthlyTmean = sum(Tmean) / n,
        .groups = "drop"
    )
head(monthly_tmean)
tail(monthly_tmean)

# Aggregate monthly data to yearly.
yearly_annual_tmean <- monthly_tmean |>
    mutate(date = floor_date(date, "year")) |>
    group_by(ID_coords, lon, lat, date) |>
    summarise(
        n = n(),
        YearlyTmean = sum(MonthlyTmean) / n,
        .groups = "drop"
    )
head(yearly_annual_tmean)
tail(yearly_annual_tmean)
# save yearly data
save(yearly_annual_tmean,
    file = "I:/DATA/easyclimate/input/yearly_Tmean1950-1970.RData"
)

# Add the mean annual temp of 1950 to the data.
head(yearly_tmean)
mean_annual_tmean <- bind_rows(yearly_tmean, yearly_annual_tmean)
mean_annual_tmean <- mean_annual_tmean[order(mean_annual_tmean$ID_coords), ]
head(mean_annual_tmean)
tail(mean_annual_tmean)

# Calculate the mean annual temperature represent 1950-1970 for each plot. #
mean_annual_tmean <- mean_annual_tmean %>%
    group_by(ID_coords, lon, lat, grp = as.integer(gl(n(), 21, n()))) %>%
    summarise(
        n = n(),
        AnnualTmean = sum(YearlyTmean) / n,
        .groups = "drop"
    )

# Check the data
head(mean_annual_tmean)
mean_annual_tmean <- mean_annual_tmean[, -4]
hist(mean_annual_tmean$AnnualTmean)

save(mean_annual_tmean,
    file = "I:/DATA/easyclimate/output/Mean_annualTemp_REplot1950-1970.RData"
)