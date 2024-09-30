library(dplyr)
library(lubridate)
library(ggplot2)

# Load easyclimate data.
load("I:/DATA/easyclimate/input/dailyTmax_2000-2020.RData")
load("I:/DATA/easyclimate/input/dailyTmin_2000-2020.RData")

# Covert df to tibble.
tasmax_yrs <- as_tibble(tasmax_yrs)
head(tasmax_yrs)
tail(tasmax_yrs)
tasmin_yrs <- as_tibble(tasmin_yrs)
head(tasmin_yrs)
tail(tasmin_yrs)

# Calculate the T mean by adding min and max together and average.
tmean <- merge(tasmin_yrs, tasmax_yrs,
    by = c("ID_coords", "date", "lon", "lat")
)
head(tmean)
tail(tmean)
id <- unique(tmean$ID_coords) #Correct.

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


# Check the distribution of the data.
plot1 <- yearly_annual_tmean[grep("500", yearly_annual_tmean$ID_coords),]
ggplot(data = plot1,
       aes( x= date, y = YearlyTmean, group = ID_coords, color = name)) +
  geom_line(aes(colour=ID_coords)) 


# Calculate the mean annual temperature represent 2000-2020 for each plot. #
mean_annual_tmean <- yearly_annual_tmean %>%
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
    file = "I:/DATA/easyclimate/output/Mean_annualTemp_REplot2000-2020.RData"
)
