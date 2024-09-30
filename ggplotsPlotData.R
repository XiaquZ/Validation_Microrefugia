library(ggplot2)

load("I:/DATA/output/MicroClimPlant_REplotV3.1_1950s.RData")
p1 <- hist(plots_micro$first_year)
p2 <- hist(plots_micro$resurvey_year)
plot(p1,
    col = rgb(0, 0, 1, 1 / 4),
    xlim = c(1950, 2024),
    xlab = "Year",
    main = "First and resurvey year"
) # first histogram
plot(p2, col = rgb(1, 0, 0, 1 / 4), xlim = c(1950, 2024), add = T) # second

