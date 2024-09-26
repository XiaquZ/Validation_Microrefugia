library(tidyverse)
library(dplyr)
# load plot CIT data of different survey time.
load("I:/DATA/output/CommunityInferredTemp/CIT_Allsurveys_maxTSummer.RData")
load("I:/DATA/output/CITyearly/Base2Re2_maxt_Gs_1995s.RData")

colnames(plot_maxt_summ)[1] <- "plotID"
plot_summer <- data.frame(no_na1995s02[, c(1:3)])
plot_summer02 <- merge(plot_summer, plot_maxt_summ,
    by = "plotID",
)
# extract the delta year for calculating CIT.
match("deltayear1", names(no_na1995s02)) # get the column number
match("deltayear2", names(no_na1995s02))
delta_yr <- no_na1995s02[, c(1, 11, 14)]
plot_summer02 <- merge(plot_summer02, delta_yr,
    by = "plotID",
)
plot_summer02 <- subset(plot_summer02, select = -c(R3, R4, R5))
plot_summer02$deltaCIT1 <- plot_summer02$R1 - plot_summer02$baseline
plot_summer02$deltaCIT2 <- plot_summer02$R2 - plot_summer02$R1
sum(plot_summer02$deltaCIT1 > 0) # 200
sum(plot_summer02$deltaCIT2 > 0) # 208
plot_summer02$citPeryear1 <- plot_summer02$deltaCIT1 / plot_summer02$deltayear1
plot_summer02$citPeryear2 <- plot_summer02$deltaCIT2 / plot_summer02$deltayear2
hist(plot_summer02$citPeryear1)

save(plot_summer02,
    file = "I:/DATA/output/CITyearly/Base2Re2_maxt_Summer_1995s.RData"
)
#### EXtract microclimate data. 05_
