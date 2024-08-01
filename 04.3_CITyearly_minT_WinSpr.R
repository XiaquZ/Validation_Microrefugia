library(tidyverse)
library(dplyr)
# load plot CIT data of different survey time.
load("I:/DATA/output/CommunityInferredTemp/CIT_Allsurveys_minTWinSpr.RData")
load("I:/DATA/output/CITyearly/Base2Re2_maxt_Gs_1995s.RData")

colnames(plot_mint)[1] <- "plotID"
plot_xy <- data.frame(no_na1995s02[, c(1:3)])
plot_mint02 <- merge(plot_mint, plot_xy,
    by = "plotID",
)
# extract the delta year for calculating CIT.
match("deltayear1", names(no_na1995s02)) # get the column number
match("deltayear2", names(no_na1995s02))
delta_yr <- no_na1995s02[, c(1, 11, 14)]
plot_mint02 <- merge(plot_mint02, delta_yr,
    by = "plotID",
)
plot_mint02 <- subset(plot_mint02, select = -c(R3, R4, R5))
plot_mint02 <- plot_mint02[, c(1, 5, 6, 2, 3, 4, 7, 8)]
head(plot_mint02)
plot_mint02$deltaCIT1 <- plot_mint02$R1 - plot_mint02$baseline
plot_mint02$deltaCIT2 <- plot_mint02$R2 - plot_mint02$R1
sum(plot_mint02$deltaCIT1 > 0) # 191
sum(plot_mint02$deltaCIT2 > 0) # 216
plot_mint02$citPeryear1 <- plot_mint02$deltaCIT1 / plot_mint02$deltayear1
plot_mint02$citPeryear2 <- plot_mint02$deltaCIT2 / plot_mint02$deltayear2
hist(plot_mint02$citPeryear1)

save(plot_mint02,
    file = "I:/DATA/output/CITyearly/Base2Re2_mint_WinSpr_1995s.RData"
)
#### EXtract microclimate data. 05_
