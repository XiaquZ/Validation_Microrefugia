library(ggeffects)
library(ggplot2)
library(ggtext)
library(showtext)
library(gghighlight)
library(tidyverse)

# ggeffects
warm1 <- predict_response(
    lm_warmPerYr_citPerYr,
    "warmingPerYr",
    margin = "mean_mode"
)
plot(warm1)
