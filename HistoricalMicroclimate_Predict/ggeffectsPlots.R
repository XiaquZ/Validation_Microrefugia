library(ggeffects)
library(ggplot2)
library(ggtext)
library(showtext)
library(gghighlight)
library(tidyverse)

# ggeffects
warm1 <- predict_response(
    lm_warmPYr_deltaCover,
    "delta_cover",
    margin = "mean_mode"
)
plot(warm1)

# ggeffects
warm2 <- predict_response(
    lm_warmM_deltaCover,
    "delta_cover",
    margin = "mean_mode"
)
plot(warm2)

# ggeffects
warm3 <- predict_response(
    lm_MIwarm_deltacit,
    "MI_warming",
    margin = "mean_mode"
)
plot(warm3)

# ggeffects
warm4 <- predict_response(
    lm_warmPerYr_citPerYr,
    "warmingPerYr",
    margin = "mean_mode"
)
plot(warm4)

# ggeffects
warm5 <- predict_response(
    lm_MIwarm_deltaCover,
    "delta_cover",
    margin = "mean_mode"
)
plot(warm5)

# cover and cit
covercit <- predict_response(
    lm_cover_cit,
    "delta_cover",
    margin = "mean_mode"
)
plot(covercit)
