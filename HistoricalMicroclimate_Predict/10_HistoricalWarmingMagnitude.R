library(dplyr)

# Load data
load("I:/DATA/output/MicroClimPlant_CIT/MicroClimP_REp3.1_1950s.RData")
load("I:/DATA/output/hist_micro/historical_warmingM.RData")

# Calculate warming magnitude
warming$micro_current <- bio1_current$ForestClim_01[match(
    bio1_current$plotID, warming$plotID
)]
head(warming)

warming$hist_warming <- warming$micro_current - warming$micro_hist
hist(warming$hist_warming)
warm_max <- max(warming$hist_warming)
warm_min <- min(warming$hist_warming)

# Rescale  warming magnitude
quantile_ls <- quantile(warming$hist_warming, c(.05, .95), na.rm = T)
hist(warming$hist_warming)
warming <- warming |>
    mutate(
        MI_warming = ifelse(
            warming$hist_warming < quantile_ls[[1]],
            1,
            ifelse(warming$hist_warming > quantile_ls[[2]],
                0,
                (warm_max - warming$hist_warming) / (warm_max - warm_min)
            )
        )
    )
plot(warming$hist_warming, warming$MI_warming)

# Add warming MI to the plot CIT dataframe.
plots_micro$MI_warming <- warming$MI_warming
plots_micro$hist_warming <- warming$hist_warming
plots_micro$warming_decade <- plots_micro$hist_warming / 5
plots_micro$warmingPerYr <- plots_micro$hist_warming / 50

# Drop the columns that will not be used anymore.
plots_micro <- plots_micro[, -c(13:15)]
head(plots_micro)
save(plots_micro,
    file = "I:/DATA/output/hist_micro/MicroClimP_REp3.1_1950s.RData"
)


# Subset data based on the canopy cover change
sub_plot <- plots_micro[
    plots_micro$delta_cover > -50 & plots_micro$delta_cover < 50,
]
hist(sub_plot$delta_cover)

# Subset the MIs of warming based on the MI data distribution.
sub_plot <- sub_plot[
    sub_plot$MI_warming > 0.4 & sub_plot$MI_warming < 0.7,
] 
hist(sub_plot$MI_warming)

# Remove plots without canopy cover data.
sub_plot <- as_tibble(sub_plot)
head(sub_plot)
sub_plot <- sub_plot[rowSums(is.na(sub_plot[,13:15]))!=3,]
anyNA(sub_plot$delta_cover)

hist(sub_plot$delta_cover)
hist(plots_micro$delta_cover)

#### Inspect the subplot data. Relationships between variables. ####

# Warming per year with delta cit change per year.
plot(sub_plot$warmingPerYr, sub_plot$CITperYr)
lm_warmPerYr_citPerYr <- lm(
    CITperYr ~ warmingPerYr,
    data = sub_plot
)
summary(lm_warmPerYr_citPerYr) # Not sig.

# Check the relationship between delta cit and cover.
plot(sub_plot$delta_cover, sub_plot$deltaCIT)
lm_cover_cit <- lm(
    deltaCIT ~ delta_cover,
    data = sub_plot
)
summary(lm_cover_cit) # not sig.

# # Warming per year with delta canopy cover.
# plot(sub_plot$delta_cover, sub_plot$warmingPerYr)
# lm_warmPYr_deltaCover <- lm(
#     warmingPerYr ~ delta_cover,
#     data = sub_plot
# )
# summary(lm_warmPYr_deltaCover) # p-value not sig.
# # save(lm_warmPYr_deltaCover,
# #     file = "I:/DATA/output/lm_histmicro/lm_WarmPYr_deltaCover.RData"
# # )

# # Warming mag with delta canopy cover
# plot( sub_plot$delta_cover, sub_plot$hist_warming)
# lm_warmM_deltaCover <- lm(
#     hist_warming ~ delta_cover,
#     data = sub_plot
# )
# summary(lm_warmM_deltaCover) # p-value not sig.
# # save(lm_warmM_deltaCover,
# #     file = "I:/DATA/output/lm_histmicro/lm_WarmM_deltaCover.RData"
# # )

# # MI of warming magnitude with delta canopy vover.
# plot(sub_plot$MI_warming, sub_plot$delta_cover)
# lm_MIwarm_deltaCover <- lm(
#     MI_warming ~ delta_cover,
#     data = sub_plot
# )
# summary(lm_MIwarm_deltaCover) # p-value not sig.

# # MI of Warming magnitude with delta cit.
# plot(sub_plot$MI_warming, sub_plot$deltaCIT)
# lm_MIwarm_deltacit <- lm(
#     deltaCIT ~ MI_warming,
#     data = sub_plot
# )
# summary(lm_MIwarm_deltacit) # p-value sig.
# save(lm_MIwarm_deltacit,
#     file = "I:/DATA/output/lm_histmicro/lm_MIwarm_deltaCIT.RData"
# )

# # MI of Warming magnitude with cit per year.
# plot(sub_plot$MI_warming, sub_plot$CITperYr)
# lm_MIwarm_citPerYr <- lm(
#     CITperYr ~ MI_warming,
#     data = sub_plot
# )
# summary(lm_MIwarm_citPerYr) # Not sig.

# Save data
save(lm_df, file = "I:/DATA/output/hist_micro/historicalMicroOffset.RData")
save(warming, file = "I:/DATA/output/hist_micro/historical_warmingM.RData")
save(plots_micro,
    file = "I:/DATA/output/MicroClimPlant_CIT/MicroClimP_REp3.1_1950s.RData"
)