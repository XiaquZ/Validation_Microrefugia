library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

####################################################################################
#   Figures for parts 1-3 of 07_SpeciesThermalNiche_BinaryRange_CIT.R:
#   species thermal niches (part 1), CIT (part 2) and the bootstrapped floristic
#   temperature FT / LT / RT (part 3, De Frenne et al. 2013).
#   Main values: weighted by inverse niche width (*_w), mean over the n_boot draws.
#   Reads the saved outputs only; writes PNG files to fig_dir.
####################################################################################

n_boot  <- 1000 # must match the part 3 run
fig_dir <- "I:/Figures_WP3"

e <- new.env()
load("I:/DATA/output/CIT/CIT_SDMniche_BIO5_BIO6.RData", envir = e) # cit, niches, ...
load(paste0("I:/DATA/output/CIT/FloristicTemp_boot", n_boot, "_SDMniche_BIO5_BIO6.RData"),
     envir = e) # ft_boot, ft_summary
cit <- e$cit
niches <- e$niches
plot_species <- e$plot_species
ft_summary <- e$ft_summary
rm(e)

load("I:/DATA/input/forestREplot/version3.1/plot.data_forestREplot_V3.1.RData") # plot_data
# EU_082: latitude and longitude swapped in v3.1 (all plots), corrected as in
# 01_CleanData.R
coords <- plot_data %>%
  filter(grepl("^EU_", plotID)) %>%
  mutate(swap = grepl("^EU_082_", plotID),
         lat = ifelse(swap, longitude, latitude),
         longitude = ifelse(swap, latitude, longitude),
         latitude = lat) %>%
  dplyr::select(plotID, latitude, longitude)
stopifnot(all(coords$latitude > 34 & coords$latitude < 72 &
              coords$longitude > -25 & coords$longitude < 45, na.rm = TRUE))

var_lab <- c(BIO5 = "BIO5: max. temperature of the warmest month (°C)",
             BIO6 = "BIO6: min. temperature of the coldest month (°C)")
index_col <- c(LT = "#2c6aa8", FT = "grey20", RT = "#c0513a")

save_fig <- function(p, name, w, h) {
  ggsave(file.path(fig_dir, paste0("CIT07_", name, ".png")), p,
         width = w, height = h, units = "cm", dpi = 300)
}
theme_set(theme_bw(base_size = 10) +
            theme(panel.grid.minor = element_blank(),
                  strip.background = element_rect(fill = "grey95")))

# FT / LT / RT summary in long format: one row per plot-survey, index, variable
# and weighting, with the mean and 95% interval over the draws
ft_long <- ft_summary %>%
  pivot_longer(matches("^(FT|LT|RT)_BIO[56]"),
               names_to = c("index", "variable", "stat"),
               names_pattern = "^(FT|LT|RT)_(BIO[56])_(w_mean|w_lo|w_hi|mean|lo|hi)$") %>%
  mutate(weighted = startsWith(stat, "w_"), stat = sub("^w_", "", stat)) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(index = factor(index, levels = c("LT", "FT", "RT")))
ft_w <- filter(ft_long, weighted)

####################################################################################
# 1. Species thermal niches (part 1): median and Q05-Q95 per species
####################################################################################
niche_plot <- function(v) {
  niches %>%
    filter(variable == v) %>%
    mutate(species = reorder(gsub("_", " ", species), median)) %>%
    ggplot(aes(y = species)) +
    geom_linerange(aes(xmin = Q05, xmax = Q95), colour = "grey60") +
    geom_point(aes(x = median), size = 0.9) +
    labs(x = var_lab[[v]], y = NULL) +
    theme(axis.text.y = element_text(size = 4.5, face = "italic"))
}
save_fig(niche_plot("BIO5") + niche_plot("BIO6") +
           plot_annotation(caption = "Point: niche median; line: 5th-95th percentile of the SDM range cells"),
         "fig01_species_niches", 22, 30)

# Niche width (Q95 - Q05): the weights of the *_w values are 1 / width
p <- niches %>%
  ggplot(aes(width)) +
  geom_histogram(bins = 30, fill = "grey60", colour = "white") +
  facet_wrap(~variable, scales = "free", labeller = as_labeller(var_lab)) +
  labs(x = "Niche width, Q95 - Q05 (°C)", y = "Number of species")
save_fig(p, "fig02_niche_width", 18, 8)

####################################################################################
# 2. Map of the floristic temperature (weighted FT, baseline survey)
####################################################################################
world <- map_data("world")
map_d <- ft_w %>%
  filter(index == "FT", surveyTyp == "B") %>%
  left_join(coords, by = "plotID")
xl <- range(map_d$longitude, na.rm = TRUE) + c(-2, 2)
yl <- range(map_d$latitude, na.rm = TRUE) + c(-1, 1)
map_plot <- function(v, option) {
  ggplot() +
    geom_polygon(data = world, aes(long, lat, group = group),
                 fill = "grey92", colour = "grey70", linewidth = 0.2) +
    geom_point(data = filter(map_d, variable == v),
               aes(longitude, latitude, colour = mean), size = 0.8) +
    scale_colour_viridis_c(option = option, name = "FT (°C)") +
    coord_quickmap(xlim = xl, ylim = yl) +
    labs(x = NULL, y = NULL, title = var_lab[[v]]) +
    theme(plot.title = element_text(size = 9))
}
save_fig(map_plot("BIO5", "inferno") + map_plot("BIO6", "mako") +
           plot_annotation(caption = "Weighted floristic temperature, mean of the draws; baseline survey"),
         "fig03_map_FT_baseline", 26, 12)

####################################################################################
# 3. Part 2 CIT vs part 3 FT (both weighted)
####################################################################################
cmp <- ft_w %>%
  filter(index == "FT") %>%
  dplyr::select(sample, variable, FT = mean) %>%
  inner_join(cit %>%
               dplyr::select(sample, BIO5 = CIT_BIO5_w, BIO6 = CIT_BIO6_w) %>%
               pivot_longer(c(BIO5, BIO6), names_to = "variable", values_to = "CIT"),
             by = c("sample", "variable"))
cmp_stats <- cmp %>%
  group_by(variable) %>%
  summarise(lab = sprintf("r = %.3f\nmean FT - CIT = %+.2f °C", cor(CIT, FT), mean(FT - CIT)),
            .groups = "drop")
print(cmp_stats)
p <- ggplot(cmp, aes(CIT, FT)) +
  geom_abline(linetype = 2, colour = "grey50") +
  geom_point(size = 0.4, alpha = 0.3) +
  geom_text(data = cmp_stats, aes(-Inf, Inf, label = lab), hjust = -0.05, vjust = 1.2, size = 3) +
  facet_wrap(~variable, scales = "free", labeller = as_labeller(var_lab)) +
  labs(x = "CIT, part 2 (niche medians, °C)", y = "FT, part 3 (mean of the draws, °C)")
save_fig(p, "fig04_CIT_vs_FT", 20, 10)

####################################################################################
# 4. Cold end, mean and warm end of the communities (LT, FT, RT)
####################################################################################
p <- ggplot(ft_w, aes(mean, fill = index, colour = index)) +
  geom_density(alpha = 0.25) +
  scale_fill_manual(values = index_col) +
  scale_colour_manual(values = index_col) +
  facet_wrap(~variable, scales = "free", labeller = as_labeller(var_lab)) +
  labs(x = "Mean of the draws (°C)", y = "Density (plot-surveys)", fill = NULL, colour = NULL,
       caption = "LT: 5th percentile, FT: mean, RT: 95th percentile of the species in a plot (weighted)")
save_fig(p, "fig05_LT_FT_RT_distribution", 20, 9)

# Example plot-surveys: LT, FT and RT with their 95% intervals over the draws
set.seed(42)
ex <- sample(unique(ft_w$sample[ft_w$surveyTyp == "B"]), 25)
ex_d <- ft_w %>%
  filter(sample %in% ex) %>%
  mutate(sample = reorder(sample, ifelse(index == "FT" & variable == "BIO5", mean, NA),
                          FUN = function(x) mean(x, na.rm = TRUE)))
p <- ggplot(ex_d, aes(y = sample, x = mean, xmin = lo, xmax = hi, colour = index)) +
  geom_pointrange(size = 0.15, position = position_dodge(width = 0.6)) +
  scale_colour_manual(values = index_col) +
  facet_wrap(~variable, scales = "free_x", labeller = as_labeller(var_lab)) +
  labs(x = "°C (point: mean of the draws; line: 95% interval)", y = NULL, colour = NULL,
       caption = "25 random baseline plot-surveys, ordered by FT of BIO5")
save_fig(p, "fig06_example_plots_LT_FT_RT", 22, 16)

####################################################################################
# 5. Uncertainty: 95% interval width vs number of species (min_species check)
####################################################################################
ci_d <- ft_w %>%
  mutate(width = hi - lo,
         n_cls = factor(ifelse(n_species >= 20, "20+", n_species),
                        levels = c(as.character(3:19), "20+")))
print(ci_d %>% group_by(index, variable, n_cls) %>%
        summarise(n = n(), median_width = median(width), .groups = "drop") %>%
        filter(n_cls %in% c("3", "4", "5", "10", "20+")), n = Inf)
p <- ggplot(ci_d, aes(n_cls, width, fill = index)) +
  geom_boxplot(outlier.size = 0.2, linewidth = 0.3) +
  scale_fill_manual(values = index_col, guide = "none") +
  facet_grid(index ~ variable, scales = "free_y") +
  labs(x = "Number of modelled species in the plot-survey", y = "Width of the 95% interval (°C)")
save_fig(p, "fig07_CIwidth_vs_nspecies", 24, 16)

####################################################################################
# 6. Weighted vs unweighted FT
####################################################################################
wu <- ft_long %>%
  filter(index == "FT") %>%
  dplyr::select(sample, variable, weighted, mean) %>%
  pivot_wider(names_from = weighted, values_from = mean, names_prefix = "w_")
p <- ggplot(wu, aes(w_FALSE, w_TRUE)) +
  geom_abline(linetype = 2, colour = "grey50") +
  geom_point(size = 0.4, alpha = 0.3) +
  facet_wrap(~variable, scales = "free", labeller = as_labeller(var_lab)) +
  labs(x = "FT, unweighted (°C)", y = "FT, weighted by 1 / niche width (°C)")
save_fig(p, "fig08_FT_weighted_vs_unweighted", 20, 10)

####################################################################################
# 7. Example plot (as Fig. 1C-D in De Frenne et al. 2013): thermal distributions
#    of its species, and the community at the baseline and the last resurvey
####################################################################################
# Species distributions: the part 1 samples of BIO5 / BIO6 over the SDM range
# cells (the range-based equivalent of their thermal response curves).
samp_dir <- "N:/SDMs/SDMs_current/ThermalNiche/samples"
# Plot to show; NULL = the plot with the largest increase of the weighted FT
# (BIO5) between the baseline and the last resurvey, among plots with 5-15
# species in both surveys (a clear example, not a typical plot)
example_plot <- NULL

norm_name <- function(x) tolower(trimws(gsub("[_ ]+", " ", x)))
surv_order <- c("B", paste0("R", 1:5))
last_survey <- function(s) s[which.max(match(s, surv_order))]

if (is.null(example_plot)) {
  cand <- ft_summary %>%
    filter(n_species >= 5, n_species <= 15) %>%
    group_by(plotID) %>%
    filter("B" %in% surveyTyp, n() >= 2) %>%
    summarise(last = last_survey(surveyTyp),
              dFT = FT_BIO5_w_mean[surveyTyp == last] - FT_BIO5_w_mean[surveyTyp == "B"],
              .groups = "drop")
  ex_plot <- cand$plotID[which.max(cand$dFT)]
  ex_note <- "plot with the largest FT increase (BIO5) among plots with 5-15 species"
} else {
  ex_plot <- example_plot
  ex_note <- "selected plot"
}
ex_surv <- c("B", last_survey(ft_summary$surveyTyp[ft_summary$plotID == ex_plot]))
stopifnot(ex_surv[2] != "B")

# Survey years for the labels
yr_col <- c(B = "year_baseline_survey", setNames(paste0("year_resurvey_R", 1:5), paste0("R", 1:5)))
yrs <- vapply(ex_surv, function(s) as.character(plot_data[[yr_col[[s]]]][plot_data$plotID == ex_plot]), "")
surv_lab <- setNames(paste0(c("Baseline", "Resurvey"), " ", ex_surv, " (", yrs, ")"), ex_surv)

# Species of the two surveys: in both, lost (baseline only) or gained (resurvey only)
sp_ex <- plot_species %>%
  filter(sample %in% paste(ex_plot, ex_surv, sep = "_")) %>%
  mutate(survey = sub("^.*_", "", sample)) %>%
  group_by(species_name, species_key) %>%
  summarise(status = case_when(all(ex_surv %in% survey) ~ "in both surveys",
                               "B" %in% survey ~ "lost", TRUE ~ "gained"),
            .groups = "drop") %>%
  mutate(species = gsub("_", " ", species_name),
         status = factor(status, levels = c("in both surveys", "lost", "gained")))
print(sp_ex, n = Inf)

sp_files <- list.files(samp_dir, pattern = "\\.rds$", full.names = TRUE)
names(sp_files) <- norm_name(tools::file_path_sans_ext(basename(sp_files)))
stopifnot(all(sp_ex$species_key %in% names(sp_files)))

niche_ex <- niches %>%
  mutate(species_key = norm_name(species)) %>%
  filter(species_key %in% sp_ex$species_key)
lims <- lapply(c(BIO5 = "BIO5", BIO6 = "BIO6"), function(v) {
  n <- filter(niche_ex, variable == v)
  c(min(n$Q05), max(n$Q95)) + c(-5, 5)
})

# Kernel density of each species' sampled cells, on a common grid per variable
dens <- bind_rows(lapply(seq_len(nrow(sp_ex)), function(k) {
  s <- readRDS(sp_files[[sp_ex$species_key[k]]])
  bind_rows(lapply(c("BIO5", "BIO6"), function(v) {
    x <- s[, v]
    d <- density(x[!is.na(x)], n = 512, from = lims[[v]][1], to = lims[[v]][2])
    data.frame(species_key = sp_ex$species_key[k], variable = v, x = d$x, y = d$y)
  }))
})) %>%
  left_join(dplyr::select(sp_ex, species_key, species, status), by = "species_key")

# Community distribution per survey: mixture of its species' distributions,
# weighted by 1 / niche width as the weighted FT (an illustration: LT and RT are
# computed per draw over the species, see part 3)
comm <- bind_rows(lapply(ex_surv, function(s) {
  keep <- if (s == "B") c("in both surveys", "lost") else c("in both surveys", "gained")
  dens %>%
    filter(status %in% keep) %>%
    left_join(dplyr::select(niche_ex, species_key, variable, width),
              by = c("species_key", "variable")) %>%
    group_by(variable, x) %>%
    summarise(y = sum(y / width) / sum(1 / width), .groups = "drop") %>%
    mutate(survey = s)
})) %>%
  mutate(survey = factor(surv_lab[survey], levels = surv_lab))

# LT, FT, RT of both surveys (weighted, mean of the draws)
marks <- ft_w %>%
  filter(plotID == ex_plot, surveyTyp %in% ex_surv) %>%
  mutate(survey = factor(surv_lab[surveyTyp], levels = surv_lab))
arrows_d <- marks %>%
  dplyr::select(variable, index, surveyTyp, mean) %>%
  pivot_wider(names_from = surveyTyp, values_from = mean) %>%
  rename(x_b = all_of(ex_surv[1]), x_e = all_of(ex_surv[2])) %>%
  left_join(comm %>% group_by(variable) %>% summarise(ytop = max(y), .groups = "drop"),
            by = "variable")
print(arrows_d)

sp_col <- setNames(scales::hue_pal()(nrow(sp_ex)), sort(sp_ex$species))
species_panel <- function(v) {
  ggplot(filter(dens, variable == v), aes(x, y, colour = species, linetype = status)) +
    geom_line(linewidth = 0.6) +
    scale_colour_manual(values = sp_col) +
    scale_linetype_manual(values = c("in both surveys" = 1, lost = 2, gained = 3), drop = FALSE) +
    labs(x = var_lab[[v]], y = "Density of range cells", colour = NULL, linetype = NULL) +
    theme(legend.text = element_text(face = "italic", size = 7))
}
community_panel <- function(v) {
  a <- filter(arrows_d, variable == v)
  ggplot() +
    geom_line(data = filter(comm, variable == v), aes(x, y, colour = survey), linewidth = 1.1) +
    geom_vline(data = filter(marks, variable == v),
               aes(xintercept = mean, linetype = survey), colour = "grey40", linewidth = 0.4) +
    geom_segment(data = a, aes(x = x_b, xend = x_e, y = ytop * 1.08, yend = ytop * 1.08),
                 colour = "#2e9e44", linewidth = 1,
                 arrow = arrow(length = unit(2, "mm"), type = "closed")) +
    geom_text(data = a, aes(x = (x_b + x_e) / 2, y = ytop * 1.17, label = index), size = 3) +
    scale_colour_manual(values = c("grey70", "grey20")) +
    scale_linetype_manual(values = c(2, 1)) +
    labs(x = var_lab[[v]], y = "Community density (weighted)", colour = NULL, linetype = NULL)
}
p <- (species_panel("BIO5") + species_panel("BIO6") + plot_layout(guides = "collect")) /
  (community_panel("BIO5") + community_panel("BIO6") + plot_layout(guides = "collect")) +
  plot_annotation(
    title = paste0(ex_plot, ": ", surv_lab[1], " vs ", surv_lab[2]),
    subtitle = ex_note,
    caption = paste("Top: species' BIO5 / BIO6 over their SDM range cells (solid: in both surveys,",
                    "dashed: lost, dotted: gained).\nBottom: community = species mixture weighted by",
                    "1 / niche width; vertical lines and arrows: LT, FT, RT (weighted, mean of the draws)."))
save_fig(p, paste0("fig09_example_plot_", ex_plot), 28, 20)
