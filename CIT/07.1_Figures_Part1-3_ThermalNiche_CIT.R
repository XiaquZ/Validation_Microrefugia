library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

####################################################################################
#   Figures for parts 1-4 of 07_SpeciesThermalNiche_BinaryRange_CIT.R:
#   species thermal niches (part 1), CIT (part 2), the bootstrapped floristic
#   temperature FT / LT / RT (part 3, De Frenne et al. 2013) and its change per
#   decade (part 4, section 8).
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
# Plots to show, among plots with 5-15 species in both surveys (change of the
# weighted FT of BIO5 between the baseline and the last resurvey):
#   - extreme: the largest FT increase (a clear example, not a typical plot)
#   - typical: FT change closest to the median, with at least one species lost
#     and one gained (so that the turnover can be seen)
#   - example_plot: an additional plot of your choice (NULL = none)
example_plot <- NULL

norm_name <- function(x) tolower(trimws(gsub("[_ ]+", " ", x)))
surv_order <- c("B", paste0("R", 1:5))
last_survey <- function(s) s[which.max(match(s, surv_order))]

cand <- ft_summary %>%
  filter(n_species >= 5, n_species <= 15) %>%
  group_by(plotID) %>%
  filter("B" %in% surveyTyp, n() >= 2) %>%
  summarise(last = last_survey(surveyTyp),
            dFT = FT_BIO5_w_mean[surveyTyp == last] - FT_BIO5_w_mean[surveyTyp == "B"],
            .groups = "drop")
# Species lost and gained between the baseline and the last resurvey
turnover <- plot_species %>%
  mutate(plotID = sub("_[^_]+$", "", sample), surveyTyp = sub("^.*_", "", sample)) %>%
  inner_join(dplyr::select(cand, plotID, last), by = "plotID") %>%
  filter(surveyTyp == "B" | surveyTyp == last) %>%
  group_by(plotID, species_key) %>%
  summarise(in_b = "B" %in% surveyTyp, in_e = any(surveyTyp != "B"), .groups = "drop") %>%
  group_by(plotID) %>%
  summarise(n_lost = sum(in_b & !in_e), n_gained = sum(!in_b & in_e), .groups = "drop")
cand <- left_join(cand, turnover, by = "plotID")
typ <- filter(cand, n_lost >= 1, n_gained >= 1)

ex_list <- list(
  list(plot = cand$plotID[which.max(cand$dFT)], tag = "a_extreme",
       note = "plot with the largest FT increase (BIO5) among plots with 5-15 species"),
  list(plot = typ$plotID[which.min(abs(typ$dFT - median(cand$dFT)))], tag = "b_typical",
       note = sprintf(paste("typical plot: FT change (BIO5) closest to the median of plots with",
                            "5-15 species (%+.2f °C), with species lost and gained"),
                      median(cand$dFT)))
)
if (!is.null(example_plot)) {
  ex_list[[3]] <- list(plot = example_plot, tag = "c_selected", note = "selected plot")
}

sp_files <- list.files(samp_dir, pattern = "\\.rds$", full.names = TRUE)
names(sp_files) <- norm_name(tools::file_path_sans_ext(basename(sp_files)))
yr_col <- c(B = "year_baseline_survey", setNames(paste0("year_resurvey_R", 1:5), paste0("R", 1:5)))

example_fig <- function(ex_plot, ex_note, tag) {
  ex_surv <- c("B", last_survey(ft_summary$surveyTyp[ft_summary$plotID == ex_plot]))
  stopifnot(ex_surv[2] != "B")

  yrs <- vapply(ex_surv, function(s) as.character(plot_data[[yr_col[[s]]]][plot_data$plotID == ex_plot]), "")

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
  stopifnot(all(sp_ex$species_key %in% names(sp_files)))
  n_status <- table(sp_ex$status)

  # Survey labels (title and legend): year and number of modelled species
  n_surv <- n_status[["in both surveys"]] + c(n_status[["lost"]], n_status[["gained"]])
  surv_lab <- setNames(paste0(c("Baseline", "Resurvey"), " ", ex_surv, " (", yrs, ", ",
                              n_surv, " species)"), ex_surv)

  niche_ex <- niches %>%
    mutate(species_key = norm_name(species)) %>%
    filter(species_key %in% sp_ex$species_key)
  lims <- lapply(c(BIO5 = "BIO5", BIO6 = "BIO6"), function(v) {
    n <- filter(niche_ex, variable == v)
    c(min(n$Q05), max(n$Q95)) + c(-5, 5)
  })

  # Kernel density of each species' sampled cells, on a common grid per variable.
  # A smoothed histogram: turns the ~1e5 sampled values into a curve without bin
  # edges (bandwidth from density(), Silverman's rule). Each curve has area 1, so
  # species with small and large ranges are comparable. The common grid (from, to,
  # n) lets the species curves be averaged into the community curve below. For
  # display only: FT / LT / RT (part 3) come from the sampled values themselves.
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
      subtitle = sprintf("%s\nspecies: %d in both surveys, %d lost, %d gained", ex_note,
                         n_status[["in both surveys"]], n_status[["lost"]], n_status[["gained"]]),
      caption = paste("Top: species' BIO5 / BIO6 over their SDM range cells (solid: in both surveys,",
                      "dashed: lost, dotted: gained).\nBottom: community = species mixture weighted by",
                      "1 / niche width; vertical lines and arrows: LT, FT, RT (weighted, mean of the draws)."))
  save_fig(p, paste0("fig09", tag, "_example_plot_", ex_plot), 28, 20)
}

for (ex in ex_list) example_fig(ex$plot, ex$note, ex$tag)

####################################################################################
# 8. Temporal change (part 4): rates of LT, FT and RT in °C per decade
####################################################################################
e <- new.env()
load(paste0("I:/DATA/output/CIT/CIT_change_boot", n_boot, "_SDMniche_BIO5_BIO6.RData"),
     envir = e) # cit_change, ft_change_boot, ft_change_summary, survey_year
ft_change_boot <- e$ft_change_boot
ft_change_summary <- e$ft_change_summary
rm(e)

rate_lab <- "°C per decade"
dataset_of <- function(x) sub("^(EU_[0-9]+[a-z]?)_.*", "\\1", x)

# Plot rates in long format: one row per plot, index, variable and weighting,
# with the mean and 95% interval over the draws
chg_long <- ft_change_summary %>%
  pivot_longer(matches("^d(FT|LT|RT)_BIO[56]"),
               names_to = c("index", "variable", "w", "stat"),
               names_pattern = "^d(FT|LT|RT)_(BIO[56])_(w_)?dec_(mean|lo|hi)$") %>%
  mutate(weighted = coalesce(w, "") == "w_") %>% # unmatched (w_)? -> "" or NA
  dplyr::select(-w) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(index = factor(index, levels = c("LT", "FT", "RT")))
chg_w <- filter(chg_long, weighted)

# A. Map of the plot rates (weighted FT). Colour limits at the 98th percentile
# of |rate|, so that a few extreme plots do not flatten the scale (larger values
# get the end colour); strongest rates drawn on top. Map extent as in section 2.
map_chg <- chg_w %>%
  filter(index == "FT") %>%
  left_join(coords, by = "plotID") %>%
  arrange(abs(mean))
map_chg_plot <- function(v) {
  d <- filter(map_chg, variable == v)
  lim <- quantile(abs(d$mean), 0.98, na.rm = TRUE)
  ggplot() +
    geom_polygon(data = world, aes(long, lat, group = group),
                 fill = "grey92", colour = "grey70", linewidth = 0.2) +
    geom_point(data = d, aes(longitude, latitude, fill = mean),
               shape = 21, colour = "grey40", stroke = 0.1, size = 1.2) +
    scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0,
                         limits = c(-lim, lim), oob = scales::squish,
                         name = paste0("Change in FT\n(", rate_lab, ")")) +
    coord_quickmap(xlim = xl, ylim = yl) +
    labs(x = NULL, y = NULL, title = var_lab[[v]]) +
    theme(plot.title = element_text(size = 9))
}
save_fig(map_chg_plot("BIO5") + map_chg_plot("BIO6") +
           plot_annotation(caption = paste("Change of the weighted floristic temperature per plot,",
                                           "mean of the draws; red: warmer, blue: colder community")),
         "fig10_map_dFT", 26, 12)

# B. Mean rate over all plots (as Fig. 2 in De Frenne et al. 2013): per draw the
# mean over the plots, then the mean and 95% interval over the draws. The
# interval covers the niche uncertainty only, not the variation among plots or
# datasets (that needs a model with dataset as random effect).
overall <- ft_change_boot %>%
  group_by(boot) %>%
  summarise(across(matches("^d(FT|LT|RT)_BIO[56]"), mean), .groups = "drop") %>%
  pivot_longer(-boot, names_to = c("index", "variable", "w"),
               names_pattern = "^d(FT|LT|RT)_(BIO[56])_(w_)?dec$") %>%
  group_by(index, variable, weighted = coalesce(w, "") == "w_") %>%
  summarise(mean = mean(value), lo = quantile(value, 0.025), hi = quantile(value, 0.975),
            .groups = "drop") %>%
  mutate(index = factor(index, levels = c("LT", "FT", "RT")),
         weighting = ifelse(weighted, "weighted (1 / niche width)", "unweighted"))
print(overall)
p <- ggplot(overall, aes(index, mean, ymin = lo, ymax = hi, colour = index, shape = weighting)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_pointrange(size = 0.3, position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = index_col, guide = "none") +
  scale_shape_manual(values = c("weighted (1 / niche width)" = 16, unweighted = 1)) +
  facet_wrap(~variable, scales = "free_y", labeller = as_labeller(var_lab)) +
  labs(x = NULL, y = paste0("Mean change over ", n_distinct(ft_change_boot$plotID),
                            " plots (", rate_lab, ")"), shape = NULL,
       caption = paste("Point: mean of the draws; line: 95% interval over the draws",
                       "(niche uncertainty only)")) +
  theme(legend.position = "bottom")
save_fig(p, "fig11_overall_rates_LT_FT_RT", 18, 10)

# C. Distribution of the plot rates (weighted). Labels: mean over the plots and
# share of plots whose 95% interval over the draws lies above or below 0
share <- chg_w %>%
  group_by(index, variable) %>%
  summarise(lab = sprintf("mean %+.3f\ninterval > 0: %.0f%%\ninterval < 0: %.0f%%",
                          mean(mean), 100 * mean(lo > 0), 100 * mean(hi < 0)),
            .groups = "drop")
print(share)
p <- ggplot(chg_w, aes(mean, fill = index)) +
  geom_histogram(bins = 60) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey30") +
  geom_text(data = share, aes(Inf, Inf, label = lab), inherit.aes = FALSE,
            hjust = 1.05, vjust = 1.2, size = 2.6) +
  scale_fill_manual(values = index_col, guide = "none") +
  facet_grid(index ~ variable, scales = "free") +
  labs(x = paste0("Change per plot, mean of the draws (", rate_lab, ")"), y = "Number of plots",
       caption = "Weighted by 1 / niche width")
save_fig(p, "fig12_rate_distribution", 20, 16)

# D. Mean rate per dataset (weighted FT), ordered by latitude: per draw the mean
# over the plots of the dataset, then the mean and 95% interval over the draws.
# Grey points: the single plots. Label: dataset (number of plots).
ds_lat <- coords %>%
  mutate(dataset = dataset_of(plotID)) %>%
  group_by(dataset) %>%
  summarise(lat = mean(latitude, na.rm = TRUE), .groups = "drop")
ds <- ft_change_boot %>%
  mutate(dataset = dataset_of(plotID)) %>%
  group_by(dataset, boot) %>%
  summarise(n_plots = n(), across(c(dFT_BIO5_w_dec, dFT_BIO6_w_dec), mean), .groups = "drop") %>%
  pivot_longer(starts_with("dFT"), names_to = "variable", names_pattern = "^dFT_(BIO[56])_w_dec$") %>%
  group_by(dataset, variable, n_plots) %>%
  summarise(mean = mean(value), lo = quantile(value, 0.025), hi = quantile(value, 0.975),
            .groups = "drop") %>%
  left_join(ds_lat, by = "dataset") %>%
  mutate(label = reorder(paste0(dataset, " (", n_plots, ")"), lat))
ds_plots <- chg_w %>%
  filter(index == "FT") %>%
  mutate(dataset = dataset_of(plotID)) %>%
  inner_join(distinct(ds, dataset, label), by = "dataset")
p <- ggplot(ds, aes(mean, label)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_jitter(data = ds_plots, colour = "grey70", size = 0.3, alpha = 0.4, height = 0.2, width = 0) +
  geom_pointrange(aes(xmin = lo, xmax = hi), colour = index_col[["FT"]], size = 0.2) +
  facet_wrap(~variable, scales = "free_x", labeller = as_labeller(var_lab)) +
  labs(x = paste0("Change in FT (", rate_lab, ")"), y = "Dataset (number of plots), south to north",
       caption = paste("Point and line: mean over the plots of a dataset and 95% interval over",
                       "the draws; grey: single plots (mean of the draws)")) +
  theme(axis.text.y = element_text(size = 6))
save_fig(p, "fig13_rate_per_dataset", 22, 24)
