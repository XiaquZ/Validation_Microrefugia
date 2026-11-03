library(dplyr)
library(tidyr)
library(ggplot2)

####################################################################################
#   Compare species thermal niches (BIO5, BIO6) from
#     - 08 (main): realised response curve from raw maxnet output at the
#       thickened background ("_RawBG" outputs)
#     - 07: microclimate sampled inside the binary (p10) SDM range
#   1. Curves per species: 07 = density of the sampled temperatures (same kernel
#      SD as in 08), 08 = relative occurrence rate; both rescaled to max = 1.
#   2. Niche statistics (Q05, median, mean, optimum, Q95, width): 07 vs 08 with
#      1:1 line, plus correlation and mean difference per statistic.
#   The two curves answer different questions: 07 is the temperature distribution
#   of the range area, so temperatures that cover much land weigh more; 08 is the
#   occurrence rate relative to the background, so that availability is divided
#   out. Some shift between them is expected.
#   Reads existing outputs only; plots go to the screen unless pdf_file is set.
####################################################################################

# ---- Paths (adjust) ----
dir_08    <- "I:/DATA/SDM_current/ThermalNiche/"  # outputs of 08
dir_07    <- "I:/DATA/SDM_current/ThermalNiche_07/" # outputs of 07 (incl. samples/), copied from N:
suffix_08 <- "_RawBG_TEST"                          # "_RawBG" after the full 08 run
fig_dir   <- file.path(dir_08, "Figures_Compare_07_08") # PNG output; NULL = screen only
bw        <- 0.5  # kernel SD (°C), as in 08
sp_page   <- 4    # species per figure in the curve plots
cols      <- c("07 binary range" = "#E07B39", "08 response curve" = "#2C7BB6")

norm_name <- function(x) tolower(trimws(gsub("[_ ]+", " ", x)))

niche_08 <- read.csv(file.path(dir_08, paste0("Species_ThermalNiche_ResponseCurve_BIO5_BIO6", suffix_08, ".csv")))
curves_08 <- readRDS(file.path(dir_08, paste0("Species_ResponseCurves_BIO5_BIO6", suffix_08, ".rds")))
niche_07 <- read.csv(file.path(dir_07, "Species_ThermalNiche_BIO5_BIO6.csv"))

# Species in both
keys_08 <- unique(norm_name(niche_08$species))
keys_07 <- unique(norm_name(niche_07$species))
keys <- intersect(keys_08, keys_07)
cat(length(keys), "species in both;",
    length(setdiff(keys_08, keys)), "only in 08;",
    length(setdiff(keys_07, keys)), "only in 07\n")

####################################################################################
############################   1. Curves per species  ##############################
####################################################################################
# 07 curve: kernel density of the range samples on the 08 grid, max = 1
sample_files <- list.files(file.path(dir_07, "samples"), pattern = "\\.rds$", full.names = TRUE)
sample_files <- sample_files[norm_name(tools::file_path_sans_ext(basename(sample_files))) %in% keys]

curves_07 <- do.call(rbind, lapply(sample_files, function(f) {
  s <- readRDS(f)
  key <- norm_name(tools::file_path_sans_ext(basename(f)))
  do.call(rbind, lapply(c("BIO5", "BIO6"), function(v) {
    g <- curves_08 %>% filter(norm_name(species) == key, variable == v)
    d <- density(s[[v]], bw = bw, from = min(g$x), to = max(g$x), n = nrow(g))
    data.frame(species_key = key, variable = v, x = d$x,
               value = d$y / max(d$y), method = "07 binary range")
  }))
}))

curves_cmp <- curves_08 %>%
  mutate(species_key = norm_name(species)) %>%
  filter(species_key %in% keys, !is.na(rate)) %>%
  transmute(species_key, variable, x, value = rate, method = "08 response curve") %>%
  bind_rows(curves_07)

if (!is.null(fig_dir)) dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Print to screen and, if fig_dir is set, save as PNG
save_fig <- function(p, file, width, height) {
  print(p)
  if (!is.null(fig_dir)) ggsave(file.path(fig_dir, file), p, width = width, height = height, dpi = 300)
}

# Species names as in 08 (capitalised) for the panel labels
# (genus and epithet on two lines to keep the strips narrow)
sp_label <- setNames(sub(" ", "\n", unique(niche_08$species)), norm_name(unique(niche_08$species)))

# Median of each method (the species value used for CIT) as dashed lines
medians <- bind_rows(
  niche_07 %>% transmute(species_key = norm_name(species), variable, median, method = "07 binary range"),
  niche_08 %>% transmute(species_key = norm_name(species), variable, median, method = "08 response curve")
) %>%
  filter(species_key %in% keys)

pages <- split(sort(keys), ceiling(seq_along(keys) / sp_page))
for (i in seq_along(pages)) {
  p <- pages[[i]]
  fig <- ggplot(filter(curves_cmp, species_key %in% p), aes(x, value, colour = method)) +
    geom_line(linewidth = 0.8) +
    geom_vline(data = filter(medians, species_key %in% p),
               aes(xintercept = median, colour = method), linetype = 2, linewidth = 0.5,
               show.legend = FALSE) +
    facet_grid(species_key ~ variable, scales = "free_x",
               labeller = labeller(species_key = sp_label)) +
    scale_colour_manual(values = cols) +
    labs(x = "Microclimate temperature (°C)", y = "Relative value (max = 1)", colour = NULL,
         caption = "Dashed lines: median of each curve (species value used for CIT)") +
    theme_bw(base_size = 10) +
    theme(legend.position = "bottom",
          strip.text.y = element_text(face = "italic", angle = 0),
          panel.grid.minor = element_blank())
  save_fig(fig, sprintf("Compare_curves_07_08_%02d.png", i), width = 8, height = 1.2 + 2 * length(p))
}

####################################################################################
###########################   2. Niche statistics  #################################
####################################################################################
stat_names <- c("Q05", "median", "mean", "optimum", "Q95", "width")

stats_cmp <- inner_join(
  niche_07 %>% mutate(species_key = norm_name(species)) %>%
    select(species_key, variable, all_of(stat_names)) %>%
    pivot_longer(all_of(stat_names), names_to = "stat", values_to = "v07"),
  niche_08 %>% mutate(species_key = norm_name(species)) %>%
    select(species_key, variable, edge_opt, all_of(stat_names)) %>%
    pivot_longer(all_of(stat_names), names_to = "stat", values_to = "v08"),
  by = c("species_key", "variable", "stat")
) %>%
  mutate(stat = factor(stat, levels = stat_names))

# Correlation and mean difference (08 - 07) per statistic
stats_cmp %>%
  group_by(variable, stat) %>%
  summarise(
    n = n(),
    r = cor(v07, v08, use = "complete.obs"),
    mean_diff = mean(v08 - v07, na.rm = TRUE),
    sd_diff = sd(v08 - v07, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(n = Inf)

# Scatter with 1:1 line; optimum at the edge of the 08 curve marked
for (v in c("BIO5", "BIO6")) {
  fig <- ggplot(filter(stats_cmp, variable == v), aes(v07, v08, colour = edge_opt)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50") +
    geom_point(alpha = 0.6, size = 1.5) +
    facet_wrap(~stat, scales = "free") +
    scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "red"),
                        name = "08 optimum at curve edge") +
    labs(x = paste0("07 binary range: ", v, " (°C)"),
         y = paste0("08 response curve: ", v, " (°C)"), title = v) +
    theme_bw() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  save_fig(fig, paste0("Compare_stats_07_08_", v, ".png"), width = 9, height = 6.5)
}

# Largest disagreements in the species values used for CIT (median in both)
stats_cmp %>%
  select(-edge_opt) %>%
  pivot_wider(names_from = stat, values_from = c(v07, v08)) %>%
  mutate(diff_cit = v08_median - v07_median) %>%
  arrange(desc(abs(diff_cit))) %>%
  select(species_key, variable, v07_median, v08_median, diff_cit) %>%
  head(20)
