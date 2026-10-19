library(terra)
library(dplyr)
library(tidyr)

####################################################################################
#   Species thermal niches (BIO5, BIO6) from the continuous (cloglog) SDM maps,
#   and community-inferred temperature (CIT).
#
#   Threshold-free sensitivity check of 07_SpeciesThermalNiche_BinaryRange_CIT.R:
#     - 07: random cells inside the p10 binary range, each cell weight 1.
#     - Here: random cells over the whole map, each cell weighted by its
#       suitability -> climate distribution of the cells the model expects to be
#       occupied. Unlike the response curve (08), this multiplies the maxent
#       ratio f_occ / f_bg by what is available in the landscape, so it does not
#       depend on the background design (thickening radius) in the same way.
#   Weighting instead of sampling with method = "weights" avoids reading the full
#   25 m map. Caveat: low suitability over a large area can carry much of the
#   weight (cloglog rarely reaches 0) -> check w_low10 in the output.
####################################################################################

# ---- Paths (adjust) ----
suit_dir     <- "N:/SDMs/SDMs_current/Results/OriginalSDM_CurrentLogisticMaps_aligned/" # merged EU continuous maps
suit_pattern <- "\\.tif$"
# prefixes written by the SDM script: logistic_<sp>.tif / Average_<n>_mdls_<sp>.tif
suit_prefix  <- "^(logistic_|Average_[0-9]+_mdls_)"
clim_files   <- c(
  BIO5 = "N:/SDMs/SDMs_current/Predictors/Micro_BIO5_EU_CHELSAbased_2000-2020.tif",
  BIO6 = "N:/SDMs/SDMs_current/Predictors/Micro_BIO6_EU_CHELSAbased_2000-2020.tif"
)
out_dir <- "N:/SDMs/SDMs_current/ThermalNiche/"
n_samp  <- 2e5 # random cells per species (weighted, so the effective size is lower: see ess)

clim <- rast(clim_files)
names(clim) <- names(clim_files)

####################################################################################
########################   1. Thermal niche per species  ###########################
####################################################################################
wquantile <- function(x, w, p) {
  o <- order(x)
  cw <- cumsum(w[o]) / sum(w)
  sapply(p, function(pp) x[o][which(cw >= pp)[1]])
}

wmode_dens <- function(x, w) {
  d <- density(x, weights = w / sum(w), bw = bw.nrd0(x), n = 2048)
  d$x[which.max(d$y)]
}

sample_suitability <- function(suit_file, species_name, clim, n_samp) {
  suit <- rast(suit_file)
  set.seed(16)
  t0 <- Sys.time()
  s <- spatSample(suit, size = n_samp, method = "random", na.rm = TRUE,
                  xy = TRUE, cells = TRUE)
  names(s)[ncol(s)] <- "suit"
  cat(sprintf("  sampling: %s cells drawn, %.1f min\n",
              format(nrow(s), big.mark = ","),
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))

  # Extract in file order, as in 07
  s <- s[order(s$cell), ]
  t0 <- Sys.time()
  env <- terra::extract(clim, s[, c("x", "y")], ID = FALSE)
  cat(sprintf("  climate extraction: %.1f min\n",
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  out <- na.omit(cbind(species = species_name, s[, c("x", "y", "suit")], env))
  # Maps were saved as cloglog * 100 -> back to 0-1
  if (max(out$suit) > 1) out$suit <- out$suit / 100
  out
}

niche_one_species <- function(suit_file, clim, n_samp) {
  species_name <- sub(suit_prefix, "", tools::file_path_sans_ext(basename(suit_file)))
  sample_file <- file.path(out_dir, "samples_weighted", paste0(species_name, ".rds"))

  # Resume, as in 07
  if (file.exists(sample_file)) {
    cat("  samples found, sampling skipped\n")
    samples <- readRDS(sample_file)
  } else {
    samples <- sample_suitability(suit_file, species_name, clim, n_samp)
    saveRDS(samples, sample_file)
  }

  w <- samples$suit
  stats <- lapply(names(clim), function(v) {
    x <- samples[[v]]
    q <- wquantile(x, w, c(0.05, 0.5, 0.95))
    data.frame(
      species = species_name,
      variable = v,
      Q05 = q[1],
      median = q[2],
      mean = weighted.mean(x, w),
      optimum = wmode_dens(x, w),
      Q95 = q[3],
      width = q[3] - q[1],
      n_sampled = length(x),
      # effective sample size of the weighted sample
      ess = sum(w)^2 / sum(w^2),
      # share of the weight from cells with suitability < 0.1: high values mean
      # the low-suitability background drives the niche estimate
      w_low10 = sum(w[w < 0.1]) / sum(w)
    )
  })
  do.call(rbind, stats)
}

dir.create(file.path(out_dir, "samples_weighted"), recursive = TRUE, showWarnings = FALSE)
suit_files <- list.files(suit_dir, pattern = suit_pattern, full.names = TRUE)

# ---- Test run on a few species first ----
test_run     <- TRUE
test_species <- c("Paris quadrifolia", "Anemone ranunculoides")
n_test       <- 3

if (test_run) {
  if (length(test_species) > 0) {
    suit_files <- suit_files[grepl(paste(test_species, collapse = "|"), basename(suit_files))]
  } else {
    suit_files <- head(suit_files, n_test)
  }
}
print(basename(suit_files))

niches_w <- do.call(rbind, lapply(suit_files, function(f) {
  cat("Thermal niche (suitability-weighted):", basename(f), "\n")
  print(system.time(res <- niche_one_species(f, clim, n_samp)))
  res
}))
rownames(niches_w) <- NULL
suffix <- if (test_run) "_TEST" else ""
write.csv(niches_w,
  file.path(out_dir, paste0("Species_ThermalNiche_SuitWeighted_BIO5_BIO6", suffix, ".csv")),
  row.names = FALSE
)

# Species whose niche is driven by low-suitability cells or with a small
# effective sample -> interpret with care
niches_w %>% filter(w_low10 > 0.25 | ess < 1000)

# Test check: weighted (this script) vs binary-range (07) densities
if (test_run) {
  for (f in suit_files) {
    sp <- sub(suit_prefix, "", tools::file_path_sans_ext(basename(f)))
    s_w <- readRDS(file.path(out_dir, "samples_weighted", paste0(sp, ".rds")))
    f_b <- file.path(out_dir, "samples", paste0(sp, ".rds"))
    par(mfrow = c(1, 2))
    for (v in names(clim)) {
      d_w <- density(s_w[[v]], weights = s_w$suit / sum(s_w$suit), bw = bw.nrd0(s_w[[v]]))
      if (file.exists(f_b)) {
        d_b <- density(readRDS(f_b)[[v]])
        plot(d_b, main = paste(sp, v), xlab = paste(v, "(°C)"),
             ylim = range(0, d_b$y, d_w$y))
        lines(d_w, col = "firebrick")
        legend("topleft", c("binary range (07)", "suit-weighted"),
               col = c("black", "firebrick"), lty = 1, bty = "n")
      } else {
        plot(d_w, main = paste(sp, v), xlab = paste(v, "(°C)"), col = "firebrick")
      }
    }
  }
  par(mfrow = c(1, 1))
}

####################################################################################
########################   2. Community-inferred temperature  ######################
####################################################################################
niches_w <- read.csv(file.path(out_dir, "Species_ThermalNiche_SuitWeighted_BIO5_BIO6.csv"))

# Same species value as in 07
niche_value <- "median"

norm_name <- function(x) tolower(trimws(gsub("[_ ]+", " ", x)))

# forestREplot name -> SDM name, as in 07 (from 06.1_SpeciesNameMatch_SDM_forestREplot.R)
sp_match <- read.csv("I:/DATA/output/CIT/SpeciesMatch_SDM_forestREplot.csv")
sp_match <- sp_match[sp_match$used_in_cit & sp_match$match_type != "exact", ]
syn_fr2sdm <- setNames(sp_match$sdm_species, sp_match$fr_species)
fr_to_sdm <- function(x) ifelse(x %in% names(syn_fr2sdm), unname(syn_fr2sdm[x]), x)

niche_wide <- niches_w %>%
  mutate(species_key = norm_name(species)) %>%
  dplyr::select(species_key, variable, value = all_of(niche_value), width) %>%
  pivot_wider(names_from = variable, values_from = c(value, width))

# Vegetation data, layers and 9999 values handled as in 07: herb (H, SH) and
# shrub (S) layer; 9999 = not surveyed / not monitored -> no presence, kept in
# not_monitored for CIT changes between surveys (no pseudo-turnover)
# Duplicated datasets (same plots in two versions), keep one of each:
#   - EU_010a = EU_010b plus the 1971 and 1993 surveys, in which only selected
#     species were recorded; the providers advise EU_010b for community analyses
#   - EU_009b repeats the 1976-2013 surveys of EU_009a with other cover values
#     (same species lists); EU_009a has the longer series (from 1935)
drop_datasets <- c("EU_009b", "EU_010a")
dataset_of <- function(sample) sub("^(EU_[0-9]+[a-z]?)_.*", "\\1", sample)
load("I:/DATA/input/forestREplot/version3.1/veg.data_forestREplot_V3.1.RData") # veg_data
veg <- veg_data %>%
  filter(grepl("^EU_", sample), !dataset_of(sample) %in% drop_datasets,
         layer %in% c("H", "SH", "S")) %>%
  mutate(
    species_name = fr_to_sdm(species_name),
    species_key = norm_name(species_name)
  )
# Plot-surveys not surveyed at all: every row 9999 (all layers)
not_surveyed <- veg_data %>%
  filter(grepl("^EU_", sample), !dataset_of(sample) %in% drop_datasets) %>%
  group_by(sample) %>%
  summarise(all_9999 = all(abundance == 9999), .groups = "drop") %>%
  filter(all_9999) %>%
  pull(sample)
# Species not monitored in a surveyed plot-survey (checked 2026-10-19: always the
# whole survey, i.e. all plots of that survey, as documented by the providers;
# a species that was monitored but not found has no row at all, so true
# colonisations are not affected)
not_monitored <- veg %>%
  filter(abundance == 9999, !sample %in% not_surveyed,
         species_key %in% niche_wide$species_key) %>%
  distinct(sample, species_name, species_key) %>%
  # safety: never a species with a real record in the same plot-survey
  anti_join(veg %>% filter(abundance != 9999) %>% distinct(sample, species_key),
            by = c("sample", "species_key")) %>%
  mutate(plotID = sub("_[^_]+$", "", sample), surveyTyp = sub("^.*_", "", sample))
plot_species <- veg %>%
  filter(abundance != 9999) %>%
  group_by(sample, species_name, species_key) %>%
  summarise(herb = any(layer %in% c("H", "SH")), .groups = "drop") %>%
  inner_join(niche_wide, by = "species_key")

cit_from <- function(d) {
  d %>%
    group_by(sample) %>%
    summarise(
      n_species = n(),
      CIT_BIO5 = mean(value_BIO5),
      CIT_BIO6 = mean(value_BIO6),
      CIT_BIO5_w = weighted.mean(value_BIO5, 1 / width_BIO5),
      CIT_BIO6_w = weighted.mean(value_BIO6, 1 / width_BIO6),
      .groups = "drop"
    )
}

# Same filter as 07
min_species <- 3

# Herb + shrub layer; herb layer only as sensitivity check (*_herb), as in 07
cit_w <- cit_from(plot_species) %>%
  left_join(cit_from(filter(plot_species, herb)) %>%
              rename_with(~ paste0(.x, "_herb"), -sample), by = "sample") %>%
  filter(n_species >= min_species) %>%
  mutate(
    across(starts_with("CIT") & ends_with("_herb"),
           ~ ifelse(!is.na(n_species_herb) & n_species_herb >= min_species, .x, NA)),
    plotID = sub("_[^_]+$", "", sample),
    surveyTyp = sub("^.*_", "", sample)
  )

save(cit_w, niches_w, plot_species, not_monitored, not_surveyed,
     file = "I:/DATA/output/CIT/CIT_SDMsuitweighted_BIO5_BIO6.RData")

####################################################################################
################   3. Bootstrapped floristic temperature (De Frenne 2013)  ##########
####################################################################################
# As in 07, but cells are drawn with probability proportional to suitability.
# BIO5 and BIO6 stay paired (one cell per species and draw).
sample_dir <- file.path(out_dir, "samples_weighted")
n_boot <- 500

set.seed(16)
sp_files <- list.files(sample_dir, pattern = "\\.rds$", full.names = TRUE)
sp_draws <- lapply(sp_files, function(f) {
  s <- readRDS(f)
  s[sample.int(nrow(s), n_boot, replace = TRUE, prob = s$suit), c("BIO5", "BIO6")]
})
sp_keys <- norm_name(tools::file_path_sans_ext(basename(sp_files)))
draws <- lapply(c(BIO5 = "BIO5", BIO6 = "BIO6"), function(v) {
  m <- t(sapply(sp_draws, function(d) d[[v]]))
  rownames(m) <- sp_keys
  m
})

plot_idx <- split(
  match(plot_species$species_key, sp_keys),
  plot_species$sample
)
plot_idx <- lapply(plot_idx, function(i) i[!is.na(i)])
plot_idx <- plot_idx[lengths(plot_idx) >= min_species]

ft_boot_w <- do.call(rbind, lapply(names(plot_idx), function(s) {
  i <- plot_idx[[s]]
  out <- data.frame(sample = s, boot = seq_len(n_boot), n_species = length(i))
  for (v in names(draws)) {
    m <- draws[[v]][i, , drop = FALSE]
    out[[paste0("FT_", v)]] <- colMeans(m)
    out[[paste0("LT_", v)]] <- apply(m, 2, quantile, probs = 0.05)
    out[[paste0("RT_", v)]] <- apply(m, 2, quantile, probs = 0.95)
  }
  out
}))
ft_boot_w <- ft_boot_w %>%
  mutate(
    plotID = sub("_[^_]+$", "", sample),
    surveyTyp = sub("^.*_", "", sample)
  )

ft_summary_w <- ft_boot_w %>%
  group_by(sample, plotID, surveyTyp, n_species) %>%
  summarise(across(starts_with(c("FT_", "LT_", "RT_")),
    list(mean = mean, lo = ~ quantile(.x, 0.025), hi = ~ quantile(.x, 0.975))
  ), .groups = "drop")

save(ft_boot_w, ft_summary_w, file = "I:/DATA/output/CIT/FloristicTemp_boot_SDMsuitweighted_BIO5_BIO6.RData")
