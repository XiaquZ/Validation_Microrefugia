library(terra)
library(dplyr)
library(tidyr)

####################################################################################
#   Species thermal niches (BIO5, BIO6) from the merged EU binary SDM maps, and
#   community-inferred temperature (CIT).
#
#   Same recipe as Haesen et al. (2023) and ClimPlant (Vangansbeke et al. 2021):
#   randomly sample cells inside the species' (predicted) range and summarise the
#   climate there. Differences:
#     - ClimPlant: expert atlas ranges, 1000 cells WITH replacement, WorldClim
#       macroclimate at 10 arc-min (range-map resolution limits the climate grain).
#     - Haesen: 1e6 pixels over the p10 suitable forest area; Q05, mode, Q95, width.
#     - Here: p10-thresholded SDM range at the same grain as the microclimate
#       predictors, so using microclimate BIO5/BIO6 at that grain is justified.
####################################################################################

# ---- Paths (adjust) ----
bin_dir     <- "N:/SDMs/SDMs_current/Results/OriginalSDM_CurrentBinaryMaps_aligned/" # merged EU binary maps
bin_pattern <- "\\.tif$" # e.g. "^binary_.*\\.tif$"
clim_files  <- c(
  BIO5 = "N:/SDMs/SDMs_current/Predictors/Micro_BIO5_EU_CHELSAbased_2000-2020.tif",
  BIO6 = "N:/SDMs/SDMs_current/Predictors/Micro_BIO6_EU_CHELSAbased_2000-2020.tif"
)
out_dir <- "N:/SDMs/SDMs_current/ThermalNiche/"
n_samp  <- 1e5 # suitable cells per species (ClimPlant 1e3, Haesen 1e6)
max_ram_gb <- 60 # RAM budget over all parallel processes (shared workstation)

clim <- rast(clim_files)
names(clim) <- names(clim_files)

####################################################################################
########################   1. Thermal niche per species  ###########################
####################################################################################
mode_dens <- function(x) {
  d <- density(x, n = 2048)
  d$x[which.max(d$y)]
}

niche_one_species <- function(bin_file, clim, n_samp) {
  species_name <- sub("^binary_", "", tools::file_path_sans_ext(basename(bin_file)))
  sample_file <- file.path(out_dir, "samples", paste0(species_name, ".rds"))

  # Resume: species with saved samples (earlier or interrupted run) skip the
  # sampling and only recompute the statistics. Delete the .rds files after
  # changing n_samp or the binary maps.
  if (file.exists(sample_file)) {
    cat("  samples found, sampling skipped\n")
    samples <- readRDS(sample_file)
  } else {
    samples <- sample_range(bin_file, species_name, clim, n_samp)
    # Keep the raw values (like ClimPlant's per-species value files) for density
    # plots and uncertainty propagation later. Written to a temporary file first,
    # so a process killed while saving leaves no half-written .rds for the resume.
    tmp_file <- paste0(sample_file, ".tmp")
    saveRDS(samples, tmp_file)
    file.rename(tmp_file, sample_file)
  }

  stats <- lapply(names(clim), function(v) {
    x <- samples[[v]]
    q <- quantile(x, c(0.05, 0.5, 0.95))
    data.frame(
      species = species_name,
      variable = v,
      Q05 = q[[1]],
      median = q[[2]],
      mean = mean(x),
      optimum = mode_dens(x),
      Q95 = q[[3]],
      width = q[[3]] - q[[1]],
      n_sampled = length(x)
    )
  })
  do.call(rbind, stats)
}

sample_range <- function(bin_file, species_name, clim, n_samp) {
  bin <- rast(bin_file)

  # Sample random non-NA cells of the binary map and keep the suitable ones (== 1).
  # No ifel()/classify() on the full map: at 25 m the EU grid has ~21 billion
  # cells, so rewriting it per species would take far longer than sampling.
  # If too few suitable cells come back (small range), draw again with a larger
  # sample until n_samp is reached or max_rounds is hit.
  # Sampling coordinates and extracting climate afterwards avoids requiring the
  # binary map and the climate rasters to be on exactly the same grid.
  set.seed(16)
  pts <- NULL
  draw_size <- 2 * n_samp
  max_rounds <- 5
  n_drawn <- 0
  for (round in seq_len(max_rounds)) {
    t0 <- Sys.time()
    s <- spatSample(bin, size = draw_size, method = "random", na.rm = TRUE,
                    xy = TRUE, cells = TRUE)
    n_drawn <- n_drawn + nrow(s)
    # spatSample can return fewer non-NA cells than requested (warning) -> report
    # the actual number
    cat(sprintf("  sampling round %d: %s cells drawn, ",
                round, format(nrow(s), big.mark = ",")))
    s <- s[s[[ncol(s)]] == 1, c("cell", "x", "y")]
    pts <- unique(rbind(pts, s)) # drop cells drawn twice
    cat(sprintf("%d suitable so far, %.1f min\n", nrow(pts),
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    if (nrow(pts) >= n_samp) break
    # size the next draw from the cells still missing and the observed share of
    # suitable cells (1.5 = margin for estimation error and duplicate cells).
    # Capped at 2e7 cells (~0.6 GB per draw) so that parallel processes do not
    # run out of RAM on species with tiny ranges; those may end below n_samp
    # (flagged by n_sampled < n_samp).
    share <- max(nrow(pts) / n_drawn, 1e-4)
    draw_size <- min(ceiling(1.5 * (n_samp - nrow(pts)) / share), 2e7)
  }
  # Random subset (not the first rows, which could follow the raster order)
  if (nrow(pts) > n_samp) pts <- pts[sample.int(nrow(pts), n_samp), ]

  # Extract in file order (row by row): cells sorted by cell number are read
  # sequentially instead of jumping around the large GeoTIFFs.
  pts <- pts[order(pts$cell), ]
  t0 <- Sys.time()
  env <- terra::extract(clim, pts[, c("x", "y")], ID = FALSE)
  cat(sprintf("  climate extraction: %.1f min\n",
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  na.omit(cbind(species = species_name, pts[, c("x", "y")], env))
}

dir.create(file.path(out_dir, "samples"), recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(file.path(out_dir, "samples"))) stop("Cannot create output folder: ", out_dir)
bin_files <- list.files(bin_dir, pattern = bin_pattern, full.names = TRUE)

# ---- Test run on a few species first ----
# TRUE: run only the species in test_species (or the first n_test files if
# test_species is empty) and write to a separate *_TEST.csv.
# FALSE: run all species.
test_run     <- FALSE
test_species <- c("binary_Paris quadrifolia","binary_Anemone ranunculoides") # e.g. c("Paris quadrifolia", "Anemone nemorosa")
n_test       <- 3

# Chunk mode (see below) always runs all species
args <- commandArgs(trailingOnly = TRUE)
chunk_mode <- length(args) == 2
if (chunk_mode) test_run <- FALSE

if (test_run) {
  if (length(test_species) > 0) {
    bin_files <- bin_files[grepl(paste(test_species, collapse = "|"), basename(bin_files))]
  } else {
    bin_files <- head(bin_files, n_test)
  }
}

# ---- Parallel runs: split the species over several R processes ----
# From a terminal, one process per chunk (here 3):
#   Rscript 07_SpeciesThermalNiche_BinaryRange_CIT.R 1 3 > log_chunk1.txt 2>&1
#   Rscript 07_SpeciesThermalNiche_BinaryRange_CIT.R 2 3 > log_chunk2.txt 2>&1
#   Rscript 07_SpeciesThermalNiche_BinaryRange_CIT.R 3 3 > log_chunk3.txt 2>&1
# Each process only writes samples/*.rds and stops after the sampling. Afterwards,
# run the script once without arguments: all samples are found (resume), so it
# only computes the statistics, writes the full CSV and continues with parts 2-3.
if (chunk_mode) {
  chunk <- as.integer(args[1])
  n_chunks <- as.integer(args[2])
  bin_files <- bin_files[seq_along(bin_files) %% n_chunks == chunk - 1]
  # Share the RAM budget between the processes. memmax limits terra only; the
  # sampled data frames add < 2 GB per process (draw cap 2e7 cells).
  terraOptions(memmax = max_ram_gb / n_chunks - 2)
  cat(sprintf("Chunk %d of %d: %d species\n", chunk, n_chunks, length(bin_files)))
}
print(basename(bin_files))

niches <- do.call(rbind, lapply(bin_files, function(f) {
  cat("Thermal niche:", basename(f), format(Sys.time(), "(%H:%M)"), "\n")
  # An error in one species (e.g. a corrupt map) is logged and skipped, so the
  # remaining species still run; rerunning the script retries it.
  tryCatch({
    print(system.time(res <- niche_one_species(f, clim, n_samp)))
    res
  }, error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n")
    NULL
  })
}))
if (chunk_mode) {
  cat("Chunk finished:", format(Sys.time()), "\n")
  quit(save = "no")
}
rownames(niches) <- NULL
niche_file <- file.path(
  out_dir,
  if (test_run) "Species_ThermalNiche_BIO5_BIO6_TEST.csv" else "Species_ThermalNiche_BIO5_BIO6.csv"
)
write.csv(niches, niche_file, row.names = FALSE)

# Species with a very small suitable area return fewer cells -> check before use
# (a few cells per species are always lost where the climate rasters are NA, e.g.
# along coasts, so only shortfalls > 1% are listed)
niches %>% filter(n_sampled < 0.99 * n_samp)
# Species that failed (see ERROR in the log) -> rerun the script to retry them
setdiff(sub("^binary_", "", tools::file_path_sans_ext(basename(bin_files))), niches$species)

# Test check: density of BIO5 / BIO6 over the suitable area (cf. Haesen Fig. 4),
# with Q05, median and Q95 as dashed lines.
if (test_run) {
  for (f in bin_files) {
    sp <- sub("^binary_", "", tools::file_path_sans_ext(basename(f)))
    s <- readRDS(file.path(out_dir, "samples", paste0(sp, ".rds")))
    par(mfrow = c(1, 2))
    for (v in names(clim)) {
      plot(density(s[[v]]), main = paste(sp, v), xlab = paste(v, "(°C)"))
      abline(v = quantile(s[[v]], c(0.05, 0.5, 0.95)), lty = 2, col = "grey40")
    }
  }
  par(mfrow = c(1, 1))
}

####################################################################################
########################   2. Community-inferred temperature  ######################
####################################################################################
niches <- read.csv(file.path(out_dir, "Species_ThermalNiche_BIO5_BIO6.csv"))

# Species "value" used for CIT: median is robust to the multimodal
# distributions seen in Haesen Fig. 4; swap for "optimum" to follow Haesen strictly.
niche_value <- "median"

norm_name <- function(x) tolower(trimws(gsub("[_ ]+", " ", x)))

# forestREplot name -> SDM name (synonyms and broader names), from
# 06.1_SpeciesNameMatch_SDM_forestREplot.R, which logs every match decision
sp_match <- read.csv("I:/DATA/output/CIT/SpeciesMatch_SDM_forestREplot.csv")
sp_match <- sp_match[sp_match$used_in_cit & sp_match$match_type != "exact", ]
syn_fr2sdm <- setNames(sp_match$sdm_species, sp_match$fr_species)
fr_to_sdm <- function(x) ifelse(x %in% names(syn_fr2sdm), unname(syn_fr2sdm[x]), x)

niche_wide <- niches %>%
  mutate(species_key = norm_name(species)) %>%
  dplyr::select(species_key, variable, value = all_of(niche_value), width) %>%
  pivot_wider(names_from = variable, values_from = c(value, width))

# Vegetation data of forestREplot v3.1, Europe (long format: sample, species_name,
# layer, abundance). Layers: herb (H), shrub and herb not separated (SH) and shrub
# (S); the tree layer (T) is excluded. Several modelled species are woody
# understorey species recorded as shrubs (e.g. Daphne, Lonicera).
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

# Abundance 9999 = not surveyed / not monitored (forestREplot data providers):
# the presence is unknown, so these rows are no presence records.
#   - all rows of a plot-survey 9999: plot not surveyed -> no CIT for that survey
#     (not_surveyed)
#   - single species 9999: species not monitored in that survey (e.g. EU_010a in
#     1971 and 1993, EU_040 vernal species in 2008). Kept in not_monitored: a CIT change
#     between two surveys must use only species monitored in both surveys,
#     otherwise an unmonitored species looks like a colonisation (pseudo-turnover).
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

# Presences of the modelled species per plot-survey; herb = recorded in H or SH
# (for the herb-only sensitivity check)
plot_species <- veg %>%
  filter(abundance != 9999) %>%
  group_by(sample, species_name, species_key) %>%
  summarise(herb = any(layer %in% c("H", "SH")), .groups = "drop") %>%
  inner_join(niche_wide, by = "species_key")

# Presence-absence CIT, unweighted and weighted by inverse niche width
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

# Only 140 forest specialists are modelled: drop plot-surveys with too few of them
min_species <- 3

# Main CIT: herb + shrub layer. Sensitivity: herb layer only (*_herb columns,
# NA where fewer than min_species herb-layer species)
cit <- cit_from(plot_species) %>%
  left_join(cit_from(filter(plot_species, herb)) %>%
              rename_with(~ paste0(.x, "_herb"), -sample), by = "sample") %>%
  filter(n_species >= min_species) %>%
  mutate(
    across(starts_with("CIT") & ends_with("_herb"),
           ~ ifelse(!is.na(n_species_herb) & n_species_herb >= min_species, .x, NA)),
    # sample = "EU_xxx_plot_survey": split back into plotID and survey type
    plotID = sub("_[^_]+$", "", sample),
    surveyTyp = sub("^.*_", "", sample)
  )

# Sensitivity check: herb + shrub vs herb layer only
cat("Plot-surveys with a CIT:", nrow(cit),
    "| with shrub-layer species added:", sum(cit$n_species > coalesce(cit$n_species_herb, 0L)), "\n")
cit %>%
  summarise(
    r_BIO5 = cor(CIT_BIO5, CIT_BIO5_herb, use = "complete.obs"),
    r_BIO6 = cor(CIT_BIO6, CIT_BIO6_herb, use = "complete.obs"),
    mean_diff_BIO5 = mean(CIT_BIO5 - CIT_BIO5_herb, na.rm = TRUE),
    mean_diff_BIO6 = mean(CIT_BIO6 - CIT_BIO6_herb, na.rm = TRUE),
    max_abs_diff_BIO5 = max(abs(CIT_BIO5 - CIT_BIO5_herb), na.rm = TRUE),
    max_abs_diff_BIO6 = max(abs(CIT_BIO6 - CIT_BIO6_herb), na.rm = TRUE)
  )

# plot_species and not_monitored are needed to compute CIT changes between two
# surveys from the species monitored in both
save(cit, niches, plot_species, not_monitored, not_surveyed,
     file = "I:/DATA/output/CIT/CIT_SDMniche_BIO5_BIO6.RData")

####################################################################################
################   3. Bootstrapped floristic temperature (De Frenne 2013)  ##########
####################################################################################
# De Frenne et al. (2013, PNAS): instead of one value per species, draw one
# temperature value per species from its thermal distribution, average over the
# species in the plot (floristic temperature, FT) and take the 5th / 95th
# percentiles (cold tail LT, warm tail RT); repeat 500 times. This propagates niche
# width and uncertainty.
# As in the CIT of part 2, each statistic is also computed with the species
# weighted by inverse niche width (*_w: weighted mean and weighted 5th / 95th
# percentiles), so that narrow-niche species carry more information. The draws
# already reflect niche width, so the weighting adds to that; the unweighted
# values follow De Frenne et al. exactly.
# Draws come from the suitable-area samples saved in part 1 (the SDM-based
# equivalent of their response curves). One cell is drawn per species and draw,
# so BIO5 and BIO6 stay paired at the same location.
sample_dir <- file.path(out_dir, "samples")
n_boot <- 500

set.seed(16)
sp_files <- list.files(sample_dir, pattern = "\\.rds$", full.names = TRUE)
sp_draws <- lapply(sp_files, function(f) {
  s <- readRDS(f)
  s[sample.int(nrow(s), n_boot, replace = TRUE), c("BIO5", "BIO6")]
})
sp_keys <- norm_name(tools::file_path_sans_ext(basename(sp_files)))
# species x bootstrap matrices
draws <- lapply(c(BIO5 = "BIO5", BIO6 = "BIO6"), function(v) {
  m <- t(sapply(sp_draws, function(d) d[[v]]))
  rownames(m) <- sp_keys
  m
})

# Rows of the draw matrices for the species present in each plot-survey
plot_idx <- split(
  match(plot_species$species_key, sp_keys),
  plot_species$sample
)
plot_idx <- lapply(plot_idx, function(i) i[!is.na(i)])
plot_idx <- plot_idx[lengths(plot_idx) >= min_species]

# Inverse niche width per species (same rows as the draw matrices)
inv_width <- lapply(c(BIO5 = "BIO5", BIO6 = "BIO6"), function(v) {
  1 / niche_wide[[paste0("width_", v)]][match(sp_keys, niche_wide$species_key)]
})

# Weighted quantile: smallest value at which the cumulative weight reaches p
wquantile <- function(x, w, p) {
  o <- order(x)
  x[o][which(cumsum(w[o]) / sum(w) >= p)[1]]
}

# FT, LT and RT per bootstrap draw for the species in rows i of the draw
# matrices, unweighted and weighted by inverse niche width (also used in part 4)
ft_stats <- function(i) {
  out <- data.frame(boot = seq_len(n_boot))
  for (v in names(draws)) {
    m <- draws[[v]][i, , drop = FALSE]
    w <- inv_width[[v]][i]
    out[[paste0("FT_", v)]] <- colMeans(m)
    out[[paste0("LT_", v)]] <- apply(m, 2, quantile, probs = 0.05)
    out[[paste0("RT_", v)]] <- apply(m, 2, quantile, probs = 0.95)
    out[[paste0("FT_", v, "_w")]] <- colSums(m * w) / sum(w) # w recycled per species (row)
    out[[paste0("LT_", v, "_w")]] <- apply(m, 2, wquantile, w = w, p = 0.05)
    out[[paste0("RT_", v, "_w")]] <- apply(m, 2, wquantile, w = w, p = 0.95)
  }
  out
}

# Long table: one row per plot-survey x bootstrap sample
ft_boot <- bind_rows(lapply(names(plot_idx), function(s) {
  data.frame(sample = s, n_species = length(plot_idx[[s]]), ft_stats(plot_idx[[s]]))
}))
ft_boot <- ft_boot %>%
  mutate(
    plotID = sub("_[^_]+$", "", sample),
    surveyTyp = sub("^.*_", "", sample)
  )

# Per plot-survey summary over the 500 draws (for maps / quick checks). The
# change over time is computed per draw in part 4, as in De Frenne et al.
ft_summary <- ft_boot %>%
  group_by(sample, plotID, surveyTyp, n_species) %>%
  summarise(across(starts_with(c("FT_", "LT_", "RT_")),
    list(mean = mean, lo = ~ quantile(.x, 0.025), hi = ~ quantile(.x, 0.975))
  ), .groups = "drop")

save(ft_boot, ft_summary, file = "I:/DATA/output/CIT/FloristicTemp_boot_SDMniche_BIO5_BIO6.RData")

####################################################################################
#####################   4. Temporal change: delta CIT per year  ####################
####################################################################################
# One interval per plot: from the baseline to the most recent resurvey, as in De
# Frenne et al. (2013). Only species monitored in both surveys are used
# (not_monitored), so that an unmonitored species does not look like a
# colonisation or a loss (pseudo-turnover); species really gained or lost are
# kept. If one of the two surveys then has fewer than min_species modelled
# species, the next survey is used as baseline (or the previous one as end): the
# longest interval with enough species in both surveys is chosen.
# Needs part 2 (plot_species, not_monitored, cit_from) and part 3 (ft_stats).
#
# Counts (2026-10-19; they depend on the species lists only, not on the niche
# values; EU_009b and EU_010a excluded, min_species = 3):
#   - 3,059 plots get a delta CIT, of 3,770 plots with a CIT (the others have
#     only one survey with enough species)
#   - interval length: median 33 years, range 3-78
#   - baseline moved to a later survey in 45 plots, end moved to an earlier
#     survey in 64 plots, because of too few species
#   - EU_040: interval 1988 -> 2015; the 2008 resurvey, in which the vernal
#     species were not monitored, is skipped
#
# FT / LT / RT (part 3, De Frenne et al. 2013), per bootstrap draw over the
# species of a plot-survey: FT = floristic temperature (mean), LT = left (cold)
# tail (5th percentile), RT = right (warm) tail (95th percentile).

# Survey years per plot-survey. A few entries are not plain years: "1956-57",
# "1955 and 57", "1998/99" and "2019-2020" -> mid-point; "1963?" -> 1963;
# "1976 (T, S 1975)" -> 1976 (year of the herb layer survey)
parse_year <- function(x) {
  x <- sub("\\(.*\\)", "", as.character(x))
  vapply(regmatches(x, gregexpr("[0-9]{2,4}", x)), function(y) {
    if (!length(y)) return(NA_real_)
    y <- as.numeric(y)
    y[y < 100] <- floor(y[1] / 100) * 100 + y[y < 100] # "1956-57" -> 1956, 1957
    mean(y)
  }, numeric(1))
}
load("I:/DATA/input/forestREplot/version3.1/plot.data_forestREplot_V3.1.RData") # plot_data
survey_year <- plot_data %>%
  filter(grepl("^EU_", plotID)) %>%
  dplyr::select(plotID, B = year_baseline_survey, R1 = year_resurvey_R1,
                R2 = year_resurvey_R2, R3 = year_resurvey_R3, R4 = year_resurvey_R4,
                R5 = year_resurvey_R5) %>%
  mutate(across(-plotID, as.character)) %>%
  pivot_longer(-plotID, names_to = "surveyTyp", values_to = "year_raw",
               values_drop_na = TRUE) %>%
  mutate(year = parse_year(year_raw), sample = paste(plotID, surveyTyp, sep = "_"))

# All candidate pairs (earlier survey b, later survey e) per plot
pres <- distinct(plot_species, sample, species_key, herb)
nm <- distinct(not_monitored, sample, species_key)
surveys <- pres %>%
  distinct(sample) %>%
  inner_join(survey_year, by = "sample") %>%
  dplyr::select(plotID, sample, year)
pairs <- inner_join(surveys, surveys, by = "plotID", suffix = c("_b", "_e"),
                    relationship = "many-to-many") %>%
  filter(year_e > year_b)

# Species of each pair: present in a survey and monitored in the other survey
pair_species <- bind_rows(
  pairs %>%
    inner_join(pres, by = c("sample_b" = "sample"), relationship = "many-to-many") %>%
    anti_join(nm, by = c("sample_e" = "sample", "species_key")) %>%
    mutate(survey = "b", sample = sample_b),
  pairs %>%
    inner_join(pres, by = c("sample_e" = "sample"), relationship = "many-to-many") %>%
    anti_join(nm, by = c("sample_b" = "sample", "species_key")) %>%
    mutate(survey = "e", sample = sample_e)
)

# Longest interval with >= min_species species in both surveys (ties: earliest
# baseline)
chosen <- pair_species %>%
  count(plotID, sample_b, sample_e, year_b, year_e, survey) %>%
  pivot_wider(names_from = survey, values_from = n, values_fill = 0) %>%
  filter(b >= min_species, e >= min_species) %>%
  group_by(plotID) %>%
  arrange(desc(year_e - year_b), year_b, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(plotID, sample_b, sample_e, year_b, year_e, n_years = year_e - year_b)

# CIT of both surveys from the species of the chosen pair (as in part 2:
# unweighted, weighted by inverse niche width, and herb layer only)
chg_species <- pair_species %>%
  semi_join(chosen, by = c("plotID", "sample_b", "sample_e")) %>%
  inner_join(niche_wide, by = "species_key")
cit_pair <- cit_from(chg_species) %>%
  left_join(cit_from(filter(chg_species, herb)) %>%
              rename_with(~ paste0(.x, "_herb"), -sample), by = "sample") %>%
  mutate(across(starts_with("CIT") & ends_with("_herb"),
                ~ ifelse(!is.na(n_species_herb) & n_species_herb >= min_species, .x, NA)))

cit_change <- chosen %>%
  left_join(rename_with(cit_pair, ~ paste0(.x, "_b"), -sample), by = c("sample_b" = "sample")) %>%
  left_join(rename_with(cit_pair, ~ paste0(.x, "_e"), -sample), by = c("sample_e" = "sample"))
for (v in grep("^CIT_", names(cit_pair), value = TRUE)) {
  cit_change[[paste0("d", v, "_yr")]] <-
    (cit_change[[paste0(v, "_e")]] - cit_change[[paste0(v, "_b")]]) / cit_change$n_years
}

# De Frenne values: FT, LT, RT per draw from the same species, change per year
# per draw, then the mean and 95% interval over the draws
chg_idx <- split(match(chg_species$species_key, sp_keys), chg_species$sample)
chg_idx <- lapply(chg_idx, function(i) i[!is.na(i)])
ft_change_boot <- bind_rows(lapply(seq_len(nrow(chosen)), function(k) {
  p <- chosen[k, ]
  sb <- ft_stats(chg_idx[[p$sample_b]])
  se <- ft_stats(chg_idx[[p$sample_e]])
  st <- setdiff(names(sb), "boot")
  d <- (se[st] - sb[st]) / p$n_years
  names(d) <- paste0("d", st, "_yr")
  data.frame(plotID = p$plotID, boot = sb$boot, d)
}))
ft_change_summary <- ft_change_boot %>%
  group_by(plotID) %>%
  summarise(across(starts_with("d"),
    list(mean = mean, lo = ~ quantile(.x, 0.025), hi = ~ quantile(.x, 0.975))
  ), .groups = "drop") %>%
  left_join(chosen, by = "plotID")

# Checks: plots with a change, interval lengths, plots where the baseline or the
# end survey had to be moved
first_last <- surveys %>%
  group_by(plotID) %>%
  summarise(first = sample[which.min(year)], last = sample[which.max(year)], .groups = "drop")
cat("Plots with a CIT change:", nrow(cit_change), "\n")
print(summary(cit_change$n_years))
chosen %>%
  left_join(first_last, by = "plotID") %>%
  summarise(baseline_moved = sum(sample_b != first), end_moved = sum(sample_e != last))
cit_change %>%
  summarise(across(starts_with("dCIT"), ~ mean(.x, na.rm = TRUE)))

save(cit_change, ft_change_boot, ft_change_summary, survey_year,
     file = "I:/DATA/output/CIT/CIT_change_SDMniche_BIO5_BIO6.RData")
