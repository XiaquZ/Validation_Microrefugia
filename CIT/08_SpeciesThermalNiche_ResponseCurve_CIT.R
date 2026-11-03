library(ENMeval)
library(maxnet)
library(dplyr)
library(tidyr)
library(ggplot2)

####################################################################################
#   Species thermal niches (BIO5, BIO6) from the SDM response curves, and
#   community-inferred temperature (CIT).
#
#   MAIN ANALYSIS. Replaces the binary-range sampling of
#   07_SpeciesThermalNiche_BinaryRange_CIT.R: the niche comes from the fitted
#   model itself, without thresholding. The partial-dependence version is kept as
#   a sensitivity check in 08.1_SpeciesThermalNiche_PartialDependence_Sensitivity.R.
#     - Model: the ENMeval maxnet model(s) with the lowest delta AICc per species
#       (10_TrainCurrentSDMs.R: SWD, raw predictor values, 10,000 background
#       points from the 1000 km buffer-overlap surface). Ties are averaged.
#     - Response curve: realised relative occurrence rate along BIO5 /
#       BIO6. Raw maxnet output (estimates f1(z)/f(z)) is predicted at the
#       background points and averaged along temperature with a Gaussian kernel.
#       The background is the reference distribution f(z) of the model, so no
#       extra weights. Other predictors vary with temperature as they do in the
#       background (not a partial-dependence curve). Rescaled to max = 1; grid
#       values with too few background points nearby (eff_n < min_eff_n) are NA.
#     - Niche statistics: the realised curve is normalised to a density over the
#       grid -> Q05, median, mean, Q95, width; optimum = temperature of max rate.
#   The output table has the same columns as in script 07, so the CIT step is the
#   same.
####################################################################################

# ---- Paths (adjust) ----
mdl_dir     <- "I:/DATA/SDM_current/models/"
mdl_pattern <- "_ENMeval_swd\\.RData$"
out_dir     <- "I:/DATA/SDM_current/ThermalNiche/"
# Added to every output name; the same names without the tag are the
# partial-dependence results of script 08.1
method_tag  <- "_RawBG"
vars <- c(
  BIO5 = "Micro_BIO5_EU_CHELSAbased_2000.2020",
  BIO6 = "Micro_BIO6_EU_CHELSAbased_2000.2020"
)
n_grid    <- 200 # temperature values along each response curve
bw        <- 0.5 # kernel SD (°C) for the realised curve
min_eff_n <- 30  # minimum effective number of background points per grid value
edge_tol  <- 1   # optimum within this distance (°C) of a curve end -> edge_opt

####################################################################################
#####################   1. Response curves and thermal niche  ######################
####################################################################################
# Raw output of the selected model(s) at the background points. Each model is
# scaled to sum to 1 (as MaxEnt raw output), so tied models can be averaged;
# subtracting max(lp) avoids overflow in exp().
raw_bg <- function(mdls, env_bg) {
  raw <- lapply(mdls, function(m) {
    lp <- as.numeric(predict(m, env_bg, type = "link", clamp = FALSE))
    r <- exp(lp - max(lp))
    r / sum(r)
  })
  Reduce(`+`, raw) / length(raw)
}

# Realised curve: kernel (Nadaraya-Watson) mean of raw output at each grid value.
# eff_n = Kish's effective number of background points behind that value.
resp_realised <- function(t_bg, raw, x) {
  K <- outer(x, t_bg, function(g, ti) dnorm(ti, mean = g, sd = bw))
  sum_k <- rowSums(K)
  r <- as.numeric(K %*% raw) / sum_k
  eff_n <- sum_k^2 / rowSums(K^2)
  ok <- sum_k > 0 & eff_n >= min_eff_n
  r[is.na(ok) | !ok] <- NA
  data.frame(rate = r / max(r, na.rm = TRUE), eff_n = eff_n)
}

# Niche statistics from a curve treated as a density over the grid
# (NA grid values, i.e. outside the background support, are dropped)
curve_stats <- function(x, suit) {
  keep <- !is.na(suit)
  x <- x[keep]
  suit <- suit[keep]
  w <- suit / sum(suit)
  cdf <- cumsum(w)
  q <- function(p) x[which(cdf >= p)[1]]
  data.frame(
    Q05 = q(0.05),
    median = q(0.5),
    mean = sum(x * w),
    optimum = x[which.max(suit)],
    Q95 = q(0.95),
    width = q(0.95) - q(0.05),
    # TRUE: optimum within edge_tol of the edge of the supported range ->
    # monotonic curve, optimum (and to a lesser extent the other statistics)
    # truncated. A tolerance, as small wiggles at the cut-off can put the
    # maximum a few grid values inside the edge.
    edge_opt = min(abs(x[which.max(suit)] - range(x))) <= edge_tol
  )
}

niche_one_species <- function(mdl_file, vars, n_grid) {
  species_name <- sub(mdl_pattern, "", basename(mdl_file))
  e <- new.env()
  load(mdl_file, envir = e) # e.swd
  e.swd <- e$e.swd

  # Selected model(s): minimum delta AICc, ties averaged (which() drops NA AICc)
  res <- eval.results(e.swd)
  if (all(is.na(res$delta.AICc))) stop("delta.AICc is NA for every setting: ", species_name)
  best <- which(res$delta.AICc == min(res$delta.AICc, na.rm = TRUE))
  mdls <- e.swd@models[best]
  mdl <- mdls[[1]]

  # Background with its predictor values (SWD); model predictors only
  pred_names <- names(mdl$samplemeans)
  stopifnot(all(pred_names %in% names(e.swd@bg)))
  env_bg <- e.swd@bg[, pred_names]
  raw <- raw_bg(mdls, env_bg)

  # Which temperature features are retained (union over tied models)
  feats <- unique(unlist(lapply(mdls, function(m) names(m$betas))))

  curves <- list()
  stats <- list()
  for (v in names(vars)) {
    var <- vars[[v]]
    x <- seq(mdl$varmin[[var]], mdl$varmax[[var]], length.out = n_grid)
    rc <- resp_realised(env_bg[[var]], raw, x)

    curves[[v]] <- data.frame(
      species = species_name, variable = v, x = x,
      rate = rc$rate, eff_n = rc$eff_n
    )
    stats[[v]] <- data.frame(
      species = species_name,
      variable = v,
      curve_stats(x, rc$rate),
      n_feat_var = sum(grepl(var, feats, fixed = TRUE)), # 0 -> only via correlated predictors
      n_occs = nrow(e.swd@occs),
      n_bg = nrow(env_bg),
      tune_args = paste(as.character(res$tune.args[best]), collapse = " | ")
    )
  }
  list(stats = do.call(rbind, stats), curves = do.call(rbind, curves))
}

mdl_files <- list.files(mdl_dir, pattern = mdl_pattern, full.names = TRUE)

# ---- Test run on a few species first ----
# TRUE: run only the species in test_species (or the first n_test files if
# test_species is empty) and write to a separate *_TEST.csv.
# FALSE: run all species.
test_run     <- TRUE
test_species <- c("Viola reichenbachiana", "Paris quadrifolia", "Actaea spicata",
                  "Brachypodium sylvaticum", "Calamagrostis chalybaea")
n_test       <- 3

if (test_run) {
  if (length(test_species) > 0) {
    mdl_files <- mdl_files[grepl(paste(test_species, collapse = "|"), basename(mdl_files))]
  } else {
    mdl_files <- head(mdl_files, n_test)
  }
}
print(basename(mdl_files))

res_all <- lapply(mdl_files, function(f) {
  cat("Response curves:", basename(f), "\n")
  print(system.time(r <- niche_one_species(f, vars, n_grid)))
  r
})
niches_rc <- do.call(rbind, lapply(res_all, `[[`, "stats"))
curves_rc <- do.call(rbind, lapply(res_all, `[[`, "curves"))
rownames(niches_rc) <- NULL
rownames(curves_rc) <- NULL

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
suffix <- if (test_run) "_TEST" else ""
write.csv(niches_rc, file.path(out_dir, paste0("Species_ThermalNiche_ResponseCurve_BIO5_BIO6", method_tag, suffix, ".csv")),
  row.names = FALSE
)
saveRDS(curves_rc, file.path(out_dir, paste0("Species_ResponseCurves_BIO5_BIO6", method_tag, suffix, ".rds")))

# Unit check: bw assumes °C (BIO5 roughly 10 to 35, BIO6 roughly -20 to 5)
curves_rc %>%
  filter(!is.na(rate)) %>%
  group_by(variable) %>%
  summarise(min = min(x), max = max(x))

# Checks: monotonic curves (optimum at the range edge) and temperature not in the model
niches_rc %>% filter(edge_opt | n_feat_var == 0)

# Test check: response curves with Q05, median and Q95
if (test_run) {
  for (sp in unique(curves_rc$species)) {
    d <- filter(curves_rc, species == sp, !is.na(rate))
    s <- filter(niches_rc, species == sp)
    p <- ggplot(d, aes(x, rate)) +
      geom_line(linewidth = 1) +
      geom_vline(data = s, aes(xintercept = Q05), linetype = 2, colour = "grey40") +
      geom_vline(data = s, aes(xintercept = median), linetype = 2, colour = "grey40") +
      geom_vline(data = s, aes(xintercept = Q95), linetype = 2, colour = "grey40") +
      facet_wrap(~variable, scales = "free_x") +
      labs(x = "Microclimate temperature (°C)", y = "Relative occurrence rate (max = 1)", title = sp) +
      theme_bw()
    print(p)
  }
}

####################################################################################
########################   2. Community-inferred temperature  ######################
####################################################################################
niches_rc <- read.csv(file.path(out_dir, paste0("Species_ThermalNiche_ResponseCurve_BIO5_BIO6", method_tag, ".csv")))
curves_rc <- readRDS(file.path(out_dir, paste0("Species_ResponseCurves_BIO5_BIO6", method_tag, ".rds")))

# Species value used for CIT: the median, as in script 07. It is less pulled by
# curve tails cut off at min_eff_n than the mean, and stable unlike the optimum
# (truncated for monotonic curves (edge_opt), jumps between peaks of bimodal
# curves). CITs from the curve mean and optimum are kept as sensitivity checks
# (*_mean, *_opt).
niche_value <- "median"

norm_name <- function(x) tolower(trimws(gsub("[_ ]+", " ", x)))

# forestREplot name -> SDM name, as in 07 (from 06.1_SpeciesNameMatch_SDM_forestREplot.R)
sp_match <- read.csv("I:/DATA/output/CIT/SpeciesMatch_SDM_forestREplot.csv")
sp_match <- sp_match[sp_match$used_in_cit & sp_match$match_type != "exact", ]
syn_fr2sdm <- setNames(sp_match$sdm_species, sp_match$fr_species)
fr_to_sdm <- function(x) ifelse(x %in% names(syn_fr2sdm), unname(syn_fr2sdm[x]), x)

# Optimum sensitivity check: species whose optimum lies at the edge of the curve
# (edge_opt, curve still rising where the background data end) have no real
# optimum, only the cut-off temperature. They would pull the optimum CIT toward
# that cut-off, so their optimum is set to NA and they are left out of *_opt
# only (per variable: a species flagged for BIO5 still counts in CIT_BIO6_opt).
# They stay in the main (median) CIT and the mean check, as their median and
# mean are still meaningful.
niche_wide <- niches_rc %>%
  mutate(species_key = norm_name(species),
         optimum = ifelse(edge_opt, NA, optimum)) %>%
  dplyr::select(species_key, variable, value = all_of(niche_value), width,
                sens_mean = mean, sens_opt = optimum) %>%
  pivot_wider(names_from = variable, values_from = c(value, width, sens_mean, sens_opt))

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

# Presence-absence CIT, unweighted and weighted by inverse niche width;
# sensitivity: unweighted CIT from the curve mean (*_mean) and optimum (*_opt).
# *_opt averages over the species with a real optimum (edge_opt species are NA,
# see above); n_species_opt_* = number of species used, NA if none.
mean_or_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
cit_from <- function(d) {
  d %>%
    group_by(sample) %>%
    summarise(
      n_species = n(),
      CIT_BIO5 = mean(value_BIO5),
      CIT_BIO6 = mean(value_BIO6),
      CIT_BIO5_w = weighted.mean(value_BIO5, 1 / width_BIO5),
      CIT_BIO6_w = weighted.mean(value_BIO6, 1 / width_BIO6),
      CIT_BIO5_mean = mean(sens_mean_BIO5),
      CIT_BIO6_mean = mean(sens_mean_BIO6),
      CIT_BIO5_opt = mean_or_na(sens_opt_BIO5),
      CIT_BIO6_opt = mean_or_na(sens_opt_BIO6),
      n_species_opt_BIO5 = sum(!is.na(sens_opt_BIO5)),
      n_species_opt_BIO6 = sum(!is.na(sens_opt_BIO6)),
      .groups = "drop"
    )
}

# Same filter as script 07, so the two CITs cover the same plots
min_species <- 3

# Herb + shrub layer; herb layer only as sensitivity check (*_herb), as in 07
cit_rc <- cit_from(plot_species) %>%
  left_join(cit_from(filter(plot_species, herb)) %>%
              rename_with(~ paste0(.x, "_herb"), -sample), by = "sample") %>%
  filter(n_species >= min_species) %>%
  mutate(
    across(starts_with("CIT") & ends_with("_herb"),
           ~ ifelse(!is.na(n_species_herb) & n_species_herb >= min_species, .x, NA)),
    # Optimum check: same minimum, counted over the species with a real optimum
    CIT_BIO5_opt = ifelse(n_species_opt_BIO5 >= min_species, CIT_BIO5_opt, NA),
    CIT_BIO6_opt = ifelse(n_species_opt_BIO6 >= min_species, CIT_BIO6_opt, NA),
    CIT_BIO5_opt_herb = ifelse(n_species_opt_BIO5_herb >= min_species, CIT_BIO5_opt_herb, NA),
    CIT_BIO6_opt_herb = ifelse(n_species_opt_BIO6_herb >= min_species, CIT_BIO6_opt_herb, NA),
    # sample = "EU_xxx_plot_survey": split back into plotID and survey type
    plotID = sub("_[^_]+$", "", sample),
    surveyTyp = sub("^.*_", "", sample)
  )

cit_rc %>%
  summarise(
    r_BIO5 = cor(CIT_BIO5, CIT_BIO5_herb, use = "complete.obs"),
    r_BIO6 = cor(CIT_BIO6, CIT_BIO6_herb, use = "complete.obs"),
    mean_diff_BIO5 = mean(CIT_BIO5 - CIT_BIO5_herb, na.rm = TRUE),
    mean_diff_BIO6 = mean(CIT_BIO6 - CIT_BIO6_herb, na.rm = TRUE)
  )

# Sensitivity: CIT from the curve median (main) vs mean and optimum.
# Mean difference = sensitivity - main. The optimum CIT is NA in plot-surveys
# where all species are edge_opt, so those are skipped.
lapply(c(mean = "_mean", optimum = "_opt"), function(s) {
  cit_rc %>%
    summarise(
      r_BIO5 = cor(CIT_BIO5, .data[[paste0("CIT_BIO5", s)]], use = "complete.obs"),
      r_BIO6 = cor(CIT_BIO6, .data[[paste0("CIT_BIO6", s)]], use = "complete.obs"),
      mean_diff_BIO5 = mean(.data[[paste0("CIT_BIO5", s)]] - CIT_BIO5, na.rm = TRUE),
      mean_diff_BIO6 = mean(.data[[paste0("CIT_BIO6", s)]] - CIT_BIO6, na.rm = TRUE)
    )
})
# Share of species left out of the optimum check, per variable
niches_rc %>% group_by(variable) %>% summarise(n = n(), n_edge_opt = sum(edge_opt))

save(cit_rc, niches_rc, plot_species, not_monitored, not_surveyed,
     file = paste0("I:/DATA/output/CIT/CIT_SDMresponse_BIO5_BIO6", method_tag, ".RData"))

####################################################################################
################   3. Bootstrapped floristic temperature (De Frenne 2013)  ##########
####################################################################################
# As in script 07, but each species' temperature draw comes from its response
# realised curve (grid values drawn with probability proportional to the rate;
# NA = outside the background support -> 0), which is closer to De Frenne et
# al. (2013) who also used response curves.
# Note: BIO5 and BIO6 are drawn independently here (no paired locations as in 07).
n_boot <- 500

set.seed(16)
curve_list <- split(curves_rc, curves_rc$species)
sp_keys <- norm_name(names(curve_list))
draws <- lapply(c(BIO5 = "BIO5", BIO6 = "BIO6"), function(v) {
  m <- t(sapply(curve_list, function(d) {
    d <- d[d$variable == v, ]
    sample(d$x, n_boot, replace = TRUE, prob = ifelse(is.na(d$rate), 0, d$rate))
  }))
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

# Long table: one row per plot-survey x bootstrap sample
ft_boot_rc <- do.call(rbind, lapply(names(plot_idx), function(s) {
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
ft_boot_rc <- ft_boot_rc %>%
  mutate(
    plotID = sub("_[^_]+$", "", sample),
    surveyTyp = sub("^.*_", "", sample)
  )

ft_summary_rc <- ft_boot_rc %>%
  group_by(sample, plotID, surveyTyp, n_species) %>%
  summarise(across(starts_with(c("FT_", "LT_", "RT_")),
    list(mean = mean, lo = ~ quantile(.x, 0.025), hi = ~ quantile(.x, 0.975))
  ), .groups = "drop")

save(ft_boot_rc, ft_summary_rc,
     file = paste0("I:/DATA/output/CIT/FloristicTemp_boot_SDMresponse_BIO5_BIO6", method_tag, ".RData"))

####################################################################################
#################   4. Compare with the range-sampling CIT (script 07)  ############
####################################################################################
load("I:/DATA/output/CIT/CIT_SDMniche_BIO5_BIO6.RData") # cit
cmp <- inner_join(cit, cit_rc, by = c("sample", "plotID", "surveyTyp"), suffix = c("_range", "_curve"))
cor(cmp$CIT_BIO5_range, cmp$CIT_BIO5_curve)
cor(cmp$CIT_BIO6_range, cmp$CIT_BIO6_curve)
