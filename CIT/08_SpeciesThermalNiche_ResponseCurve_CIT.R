library(ENMeval)
library(maxnet)
library(dplyr)
library(tidyr)
library(ggplot2)

####################################################################################
#   Species thermal niches (BIO5, BIO6) from the SDM response curves, and
#   community-inferred temperature (CIT).
#
#   Alternative to 07_SpeciesThermalNiche_CIT.R, which samples climate inside the
#   binary SDM range. Here the niche comes from the fitted model itself:
#     - Model: the ENMeval maxnet model with the lowest delta AICc per species.
#     - Response curve: partial dependence over the occurrence points. BIO5 / BIO6
#       is set to each value of a grid (varmin - varmax of the training data) at
#       every occurrence, keeping the other predictors at their observed values,
#       and the cloglog suitability is averaged. The models contain many product
#       features (e.g. BIO5:BIO6, BIO6:clay), so the classic marginal curve (other
#       predictors at their sample means) is only one slice; it is saved too
#       for comparison.
#     - Niche statistics: the curve is normalised to a density over the grid ->
#       Q05, median, mean, Q95, width; optimum = temperature of max suitability.
#   The output table has the same columns as in script 07, so the CIT step is the
#   same.
####################################################################################

# ---- Paths (adjust) ----
mdl_dir     <- "I:/DATA/SDM_current/models/"
mdl_pattern <- "_ENMeval_swd\\.RData$"
out_dir     <- "I:/DATA/SDM_current/ThermalNiche/"
vars <- c(
  BIO5 = "Micro_BIO5_EU_CHELSAbased_2000.2020",
  BIO6 = "Micro_BIO6_EU_CHELSAbased_2000.2020"
)
n_grid <- 200 # temperature values along each response curve

####################################################################################
#####################   1. Response curves and thermal niche  ######################
####################################################################################
# Partial dependence: all occurrences x all grid values in one predict() call
resp_pdp <- function(mdl, var, env, x) {
  nd <- env[rep(seq_len(nrow(env)), times = length(x)), , drop = FALSE]
  nd[[var]] <- rep(x, each = nrow(env))
  p <- as.numeric(predict(mdl, nd, type = "cloglog", clamp = TRUE))
  as.numeric(tapply(p, rep(seq_along(x), each = nrow(env)), mean))
}

# Marginal curve: other predictors at their sample means
resp_marginal <- function(mdl, var, x) {
  nd <- as.data.frame(as.list(mdl$samplemeans))
  nd <- nd[rep(1, length(x)), , drop = FALSE]
  nd[[var]] <- x
  as.numeric(predict(mdl, nd, type = "cloglog", clamp = TRUE))
}

# Niche statistics from a curve treated as a density over the grid
curve_stats <- function(x, suit) {
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
    max_suit = max(suit),
    # TRUE: optimum at the edge of the training range -> monotonic curve,
    # optimum (and to a lesser extent the other statistics) truncated
    edge_opt = which.max(suit) %in% c(1, length(x))
  )
}

niche_one_species <- function(mdl_file, vars, n_grid) {
  species_name <- sub(mdl_pattern, "", basename(mdl_file))
  e <- new.env()
  load(mdl_file, envir = e) # e.swd
  e.swd <- e$e.swd

  # Best model: minimum delta AICc (which.min also handles ties and NAs)
  res <- eval.results(e.swd)
  best <- which.min(res$delta.AICc)
  mdl <- e.swd@models[[best]]

  # Occurrences with their predictor values (SWD); keep the model predictors only
  pred_names <- names(mdl$samplemeans)
  stopifnot(all(pred_names %in% names(e.swd@occs)))
  env <- e.swd@occs[, pred_names]

  # Which temperature features are retained in the model
  feats <- names(mdl$betas)

  curves <- list()
  stats <- list()
  for (v in names(vars)) {
    var <- vars[[v]]
    x <- seq(mdl$varmin[[var]], mdl$varmax[[var]], length.out = n_grid)
    suit_pdp <- resp_pdp(mdl, var, env, x)
    suit_mar <- resp_marginal(mdl, var, x)

    curves[[v]] <- data.frame(
      species = species_name, variable = v, x = x,
      suit_pdp = suit_pdp, suit_marginal = suit_mar
    )
    stats[[v]] <- data.frame(
      species = species_name,
      variable = v,
      curve_stats(x, suit_pdp),
      # optimum of the marginal curve, to see how much the interactions matter
      optimum_marginal = x[which.max(suit_mar)],
      n_feat_var = sum(grepl(var, feats, fixed = TRUE)), # 0 -> flat curve
      n_occs = nrow(env),
      tune_args = as.character(res$tune.args[best])
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
test_species <- c("Viola reichenbachiana", "Paris quadrifolia") # e.g. c("Paris quadrifolia", "Anemone nemorosa")
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
write.csv(niches_rc, file.path(out_dir, paste0("Species_ThermalNiche_ResponseCurve_BIO5_BIO6", suffix, ".csv")),
  row.names = FALSE
)
saveRDS(curves_rc, file.path(out_dir, paste0("Species_ResponseCurves_BIO5_BIO6", suffix, ".rds")))

# Checks: monotonic curves (optimum at the range edge), temperature not in the
# model (flat curve), and large shifts between partial dependence and marginal optimum
niches_rc %>% filter(edge_opt | n_feat_var == 0)
niches_rc %>% mutate(opt_shift = optimum - optimum_marginal) %>% arrange(desc(abs(opt_shift)))

# Test check: partial dependence vs marginal curve, with Q05, median and Q95
if (test_run) {
  for (sp in unique(curves_rc$species)) {
    d <- curves_rc %>%
      filter(species == sp) %>%
      pivot_longer(c(suit_pdp, suit_marginal), names_to = "method", values_to = "suit")
    s <- filter(niches_rc, species == sp)
    p <- ggplot(d, aes(x, suit, colour = method)) +
      geom_line(linewidth = 1) +
      geom_vline(data = s, aes(xintercept = Q05), linetype = 2, colour = "grey40") +
      geom_vline(data = s, aes(xintercept = median), linetype = 2, colour = "grey40") +
      geom_vline(data = s, aes(xintercept = Q95), linetype = 2, colour = "grey40") +
      facet_wrap(~variable, scales = "free_x") +
      labs(x = "Microclimate temperature (°C)", y = "Suitability (cloglog)", title = sp, colour = NULL) +
      theme_bw() +
      theme(legend.position = "bottom")
    print(p)
  }
}

####################################################################################
########################   2. Community-inferred temperature  ######################
####################################################################################
niches_rc <- read.csv(file.path(out_dir, "Species_ThermalNiche_ResponseCurve_BIO5_BIO6.csv"))
curves_rc <- readRDS(file.path(out_dir, "Species_ResponseCurves_BIO5_BIO6.rds"))

# Species value used for CIT: the optimum is truncated for monotonic curves
# (edge_opt), so the curve mean is the default; swap for "median" or "optimum".
niche_value <- "mean"

norm_name <- function(x) tolower(trimws(gsub("[_ ]+", " ", x)))

# forestREplot name -> SDM name, as in 07 (from 06.1_SpeciesNameMatch_SDM_forestREplot.R)
sp_match <- read.csv("I:/DATA/output/CIT/SpeciesMatch_SDM_forestREplot.csv")
sp_match <- sp_match[sp_match$used_in_cit & sp_match$match_type != "exact", ]
syn_fr2sdm <- setNames(sp_match$sdm_species, sp_match$fr_species)
fr_to_sdm <- function(x) ifelse(x %in% names(syn_fr2sdm), unname(syn_fr2sdm[x]), x)

niche_wide <- niches_rc %>%
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

save(cit_rc, niches_rc, plot_species, not_monitored, not_surveyed,
     file = "I:/DATA/output/CIT/CIT_SDMresponse_BIO5_BIO6.RData")

####################################################################################
################   3. Bootstrapped floristic temperature (De Frenne 2013)  ##########
####################################################################################
# As in script 07, but each species' temperature draw comes from its response
# curve (grid values drawn with probability proportional to suitability), which
# is closer to De Frenne et al. (2013) who also used response curves.
# Note: BIO5 and BIO6 are drawn independently here (no paired locations as in 07).
n_boot <- 500

set.seed(16)
curve_list <- split(curves_rc, curves_rc$species)
sp_keys <- norm_name(names(curve_list))
draws <- lapply(c(BIO5 = "BIO5", BIO6 = "BIO6"), function(v) {
  m <- t(sapply(curve_list, function(d) {
    d <- d[d$variable == v, ]
    sample(d$x, n_boot, replace = TRUE, prob = d$suit_pdp)
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

save(ft_boot_rc, ft_summary_rc, file = "I:/DATA/output/CIT/FloristicTemp_boot_SDMresponse_BIO5_BIO6.RData")

####################################################################################
#################   4. Compare with the range-sampling CIT (script 07)  ############
####################################################################################
load("I:/DATA/output/CIT/CIT_SDMniche_BIO5_BIO6.RData") # cit
cmp <- inner_join(cit, cit_rc, by = c("sample", "plotID", "surveyTyp"), suffix = c("_range", "_curve"))
cor(cmp$CIT_BIO5_range, cmp$CIT_BIO5_curve)
cor(cmp$CIT_BIO6_range, cmp$CIT_BIO6_curve)
