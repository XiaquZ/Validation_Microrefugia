library(dplyr)
library(tidyr)
library(lme4)

####################################################################################
#   Partition of the plot-level CIT change into extinction and colonisation of
#   cold- and warm-adapted species (Borderieux et al. 2024, Nat. Ecol. Evol.,
#   equation 1), applied to the forestREplot resurveys.
#
#   Borderieux et al. pooled pseudo-paired NFI plots per ecoregion. Equation 1 is
#   an exact decomposition of the change of a (weighted) mean, so it holds for a
#   single resurveyed plot as well. For one plot with baseline b and resurvey e:
#
#     CIT_e - CIT_b = sum_i  w_i (T_i - CIT_b) (o_ie - o_ib) / sum_i w_i o_ie
#
#   (because sum_i w_i (T_i - CIT_b) o_ib = 0 by definition of CIT_b), with
#     T_i  = species thermal niche value (niche median, as in script 07)
#     o_i  = presence (1/0); at plot level o_ie - o_ib is -1 (lost), +1 (gained)
#            or 0 (persisting), so persisting species do not contribute
#     w_i  = 1 (unweighted) or 1 / niche width (as the main CIT of script 07)
#   Components (deg C per decade), as in their Fig. 3a:
#     ext      = lost species (o_ie - o_ib < 0);  col = gained species (> 0)
#     ext_cold = lost cold-adapted (T_i < reference): always >= 0 (warming)
#     ext_warm = lost warm-adapted:   always <= 0
#     col_cold = gained cold-adapted: always <= 0
#     col_warm = gained warm-adapted: always >= 0
#   ext + col = ext_cold + ext_warm + col_cold + col_warm = delta CIT.
#   Because the sign of each sub-component is fixed by construction, they are
#   tested against a null model (niche values permuted among species), not
#   against 0, as in Borderieux et al.
#
#   Cold / warm reference ("locally" cold- or warm-adapted). Borderieux et al.
#   used the occurrence-weighted mean thermal optimum of the past plots of the
#   ecoregion. Two versions here:
#     ref = "plot"    (main): the plot's own baseline CIT (CIT_b). With this
#                     reference the signs above hold exactly.
#     ref = "dataset" (sensitivity): mean over all baseline occurrences of the
#                     forestREplot dataset, the closest analogue of their
#                     ecoregion. Only the cold / warm labels change; the
#                     contributions and their sum are the same. A species lost
#                     from a cold plot can then be "warm" relative to the
#                     dataset and still contribute to warming, so the signs of
#                     the sub-components are no longer fixed.
#
#   Presence-absence only, as in Borderieux et al.; cover is not used. Species set
#   and interval per plot are those of script 07 part 4 (species monitored in both
#   surveys, longest interval >= min_years with >= min_species species), so the
#   total equals dCIT_*_dec of cit_change (checked below).
####################################################################################

# Outputs of script 07 (parts 2 and 4)
load("I:/DATA/output/CIT/CIT_SDMniche_BIO5_BIO6.RData")             # plot_species, not_monitored
load("I:/DATA/output/CIT/CIT_change_boot1000_SDMniche_BIO5_BIO6.RData") # cit_change

n_null <- 200 # permutations of the null model (Borderieux et al.: 200)

dataset_of <- function(x) sub("^(EU_[0-9]+[a-z]?)_.*", "\\1", x)

####################################################################################
#######################   1. Species gained and lost per plot   ####################
####################################################################################
nm <- distinct(not_monitored, sample, species_key)
pres <- distinct(plot_species, sample, species_key)
chosen <- dplyr::select(cit_change, plotID, sample_b, sample_e, n_years)

# Present in a survey and monitored in the other survey (as in script 07 part 4)
occ <- full_join(
  chosen %>%
    inner_join(pres, by = c("sample_b" = "sample")) %>%
    anti_join(nm, by = c("sample_e" = "sample", "species_key")) %>%
    transmute(plotID, n_years, species_key, o_b = 1),
  chosen %>%
    inner_join(pres, by = c("sample_e" = "sample")) %>%
    anti_join(nm, by = c("sample_b" = "sample", "species_key")) %>%
    transmute(plotID, n_years, species_key, o_e = 1),
  by = c("plotID", "n_years", "species_key")
) %>%
  mutate(o_b = coalesce(o_b, 0), o_e = coalesce(o_e, 0), dataset = dataset_of(plotID))

# One niche per species (niche median and width, as in script 07)
niche_sp <- plot_species %>%
  distinct(species_key, value_BIO5, value_BIO6, width_BIO5, width_BIO6)

# Long format: one row per plot x species x variable x weighting
to_long <- function(d) {
  d %>%
    pivot_longer(c(value_BIO5, value_BIO6, width_BIO5, width_BIO6),
                 names_to = c(".value", "variable"), names_sep = "_") %>%
    rename(T = value) %>%
    cross_join(tibble(weighting = c("unweighted", "w"))) %>%
    mutate(w = if_else(weighting == "w", 1 / width, 1))
}

####################################################################################
############################   2. Partition (equation 1)   #########################
####################################################################################
partition <- function(d) {
  d <- d %>%
    group_by(plotID, variable, weighting) %>%
    mutate(
      cit_b = sum(w * T * o_b) / sum(w * o_b),
      d_occ = o_e - o_b,
      contrib = w * (T - cit_b) * d_occ / sum(w * o_e) / (n_years / 10)
    ) %>%
    # Dataset reference: weighted mean over all baseline occurrences of the
    # dataset (Topt_ecoreg_past of Borderieux et al.)
    group_by(dataset, variable, weighting) %>%
    mutate(ref_dataset = sum(w * T * o_b) / sum(w * o_b)) %>%
    ungroup()

  bind_rows(
    mutate(d, ref = "plot", cold = T < cit_b),
    mutate(d, ref = "dataset", cold = T < ref_dataset)
  ) %>%
    group_by(plotID, dataset, n_years, variable, weighting, ref) %>%
    summarise(
      dCIT = sum(contrib),
      ext = sum(contrib[d_occ < 0]),
      col = sum(contrib[d_occ > 0]),
      ext_cold = sum(contrib[d_occ < 0 & cold]),
      ext_warm = sum(contrib[d_occ < 0 & !cold]),
      col_cold = sum(contrib[d_occ > 0 & cold]),
      col_warm = sum(contrib[d_occ > 0 & !cold]),
      n_b = sum(o_b),
      n_e = sum(o_e),
      n_persist = sum(o_b == 1 & o_e == 1),
      n_lost = sum(d_occ < 0),
      n_gained = sum(d_occ > 0),
      n_lost_cold = sum(d_occ < 0 & cold),
      n_lost_warm = sum(d_occ < 0 & !cold),
      n_gained_cold = sum(d_occ > 0 & cold),
      n_gained_warm = sum(d_occ > 0 & !cold),
      .groups = "drop"
    )
}

comp_cols <- c("dCIT", "ext", "col", "ext_cold", "ext_warm", "col_cold", "col_warm")

ext_col <- partition(to_long(inner_join(occ, niche_sp, by = "species_key")))

# Check: the total equals the delta CIT of script 07 part 4 (to rounding)
ext_col %>%
  filter(ref == "plot") %>%
  left_join(dplyr::select(cit_change, plotID, dCIT_BIO5_dec, dCIT_BIO6_dec,
                          dCIT_BIO5_w_dec, dCIT_BIO6_w_dec), by = "plotID") %>%
  mutate(dCIT_07 = case_when(
    variable == "BIO5" & weighting == "unweighted" ~ dCIT_BIO5_dec,
    variable == "BIO6" & weighting == "unweighted" ~ dCIT_BIO6_dec,
    variable == "BIO5" ~ dCIT_BIO5_w_dec,
    TRUE ~ dCIT_BIO6_w_dec
  )) %>%
  group_by(variable, weighting) %>%
  summarise(n_plots = n(), max_abs_diff = max(abs(dCIT - dCIT_07)), .groups = "drop")

# Species turnover per plot (the same for both variables and weightings)
ext_col %>%
  filter(variable == "BIO5", weighting == "w", ref == "plot") %>%
  summarise(
    plots = n(),
    plots_with_loss = sum(n_lost > 0),
    plots_with_gain = sum(n_gained > 0),
    lost_total = sum(n_lost),
    gained_total = sum(n_gained),
    median_share_lost = median(n_lost / n_b),
    median_share_gained = median(n_gained / n_e)
  )

####################################################################################
#########################   3. Null model: permuted niches   #######################
####################################################################################
# As the random thermal optimum model of Borderieux et al.: the niche values are
# permuted among the species of the dataset's species pool (the analogue of their
# ecoregion pool), the gains and losses stay as observed, and the partition is
# repeated n_null times. Niche median and width are permuted together, so the
# weighted version keeps each niche intact. Without a link between a species'
# niche and its gain or loss, the observed components equal the null ones.
pool <- distinct(occ, dataset, species_key)

set.seed(16)
null_runs <- bind_rows(lapply(seq_len(n_null), function(k) {
  if (k %% 20 == 0) cat("Null model:", k, "of", n_null, format(Sys.time(), "(%H:%M)"), "\n")
  perm <- pool %>%
    group_by(dataset) %>%
    mutate(src = species_key[sample.int(n())]) %>%
    ungroup() %>%
    left_join(niche_sp, by = c("src" = "species_key")) %>%
    dplyr::select(-src)
  # pool species differ per dataset, so the niche is joined by dataset as well
  occ_perm <- occ %>% inner_join(perm, by = c("dataset", "species_key"))
  partition(to_long(occ_perm)) %>%
    dplyr::select(plotID, variable, weighting, ref, all_of(comp_cols)) %>%
    mutate(run = k)
}))

# Null mean per plot and component
null_mean <- null_runs %>%
  group_by(plotID, variable, weighting, ref) %>%
  summarise(across(all_of(comp_cols), mean), .groups = "drop")

####################################################################################
###########################   4. Observed vs null model   ##########################
####################################################################################
# a) As Borderieux et al. (ecoregion -> dataset): mean component per dataset,
#    observed vs null, two-sided paired Wilcoxon signed-rank test across datasets.
#    Datasets contribute equally, whatever their number of plots.
obs_null <- ext_col %>%
  dplyr::select(plotID, dataset, variable, weighting, ref, all_of(comp_cols)) %>%
  pivot_longer(all_of(comp_cols), names_to = "component", values_to = "obs") %>%
  left_join(null_mean %>%
              pivot_longer(all_of(comp_cols), names_to = "component", values_to = "null"),
            by = c("plotID", "variable", "weighting", "ref", "component")) %>%
  mutate(component = factor(component, levels = comp_cols))

test_dataset <- obs_null %>%
  group_by(dataset, variable, weighting, ref, component) %>%
  summarise(obs = mean(obs), null = mean(null), .groups = "drop") %>%
  group_by(variable, weighting, ref, component) %>%
  summarise(
    n_datasets = n(),
    mean_obs = mean(obs),
    mean_null = mean(null),
    p_wilcox = wilcox.test(obs, null, paired = TRUE)$p.value,
    .groups = "drop"
  )

# b) Plot level: observed - null per plot, intercept-only mixed model with the
#    dataset as random intercept (plots nested in datasets). Estimate in deg C
#    per decade, Wald 95% CI.
test_plot <- obs_null %>%
  group_by(variable, weighting, ref, component) %>%
  group_modify(function(d, key) {
    m <- lmer(I(obs - null) ~ 1 + (1 | dataset), data = d)
    ci <- confint(m, parm = "(Intercept)", method = "Wald")
    tibble(n_plots = nrow(d), mean_obs = mean(d$obs), mean_null = mean(d$null),
           diff = fixef(m)[[1]], diff_lo = ci[1], diff_hi = ci[2])
  }) %>%
  ungroup()

# Main results: weighted CIT, plot reference
test_dataset %>% filter(weighting == "w", ref == "plot") %>% print(n = Inf)
test_plot %>% filter(weighting == "w", ref == "plot") %>% print(n = Inf)

save(ext_col, null_runs, obs_null, test_dataset, test_plot,
     file = "I:/DATA/output/CIT/ExtCol_partition_SDMniche_BIO5_BIO6.RData")
