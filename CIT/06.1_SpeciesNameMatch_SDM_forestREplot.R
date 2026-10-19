library(dplyr)

####################################################################################
#   Match the 140 SDM species to the species names of the forestREplot v3.1 herb
#   and shrub layer and log every decision. The output (one row per SDM species x
#   forestREplot name) is read by scripts 07-09 to translate forestREplot names
#   into SDM names before the CIT is calculated.
#
#   match_type:
#     exact        same name
#     synonym      same taxon under another name (nomenclature differs)
#     broader_name forestREplot records the species / group that contains the
#                  SDM taxon (subspecies or segregate not separated in the plots)
#     not_used     related name, but clearly a different taxon -> not matched
#     absent       no corresponding name in the plots
#   Rule: keep a match unless there is clear evidence of a different taxon.
#
#   Decision log (details in the comments below):
#   2026-10-14  Names from the raw v3.1 data, not from Cleaned_EU_HerbL.RData,
#               whose cleaning changed some names (e.g. "Circaea ×intermedia").
#               103 exact matches; synonyms added after a check of all unmatched
#               species against same-genus names in the plots.
#   2026-10-15  Broader names kept unless a different taxon is clear (Dactylis,
#               Calamagrostis, Senecio nemorensis); Senecio ovatus not matched.
#   2026-10-16  Herb (H, SH) and shrub (S) layer used, tree layer (T) excluded:
#               several modelled species are woody understorey species recorded
#               as shrubs (Daphne, Lonicera, Staphylea, Ruscus, Ribes, Euonymus).
#               Abundance 9999 is no presence: per the forestREplot providers it
#               marks a plot not resurveyed or a species not monitored in that
#               survey (verified 2026-10-19: always a whole survey, as documented).
#   2026-10-19  Duplicated datasets EU_009b and EU_010a excluded (see below).
#               Pyrola chlorantha became absent: its only 2 records were in
#               EU_010a (1993). Result: 102 exact, 8 synonym, 4 broader name,
#               26 absent -> 114 species used in the CIT.
####################################################################################

bin_dir  <- "N:/SDMs/SDMs_current/Results/OriginalSDM_CurrentBinaryMaps_aligned/"
out_file <- "I:/DATA/output/CIT/SpeciesMatch_SDM_forestREplot.csv"

# SDM species = names of the binary maps. sdm_sp can be set before sourcing this
# script where N: is not mounted.
if (!exists("sdm_sp")) {
  sdm_sp <- sub("^binary_", "", tools::file_path_sans_ext(list.files(bin_dir, pattern = "\\.tif$")))
}
stopifnot(length(sdm_sp) > 0)

# Vegetation data of forestREplot v3.1 as used for the CIT (07-09): herb (H, SH)
# and shrub (S) layer, Europe; abundance 9999 = not surveyed / not monitored,
# so no presence record
# Duplicated datasets (same plots in two versions), keep one of each:
#   - EU_010a = EU_010b plus the 1971 and 1993 surveys, in which only selected
#     species were recorded; the providers advise EU_010b for community analyses
#   - EU_009b repeats the 1976-2013 surveys of EU_009a with other cover values
#     (same species lists); EU_009a has the longer series (from 1935)
drop_datasets <- c("EU_009b", "EU_010a")
dataset_of <- function(sample) sub("^(EU_[0-9]+[a-z]?)_.*", "\\1", sample)
load("I:/DATA/input/forestREplot/version3.1/veg.data_forestREplot_V3.1.RData") # veg_data
sp_veg <- veg_data %>%
  filter(grepl("^EU_", sample), !dataset_of(sample) %in% drop_datasets,
         layer %in% c("H", "SH", "S"), abundance != 9999,
         !is.na(species_name))
fr_names <- unique(sp_veg$species_name)

norm_name <- function(x) tolower(trimws(gsub("[_ ]+", " ", x)))

# ---- Manual decisions (checked 2026-10-14 / 15) ----
manual <- tribble(
  ~sdm_species, ~fr_species, ~match_type, ~note,
  "Anemone ranunculoides", "Anemonoides ranunculoides", "synonym",
  "genus Anemonoides in forestREplot",
  "Bromopsis benekenii", "Bromus benekenii", "synonym",
  "genus Bromus s.l. in forestREplot",
  "Bromopsis ramosa", "Bromus ramosus", "synonym",
  "genus Bromus s.l. in forestREplot",
  # "×" = hybrid sign, written without a space in forestREplot
  "Circaea x intermedia", "Circaea ×intermedia", "synonym",
  "hybrid sign written differently in forestREplot",
  "Drymochloa sylvatica", "Festuca altissima", "synonym",
  "Festuca altissima in forestREplot",
  "Schedonorus giganteus", "Festuca gigantea", "synonym",
  "Festuca gigantea in forestREplot",
  "Hypopitys monotropa", "Monotropa hypopitys", "synonym",
  "Monotropa hypopitys in forestREplot",
  "Hacquetia epipactis", "Sanicula epipactis", "synonym",
  "Sanicula epipactis in forestREplot",
  "Clinopodium menthifolium subsp. menthifolium", "Clinopodium menthifolium", "broader_name",
  "subspecies not recorded in the plots",
  "Dactylis glomerata subsp. lobata", "Dactylis glomerata", "broader_name",
  paste("subspecies not separated in the plots; the forest subsp. lobata (D. polygama)",
        "is assumed in forest plots, but western records may include subsp. glomerata"),
  "Calamagrostis chalybaea", "Calamagrostis purpurea", "broader_name",
  "C. chalybaea is a northern segregate of the C. purpurea group; 1 plot-survey (EU_045, W Norway)",
  "Senecio hercynicus", "Senecio nemorensis", "broader_name",
  paste("S. nemorensis group; recorded separately from S. ovatus (together in only 2",
        "plot-surveys), Central / E Europe 176-1245 m; S. hercynicus itself not verifiable"),
  "Senecio hercynicus", "Senecio ovatus", "not_used",
  paste("S. ovatus is a separate, widespread species (lowland to montane, 879",
        "plot-surveys); the niche of the montane S. hercynicus would bias its plots cold")
)
manual <- filter(manual, sdm_species %in% sdm_sp)
miss <- setdiff(manual$fr_species, fr_names)
if (length(miss)) warning("Manual forestREplot names not in the data: ", paste(miss, collapse = ", "))

# ---- Exact matches ----
exact <- tibble(sdm_species = sdm_sp) %>%
  mutate(fr_species = fr_names[match(norm_name(sdm_species), norm_name(fr_names))]) %>%
  filter(!is.na(fr_species)) %>%
  mutate(match_type = "exact", note = "")

# ---- Absent: no exact or manual match ----
# Absent species with a specific reason (otherwise the note lists the names of
# the same genus in the plots)
absent_reason <- c(
  "Pyrola chlorantha" = paste(
    "only 2 records in forestREplot v3.1 (EU_010a, 1993 survey), a dataset",
    "excluded as an incomplete duplicate of EU_010b; not found in 1999-2014"
  )
)
genus_note <- function(s) {
  m <- grep(paste0("^", sub(" .*", "", s), " "), fr_names, value = TRUE)
  m <- sort(m[!grepl("UnknownSp", m)])
  if (length(m)) paste("same genus in the plots:", paste(m, collapse = ", ")) else "genus not in the plots"
}
absent <- tibble(sdm_species = setdiff(sdm_sp, c(exact$sdm_species, manual$sdm_species))) %>%
  mutate(fr_species = NA_character_, match_type = "absent",
         note = ifelse(sdm_species %in% names(absent_reason),
                       absent_reason[sdm_species],
                       vapply(sdm_species, genus_note, "")))

# ---- Combine with the number of plots per forestREplot name ----
fr_stats <- sp_veg %>%
  mutate(plotID = sub("_[^_]+$", "", sample), dataset = substr(sample, 1, 6)) %>%
  group_by(fr_species = species_name) %>%
  summarise(n_plot_surveys = n_distinct(sample), n_plots = n_distinct(plotID),
            n_datasets = n_distinct(dataset), .groups = "drop")

sp_match <- bind_rows(exact, manual, absent) %>%
  left_join(fr_stats, by = "fr_species") %>%
  mutate(used_in_cit = match_type %in% c("exact", "synonym", "broader_name")) %>%
  arrange(sdm_species, desc(used_in_cit)) %>%
  dplyr::select(sdm_species, fr_species, match_type, used_in_cit,
                n_plot_surveys, n_plots, n_datasets, note)

# Checks: every SDM species logged; a forestREplot name maps to one SDM species only
stopifnot(setequal(sp_match$sdm_species, sdm_sp))
used <- filter(sp_match, used_in_cit)
stopifnot(!anyDuplicated(used$fr_species))

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
write.csv(sp_match, out_file, row.names = FALSE, na = "")

# ---- Summary ----
cat("SDM species:", length(sdm_sp), "\n")
print(sp_match %>% distinct(sdm_species, .keep_all = TRUE) %>% count(match_type))
cat("SDM species used in the CIT:", n_distinct(used$sdm_species), "\n")

####################################################################################
#################   Appendix table: one row per SDM species  #######################
####################################################################################
appendix_file <- "I:/DATA/output/CIT/AppendixTable_SpeciesMatch_SDM_forestREplot"
min_species <- 3 # as in 07-09: plot-surveys with fewer SDM species get no CIT

# Plot-surveys that get a CIT, and in how many of them each SDM species occurs
veg_sdm <- sp_veg %>%
  inner_join(dplyr::select(used, fr_species, sdm_species), by = c("species_name" = "fr_species")) %>%
  distinct(sample, sdm_species)
cit_samples <- veg_sdm %>% count(sample) %>% filter(n >= min_species) %>% pull(sample)
n_cit <- veg_sdm %>%
  filter(sample %in% cit_samples) %>%
  count(sdm_species, name = "n_cit_plot_surveys")

# Names that were not matched are reported in the note of their SDM species
not_used <- sp_match %>%
  filter(match_type == "not_used") %>%
  group_by(sdm_species) %>%
  summarise(not_used_note = paste0("not matched: ", fr_species, " (", note, ")", collapse = "; "))

type_label <- c(exact = "Exact", synonym = "Synonym", broader_name = "Broader name", absent = "Absent")
appendix <- sp_match %>%
  filter(match_type != "not_used") %>%
  left_join(not_used, by = "sdm_species") %>%
  left_join(n_cit, by = "sdm_species") %>%
  mutate(
    across(c(n_plot_surveys, n_plots, n_datasets, n_cit_plot_surveys), ~ coalesce(as.integer(.x), 0L)),
    note = case_when(
      match_type == "exact" ~ "",
      match_type == "absent" & sdm_species %in% names(absent_reason) ~ note,
      match_type == "absent" ~ "not recorded in the herb or shrub layer of the forestREplot v3.1 datasets used",
      TRUE ~ note
    ),
    note = ifelse(is.na(not_used_note), note, paste0(note, "; ", not_used_note))
  ) %>%
  arrange(sdm_species) %>%
  transmute(
    `Species (SDM)` = sdm_species,
    `Name in forestREplot` = ifelse(match_type %in% c("synonym", "broader_name"), fr_species, ""),
    Match = unname(type_label[match_type]),
    `Plot-surveys` = n_plot_surveys,
    Plots = n_plots,
    Datasets = n_datasets,
    `CIT plot-surveys` = n_cit_plot_surveys,
    Note = note
  )
stopifnot(nrow(appendix) == length(sdm_sp))

# Summary with definitions (metadata for the appendix)
type_def <- c(
  Exact = paste(
    "The SDM species name is identical to the name in forestREplot (ignoring case",
    "and spacing). Used in the CIT."
  ),
  Synonym = paste(
    "Same taxon, recorded in forestREplot under another name because the two sources",
    "follow different nomenclature (e.g. Anemone ranunculoides = Anemonoides",
    "ranunculoides). Used in the CIT."
  ),
  `Broader name` = paste(
    "forestREplot records the taxon at a coarser level (the species or species group",
    "that contains the modelled taxon), e.g. subspecies not separated in the plots",
    "(Dactylis glomerata for D. glomerata subsp. lobata). Matched unless there is clear",
    "evidence of a different taxon. Used in the CIT."
  ),
  Absent = paste(
    "No corresponding name in the herb or shrub layer of the forestREplot v3.1 datasets used (EU_009b and EU_010a excluded as duplicates). Not used in the CIT."
  )
)
appendix_summary <- appendix %>%
  count(Match, name = "N") %>%
  arrange(match(Match, type_label)) %>%
  mutate(Definition = unname(type_def[Match])) %>%
  bind_rows(tibble(
    Match = c("Used in the CIT", "CIT plot-surveys", "CIT plots"),
    N = c(n_distinct(used$sdm_species), length(cit_samples),
          n_distinct(sub("_[^_]+$", "", cit_samples))),
    Definition = c(
      "SDM species matched as Exact, Synonym or Broader name.",
      paste0("Plot-surveys (baseline or resurvey of a plot) with at least ", min_species,
             " matched SDM species in the herb or shrub layer; the CIT is calculated for these."),
      "Plots with at least one CIT plot-survey."
    )
  ))

# Description of the columns of the species table
# (empty middle column: the descriptions line up with the Definition column)
column_def <- tibble(
  Column = names(appendix),
  ` ` = "",
  Description = c(
    "Species name used for the species distribution model (SDM).",
    "Name under which the species is recorded in forestREplot, if it differs (Synonym or Broader name).",
    "Type of name match; see the definitions above.",
    "Number of plot-surveys in forestREplot v3.1 (all surveys) with the species in the herb or shrub layer.",
    "Number of plots with the species in at least one survey.",
    "Number of forestREplot datasets (regional resurvey studies) with the species.",
    paste0("Number of CIT plot-surveys (at least ", min_species, " matched SDM species) with the species."),
    "Reason for a Synonym or Broader name match; names that were considered and not matched."
  )
)

write.csv(appendix, paste0(appendix_file, ".csv"), row.names = FALSE)
if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  bold <- openxlsx::createStyle(textDecoration = "bold")
  wrap <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  openxlsx::addWorksheet(wb, "Species match")
  openxlsx::writeData(wb, "Species match", appendix, headerStyle = bold)
  # species names in italics (header excluded)
  openxlsx::addStyle(wb, "Species match", openxlsx::createStyle(textDecoration = "italic"),
                     rows = 1 + seq_len(nrow(appendix)), cols = 1:2, gridExpand = TRUE)
  openxlsx::setColWidths(wb, "Species match", cols = 1:8, widths = c(42, 26, 13, 12, 7, 9, 16, 90))
  openxlsx::freezePane(wb, "Species match", firstRow = TRUE)

  # Summary sheet: counts with definitions, then the column descriptions
  openxlsx::addWorksheet(wb, "Summary")
  openxlsx::writeData(wb, "Summary", appendix_summary, headerStyle = bold)
  col_row <- nrow(appendix_summary) + 3
  openxlsx::writeData(wb, "Summary", "Columns of the sheet 'Species match'", startRow = col_row)
  openxlsx::addStyle(wb, "Summary", bold, rows = col_row, cols = 1)
  openxlsx::writeData(wb, "Summary", column_def, startRow = col_row + 1, headerStyle = bold)
  openxlsx::addStyle(wb, "Summary", wrap, rows = 1:(col_row + nrow(column_def) + 1), cols = 1:3,
                     gridExpand = TRUE, stack = TRUE)
  openxlsx::setColWidths(wb, "Summary", cols = 1:3, widths = c(22, 8, 100))
  openxlsx::saveWorkbook(wb, paste0(appendix_file, ".xlsx"), overwrite = TRUE)
}
print(as.data.frame(appendix_summary[, c("Match", "N")]))
