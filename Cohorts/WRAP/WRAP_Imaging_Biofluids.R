# ─────────────────────────────────────────────────────────────
# Llibreries
library(data.table)
library(readr)
library(dplyr)   

# ─────────────────────────────────────────────────────────────
# Funció per netejar duplicats
dedup <- function(dt, nom_fitxer, vars_clau) {
  dt <- as.data.table(dt)
  setkeyv(dt, vars_clau)
  dups <- dt[duplicated(dt, by = key(dt))]
  
  if (nrow(dups) > 0) {
    cat("Duplicats trobats a:", nom_fitxer, "→", nrow(dups), "observacions\n")
  } else {
    cat("Cap duplicat trobat a:", nom_fitxer, "\n")
  }
  
  unique(dt, by = key(dt))
}

# ─────────────────────────────────────────────────────────────
# Es carreguen els datasets
participants <- fread("path/participants.tsv")
plasma_c2n <- fread("path/plasma_c2n_precivity_stlouis.tsv")
plasma_lilly <- fread("path/plasma_lilly_msd_ptau_217_lund.tsv")
plasma_simoa <- fread("path/plasma_simoa_gothenburg.tsv")
csf_ivd2 <- fread("path/csf_roche_elecsys_ivd2_madison.tsv")
csf_ntk1 <- fread("path/csf_roche_elecsys_ntk1_gothenburg.tsv")
csf_ntk2 <- fread("path/csf_roche_elecsys_ntk2_gothenburg.tsv")

# ─────────────────────────────────────────────────────────────
# Es netegen els duplicats
participants <- dedup(participants, "participants", c("subject_id"))
plasma_c2n <- dedup(plasma_c2n, "plasma_c2n", c("subject_id", "age_at_acquisition"))
plasma_lilly <- dedup(plasma_lilly, "plasma_lilly", c("subject_id", "age_at_acquisition"))
plasma_simoa <- dedup(plasma_simoa, "plasma_simoa", c("subject_id", "visno", "age_at_acquisition"))
csf_ivd2 <- dedup(csf_ivd2, "csf_ivd2", c("subject_id", "age_at_acquisition"))
csf_ntk1 <- dedup(csf_ntk1, "csf_ntk1", c("subject_id", "shareable_age_at_appointment"))
csf_ntk2 <- dedup(csf_ntk2, "csf_ntk2", c("subject_id", "age_at_acquisition"))

# ─────────────────────────────────────────────────────────────
# S'afegeixen sufixos per identificar l’origen de cada dataset
add_suffix <- function(dt, suffix, exclude = c("subject_id", "visno")) {
  cols_to_rename <- setdiff(names(dt), exclude)
  setnames(dt, cols_to_rename, paste0(cols_to_rename, suffix))
  dt
}

participants <- add_suffix(participants, "_participants")
plasma_c2n <- add_suffix(plasma_c2n, "_plasma_c2n")
plasma_lilly <- add_suffix(plasma_lilly, "_plasma_lilly")
plasma_simoa <- add_suffix(plasma_simoa, "_plasma_simoa")
csf_ivd2 <- add_suffix(csf_ivd2, "_csf_ivd2")
csf_ntk1 <- add_suffix(csf_ntk1, "_csf_ntk1")
csf_ntk2 <- add_suffix(csf_ntk2, "_csf_ntk2", exclude = c("subject_id", "Visit_Number_csf_ntk2"))

# ─────────────────────────────────────────────────────────────
# Merge 1
merged_1 <- merge(plasma_lilly, plasma_simoa, by = c("subject_id", "visno"), all = TRUE)
merged_1 <- merge(merged_1, csf_ntk2, by.x = c("subject_id", "visno"), by.y = c("subject_id", "Visit_Number_csf_ntk2"), all = TRUE)
merged_1[, age_ref := coalesce(age_at_acquisition_plasma_simoa,
                               age_at_acquisition_plasma_lilly,
                               age_at_acquisition_csf_ntk2)]
merged_1[, row_id := .I]

# ─────────────────────────────────────────────────────────────
# Merge 2
matches_c2n <- merge(
  merged_1[, .(row_id, subject_id, age_ref)],
  plasma_c2n,
  by = "subject_id",
  allow.cartesian = TRUE
)

matches_c2n[, age_diff := abs(age_at_acquisition_plasma_c2n - age_ref)]
matches_c2n <- matches_c2n[age_diff < 1]
matches_c2n <- matches_c2n[order(age_diff), .SD[1], by = row_id]

cols_noves_c2n <- grep("_plasma_c2n$", names(matches_c2n), value = TRUE)
matches_c2n_clean  <- matches_c2n[, c("row_id", cols_noves_c2n), with = FALSE]

merged_2 <- merge(merged_1, matches_c2n_clean, by = "row_id", all.x = TRUE)

# ─────────────────────────────────────────────────────────────
# Merge 3
matches_ivd2 <- merge(
  merged_2[, .(row_id, subject_id, age_ref)],
  csf_ivd2,
  by = "subject_id",
  allow.cartesian = TRUE
)

matches_ivd2[, age_diff := abs(age_at_acquisition_csf_ivd2 - age_ref)]
matches_ivd2 <- matches_ivd2[age_diff < 1]
matches_ivd2 <- matches_ivd2[order(age_diff), .SD[1], by = row_id]

cols_noves_ivd2 <- grep("_csf_ivd2$", names(matches_ivd2), value = TRUE)
matches_ivd2_clean  <- matches_ivd2[, c("row_id", cols_noves_ivd2), with = FALSE]

merged_3 <- merge(merged_2, matches_ivd2_clean, by = "row_id", all.x = TRUE)

# ─────────────────────────────────────────────────────────────
# Merge final
matches_ntk1 <- merge(
  merged_3[, .(row_id, subject_id, age_ref)],
  csf_ntk1,
  by = "subject_id",
  allow.cartesian = TRUE
)

matches_ntk1[, age_diff := abs(shareable_age_at_appointment_csf_ntk1 - age_ref)]
matches_ntk1 <- matches_ntk1[age_diff < 1]
matches_ntk1 <- matches_ntk1[order(age_diff), .SD[1], by = row_id]

cols_noves_ntk1 <- grep("_csf_ntk1$", names(matches_ntk1), value = TRUE)
matches_ntk1_clean  <- matches_ntk1[, c("row_id", cols_noves_ntk1), with = FALSE]

WRAP_Imaging_Biofluids_merged <- merge(merged_3, matches_ntk1_clean, by = "row_id", all.x = TRUE)

# S'afegeix la columna age_bio amb la mitjana de les edats
WRAP_Imaging_Biofluids_merged[, age_bio := rowMeans(
  .SD, na.rm = TRUE
), .SDcols = c("age_at_acquisition_csf_ivd2", 
               "age_at_acquisition_plasma_lilly", 
               "age_at_acquisition_csf_ntk2", 
               "age_at_acquisition_plasma_simoa", 
               "age_at_acquisition_plasma_c2n", 
               "shareable_age_at_appointment_csf_ntk1")]

# ─────────────────────────────────────────────────────────────
# S'eliminen les variables temporals
WRAP_Imaging_Biofluids_merged[, c("age_ref", "row_id") := NULL]

dim(merged_1)
dim(merged_2)
dim(merged_3)
dim(WRAP_Imaging_Biofluids_merged)

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
fwrite(WRAP_Imaging_Biofluids_merged, file = "path/WRAP_Imaging_Biofluids_merged.csv", sep = ",", na = "NA")