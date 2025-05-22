# ─────────────────────────────────────────────────────────────
# Llibreries
library(data.table)

# ─────────────────────────────────────────────────────────────
# Es llegeixen els datasets
bio <- fread("path/WRAP_Imaging_Biofluids_merged.csv")
MRI_atrophy <- fread("path/WRAP_Imaging_MRI_atrophy_clean.csv")
MRI_hcv <- fread("path/WRAP_Imaging_MRI_hcv_clean.csv")
MRI_wmhi <- fread("path/WRAP_Imaging_MRI_wmhi_clean.csv")
PET <- fread("path/WRAP_Imaging_PET_clean.csv")
PET2_dvr <- fread("path/WRAP_Imaging_PET_2_pib_dvr_clean.csv")
PET2_suvr <- fread("path/WRAP_Imaging_PET_2_pib_suvr_clean.csv")

# S'uneixen els fitxers PET2
PET2 <- merge(PET2_dvr, PET2_suvr, by = c("subject_id", "session_id"), all = TRUE)

cols_x <- grep("\\.x$", names(PET2), value = TRUE)
cols_y <- sub("\\.x$", ".y", cols_x)

for (i in seq_along(cols_x)) {
  var_x <- cols_x[i]
  var_y <- cols_y[i]

  PET2[[var_x]] <- ifelse(is.na(PET2[[var_x]]), PET2[[var_y]], PET2[[var_x]])

  PET2[[var_y]] <- NULL
}

names(PET2) <- gsub("\\.x$", "", names(PET2))

# ─────────────────────────────────────────────────────────────
# Es creen combinacions de subject_id + session_id
merged_0 <- rbindlist(list(
  MRI_atrophy[, .(subject_id, session_id)],
  MRI_hcv[, .(subject_id, session_id)],
  MRI_wmhi[, .(subject_id, session_id)],
  PET[, .(subject_id, session_id)],
  PET2[, .(subject_id, session_id)]
), use.names = TRUE)

merged_0 <- unique(merged_0)
setorder(merged_0, subject_id, session_id)

# ─────────────────────────────────────────────────────────────
# S'uneixen tots els datasets sobre merge 0
merged_1 <- merge(merged_0, MRI_atrophy, by = c("subject_id", "session_id"), all.x = TRUE)
merged_2 <- merge(merged_1, MRI_hcv, by = c("subject_id", "session_id"), all.x = TRUE)
merged_3 <- merge(merged_2, MRI_wmhi, by = c("subject_id", "session_id"), all.x = TRUE)
merged_4 <- merge(merged_3, PET, by = c("subject_id", "session_id"), all.x = TRUE)
merged_5 <- merge(merged_4, PET2, by = c("subject_id", "session_id"), all.x = TRUE)

# ─────────────────────────────────────────────────────────────
# Es calcula age_at_visit_WRAP_Imaging com la mitjana de les columnes age
age_cols <- grep("^age_", names(merged_5), value = TRUE)
merged_5[, age_ref := rowMeans(.SD, na.rm = TRUE), .SDcols = age_cols]
merged_5[, row_id := .I]

# ─────────────────────────────────────────────────────────────
# S'afegeix el dataset bio
matches_bio <- merge(
  merged_5[, .(row_id, subject_id, age_ref)],
  bio,
  by = "subject_id",
  allow.cartesian = TRUE
)

matches_bio[, age_diff := abs(age_bio - age_ref)]
matches_bio <- matches_bio[age_diff < 0.5]
matches_bio <- matches_bio[order(age_diff), .SD[1], by = row_id]

WRAP_Imaging_merged <- merge(merged_5, matches_bio, by = "row_id", all.x = TRUE)

# ─────────────────────────────────────────────────────────────
# S'eliminen les variables temporals
WRAP_Imaging_merged[, c("subject_id.y", "age_ref.y", "age_diff", "row_id") := NULL]

# Es renombren les columnes subject_id.x i age_ref.x
setnames(WRAP_Imaging_merged, old = c("subject_id.x", "age_ref.x"), new = c("subject_id", "age_at_visit_WRAP_Imaging"))

# Es reordenen les columnes del dataset final
age_cols_final <- grep("^age_", names(WRAP_Imaging_merged), value = TRUE)
age_cols_final <- union(setdiff(age_cols_final, "age_at_visit_WRAP_Imaging"), "age_at_visit_WRAP_Imaging")  # garantir que hi sigui

first_cols_final <- c("subject_id", "session_id", "Visit", "age_at_visit_WRAP_Imaging")
other_cols_final <- setdiff(names(WRAP_Imaging_merged), c(first_cols_final, age_cols_final))
ordered_cols <- intersect(c(first_cols_final, age_cols_final, other_cols_final), names(WRAP_Imaging_merged))

setcolorder(WRAP_Imaging_merged, ordered_cols)

# Comprovacions
dim(merged_0)
dim(merged_1)
dim(merged_2)
dim(merged_3)
dim(merged_4)
dim(merged_5)
dim(WRAP_Imaging_merged)

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
fwrite(WRAP_Imaging_merged, "path/WRAP_Imaging_merged.csv")