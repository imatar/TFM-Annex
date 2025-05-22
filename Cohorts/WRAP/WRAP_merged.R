# ─────────────────────────────────────────────────────────────
# Llibreries
library(data.table)

# ─────────────────────────────────────────────────────────────
# Es llegeixen els datasets
WRAP_Freeze_merged <- fread("path/WRAP_Freeze_merged.csv")
WRAP_Imaging_merged <- fread("path/WRAP_Imaging_merged.csv")

# S'elimina el prefix "sub-wrap" a WRAPNo
WRAP_Imaging_merged[, WRAPNo := sub("sub-wrap", "", subject_id)]

# ─────────────────────────────────────────────────────────────
# Match per edat < 0.5 anys
WRAP_Freeze_merged[, age_ref := age_at_visit_WRAP_Freeze]
WRAP_Freeze_merged[, row_id := .I]

matches <- merge(
  WRAP_Freeze_merged[, .(row_id, WRAPNo, age_ref)],
  WRAP_Imaging_merged,
  by = "WRAPNo",
  allow.cartesian = TRUE
)

matches[, age_diff := abs(age_at_visit_WRAP_Imaging - age_ref)]
matches <- matches[age_diff < 0.5]
matches <- matches[order(age_diff), .SD[1], by = row_id]

WRAP_merged <- merge(WRAP_Freeze_merged, matches, by = "row_id", all.x = TRUE)

# ─────────────────────────────────────────────────────────────
# S'elimina la columna WRAPNo.y i renombrar WRAPNo.x
WRAP_merged[, WRAPNo.y := NULL]
setnames(WRAP_merged, "WRAPNo.x", "WRAPNo")

# ─────────────────────────────────────────────────────────────
# Comprovacions
dim(WRAP_Freeze_merged)
dim(WRAP_Imaging_merged)
dim(WRAP_merged)
sum(!is.na(WRAP_merged$session_id))
sum(!is.na(WRAP_Imaging_merged$session_id))

# ─────────────────────────────────────────────────────────────
# S'identifiquen les combinacions de WRAPNo i session_id que ja existeixen a WRAP_merged
existing_combinations <- WRAP_merged[, .(WRAPNo, session_id)]

# S'afegeix una columna a WRAP_Imaging_merged per identificar si la combinació WRAPNo i session_id ja es troba a WRAP_merged
WRAP_Imaging_merged[, WRAP_merged_flag := ifelse(
  paste(WRAPNo, session_id) %in% paste(existing_combinations$WRAPNo, existing_combinations$session_id),
  "Sí", "No"
)]

# Es filtren les observacions de WRAP_Imaging_merged que no es troben a WRAP_merged
missing_combinations <- WRAP_Imaging_merged[WRAP_Imaging_merged$WRAP_merged_flag == "No"]

# S'afegeixen les observacions mancants a WRAP_merged
WRAP_merged <- rbindlist(list(WRAP_merged, missing_combinations), use.names = TRUE, fill = TRUE)

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
fwrite(WRAP_merged, "path/WRAP_merged.csv")