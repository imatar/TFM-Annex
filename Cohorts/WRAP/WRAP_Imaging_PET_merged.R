# ─────────────────────────────────────────────────────────────
# Llibreries
library(tidyverse)
library(data.table)
library(fs)

# ─────────────────────────────────────────────────────────────
# S'obtenen els fitxers que acaben en "_long.tsv"
base_dir <- "path/mk6240_suvr"
tsv_files <- dir_ls(base_dir, recurse = TRUE, glob = "*.tsv") %>%
  keep(~ grepl("_long\\.tsv$", .))

# ─────────────────────────────────────────────────────────────
# Funció per llegir cada fitxer i afegir info de pacient i sessió
read_tsv_file <- function(file_path) {
  path_parts <- strsplit(file_path, "/")[[1]]
  session_id <- path_parts[length(path_parts) - 1]
  patient_id <- path_parts[length(path_parts) - 2]
  
 df <- fread(file_path, select = c("subject_id", "age_at_appointment", "region", "atlas", "suvr", "roi_volume_cc"), na.strings = c("", "NA"))
  
  df[, `:=`(patient_id = patient_id, session_id = session_id)]
  df[, age_at_appointment := as.numeric(age_at_appointment)]
  
  return(df)
}

# ─────────────────────────────────────────────────────────────
# Es llegeixen i combinen tots els fitxers
WRAP_Imaging_PET_merged <- rbindlist(lapply(tsv_files, read_tsv_file), use.names = TRUE, fill = TRUE)
setcolorder(WRAP_Imaging_PET_merged, c("patient_id", "session_id", "subject_id", "age_at_appointment", "region", "atlas", "suvr", "roi_volume_cc"))

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
output_file <- "path/WRAP_Imaging_PET_merged.csv"
write.csv(WRAP_Imaging_PET_merged, output_file, row.names = FALSE)