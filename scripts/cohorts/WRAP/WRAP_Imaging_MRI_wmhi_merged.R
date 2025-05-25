# ─────────────────────────────────────────────────────────────
# Llibreries
library(tidyverse)
library(data.table)
library(fs)

# ─────────────────────────────────────────────────────────────
# S'obtenen els fitxers que acaben en "_wmhi.tsv"
base_dir <- "path/wmhi"
tsv_files <- dir_ls(base_dir, recurse = TRUE, glob = "*.tsv") %>%
  keep(~ grepl("_wmhi\\.tsv$", .))

# ─────────────────────────────────────────────────────────────
# Funció per llegir cada fitxer i afegir info de pacient i sessió
read_tsv_file <- function(file_path) {
  path_parts <- strsplit(file_path, "/")[[1]]
  session_id <- path_parts[length(path_parts) - 1]
  patient_id <- path_parts[length(path_parts) - 2]
  
  df <- fread(file_path, select = c("subject_id", "lesion_volume_ml", "number_of_lesions", "age_at_acquisition"), na.strings = c("", "NA"))
  
  df[, `:=`(patient_id = patient_id, session_id = session_id)]
  df[, age_at_acquisition := as.numeric(age_at_acquisition)]
  
  return(df)
}

# ─────────────────────────────────────────────────────────────
# Es llegeixen i combinen tots els fitxers
WRAP_Imaging_MRI_wmhi_merged <- rbindlist(lapply(tsv_files, read_tsv_file), use.names = TRUE, fill = TRUE)
setcolorder(WRAP_Imaging_MRI_wmhi_merged, c("patient_id", "session_id", "subject_id", "lesion_volume_ml", "number_of_lesions", "age_at_acquisition"))

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
output_file <- "path/WRAP_Imaging_MRI_wmhi_merged.csv"
write.csv(WRAP_Imaging_MRI_wmhi_merged, output_file, row.names = FALSE)