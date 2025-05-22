# ─────────────────────────────────────────────────────────────
# Llibreries
library(tidyverse)
library(data.table)
library(fs)

# ─────────────────────────────────────────────────────────────
# S'obtenen els fitxers que acaben en "_hcv.tsv"
base_dir <- "path/atrophy"
tsv_files <- dir_ls(base_dir, recurse = TRUE, glob = "*.tsv") %>%
  keep(~ grepl("_atrophy\\.tsv$", .))

# ─────────────────────────────────────────────────────────────
# Funció per llegir cada fitxer i afegir info de pacient i sessió
read_tsv_file <- function(file_path) {
  # Es comprova si el fitxer té més d'una línia
  if (length(readLines(file_path, n = 2)) < 2) {
    message("Fitxer omès per contenir menys de dues línies: ", file_path)
    return(NULL)
  }
  
  path_parts <- strsplit(file_path, "/")[[1]]
  session_id <- path_parts[length(path_parts) - 1]
  patient_id <- path_parts[length(path_parts) - 2]
  
  df <- fread(
    file_path,
    skip = 1,            # es salta la primera línia (header incorrecte)
    header = FALSE,
    na.strings = c("", "NA")
  )
  
  # Comprovació
  if (ncol(df) != 5) {
    message("Fitxer amb columnes inesperades: ", file_path)
    return(NULL)
  }
  
  setnames(df, c("subject_id", "roi_volume_mm3_1", "roi_volume_mm3_2", "roi_volume_mm3_3", "age_at_acquisition"))
  df[, `:=`(patient_id = patient_id, session_id = session_id)]
  df[, age_at_acquisition := as.numeric(age_at_acquisition)]
  
  return(df)
}


# ─────────────────────────────────────────────────────────────
# Es llegeixen i combinen tots els fitxers
WRAP_Imaging_MRI_atrophy_merged <- rbindlist(lapply(tsv_files, read_tsv_file), use.names = TRUE, fill = TRUE)
setcolorder(WRAP_Imaging_MRI_atrophy_merged, c("patient_id", "session_id", "subject_id", "roi_volume_mm3_1", "roi_volume_mm3_2","roi_volume_mm3_3","age_at_acquisition"))

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
output_file <- "path/WRAP_Imaging_MRI_atrophy_merged.csv"
write.csv(WRAP_Imaging_MRI_atrophy_merged, output_file, row.names = FALSE)