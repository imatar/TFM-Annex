# ─────────────────────────────────────────────────────────────
# Llibreries
library(tidyverse)

# ─────────────────────────────────────────────────────────────
# Es llegeix el fitxer CSV
wmhi_merged <- read.csv("path/WRAP_Imaging_MRI_wmhi_merged.csv")

# Es mantenen les columnes d'interés
wmhi_clean <- wmhi_merged %>%
  select(subject_id, session_id, age_at_acquisition, lesion_volume_ml, number_of_lesions,)

# S'afegeix el sufix "_wmhi" a totes les columnes excepte subject_id i session_id
names(wmhi_clean) <- ifelse(names(wmhi_clean) %in% c("subject_id", "session_id"),
                                      names(wmhi_clean),
                                      paste0(names(wmhi_clean), "_wmhi"))

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
output_file <- "path/WRAP_Imaging_MRI_wmhi_clean.csv"
write.csv(wmhi_clean, output_file, row.names = FALSE)