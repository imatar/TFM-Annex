# ─────────────────────────────────────────────────────────────
# Llibreries
library(tidyverse)
library(dplyr)

# ─────────────────────────────────────────────────────────────
# Es llegeix el fitxer CSV
atrophy_merged <- read.csv("path/WRAP_Imaging_MRI_atrophy_merged.csv")

# S'eliminen els duplicats
duplicated_rows <- duplicated(atrophy_merged)
atrophy_merged <- atrophy_merged[!duplicated_rows, ]

# Es calcula roi_volume_mm3
atrophy_clean <- atrophy_merged %>%
  mutate(ICV_roi_volume_mm3 = roi_volume_mm3_1 + roi_volume_mm3_2 + roi_volume_mm3_3) %>%
  select(-patient_id,-roi_volume_mm3_1, -roi_volume_mm3_2, -roi_volume_mm3_3)

# S'afegeix el sufix "atrophy" a totes les columnes excepte subject_id i session_id
names(atrophy_clean) <- ifelse(names(atrophy_clean) %in% c("subject_id", "session_id"),
                            names(atrophy_clean),
                            paste0(names(atrophy_clean), "_atrophy"))

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
output_file <- "path/WRAP_Imaging_MRI_atrophy_clean.csv"
write.csv(atrophy_clean, output_file, row.names = FALSE)