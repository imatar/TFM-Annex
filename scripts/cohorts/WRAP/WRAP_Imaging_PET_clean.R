# ─────────────────────────────────────────────────────────────
# Llibreries
library(tidyverse)

# ─────────────────────────────────────────────────────────────
# Es llegeix el fitxer CSV
WRAP_Imaging_PET_merged <- read.csv("path/WRAP_Imaging_PET_merged.csv")

# Es mantenen les columnes d'interés
WRAP_Imaging_PET_cleaned <- WRAP_Imaging_PET_merged %>%
  select(subject_id, session_id, age_at_appointment, region, atlas, suvr, roi_volume_cc)

# Es crea una columna per cada combinació de region i atlas per suvr i roi_volume_cc
WRAP_Imaging_PET_cleaned <- WRAP_Imaging_PET_cleaned %>%
  pivot_wider(names_from = c(region, atlas), 
              values_from = c(suvr, roi_volume_cc), 
              names_glue = "{region}_{atlas}_{.value}")

# Es netegen les columnes que tenen llistes
WRAP_Imaging_PET_cleaned <- WRAP_Imaging_PET_cleaned %>%
  mutate(across(where(is.list), 
                ~ sapply(., function(x) as.numeric(x[1]))))

# S'afegeix el sufix "_hcvi" a totes les columnes excepte subject_id i session_idd
names(WRAP_Imaging_PET_cleaned) <- ifelse(names(WRAP_Imaging_PET_cleaned) %in% c("subject_id", "session_id"),
                             names(WRAP_Imaging_PET_cleaned),
                             paste0(names(WRAP_Imaging_PET_cleaned), "_PET"))

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
output_file <- "path/WRAP_Imaging_PET_clean.csv"
write.csv(WRAP_Imaging_PET_cleaned, output_file, row.names = FALSE)