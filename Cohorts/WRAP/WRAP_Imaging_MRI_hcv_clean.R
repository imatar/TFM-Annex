# ─────────────────────────────────────────────────────────────
# Llibreries
library(tidyverse)

# ─────────────────────────────────────────────────────────────
# Es llegeix el fitxer CSV
hcv_merged <- read.csv("path/WRAP_Imaging_MRI_hcv_merged.csv")

# Es mantenen les columnes d'interés
hcv_clean <- hcv_merged %>%
  select(subject_id, session_id, age_at_acquisition, region, roi_volume_mm3)

# Es crea una columna per cada combinació de region i roi_volume_mm3
hcv_clean <- hcv_clean %>%
  pivot_wider(names_from = region, 
              values_from = roi_volume_mm3, 
              names_glue = "{region}_roi_volume_cc")

# Es netegen les columnes que tenen llistes
hcv_clean <- hcv_clean %>%
  mutate(across(where(is.list), 
                ~ sapply(., function(x) as.numeric(x[1]))))

# S'afegeix el sufix "_hcvi" a totes les columnes excepte subject_id i session_id
names(hcv_clean) <- ifelse(names(hcv_clean) %in% c("subject_id", "session_id"),
                            names(hcv_clean),
                            paste0(names(hcv_clean), "_hcv"))

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
output_file <- "path/WRAP_Imaging_MRI_hcv_clean.csv"
write.csv(hcv_clean, output_file, row.names = FALSE)