# ─────────────────────────────────────────────────────────────
# Llibreries
library(tidyverse)

# ─────────────────────────────────────────────────────────────
# Es llegeix el fitxer CSV
PET2_pib_suvr_merged <- read.csv("path/WRAP_Imaging_PET_2_pib_suvr_merged.csv")

# Es mantenen les columnes d'interés
PET2_pib_suvr_cleaned <- PET2_pib_suvr_merged %>%
  select(subject_id, session_id, age_at_appointment, region, atlas, dvr, roi_volume_cc)

# Es crea una columna per cada combinació de region i atlas per dvr i roi_volume_cc
PET2_pib_suvr_cleaned <- PET2_pib_suvr_cleaned %>%
  pivot_wider(names_from = c(region, atlas), 
              values_from = c(dvr, roi_volume_cc), 
              names_glue = "{region}_{atlas}_{.value}")

# Es netegen les columnes que tenen llistes
PET2_pib_suvr_cleaned <- PET2_pib_suvr_cleaned %>%
  mutate(across(where(is.list), 
                ~ sapply(., function(x) as.numeric(x[1]))))

# S'afegeix el sufix "_PET2" a totes les columnes excepte subject_id i session_id
names(PET2_pib_suvr_cleaned) <- ifelse(names(PET2_pib_suvr_cleaned) %in% c("subject_id", "session_id"),
                             names(PET2_pib_suvr_cleaned),
                             paste0(names(PET2_pib_suvr_cleaned), "_PET2"))

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
output_file <- "path/WRAP_Imaging_PET_2_pib_suvr_clean.csv"
write.csv(PET2_pib_suvr_cleaned, output_file, row.names = FALSE)