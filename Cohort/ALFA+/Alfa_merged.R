# ─────────────────────────────────────────────────────────────
# Llibreries
library(dplyr)
library(data.table)

# ─────────────────────────────────────────────────────────────
# Es llegeixen tots els fitxers CSV
Biomarkers_ATN <- read.csv("path/Sant_Pau_Fortea_APOE__Biomarkers_ATN_20241219204601_pseudonymized.csv", sep=";", stringsAsFactors = FALSE)
DS_Custom <- read.csv("path/Sant_Pau_Fortea_APOE__DS_Custom_20241219204601_pseudonymized.csv", sep=";", stringsAsFactors = FALSE)
Imagen <- read.csv("path/Sant_Pau_Fortea_APOE__Imagen_20241218_pseudonymized.csv", sep=";", stringsAsFactors = FALSE)
PACC_v1_3 <- read.csv("path/Sant_Pau_Fortea_APOE__PACC_v1_3_20241219204601_pseudonymized.csv", sep=";", stringsAsFactors = FALSE)
PET_Amyloid_VR_Global <- read.csv("path/Sant_Pau_Fortea_APOE__PET_Amyloid_VR_Global_20241219204601_pseudonymized.csv", sep=";", stringsAsFactors = FALSE)
APOE <- read.csv("path/Sant_Pau_Fortea_APOE__APOE_20241219204601_pseudonymized.csv", sep=";", stringsAsFactors = FALSE)
Biomarkers <- read.csv("path/Sant_Pau_Fortea_APOE__Biomarkers_20241219204601_pseudonymized.csv", sep=";", stringsAsFactors = FALSE)

# ─────────────────────────────────────────────────────────────
# Es normalitzen els noms de les columnes de Visit
DS_Custom <- DS_Custom %>%
  rename(Visit = Visita)

PACC_v1_3 <- PACC_v1_3 %>%
  rename(Visit = VISIT)

# Es canvia BSL per V2 al dataset PET_Amyloid_VR_Global
PET_Amyloid_VR_Global <- PET_Amyloid_VR_Global %>%
  mutate(Visit = ifelse(Visit == "BSL", "V2", Visit))

# ─────────────────────────────────────────────────────────────
# Se separen les dades del dataset Imagen en V1 i V2
Imagen_V1 <- Imagen %>%
  select(id_pseudonym, DOB, contains("_BL")) %>%
  rename_with(~ gsub("_BL", "", .x)) %>%
  mutate(Visit = "V1")

Imagen_V2 <- Imagen %>%
  select(id_pseudonym, DOB, contains("_FU")) %>%
  rename_with(~ gsub("_FU", "", .x)) %>%
  mutate(Visit = "V2")

Imagen_clean <- bind_rows(Imagen_V1, Imagen_V2)

# ─────────────────────────────────────────────────────────────
# S'agrupen les dades del dataset Biomarkers segons Assay i SampleType
setDT(Biomarkers)
Biomarkers_clean <- dcast(Biomarkers, 
                          id_pseudonym + Visit ~ Assay + SampleType, 
                          value.var = c("collection_dt", "fasting", "Centrifuged", "Result", "ResultRecalc", 
                                        "ResultU", "LabResC", "AnaDt", "LbNam", 
                                        "Platform", "Kit", "Company", "LbMethod", 
                                        "Dilution", "comment"),
                          fun.aggregate = function(x) paste(unique(x), collapse = ", "))

# ─────────────────────────────────────────────────────────────
# S'afegeixen sufixos a cada dataset
add_suffix <- function(df, suffix) {
  df %>%
    rename_with(~ ifelse(.x %in% c("id_pseudonym", "Visit"), .x, paste0(.x, "_", suffix)))
}

Biomarkers_ATN <- add_suffix(Biomarkers_ATN, "ATN")
DS_Custom <- add_suffix(DS_Custom, "DS")
Imagen_clean <- add_suffix(Imagen_clean, "Imagen")
PACC_v1_3 <- add_suffix(PACC_v1_3, "PACC")
PET_Amyloid_VR_Global <- add_suffix(PET_Amyloid_VR_Global, "PET")
APOE <- add_suffix(APOE, "APOE")
Biomarkers_clean <- add_suffix(Biomarkers_clean, "Bio")

# ─────────────────────────────────────────────────────────────
# S'uneixen tots els datasets
merged_1 <- full_join(DS_Custom, PACC_v1_3, by = c("id_pseudonym", "Visit"))
merged_2 <- full_join(merged_1, Imagen_clean, by = c("id_pseudonym", "Visit"))
merged_3 <- full_join(merged_2, Biomarkers_clean, by = c("id_pseudonym", "Visit"))
merged_4 <- full_join(merged_3, Biomarkers_ATN, by = c("id_pseudonym", "Visit"))
merged_5 <- full_join(merged_4, PET_Amyloid_VR_Global, by = c("id_pseudonym", "Visit"))
Alfa_merged <- full_join(merged_5, APOE, by = "id_pseudonym")

# ─────────────────────────────────────────────────────────────
# Es calcula l'edat en la visita
Alfa_merged$glb_DOB_DEMOG_DS <- as.Date(Alfa_merged$glb_DOB_DEMOG_DS, format = "%d/%m/%Y")
Alfa_merged$DOB_Imagen <- as.Date(Alfa_merged$DOB_Imagen, format = "%d/%m/%Y")
Alfa_merged$fcsrti_dt_PACC <- as.Date(Alfa_merged$fcsrti_dt_PACC, format = "%d/%m/%Y")

Alfa_merged <- Alfa_merged %>%
  mutate(DOB_Alfa = coalesce(glb_DOB_DEMOG_DS, DOB_Imagen))

Alfa_merged$Age_at_visit_Alfa <- as.numeric(
  difftime(Alfa_merged$fcsrti_dt_PACC, Alfa_merged$DOB_Alfa, units = "days")
) / 365.25

Alfa_merged$DX_raw <- "CN"

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
write.csv(Alfa_merged, "path/Sant_Pau_Fortea_APOE_merged.csv", row.names = FALSE)
