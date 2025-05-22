# ─────────────────────────────────────────────────────────────
# Llibreries
library(dplyr)
library(data.table)

# ─────────────────────────────────────────────────────────────
# Es carrega el dataset
Alfa <- read.csv("path/Sant_Pau_Fortea_APOE_merged.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Comprovacions
sum(!is.na(Alfa$Project_ATN))
unique(Alfa$Project_ATN)
sum(!is.na(Alfa$Project_PET))
unique(Alfa$Project_PET)
sum(!is.na(Alfa$Project_Bio))
unique(Alfa$Project_Bio)
sum(!is.na(Alfa$Proyecto_DS))
unique(Alfa$Proyecto_DS)

# Es crea la variable Hippo
Alfa$Hippo_mm3_Imagen <- Alfa$L_Hippo_mm3_Imagen + Alfa$R_Hippo_mm3_Imagen

# ─────────────────────────────────────────────────────────────
# Es defineixen les variables d'interès
vars_interes <- c(
  "id_pseudonym",
  "DOB_Alfa",
  "Visit",
  "fs_dt_PACC",
  "Project_PET",
  "Age_at_visit_Alfa",
  "F06_2_013_DS",
  "F01_010B_DS",
  "DX_raw",
  "APOE_final_APOE",
  "collection_dt_Abeta1.42_CSF_Bio",
  "Result_Abeta1.42_CSF_Bio",
  "pTau181_CSF_Roche_Elecsys_ATN",
  "tTau_CSF_Roche_Elecsys_ATN",
  "Result_NFL_CSF_Bio",
  "Result_NFL_PLASMA_Bio",
  "Result_pTau181_PLASMA_Bio",
  "Hippo_mm3_Imagen",
  "eTIV_mm3_Imagen",
  "Image_test_date_PET",
  "amyloid_PET_Centiloid_Imagen",
  "FDG_SUVR_Landau_Imagen",
  "F12_1_028C_DS",
  "F11_016C_DS"
)

Alfa_clean <- Alfa %>%
  select(any_of(vars_interes))

# ─────────────────────────────────────────────────────────────
# Es renombren les variables
cols_Alfa <- c(
  "id_pseudonym" = "Subj_id",
  "DOB_Alfa" = "DOB",
  "Visit" = "Visit",
  "fs_dt_PACC" = "Visit_date",
  "Project_PET" = "Cohort_detail",
  "Age_at_visit_Alfa" = "Age_at_visit",
  "F06_2_013_DS" = "Sex",
  "F01_010B_DS" = "Education",
  "DX_raw" = "DX_raw",
  "APOE_final_APOE" = "APOE",
  "collection_dt_Abeta1.42_CSF_Bio" = "CSF_date",
  "Result_Abeta1.42_CSF_Bio" = "CSF_AB42",
  "pTau181_CSF_Roche_Elecsys_ATN" = "CSF_pTau",
  "tTau_CSF_Roche_Elecsys_ATN" = "CSF_tTau",
  "Result_NFL_CSF_Bio" = "CSF_NfL",
  "Result_NFL_PLASMA_Bio" = "Plasma_NfL",
  "Result_pTau181_PLASMA_Bio" = "Plasma_pTau_Simoa",
  "Hippo_mm3_Imagen" = "MRI_hippo_vol_total",
  "eTIV_mm3_Imagen" = "Total_ICV",
  "Image_test_date_PET" = "PET_amyloid_date",
  "amyloid_PET_Centiloid_Imagen" = "PET_Centiloid",
  "FDG_SUVR_Landau_Imagen" = "PET_FDG",
  "F12_1_028C_DS" = "MMSE",
  "F11_016C_DS" = "CDR"
)

Alfa_clean <- Alfa_clean %>%
  rename_with(~ cols_Alfa[.x], .cols = intersect(names(Alfa_clean), names(cols_Alfa)))

# ─────────────────────────────────────────────────────────────
# Es netegen les observacions buides
cols_dades <- c("Subj_id",
                "DOB",
                "Visit",
                "Visit_date",
                "Cohort_detail",
                "Age_at_visit",
                "Sex",
                "Education",
                "DX_raw",
                "APOE")

all_cols <- colnames(Alfa_clean)
cols_check <- setdiff(all_cols, cols_dades)

is_na_or_empty <- function(x) {
  is.na(x) | (x == "")
}

rows_to_remove <- apply(Alfa_clean[, cols_check, drop=FALSE], 1, function(x) all(is_na_or_empty(x)))

Alfa_clean <- Alfa_clean[!rows_to_remove, ]

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
write.csv(Alfa_clean, "path/Alfa_clean.csv", row.names = FALSE)
