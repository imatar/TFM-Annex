# ─────────────────────────────────────────────────────────────
# Llibreries
library(data.table)

# ─────────────────────────────────────────────────────────────
# Es carrega el dataset
OASIS3 <- read.csv("path/OASIS3_merged.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Es crea la variable VISIT
setDT(OASIS3)
setorder(OASIS3, OASISID, days_to_visit_health)
OASIS3[, VISIT := NA_character_]  # Inicialitzar la columna com a caràcter
OASIS3[!is.na(days_to_visit_health), VISIT := paste0("V", seq_len(.N)), by = OASISID]

# ─────────────────────────────────────────────────────────────
# Es defineixen les variables d'interès
vars_interes <- c(
  "OASISID",
  "VISIT",
  "days_to_visit_health",
  "Project_PUP",
  "age.at.visit_health",
  "GENDER_demo",
  "ETHNIC_demo",
  "race_demo",
  "EDUC_demo",
  "dx1_UDSb4",
  "APOE_demo",
  "days_to_visit_FS",
  "TOTAL_HIPPOCAMPUS_VOLUME_FS",
  "IntraCranialVol_FS",
  "days_to_visit_amyloid",
  "Centiloid_fSUVR_TOT_CORTMEAN_amyloid",
  "Bk12_PUP",
  "Bk34_PUP",
  "Bk56_PUP",
  "MMSE_UDSb4",
  "CDRTOT_UDSb4"
)

OASIS3_clean <- OASIS3 %>%
  select(any_of(vars_interes))

# ─────────────────────────────────────────────────────────────
# Es renombren les variables
cols_OASIS3 <- c(
  "OASISID" = "Subj_id",
  "VISIT" = "Visit",
  "days_to_visit_health" = "Visit_days",
  "Project_PUP" = "Cohort_detail",
  "age.at.visit_health" = "Age_at_visit",
  "GENDER_demo" = "Gender",
  "ETHNIC_demo" = "Ethnicity",
  "race_demo" = "Race",
  "EDUC_demo" = "Education",
  "dx1_UDSb4" = "DX_raw",
  "APOE_demo" = "APOE",
  "days_to_visit_FS" = "MRI_days",
  "TOTAL_HIPPOCAMPUS_VOLUME_FS" = "MRI_hippo_vol_total",
  "IntraCranialVol_FS" = "Total_ICV",
  "days_to_visit_amyloid" = "PET_amyloid_days",
  "Centiloid_fSUVR_TOT_CORTMEAN_amyloid" = "PET_Centiloid",
  "Bk12_PUP" = "PET_tau_Bk12",
  "Bk34_PUP" = "PET_tau_Bk34",
  "Bk56_PUP" = "PET_tau_Bk56",
  "MMSE_UDSb4" = "MMSE",
  "CDRTOT_UDSb4" = "CDR"
)

OASIS3_clean <- OASIS3_clean %>%
  rename_with(~ cols_OASIS3[.x], .cols = intersect(names(OASIS3_clean), names(cols_OASIS3)))

# ─────────────────────────────────────────────────────────────
# Es netegen les observacions buides
cols_dades <- c("Subj_id",
                "Visit",
                "Visit_days",
                "Cohort_detail",
                "Age_at_visit",
                "Gender",
                "Ethnicity",
                "Race",
                "Education",
                "DX_raw",
                "APOE")

all_cols <- colnames(OASIS3_clean)
cols_check <- setdiff(all_cols, cols_dades)

is_na_or_empty <- function(x) {
  is.na(x) | (x == "")
}

rows_to_remove <- apply(OASIS3_clean[, ..cols_check], 1, function(x) all(is_na_or_empty(x)))

OASIS3_clean <- OASIS3_clean[!rows_to_remove, ]

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
write.csv(OASIS3_clean, "path/OASIS3_clean.csv", row.names = FALSE)
