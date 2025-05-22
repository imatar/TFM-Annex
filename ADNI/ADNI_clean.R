# ─────────────────────────────────────────────────────────────
# Llibreries
library(dplyr)
library(stringr)

# ─────────────────────────────────────────────────────────────
# Es carrega el dataset
ADNI <- read.csv("path/ADNI_merged.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# S'estandaritzen els valors de GENOTYPE_APOE
ADNI$GENOTYPE_APOE <- gsub("3/3", "E3/E3", ADNI$GENOTYPE_APOE)
ADNI$GENOTYPE_APOE <- gsub("3/4", "E3/E4", ADNI$GENOTYPE_APOE)
ADNI$GENOTYPE_APOE <- gsub("2/3", "E2/E3", ADNI$GENOTYPE_APOE)
ADNI$GENOTYPE_APOE <- gsub("4/4", "E4/E4", ADNI$GENOTYPE_APOE)
ADNI$GENOTYPE_APOE <- gsub("2/4", "E2/E4", ADNI$GENOTYPE_APOE)
ADNI$GENOTYPE_APOE <- gsub("2/2", "E2/E2", ADNI$GENOTYPE_APOE)

# ─────────────────────────────────────────────────────────────
# Es defineixen les variables d'interès
vars_interes <- c(
  "RID",
  "VISCODE_NUM",
  "EXAMDATE",
  "ORIGPROT",
  "AGE_AT_VISIT_ADNI",
  "PTGENDER",
  "PTETHCAT",
  "PTRACCAT",
  "PTEDUCAT",
  "DX_bl",
  "GENOTYPE_APOE",
  "ABETA",
  "PTAU",
  "TAU",
  "NfL_Q_PLASMA_FUJIREBIO",
  "pTau181.Simoa_PTAU_181_PLASMA_PTAU",
  "Roche.Elecsys.plasma.Phospho.Tau.181P._PTAU_181_PLASMA_PTAU",
  "Hippocampus",
  "ICV",
  "SUMMARY_SUVR_UCBERKELEY_AMY",
  "CENTILOIDS_UCBERKELEY_AMY",
  "FDG",
  "Bk12_UCBERKELEY_TAU",
  "Bk34_UCBERKELEY_TAU",
  "Bk56_UCBERKELEY_TAU",
  "MMSE",
  "CDR_global"
)

ADNI_clean <- ADNI %>%
  select(any_of(vars_interes))

# ─────────────────────────────────────────────────────────────
# Es renombren les variables
cols_ADNI <- c(
  "RID" = "Subj_id",
  "VISCODE_NUM" = "Visit",
  "EXAMDATE" = "Visit_date",
  "ORIGPROT" = "Cohort_detail",
  "AGE_AT_VISIT_ADNI" = "Age_at_visit",
  "PTGENDER" = "Gender",
  "PTETHCAT" = "PTEthnicity",
  "PTRACCAT" = "PTRace",
  "PTEDUCAT" = "PTEducation",
  "DX_bl" = "DX_raw",
  "GENOTYPE_APOE" = "APOE",
  "ABETA" = "CSF_AB42",
  "PTAU" = "CSF_pTau",
  "TAU" = "CSF_tTau",
  "NfL_Q_PLASMA_FUJIREBIO" = "Plasma_NfL",
  "pTau181.Simoa_PTAU_181_PLASMA_PTAU" = "Plasma_pTau_Simoa",
  "Roche.Elecsys.plasma.Phospho.Tau.181P._PTAU_181_PLASMA_PTAU" = "Plasma_pTau_Roche",
  "Hippocampus" = "MRI_hippo_vol_total",
  "ICV" = "Total_ICV",
  "SUMMARY_SUVR_UCBERKELEY_AMY" = "SUVR_Amyloid",
  "CENTILOIDS_UCBERKELEY_AMY" = "PET_Centiloid",
  "FDG" = "PET_FDG",
  "Bk12_UCBERKELEY_TAU" = "PET_tau_Bk12",
  "Bk34_UCBERKELEY_TAU" = "PET_tau_Bk34",
  "Bk56_UCBERKELEY_TAU" = "PET_tau_Bk56",
  "MMSE" = "MMSE",
  "CDR_global" = "CDR"
)

ADNI_clean <- ADNI_clean %>%
  rename_with(~ cols_ADNI[.x], .cols = intersect(names(ADNI_clean), names(cols_ADNI)))

ADNI$GENOTYPE_APOE <- gsub("([0-9])/([0-9])", "\\1/\\2", ADNI$GENOTYPE_APOE)

# ─────────────────────────────────────────────────────────────
# Es netegen les observacions buides
cols_dades <- c("Subj_id",
                "Visit",
                "Visit_date",
                "Cohort_detail",
                "Age_at_visit",
                "Gender",
                "PTEthnicity",
                "PTRace",
                "PTEducation",
                "DX_raw",
                "APOE")

all_cols <- colnames(ADNI_clean)
cols_check <- setdiff(all_cols, cols_dades)

is_na_or_empty <- function(x) {
  is.na(x) | (x == "")
}

rows_to_remove <- apply(ADNI_clean[, cols_check, drop=FALSE], 1, function(x) all(is_na_or_empty(x)))

ADNI_clean <- ADNI_clean[!rows_to_remove, ]

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
write.csv(ADNI_clean, "path/ADNI_clean.csv", row.names = FALSE)
