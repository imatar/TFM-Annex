# ─────────────────────────────────────────────────────────────
# Llibreries
library(dplyr)

# ─────────────────────────────────────────────────────────────
# Es carrega el dataset
A4 <- read.csv("path/A4_merged.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# ─────────────────────────────────────────────────────────────
# Es calgula CDR_global
A4$CDR_global_A4 <- with(A4,
                         ifelse(CDRSB_cdr == 0, 0,
                                ifelse(CDRSB_cdr > 0 & CDRSB_cdr <= 4.0, 0.5,
                                       ifelse(CDRSB_cdr > 4.0 & CDRSB_cdr <= 9.0, 1.0,
                                              ifelse(CDRSB_cdr > 9.0 & CDRSB_cdr <= 15.5, 2.0,
                                                     ifelse(CDRSB_cdr > 15.5, 3.0, NA))))))

# Es crea la variable Hippo
A4$Hippocampus_MRI <- A4$LeftHippocampus_MRI + A4$RightHippocampus_MRI

# DX_raw_A4
A4$DX_raw_A4[A4$VISCODE == "1"] <- "CN"

idx_na <- is.na(A4$DX_raw_A4)
A4$DX_raw_A4[idx_na & A4$CDR_global_A4 == 0] <- "CN"
A4$DX_raw_A4[idx_na & A4$CDR_global_A4 == 0.5] <- "MCI"
A4$DX_raw_A4[idx_na & A4$CDR_global_A4 >= 1 & A4$CDR_global_A4 <= 3] <- "Dementia"

# ─────────────────────────────────────────────────────────────
# Es defineixen les variables d'interès
vars_interes <- c(
  "BID",
  "TX_SubjInfo",
  "VISCODE_group",
  "SUBSTUDY_SV",
  "AGE_AT_VISIT_A4",
  "PTGENDER_PtDemog",
  "SEX_SubjInfo",
  "ETHNIC_SubjInfo",
  "PTETHNIC_PtDemog",
  "RACE_SubjInfo",
  "PTRACE_PtDemog",
  "EDCCNTU_SubjInfo",  
  "PTEDUCAT_PtDemog",
  "DX_raw_A4",
  "APOEGN_SubjInfo",
  "BLOOD_ELECTROCHEMILUMINESCENCE.IMMUNOASSAY_NF.L._Plasma.Neurofilament.Light.Chain_bio_Plasma",
  "BLOOD_ELECTROCHEMILUMINESCENCE.IMMUNOASSAY_TPP181_Plasma.pTauC2_bio_Plasma",
  "Date_DAYS_T0_MRI",
  "Hippocampus_MRI",
  "IntraCranialVolume_MRI",
  "Florbetapir_Composite_Summary_SUVR_amyloid",
  "AMYLCENT_SubjInfo",
  "Bk12_PetSurfer",
  "Bk34_PetSurfer",
  "Bk56_PetSurfer",
  "MMSCORE_MMSE",
  "CDR_global_A4"
)

A4_clean <- A4 %>%
  select(any_of(vars_interes))

# ─────────────────────────────────────────────────────────────
# Es renombren les variables
cols_A4 <- c(
  "BID" = "Subj_id",
  "TX_SubjInfo" = "Treatment",
  "VISCODE_group" = "Visit",
  "SUBSTUDY_SV" = "Cohort_detail",
  "AGE_AT_VISIT_A4" = "Age_at_visit",
  "PTGENDER_PtDemog" = "Gender",
  "SEX_SubjInfo" = "Sex",
  "ETHNIC_SubjInfo" = "Ethnicity",
  "PTETHNIC_PtDemog" = "PTEthnicity",
  "RACE_SubjInfo" = "Race",
  "PTRACE_PtDemog" = "PTRace",
  "EDCCNTU_SubjInfo" = "Education",  
  "PTEDUCAT_PtDemog" = "PTEducation",
  "DX_raw_A4" = "DX_raw",
  "APOEGN_SubjInfo" = "APOE",
  "BLOOD_ELECTROCHEMILUMINESCENCE.IMMUNOASSAY_NF.L._Plasma.Neurofilament.Light.Chain_bio_Plasma" = "Plasma_NfL",
  "BLOOD_ELECTROCHEMILUMINESCENCE.IMMUNOASSAY_TPP181_Plasma.pTauC2_bio_Plasma" = "Plasma_pTau_Roche",
  "Date_DAYS_T0_MRI" = "MRI_days",
  "Hippocampus_MRI" = "MRI_hippo_vol_total",
  "IntraCranialVolume_MRI" = "Total_ICV",
  "Florbetapir_Composite_Summary_SUVR_amyloid" = "SUVR_Amyloid",
  "AMYLCENT_SubjInfo" = "PET_Centiloid",
  "Bk12_PetSurfer" = "PET_tau_Bk12",
  "Bk34_PetSurfer" = "PET_tau_Bk34",
  "Bk56_PetSurfer" = "PET_tau_Bk56",
  "MMSCORE_MMSE" = "MMSE",
  "CDR_global_A4" = "CDR"
)

A4_clean <- A4_clean %>%
  rename_with(~ cols_A4[.x], .cols = intersect(names(A4_clean), names(cols_A4)))
# ─────────────────────────────────────────────────────────────
# Es netegen les observacions buides
cols_dades <- c("Subj_id",
                "Treatment",
                "Visit",
                "Cohort_detail",
                "Age_at_visit",
                "Gender",
                "Sex",
                "Ethnicity",
                "PTEthnicity",
                "Race",
                "PTRace",
                "Education",  
                "PTEducation",
                "APOE")

all_cols <- colnames(A4_clean)
cols_check <- setdiff(all_cols, cols_dades)

is_na_or_empty <- function(x) {
  is.na(x) | (x == "")
}

rows_to_remove <- apply(A4_clean[, cols_check, drop=FALSE], 1, function(x) all(is_na_or_empty(x)))

A4_clean <- A4_clean[!rows_to_remove, ]

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
write.csv(A4_clean, "path/A4_clean.csv", row.names = FALSE)
