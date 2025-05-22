# ─────────────────────────────────────────────────────────────
# Es carrega el dataset
WRAP <- read.csv("path/WRAP_merged.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Es crea la variable APOE, hippocampus i icv
WRAP$APOE <- ifelse(
  is.na(WRAP$all1_APG) | is.na(WRAP$all2_APG), 
  NA, 
  paste0("E", WRAP$all1_APG, "/E", WRAP$all2_APG)
)
WRAP$hippocampus_homic_roi_volume_cc_PET <- WRAP$hippocampus_l_homic_roi_volume_cc_PET + WRAP$hippocampus_r_homic_roi_volume_cc_PET
WRAP <- WRAP %>%
  mutate(age_at_visit = coalesce(age_at_visit_WRAP_Freeze, age_at_visit_WRAP_Imaging))

# ─────────────────────────────────────────────────────────────
# Es defineixen les variables d'interès
vars_interes <- c(
  "WRAPNo",
  "VisNo",
  "data_source_csf_ivd2",
  "age_at_visit",
  "gender_Demographics",
  "race1_Demographics",
  "EducYrs_Demographics",
  "Calculated_Consensus_dx_ConsensusConference",
  "APOE",
  "ABeta_1_42_csf_ntk1",
  "pTau_csf_ntk1",
  "tTau_csf_ntk1",
  "NFL_csf_ntk1",
  "NFL_plasma_simoa",
  "hippocampus_homic_roi_volume_cc_PET",
  "ICV_roi_volume_mm3_atrophy",
  "mmsetot_CompositeScoreCalcValues",
  "CDRRating_CDR"
)

WRAP_clean <- WRAP %>%
  select(any_of(vars_interes))

# ─────────────────────────────────────────────────────────────
# Es renombren les variables
cols_WRAP <- c(
  "WRAPNo" = "Subj_id",
  "VisNo" = "Visit",
  "data_source_csf_ivd2" = "Cohort_detail",
  "age_at_visit" = "Age_at_visit",
  "gender_Demographics" = "Gender",
  "race1_Demographics" = "Race",
  "EducYrs_Demographics" = "Education",
  "Calculated_Consensus_dx_ConsensusConference" = "DX_raw",
  "APOE" = "APOE",
  "ABeta_1_42_csf_ntk1" = "CSF_AB42",
  "pTau_csf_ntk1" = "CSF_pTau",
  "tTau_csf_ntk1" = "CSF_tTau",
  "NFL_csf_ntk1" = "CSF_NfL",
  "NFL_plasma_simoa" = "Plasma_NfL",
  "hippocampus_homic_roi_volume_cc_PET" = "MRI_hippo_vol_total",
  "ICV_roi_volume_mm3_atrophy" = "Total_ICV",
  "mmsetot_CompositeScoreCalcValues" = "MMSE",
  "CDRRating_CDR" = "CDR"
)

WRAP_clean <- WRAP_clean %>%
  rename_with(~ cols_WRAP[.x], .cols = intersect(names(WRAP_clean), names(cols_WRAP)))

# ─────────────────────────────────────────────────────────────
# Es netegen les observacions buides
cols_dades <- c("Subj_id",
                "Visit",
                "Cohort_detail",
                "Age_at_visit",
                "Gender",
                "Race",
                "Education",
                "DX_raw",
                "APOE")

all_cols <- colnames(WRAP_clean)
cols_check <- setdiff(all_cols, cols_dades)

is_na_or_empty <- function(x) {
  is.na(x) | (x == "")
}

rows_to_remove <- apply(WRAP_clean[, cols_check, drop=FALSE], 1, function(x) all(is_na_or_empty(x)))

WRAP_clean <- WRAP_clean[!rows_to_remove, ]

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
write.csv(WRAP_clean, "path/WRAP_clean.csv", row.names = FALSE)