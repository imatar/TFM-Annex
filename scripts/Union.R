# ─────────────────────────────────────────────────────────────
# Llibreries
library(data.table)
library(dplyr)
library(zoo)

# ─────────────────────────────────────────────────────────────
# Es llegeixen els datasets
A4 <- fread("path/A4_clean.csv")
ADNI <- fread("path/ADNI_clean.csv")
Alfa <- fread("path/Alfa_clean.csv")
OASIS3 <- fread("path/OASIS3_clean.csv")
WRAP <- fread("path/WRAP_clean.csv")
A4[A4 == ""] <- NA
ADNI[ADNI == ""] <- NA
Alfa[Alfa == ""] <- NA
OASIS3[OASIS3 == ""] <- NA
WRAP[WRAP == ""] <- NA

# ─────────────────────────────────────────────────────────────
# S'estandaritzen les variables
A4$MRI_hippo_vol_total <- A4$MRI_hippo_vol_total*1000
WRAP$MRI_hippo_vol_total <- WRAP$MRI_hippo_vol_total*1000

A4$Total_ICV <- A4$Total_ICV*1000
WRAP$Total_ICV <- WRAP$Total_ICV*1000000

A4 <- A4 %>%
  mutate(
    Gender = case_when(
      Gender == 1 ~ "Male",
      Gender == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    Sex = case_when(
      Sex == 2 ~ "Male",
      Sex == 1 ~ "Female",
      TRUE ~ NA_character_
    )
  )

A4 <- A4 %>%
  mutate(PTRace = case_when(
    PTRace == 1 ~ "American Indian or Alaskan Native",
    PTRace == 2 ~ "Asian",
    PTRace == 3 ~ "Native Hawaiian or Other Pacific Islander",
    PTRace == 4 ~ "Black or African American",
    PTRace == 5 ~ "White",
    PTRace == 6 ~ "Unknown",
    TRUE ~ NA_character_
  ))

A4 <- A4 %>%
  mutate(Race = case_when(
    Race == 1 ~ "White",
    Race == 2 ~ "Black or African American",
    Race == 58 ~ "Asian",
    Race == 79 ~ "Native Hawaiian or Other Pacific Islander",
    Race == 84 ~ "American Indian or Alaskan Native",
    Race == 97 ~ "Unknown",
    Race == 100 ~ "Mora than one race",
    TRUE ~ NA_character_
  ))


A4 <- A4 %>%
  mutate(PTEthnicity = case_when(
    PTEthnicity == 1 ~ "Hispanic or Latino",
    PTEthnicity == 2 ~ "Not Hispanic or Latino",
    PTEthnicity == 3 ~ "Unknown",
    TRUE ~ NA_character_
  ))

A4 <- A4 %>%
  mutate(Ethnicity = case_when(
    Ethnicity == 50 ~ "Hispanic or Latino",
    Ethnicity == 56 ~ "Not Hispanic or Latino",
    Ethnicity == 97 ~ "Unknown",
    TRUE ~ NA_character_
  ))


ADNI <- ADNI %>%
  mutate(PTRace = case_when(
    PTRace == "Am Indian/Alaskan" ~ "American Indian or Alaskan Native",
    PTRace == "Asian" ~ "Asian",
    PTRace == "Hawaiian/Other PI" ~ "Native Hawaiian or Other Pacific Islander",
    PTRace == "Black" ~ "Black or African American",
    PTRace == "White" ~ "White",
    PTRace == "Unknown" ~ "Unknown",
    PTRace == "More than one" ~ "More than one",
    TRUE ~ NA_character_
  ))

ADNI <- ADNI %>%
  mutate(PTEthnicity = case_when(
    PTEthnicity == "Not Hisp/Latino" ~ "Not Hispanic or Latino",
    PTEthnicity == "Hisp/Latino" ~ "Hispanic or Latino",
    PTEthnicity == "Unknown" ~ "Unknown",
    TRUE ~ NA_character_
  ))

Alfa <- Alfa %>%
  mutate(Sex = case_when(
    Sex == 1 ~ "Male",
    Sex == 2 ~ "Female",
    Sex == 3 ~ "Intersex",
    TRUE ~ NA_character_
  ))

Alfa$Gender <- Alfa$Sex

Alfa <- Alfa %>%
  mutate(APOE = case_when(
    APOE == 1 ~ "E2/E2",
    APOE == 2 ~ "E2/E3",
    APOE == 3 ~ "E2/E4",
    APOE == 4 ~ "E3/E3",
    APOE == 5 ~ "E3/E4",
    APOE == 6 ~ "E4/E4",
    TRUE ~ NA_character_
  ))

OASIS3 <- OASIS3 %>%
  mutate(Gender = case_when(
    Gender == 1 ~ "Male",
    Gender == 2 ~ "Female",
    TRUE ~ NA_character_
  ))

OASIS3 <- OASIS3 %>%
  mutate(Race = case_when(
    Race == "AIAN" ~ "Asian",
    Race == "ASIAN" ~ "Asian",
    Race == "Black" ~ "Black or African American",
    Race == "White" ~ "White",
    Race == "more than one" ~ "More than one",
    TRUE ~ NA_character_
  ))

OASIS3 <- OASIS3 %>%
  mutate(Ethnicity = case_when(
    Ethnicity == 0 ~ "Not Hispanic or Latino",
    Ethnicity == 1 ~ "Hispanic or Latino",
    TRUE ~ NA_character_
  ))

OASIS3$APOE <- ifelse(is.na(OASIS3$APOE) | OASIS3$APOE == "", 
                      NA, 
                      paste0("E", substr(OASIS3$APOE, 1, 1), "/E", substr(OASIS3$APOE, 2, 2)))


WRAP <- WRAP %>%
  mutate(Gender = case_when(
    Gender == 1 ~ "Male",
    Gender == 2 ~ "Female",
    Gender == 3 ~ "Intersex",
    TRUE ~ NA_character_
  ))

WRAP <- WRAP %>%
  mutate(Race = case_when(
    Race == 1 ~ "White",
    Race == 2 ~ "Black or African American",
    Race == 4 ~ "American Indian or Alaska Native",
    Race == 5 ~ "Asian",
    Race == 6 ~ "Native Hawaiian or Other Pacific Islander",
    Race == 7 ~ "Other",
    Race == 8 ~ "Unknown",
    TRUE ~ NA_character_
  ))

datasets <- list(A4, ADNI, Alfa, OASIS3, WRAP)

# ─────────────────────────────────────────────────────────────
# S'afegeix la columna Cohort a cada dataset
A4[, Cohort := "A4"]
ADNI[, Cohort := "ADNI"]
Alfa[, Cohort := "Alfa+"]
OASIS3[, Cohort := "OASIS3"]
WRAP[, Cohort := "WRAP"]

datasets <- list(A4, ADNI, Alfa, OASIS3, WRAP)

# ─────────────────────────────────────────────────────────────
# Variables que han d'estar presents
vars_final <- c(
  "Subj_id", "Treatment", "Visit", "Visit_date", "Visit_days",
  "Cohort", "Cohort_detail", "Age_at_visit", "Gender", "Sex",
  "Ethnicity", "PTEthnicity", "Race", "PTRace", "Education", "PTEducation",
  "DX_raw", "APOE", "CSF_date",
  "CSF_AB42", "CSF_pTau", "CSF_tTau", "CSF_NfL",
  "Plasma_NfL", "Plasma_pTau_Simoa", "Plasma_pTau_Roche",
  "MRI_days", "MRI_hippo_vol_total", "Total_ICV", "Hippo_normalized_icv",
  "PET_amyloid_date", "PET_amyloid_days", "SUVR_Amyloid", "PET_Centiloid",
  "PET_FDG", "PET_tau_Bk12", "PET_tau_Bk34", "PET_tau_Bk56",
  "MMSE", "CDR"
)

# ─────────────────────────────────────────────────────────────
# Es verifica que tots els datasets tenen totes les variables
datasets_fixed <- lapply(datasets, function(dt) {
  missing <- setdiff(vars_final, names(dt))
  for (col in missing) {
    dt[[col]] <- NA
  }
  dt <- dt[, ..vars_final]
  return(dt)
})


# ─────────────────────────────────────────────────────────────
# S'uneix tots els datasets
dataset_final <- rbindlist(datasets_fixed, fill = TRUE)

# ─────────────────────────────────────────────────────────────
# S'estandaritzen les variables

# MRI_hippo_vol_total, Total_ICV
dataset_final <- dataset_final %>%
  mutate(
    Total_ICV = ifelse(Total_ICV < 0, NA, Total_ICV),
    MRI_hippo_vol_total = ifelse(MRI_hippo_vol_total < 0, NA, MRI_hippo_vol_total)
  )

# Hippo_normalized_icv
dataset_final$Hippo_normalized_icv <- dataset_final$MRI_hippo_vol_total / dataset_final$Total_ICV

# Visit
dataset_final <- dataset_final %>%
  mutate(Visit = ifelse(grepl("^[0-9]+$", Visit), paste0("V", Visit), Visit))

# Age_at_visit
dataset_final$Age_at_visit <- round(dataset_final$Age_at_visit, 2)

# Education
dataset_final <- dataset_final %>%
  mutate(Education = coalesce(Education, PTEducation)) %>%
  select(-PTEducation)

# Race
dataset_final <- dataset_final %>%
  mutate(Race = coalesce(Race, PTRace)) %>%
  select(-PTRace)

# Ethnicity
dataset_final <- dataset_final %>%
  mutate(Ethnicity = coalesce(Ethnicity, PTEthnicity)) %>%
  select(-PTEthnicity)

# DX_raw
dataset_final <- dataset_final %>%
  mutate(
    DX_raw = na_if(DX_raw, "."),
    DX_raw = na_if(DX_raw, "No_Diagnosis_Calculated")
  )

# DX
dataset_final <- dataset_final %>%
  mutate(
    DX = case_when(
      DX_raw %in% c("CN", "Cognitively normal", "Cog_Unimpaired_Stable", "Cog_Unimpaired_Declining", "SMC") ~ "CN",
      DX_raw %in% c("EMCI", "LMCI", "Clinical_MCI") ~ "MCI",
      DX_raw %in% c("AD", "AD Dementia", "Dementia", 
                       "Vascular Demt, primary", "Vascular Demt, secondary",
                       "Frontotemporal demt. prim", "Frontotemporal demt. secn",
                       "DLBD, primary", "Dementia/PD, primary") ~ "Dementia",
      grepl("^AD dem", DX_raw, ignore.case = TRUE) ~ "Dementia",
      grepl("^Unc", DX_raw, ignore.case = TRUE) | grepl("uncertain", DX_raw, ignore.case = TRUE) ~ "Uncertain",
      DX_raw %in% c("0.5 in memory only", "Incipient demt PTP", "Q", "Non AD dem, Other primary", "Impaired_Not_MCI", "Incipient Non-AD dem") ~ "Other",
      DX_raw %in% c(".", "No_Diagnosis_Calculated") ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

# DX_Dementia_type
dataset_final <- dataset_final %>%
  mutate(
    DX_Dementia_type = case_when(
      DX_raw %in% c("AD", "AD Dementia") |
        grepl("^AD dem", DX_raw, ignore.case = TRUE) ~ "AD",
      grepl("^Vascular Demt", DX_raw, ignore.case = TRUE) ~ "Vascular",
      grepl("^Frontotemporal demt", DX_raw, ignore.case = TRUE) ~ "FTP",
      grepl("Non AD dem", DX_raw, ignore.case = TRUE) ~ "Non AD",
      DX_raw %in% c("DLBD, primary") ~ "DLBD",
      DX_raw %in% c("Dementia/PD, primary") ~ "PD",
      TRUE ~ NA_character_
    )
  )

# DX_detail
dataset_final <- dataset_final %>%
  mutate(
    DX_detail = case_when(
      DX_raw %in% c("AD", "AD Dementia") ~ "AD",
      DX_raw %in% c("Clinical_MCI") ~ "MCI",
      DX_raw %in% c("LMCI") ~ "LMCI",
      DX_raw %in% c("EMCI") ~ "EMCI",
      DX_raw %in% c("Cognitively normal", "CN") ~ "CN",
      DX_raw == "SMC" ~ "SMC",
      DX_raw == "Cog_Unimpaired_Declining" ~ "CN declining",
      DX_raw == "Cog_Unimpaired_Stable" ~ "CN stable",
      DX_raw %in% c("Vascular Demt, primary", "Vascular Demt, secondary",
                    "Frontotemporal demt. prim", "Frontotemporal demt. secn",
                    "DLBD, primary", "Dementia/PD, primary", "Dementia") ~ "Non AD dementia",
      grepl("^AD dem", DX_raw, ignore.case = TRUE) ~ "AD",
      grepl("^Unc", DX_raw, ignore.case = TRUE) | grepl("uncertain", DX_raw, ignore.case = TRUE) ~ "Uncertain",
      DX_raw %in% c("0.5 in memory only", "Incipient demt PTP", "Q",
                    "Non AD dem, Other primary", "Impaired_Not_MCI", "Incipient Non-AD dem") ~ "Other",
      DX_raw %in% c(".", "No_Diagnosis_Calculated") ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

# DX_bl
dataset_final[, DX_bl := DX[Visit == "V1"][1], by = Subj_id]

# Age_bl
dataset_final[, Age_bl := Age_at_visit[Visit == "V1"][1], by = Subj_id]

# CSF_AB42
dataset_final <- dataset_final %>%
  mutate(CSF_AB42 = case_when(
    grepl("^>", CSF_AB42) ~ as.numeric(gsub(">", "", CSF_AB42)) + 1,
    grepl("^<", CSF_AB42) ~ as.numeric(gsub("<", "", CSF_AB42)) - 1,
    TRUE ~ suppressWarnings(as.numeric(CSF_AB42))
  ))

# CSF_pTau
dataset_final <- dataset_final %>%
  mutate(CSF_pTau = case_when(
    grepl("^>", CSF_pTau) ~ as.numeric(gsub(">", "", CSF_pTau)) + 1,
    grepl("^<", CSF_pTau) ~ as.numeric(gsub("<", "", CSF_pTau)) - 1,
    TRUE ~ suppressWarnings(as.numeric(CSF_pTau))
  ))

# CSF_tTau
dataset_final <- dataset_final %>%
  mutate(CSF_tTau = case_when(
    grepl("^>", CSF_tTau) ~ as.numeric(gsub(">", "", CSF_tTau)) + 1,
    grepl("^<", CSF_tTau) ~ as.numeric(gsub("<", "", CSF_tTau)) - 1,
    TRUE ~ suppressWarnings(as.numeric(CSF_tTau))
  ))

# Plasma_NfL
dataset_final$Plasma_NfL <- as.numeric(dataset_final$Plasma_NfL)

# Total_visits
dataset_final[, Total_visits := .N, by = Subj_id]

# E4_carrier
dataset_final$E4_carrier <- ifelse(
  is.na(dataset_final$APOE), 
  NA, 
  ifelse(dataset_final$APOE %in% c("E4/E4", "E3/E4", "E2/E4"), "Yes", "No")
)

# E4_allele
dataset_final$E4_allele <- ifelse(
  is.na(dataset_final$APOE), 
  NA, 
  ifelse(
    dataset_final$APOE == "E4/E4", 
    "2 E4", 
    ifelse(
      dataset_final$APOE %in% c("E3/E4", "E2/E4"), 
      "1 E4", 
      "0 E4"
    )
  )
)


# ─────────────────────────────────────────────────────────────
# Es tracten els valors NA

# Cohort_detail
dataset_final[is.na(Cohort_detail), Cohort_detail := Cohort]

# Gender
dataset_final[, Gender := na.locf(Gender, na.rm = FALSE), by = Subj_id]
dataset_final[, Gender := na.locf(Gender, fromLast = TRUE, na.rm = FALSE), by = Subj_id]

# Sex
dataset_final[, Sex := na.locf(Sex, na.rm = FALSE), by = Subj_id]
dataset_final[, Sex := na.locf(Sex, fromLast = TRUE, na.rm = FALSE), by = Subj_id]

# Ethnicity
dataset_final[, Ethnicity := na.locf(Ethnicity, na.rm = FALSE), by = Subj_id]
dataset_final[, Ethnicity := na.locf(Ethnicity, fromLast = TRUE, na.rm = FALSE), by = Subj_id]

# Race
dataset_final[, Race := na.locf(Race, na.rm = FALSE), by = Subj_id]
dataset_final[, Race := na.locf(Race, fromLast = TRUE, na.rm = FALSE), by = Subj_id]

# Education
dataset_final[, Education := na.locf(Education, na.rm = FALSE), by = Subj_id]
dataset_final[, Education := na.locf(Education, fromLast = TRUE, na.rm = FALSE), by = Subj_id]

# APOE
dataset_final[, APOE := na.locf(APOE, na.rm = FALSE), by = Subj_id]
dataset_final[, APOE := na.locf(APOE, fromLast = TRUE, na.rm = FALSE), by = Subj_id]

# Treatment
dataset_final[, Treatment := na.locf(Treatment, na.rm = FALSE), by = Subj_id]
dataset_final[, Treatment := na.locf(Treatment, fromLast = TRUE, na.rm = FALSE), by = Subj_id]

# ─────────────────────────────────────────────────────────────
# Variables auxiliars
dataset_final <- dataset_final %>%
  rename(
    PET_amyloid_suvr       = SUVR_Amyloid,
    PET_amyloid_centiloid  = PET_Centiloid
  )

dataset_final <- dataset_final %>%
  mutate(
    PET_amyloid = if_else(
      !is.na(PET_amyloid_suvr) | !is.na(PET_amyloid_centiloid),
      "Sí",
      NA_character_
    )
  )

dataset_final <- dataset_final %>%
  mutate(
    PET_tau = if_else(
      !is.na(PET_tau_Bk12) | !is.na(PET_tau_Bk34) | !is.na(PET_tau_Bk56),
      "Sí",
      NA_character_
    )
  )

dataset_final <- dataset_final %>%
  mutate(
    MRI = if_else(
      !is.na(MRI_hippo_vol_total) | !is.na(Total_ICV) | !is.na(Hippo_normalized_icv),
      "Sí",
      NA_character_
    )
  )

# ─────────────────────────────────────────────────────────────
# S'ordena el dataset
dataset_final <- dataset_final %>%
  select(
    Subj_id, Treatment, Visit, Visit_date, Total_visits, Visit_days,
    Cohort, Cohort_detail, Age_at_visit, Age_bl, Gender, Sex,
    Ethnicity, Race, Education, DX_raw, DX, DX_detail, DX_Dementia_type, DX_bl, APOE, E4_carrier,
    E4_allele, CSF_date, CSF_AB42, CSF_pTau, CSF_tTau, CSF_NfL,
    Plasma_NfL, Plasma_pTau_Simoa, Plasma_pTau_Roche,
    MRI_days, MRI, MRI_hippo_vol_total, Total_ICV, Hippo_normalized_icv,
    PET_amyloid_date, PET_amyloid_days, PET_amyloid, PET_amyloid_suvr, PET_amyloid_centiloid,
    PET_FDG, PET_tau, PET_tau_Bk12, PET_tau_Bk34, PET_tau_Bk56,
    MMSE, CDR
  )

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
dataset_final[dataset_final == ""] <- NA
fwrite(dataset_final, "Union/dataset_final.csv")