# ─────────────────────────────────────────────────────────────
# Llibreries
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)

# ─────────────────────────────────────────────────────────────
# Es llegeixen tots els fitxers CSV
ADNIMERGE_oct <- read.csv("path/ADNIMERGE_24Oct2024.csv", sep=",", stringsAsFactors = FALSE)
APOE <- read.csv("path/APOERES_29Apr2025.csv", sep=",", stringsAsFactors = FALSE)
PLASMA_FUJIREBIO <- read.csv("path/UPENN_PLASMA_FUJIREBIO_QUANTERIX_29Apr2025.csv", sep=",", stringsAsFactors = FALSE)
PLASMA_PTAU <- read.csv("path/FNIH_PLASMA_PTAU_PROJECT_29Apr2025.csv", sep=",", stringsAsFactors = FALSE)
UCBERKELEY_TAU <- read.csv("path/UCBERKELEY_TAU_6MM_05May2025.csv", sep=",", stringsAsFactors = FALSE)
UCBERKELEY_AMY <- read.csv("path/UCBERKELEY_AMY_6MM_05May2025.csv", sep=",", stringsAsFactors = FALSE)

# ─────────────────────────────────────────────────────────────
# Es normalitzen els noms de les columnes EXAMDATE
UCBERKELEY_TAU <- UCBERKELEY_TAU %>% rename(EXAMDATE = SCANDATE)
UCBERKELEY_AMY <- UCBERKELEY_AMY %>% rename(EXAMDATE = SCANDATE)

# ─────────────────────────────────────────────────────────────
# Es pivoten les dades
PLASMA_PTAU_long <- PLASMA_PTAU %>%
  pivot_longer(
    cols = c(PTAU_181, PTAU_231),
    names_to = "type",
    values_to = "ptau_value"
  ) %>%
  mutate(variable_name = paste(IMMUNOASSAY, type, sep = "_"))

PLASMA_PTAU_wide <- PLASMA_PTAU_long %>%
  select(RID, VISCODE, VISCODE2, EXAMDATE, variable_name, ptau_value) %>%
  pivot_wider(names_from = variable_name, values_from = ptau_value) %>%
  select(where(~ !all(is.na(.))))

# ─────────────────────────────────────────────────────────────
# Es calculen els Braak stage
braak12_cols <- c("CTX_LH_ENTORHINAL_SUVR", "CTX_RH_ENTORHINAL_SUVR",
                  "LEFT_HIPPOCAMPUS_SUVR", "RIGHT_HIPPOCAMPUS_SUVR")

braak34_cols <- c("CTX_LH_PARAHIPPOCAMPAL_SUVR", "CTX_RH_PARAHIPPOCAMPAL_SUVR",
                  "CTX_LH_MIDDLETEMPORAL_SUVR", "CTX_RH_MIDDLETEMPORAL_SUVR",
                  "CTX_LH_FUSIFORM_SUVR", "CTX_RH_FUSIFORM_SUVR",
                  "LEFT_AMYGDALA_SUVR", "RIGHT_AMYGDALA_SUVR")

braak56_cols <- c("CTX_LH_PRECUNEUS_SUVR", "CTX_RH_PRECUNEUS_SUVR",
                  "CTX_LH_POSTERIORCINGULATE_SUVR", "CTX_RH_POSTERIORCINGULATE_SUVR",
                  "CTX_LH_SUPERIORPARIETAL_SUVR", "CTX_RH_SUPERIORPARIETAL_SUVR",
                  "CTX_LH_ROSTRALMIDDLEFRONTAL_SUVR", "CTX_RH_ROSTRALMIDDLEFRONTAL_SUVR")

# Càlcul dels valors mitjans SUVR per cada bloc Braak
UCBERKELEY_TAU <- UCBERKELEY_TAU %>%
  rowwise() %>%
  mutate(
    Bk12 = mean(c_across(all_of(braak12_cols)), na.rm = TRUE),
    Bk34 = mean(c_across(all_of(braak34_cols)), na.rm = TRUE),
    Bk56 = mean(c_across(all_of(braak56_cols)), na.rm = TRUE)
  ) %>%
  ungroup()

# ─────────────────────────────────────────────────────────────
# S'afegeixen sufixos a cada dataset
add_suffix <- function(df, suffix) {
  cols_to_keep <- c("RID", "EXAMDATE", "PTID")
  existing_keep <- intersect(names(df), cols_to_keep)
  
  df %>%
    rename_with(~ ifelse(.x %in% existing_keep, .x, paste0(.x, "_", suffix)))
}

APOE <- add_suffix(APOE, "APOE")
PLASMA_FUJIREBIO <- add_suffix(PLASMA_FUJIREBIO, "PLASMA_FUJIREBIO")
PLASMA_PTAU_wide <- add_suffix(PLASMA_PTAU_wide, "PLASMA_PTAU")
UCBERKELEY_TAU <- add_suffix(UCBERKELEY_TAU, "UCBERKELEY_TAU")
UCBERKELEY_AMY <- add_suffix(UCBERKELEY_AMY, "UCBERKELEY_AMY")

# ─────────────────────────────────────────────────────────────
# S'uneixen tots els datasets
merged_1 <- full_join(ADNIMERGE_oct, PLASMA_FUJIREBIO,  by = c("RID", "EXAMDATE", "PTID"))
merged_2 <- full_join(merged_1, PLASMA_PTAU_wide,  by = c("RID", "EXAMDATE"))
merged_3 <- full_join(merged_2, UCBERKELEY_TAU,  by = c("RID", "EXAMDATE", "PTID"))
merged_4 <- full_join(merged_3, UCBERKELEY_AMY,  by = c("RID", "EXAMDATE", "PTID"))
ADNI_merged <- full_join(merged_4, APOE, by = c("RID", "PTID"))

# ─────────────────────────────────────────────────────────────
# Es calcula la variable AGE, CDR_global, DOB
ADNI_merged[ADNI_merged == ""] <- NA

ADNI_merged <- ADNI_merged %>%
  mutate(
    VISCODE_CLEAN = trimws(tolower(VISCODE)),
    MONTHS_BTW_VISITS = case_when(
      VISCODE_CLEAN %in% c("bl", "m0", "m00") ~ 0,
      str_detect(VISCODE_CLEAN, "^m\\d+$") ~ as.numeric(str_extract(VISCODE_CLEAN, "\\d+")),
      TRUE ~ NA_real_
    ),
    AGE_AT_VISIT_ADNI = AGE + MONTHS_BTW_VISITS / 12
  ) %>%
  arrange(RID, MONTHS_BTW_VISITS) %>%
  group_by(RID) %>%
  mutate(VISCODE_NUM = paste0("V", row_number())) %>%
  ungroup()

ADNI_merged$CDR_global <- with(ADNI_merged,
                               ifelse(CDRSB == 0, 0,
                                      ifelse(CDRSB > 0 & CDRSB <= 4.0, 0.5,
                                             ifelse(CDRSB > 4.0 & CDRSB <= 9.0, 1.0,
                                                    ifelse(CDRSB > 9.0 & CDRSB <= 15.5, 2.0,
                                                           ifelse(CDRSB > 15.5, 3.0, NA))))))


ADNI_merged$EXAMDATE <- as.Date(ADNI_merged$EXAMDATE)

ADNI_merged <- ADNI_merged %>%
  mutate(DOB = if_else(VISCODE == "bl",
                       EXAMDATE - as.difftime(AGE * 365.25, units = "days"),
                       as.Date(NA)))

dob_per_subj <- ADNI_merged %>%
  filter(VISCODE == "bl") %>%
  select(RID, DOB) %>%
  distinct()

ADNI_merged <- ADNI_merged %>%
  select(-DOB) %>%
  left_join(dob_per_subj, by = "RID")

ADNI_merged <- ADNI_merged %>%
  mutate(
    AGE_AT_VISIT_ADNI = if_else(
      is.na(AGE_AT_VISIT_ADNI),
      as.numeric(difftime(EXAMDATE, DOB, units = "days")) / 365.25,
      AGE_AT_VISIT_ADNI
    )
  )

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
write.csv(ADNI_merged, "path/ADNI_merged.csv", row.names = FALSE)
