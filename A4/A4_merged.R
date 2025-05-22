# ─────────────────────────────────────────────────────────────
# Llibreries
library(dplyr)
library(data.table)
library(tidyr)
library(purrr)

# ─────────────────────────────────────────────────────────────
# Es llegeixen tots els fitxers CSV
bio_AB <- read.csv("path/biomarker_AB_Test.csv", sep=",", stringsAsFactors = FALSE)
bio_plasma <- read.csv("path/biomarker_Plasma_Roche_Results.csv", sep=",", stringsAsFactors = FALSE)
bio_pTau <- read.csv("path/biomarker_pTau217.csv", sep=",", stringsAsFactors = FALSE)

SUVR_amyloid <- read.csv("path/imaging_SUVR_amyloid.csv", sep=",", stringsAsFactors = FALSE)
SUVR_tau <- read.csv("path/imaging_SUVR_tau.csv", sep=",", stringsAsFactors = FALSE)
PET_PetSurfer <- read.csv("path/imaging_Tau_PET_PetSurfer.csv", sep=",", stringsAsFactors = FALSE)
PET_Stanford <- read.csv("path/imaging_Tau_PET_Stanford.csv", sep=",", stringsAsFactors = FALSE)
mri <- read.csv("path/imaging_volumetric_mri.csv", sep=",", stringsAsFactors = FALSE)

mmse <- read.csv("path/mmse.csv", sep=",", stringsAsFactors = FALSE)
PACC <- read.csv("path/PACC.csv", sep=",", stringsAsFactors = FALSE)
ptdemog <- read.csv("path/ptdemog.csv", sep=",", stringsAsFactors = FALSE)
SUBJINFO <- read.csv("path/SUBJINFO.csv", sep=",", stringsAsFactors = FALSE)
SV <- read.csv("path/SV.csv", sep=",", stringsAsFactors = FALSE)
cdr <- read.csv("path/cdr.csv", sep=",", stringsAsFactors = FALSE)

# ─────────────────────────────────────────────────────────────
# Es normalitzen els noms de les columnes de VISCODE
SV <- SV %>% rename(VISCODE = VISITCD)

# ─────────────────────────────────────────────────────────────
# Es pivoten les dades
bio_AB_wide <- bio_AB %>%
  mutate(variable_name = paste(LBSPEC, LBCAT, LBTESTCD, sep = "_")) %>%
  select(BID, VISCODE, variable_name, LBORRES) %>%
  pivot_wider(names_from = variable_name, values_from = LBORRES, values_fn = ~ .x[1])

bio_plasma_wide <- bio_plasma %>%
  mutate(variable_name = paste(LBSPEC, LBMETHOD, LBTESTCD, LBMTDL, sep = "_")) %>%
  select(BID, VISCODE, variable_name, LABRESN) %>%
  pivot_wider(names_from = variable_name, values_from = LABRESN)

bio_pTau_wide <- bio_pTau %>%
  mutate(variable_name = paste(TESTCD, TEST, sep = "_")) %>%
  select(BID, VISCODE, variable_name, ORRES) %>%
  pivot_wider(names_from = variable_name, values_from = ORRES, values_fn = ~ .x[1])

SUVR_amyloid_wide <- SUVR_amyloid %>%
  mutate(variable_name = paste(ligand, brain_region, sep = "_")) %>%
  select(BID, VISCODE, variable_name, suvr_cer) %>%
  pivot_wider(names_from = variable_name, values_from = suvr_cer
  )

SUVR_tau_long <- SUVR_tau %>%
  pivot_longer(
    cols = c(suvr_persi, suvr_crus),
    names_to = "type",
    values_to = "suvr_value"
  ) %>%
  mutate(variable_name = paste(ligand, brain_region, type, sep = "_"))

SUVR_tau_wide <- SUVR_tau_long %>%
  select(BID, VISCODE, variable_name, suvr_value) %>%
  pivot_wider(names_from = variable_name, values_from = suvr_value)

# ─────────────────────────────────────────────────────────────
# Es calculen els Braak stages PET tau
braak12_cols <- c("PVC_ctx.lh.entorhinal", "PVC_ctx.rh.entorhinal",
                  "PVC_Left.Hippocampus", "PVC_Right.Hippocampus")

braak34_cols <- c("PVC_ctx.lh.fusiform", "PVC_ctx.rh.fusiform",
                  "PVC_ctx.lh.parahippocampal", "PVC_ctx.rh.parahippocampal",
                  "PVC_ctx.lh.inferiortemporal", "PVC_ctx.rh.inferiortemporal",
                  "PVC_ctx.lh.middletemporal", "PVC_ctx.rh.middletemporal",
                  "PVC_ctx.lh.insula", "PVC_ctx.rh.insula")

braak56_cols <- c("PVC_ctx.lh.precuneus", "PVC_ctx.rh.precuneus",
                  "PVC_ctx.lh.posteriorcingulate", "PVC_ctx.rh.posteriorcingulate",
                  "PVC_ctx.lh.superiorfrontal", "PVC_ctx.rh.superiorfrontal",
                  "PVC_ctx.lh.superiortemporal", "PVC_ctx.rh.superiortemporal",
                  "PVC_ctx.lh.inferiorparietal", "PVC_ctx.rh.inferiorparietal")

# Calcular valors mitjans per Braak stage
PET_PetSurfer <- PET_PetSurfer %>%
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
  df %>%
    rename_with(~ ifelse(.x %in% c("BID", "VISCODE"), .x, paste0(.x, "_", suffix)))
}

bio_AB_wide <- add_suffix(bio_AB_wide, "bioAB")
bio_plasma_wide <- add_suffix(bio_plasma_wide, "bio_Plasma")
bio_pTau_wide <- add_suffix(bio_pTau_wide, "bioPTau")
SUVR_amyloid_wide <- add_suffix(SUVR_amyloid_wide, "SUVR_amyloid")
SUVR_tau_wide <- add_suffix(SUVR_tau_wide, "SUVR_tau")
PET_PetSurfer <- add_suffix(PET_PetSurfer, "PetSurfer")
PET_Stanford <- add_suffix(PET_Stanford, "PetStanford")
mri <- add_suffix(mri, "MRI")
mmse <- add_suffix(mmse, "MMSE")
PACC <- add_suffix(PACC, "PACC")
ptdemog <- add_suffix(ptdemog, "PtDemog")
SUBJINFO <- add_suffix(SUBJINFO, "SubjInfo")
SV <- add_suffix(SV, "SV")
cdr <- add_suffix(cdr, "cdr")

# Comprovacions
viscodes_dades <- unique(c(
  bio_AB_wide$VISCODE,
  bio_plasma_wide$VISCODE,
  bio_pTau_wide$VISCODE,
  SUVR_amyloid_wide$VISCODE,
  SUVR_tau_wide$VISCODE,
  PET_PetSurfer$VISCODE,
  PET_Stanford$VISCODE,
  mri$VISCODE,
  mmse$VISCODE,
  PACC$VISCODE,
  ptdemog$VISCODE,
  cdr$VISCODE
))

viscodes_sv <- unique(SV$VISCODE)

visites_fora_SV <- setdiff(viscodes_sv, viscodes_dades)

cat("VISCODE que surten a SV però no als datasets de dades:\n")
print(visites_fora_SV)

# ─────────────────────────────────────────────────────────────
# S'uneixen tots els datasets
merged_1 <- full_join(SV, mmse,  by = c("BID", "VISCODE"))
merged_2 <- full_join(merged_1, PACC, by = c("BID", "VISCODE"))
merged_3 <- full_join(merged_2, bio_AB_wide, by = c("BID", "VISCODE"))
merged_4 <- full_join(merged_3, bio_plasma_wide, by = c("BID", "VISCODE"))
merged_5 <- full_join(merged_4, bio_pTau_wide, by = c("BID", "VISCODE"))
merged_6 <- full_join(merged_5, SUVR_amyloid_wide, by = c("BID", "VISCODE"))
merged_7 <- full_join(merged_6, SUVR_tau_wide, by = c("BID", "VISCODE"))
merged_8 <- full_join(merged_7, PET_PetSurfer, by = c("BID", "VISCODE"))
merged_9 <- full_join(merged_8, PET_Stanford, by = c("BID", "VISCODE"))
merged_10 <- full_join(merged_9, mri, by = c("BID", "VISCODE"))
merged_11 <- full_join(merged_10, ptdemog, by = c("BID", "VISCODE"))
merged_12 <- full_join(merged_11, cdr, by = c("BID", "VISCODE"))
A4_merged <- full_join(merged_12, SUBJINFO, by = "BID")

# ─────────────────────────────────────────────────────────────
# S'eliminen observacions que només tenen dades de SV
A4_merged_filtrat <- A4_merged %>%
  filter(VISCODE %in% viscodes_dades)

# ─────────────────────────────────────────────────────────────
# Es calcula la variable edat
A4_merged_filtrat$AGE_AT_VISIT_A4 <- A4_merged_filtrat$AGEYR_SubjInfo + A4_merged_filtrat$SVSTDTC_DAYS_CONSENT_SV/365

# ─────────────────────────────────────────────────────────────
# S'agrupen les visites
A4_merged_filtrat$years_div <- A4_merged_filtrat$SVSTDTC_DAYS_CONSENT_SV / 365
A4_merged_filtrat$VISCODE_group <- ifelse(
  is.na(A4_merged_filtrat$years_div),
  NA,
  paste0("V", pmax(floor(A4_merged_filtrat$years_div) + 1, 1))
)

# Funció per obtenir el primer valor no NA
first_non_na <- function(x) {
  x[which(!is.na(x))[1]]
}

A4_colapsat <- A4_merged_filtrat %>%
  group_by(BID, VISCODE_group) %>%
  summarise(across(everything(), first_non_na), .groups = "drop")

# ─────────────────────────────────────────────────────────────
# Es guarda el resultat
write.csv(A4_colapsat, "path/A4_merged.csv", row.names = FALSE)
