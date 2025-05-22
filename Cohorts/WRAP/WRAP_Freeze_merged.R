# ─────────────────────────────────────────────────────────────
# Llibreries
library(tidyverse)
library(readr)

# ─────────────────────────────────────────────────────────────
# Funció per llegir i estandarditzar els datasets
llegir_dataset <- function(path, suffix, inclou_VisNo = TRUE) {
  df <- read_csv(path, show_col_types = FALSE) %>%
    mutate(WRAPNo = as.character(WRAPNo))
  
  if (inclou_VisNo && "VisNo" %in% names(df)) {
    df <- df %>% mutate(VisNo = as.numeric(VisNo))
  }
  
  cols_exclosos <- if (inclou_VisNo) c("WRAPNo", "VisNo") else "WRAPNo"
  
  df %>%
    rename_with(~ if_else(.x %in% cols_exclosos, .x, paste0(.x, "_", suffix)))
}

# ─────────────────────────────────────────────────────────────
# Es llegeixen els datasets
MedHistory <- llegir_dataset("path/MedHistory.csv", "MedHistory")
Abnormal_long <- llegir_dataset("path/Abnormal_Factor_Flags_long.csv", "Abnormal_long")
AnimalNaming <- llegir_dataset("path/AnimalNaming_ItemLevel.csv", "AnimalNaming")
AVLT <- llegir_dataset("path/AVLTPositionScores.csv", "AVLT")
Behavioral <- llegir_dataset("path/BehavioralObservationChecklist.csv", "Behavioral")
CDR <- llegir_dataset("path/CDR.csv", "CDR")
CES <- llegir_dataset("path/CES_D_Scale.csv", "CES")
CHAMPS <- llegir_dataset("path/CHAMPS.csv", "CHAMPS")
CogStateData <- llegir_dataset("path/CogStateData.csv", "CogStateData")
CompositeScoreCalcValues <- llegir_dataset("path/CompositeScoreCalcValues.csv", "CompositeScoreCalcValues")
CompositeScores <- llegir_dataset("path/CompositeScores.csv", "CompositeScores")
ConsensusConference <- llegir_dataset("path/ConsensusConference.csv", "ConsensusConference")
DailyActivity <- llegir_dataset("path/DailyActivity.csv", "DailyActivity")
FactorScores <- llegir_dataset("path/FactorScores.csv", "FactorScores")
fqryStatisticalData <- llegir_dataset("path/fqryStatisticalData.csv", "fqryStatisticalData")
GenHealthDailyAct <- llegir_dataset("path/GenHealthDailyAct.csv", "GenHealthDailyAct")
IADL <- llegir_dataset("path/IADL.csv", "IADL")
IQCode <- llegir_dataset("path/IQCode.csv", "IQCode")
LearningDifficulties <- llegir_dataset("path/LearningDifficulties.csv", "LearningDifficulties")
LIBRA <- llegir_dataset("path/LIBRA.csv", "LIBRA")
Med_Lab <- llegir_dataset("path/Med_Lab.csv", "Med_Lab")
Med_Lab_NPDC <- llegir_dataset("path/Med_Lab_NPDC.csv", "Med_Lab_NPDC")
Med_Lab_PDC <- llegir_dataset("path/Med_Lab_PDC.csv", "Med_Lab_PDC")
MIND_Diet <- llegir_dataset("path/MIND_Diet.csv", "MIND_Diet")
MIND_Diet_ScoredValues <- llegir_dataset("path/MIND_Diet_ScoredValues.csv", "MIND_Diet_ScoredValues")
MOCA <- llegir_dataset("path/MOCA.csv", "MOCA")
NeuropsychScores <- llegir_dataset("path/NeuropsychScores.csv", "NeuropsychScores")
QuickDementia <- llegir_dataset("path/QuickDementia.csv", "QuickDementia")
RobustNormZScores <- llegir_dataset("path/RobustNormZScores.csv", "RobustNormZScores")
SubjectiveHearing <- llegir_dataset("path/SubjectiveHearing.csv", "SubjectiveHearing")
TICSM <- llegir_dataset("path/TICSM.csv", "TICSM")
VisitVitals <- llegir_dataset("path/VisitVitals.csv", "VisitVitals")
APG <- llegir_dataset("path/APG.csv", "APG")
CA_Genome <- llegir_dataset("path/CA_Genome.csv", "CA_Genome")
CERAD <- llegir_dataset("path/CERAD.csv", "CERAD")
Demographics <- llegir_dataset("path/Demographics.csv", "Demographics")
Gene_DataCollection <- llegir_dataset("path/Gene_DataCollection.csv", "Gene_DataCollection")
MiscVars <- llegir_dataset("path/MiscVars.csv", "MiscVars")
NeuroPhysicalDxFirstAtVisit <- llegir_dataset("path/NeuroPhysicalDxFirstAtVisit.csv", "NeuroPhysicalDxFirstAtVisit")
ReportedFamilyHistoryAtVisit <- llegir_dataset("path/ReportedFamilyHistoryAtVisit.csv", "ReportedFamilyHistoryAtVisit")

# ─────────────────────────────────────────────────────────────
# Es neteja repeat_key de Med_Lab_NPDC i Med_Lab_PDC
Med_Lab_NPDC <- Med_Lab_NPDC %>%
  group_by(WRAPNo, VisNo) %>%
  summarise(npdcclass_Med_Lab_NPDC = paste(unique(npdcclass_Med_Lab_NPDC), collapse = ", "))

Med_Lab_PDC <- Med_Lab_PDC %>%
  group_by(WRAPNo, VisNo) %>%
  summarise(pdcclass_Med_Lab_PDC = paste(unique(pdcclass_Med_Lab_PDC), collapse = ", "))

# ─────────────────────────────────────────────────────────────
# S'uneixen els datasets
merged_1 <- merge(MedHistory, Abnormal_long, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_2 <- merge(merged_1, AnimalNaming, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_3 <- merge(merged_2, AVLT, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_4 <- merge(merged_3, Behavioral, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_5 <- merge(merged_4, CDR, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_6 <- merge(merged_5, CES, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_7 <- merge(merged_6, CHAMPS, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_8 <- merge(merged_7, CogStateData, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_9 <- merge(merged_8, CompositeScoreCalcValues, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_10 <- merge(merged_9, CompositeScores, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_11 <- merge(merged_10, ConsensusConference, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_12 <- merge(merged_11, DailyActivity, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_13 <- merge(merged_12, FactorScores, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_14 <- merge(merged_13, fqryStatisticalData, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_15 <- merge(merged_14, GenHealthDailyAct, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_16 <- merge(merged_15, IADL, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_17 <- merge(merged_16, IQCode, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_18 <- merge(merged_17, LearningDifficulties, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_19 <- merge(merged_18, LIBRA, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_20 <- merge(merged_19, Med_Lab, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_21 <- merge(merged_20, Med_Lab_NPDC, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_22 <- merge(merged_21, Med_Lab_PDC, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_23 <- merge(merged_22, MIND_Diet, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_24 <- merge(merged_23, MIND_Diet_ScoredValues, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_25 <- merge(merged_24, MOCA, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_26 <- merge(merged_25, NeuropsychScores, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_27 <- merge(merged_26, QuickDementia, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_28 <- merge(merged_27, RobustNormZScores, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_29 <- merge(merged_28, SubjectiveHearing, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_30 <- merge(merged_29, TICSM, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_31 <- merge(merged_30, VisitVitals, by = c("WRAPNo", "VisNo"), all.x = TRUE)
merged_32 <- merge(merged_31, APG, by = c("WRAPNo"), all.x = TRUE)
merged_33 <- merge(merged_32, CA_Genome, by = c("WRAPNo"), all.x = TRUE)
merged_34 <- merge(merged_33, CERAD, by = c("WRAPNo"), all.x = TRUE)
merged_35 <- merge(merged_34, Demographics, by = c("WRAPNo"), all.x = TRUE)
merged_36 <- merge(merged_35, Gene_DataCollection, by = c("WRAPNo"), all.x = TRUE)
merged_37 <- merge(merged_36, MiscVars, by = c("WRAPNo"), all.x = TRUE)
merged_38 <- merge(merged_37, NeuroPhysicalDxFirstAtVisit, by = c("WRAPNo"), all.x = TRUE)
WRAP_Freeze_merged <- merge(merged_38, ReportedFamilyHistoryAtVisit, by = c("WRAPNo"), all.x = TRUE)

# ─────────────────────────────────────────────────────────────
# Es crear la columna 'age_at_visit_WRAP_Freeze' amb les dades de 'Age_At_Visit_fqryStatisticalData'
WRAP_Freeze_merged$age_at_visit_WRAP_Freeze <- WRAP_Freeze_merged$Age_At_Visit_fqryStatisticalData
na_count <- sum(is.na(WRAP_Freeze_merged$age_at_visit_WRAP_Freeze))
print(na_count)

# ─────────────────────────────────────────────────────────────
# Comprovacions
# merge_1
sum(!is.na(Abnormal_long$Explanation_Abnormal_long))
sum(!is.na(merged_1$Explanation_Abnormal_long))

# merge_2
sum(!is.na(AnimalNaming$AN_1_AnimalNaming))
sum(!is.na(merged_2$AN_1_AnimalNaming))

# merge_3
sum(!is.na(AVLT$`T1-1_AVLT`))
sum(!is.na(merged_3$`T1-1_AVLT`))

# merge_4
sum(!is.na(Behavioral$VisType_Behavioral))
sum(!is.na(merged_4$VisType_Behavioral))

# merge_5
sum(!is.na(CDR$CDRRating_CDR))
sum(!is.na(merged_5$CDRRating_CDR))

# merge_6
sum(!is.na(CES$Typical3_CES))
sum(!is.na(merged_6$Typical3_CES))

# merge_7
sum(!is.na(CHAMPS$CHAMPS_FrndFam_CHAMPS))
sum(!is.na(merged_7$CHAMPS_FrndFam_CHAMPS))

# merge_8
sum(!is.na(CogStateData$Sex_CogStateData))
sum(!is.na(merged_8$Sex_CogStateData))

# merge_9
sum(!is.na(CompositeScoreCalcValues$ttotal_CompositeScoreCalcValues))
sum(!is.na(merged_9$ttotal_CompositeScoreCalcValues))

# merge_10
sum(!is.na(CompositeScores$z_imm_lrn_CompositeScores))
sum(!is.na(merged_10$z_imm_lrn_CompositeScores))

# merge_11
sum(!is.na(ConsensusConference$CC1_ConsensusConference))
sum(!is.na(merged_11$CC1_ConsensusConference))

# merge_12
sum(!is.na(DailyActivity$RelPar_DailyActivity))
sum(!is.na(merged_12$RelPar_DailyActivity))

# merge_13
sum(!is.na(FactorScores$z_imm_mem_adj_FactorScores))
sum(!is.na(merged_13$z_imm_mem_adj_FactorScores))

# merge_14
sum(!is.na(fqryStatisticalData$Age_At_Visit_fqryStatisticalData))
sum(!is.na(merged_14$Age_At_Visit_fqryStatisticalData))

# merge_15
sum(!is.na(GenHealthDailyAct$CurHealth_GenHealthDailyAct))
sum(!is.na(merged_15$CurHealth_GenHealthDailyAct))

# merge_16
sum(!is.na(IADL$Walking_IADL))
sum(!is.na(merged_16$Walking_IADL))

# merge_17
sum(!is.na(IQCode$Total_IQCode))
sum(!is.na(merged_17$Total_IQCode))

# merge_18
sum(!is.na(LearningDifficulties$ovecomp_LearningDifficulties))
sum(!is.na(merged_18$ovecomp_LearningDifficulties))

# merge_19
sum(!is.na(LIBRA$LIBRA_tertile_LIBRA))
sum(!is.na(merged_19$LIBRA_tertile_LIBRA))

# merge_20
sum(!is.na(Med_Lab$nocrntpd_Med_Lab))
sum(!is.na(merged_20$nocrntpd_Med_Lab))

# merge_21
sum(!is.na(Med_Lab_NPDC$npdcclass_Med_Lab_NPDC))
sum(!is.na(merged_21$npdcclass_Med_Lab_NPDC))

# merge_22
sum(!is.na(Med_Lab_PDC$pdcclass_Med_Lab_PDC))
sum(!is.na(merged_22$pdcclass_Med_Lab_PDC))

# merge_23
sum(!is.na(MIND_Diet$MIND_OlivOil_Dy_MIND_Diet))
sum(!is.na(merged_23$MIND_OlivOil_Dy_MIND_Diet))

# merge_24
sum(!is.na(MIND_Diet_ScoredValues$MINDScoredValue_GrnLfyVg_MIND_Diet_ScoredValues))
sum(!is.na(merged_24$MINDScoredValue_GrnLfyVg_MIND_Diet_ScoredValues))

# merge_25
sum(!is.na(MOCA$MOCA_TotalScore_MOCA))
sum(!is.na(merged_25$MOCA_TotalScore_MOCA))

# merge_26
sum(!is.na(NeuropsychScores$tTotal_NeuropsychScores))
sum(!is.na(merged_26$tTotal_NeuropsychScores))

# merge_27
sum(!is.na(QuickDementia$QDRS_Rating_QuickDementia))
sum(!is.na(merged_27$QDRS_Rating_QuickDementia))

# merge_28
sum(!is.na(RobustNormZScores$RNZ_WAISIIIDigitSpanForward_iqdspf_RobustNormZScores))
sum(!is.na(merged_28$RNZ_WAISIIIDigitSpanForward_iqdspf_RobustNormZScores))

# merge_29
sum(!is.na(SubjectiveHearing$SubjHear_HearProb_SubjectiveHearing))
sum(!is.na(merged_29$SubjHear_HearProb_SubjectiveHearing))

# merge_30
sum(!is.na(TICSM$TICSM_TotalScore_TICSM))
sum(!is.na(merged_30$TICSM_TotalScore_TICSM))

# merge_31
sum(!is.na(VisitVitals$height_VisitVitals))
sum(!is.na(merged_31$height_VisitVitals))

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
write.csv(WRAP_Freeze_merged, "path/WRAP_Freeze_merged.csv", row.names = FALSE)
