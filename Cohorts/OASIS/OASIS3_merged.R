# ─────────────────────────────────────────────────────────────
# Llibreries
library(data.table)

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara health_history
health_history <- fread("path/OASIS3_UDSa5_health_history.csv")

# Es comprova si hi ha duplicats
duplicats_health <- health_history[duplicated(health_history$OASIS_session_label), ]
dim(duplicats_health)

# Es calcula la distància mínima entre visites per subjecte
health_history[, days_to_visit := as.numeric(days_to_visit)]
setorder(health_history, OASISID, days_to_visit)

diffs <- health_history[, {
  d <- diff(days_to_visit)
  min_val <- ifelse(length(d) > 0, min(d, na.rm = TRUE), as.numeric(NA))  # Força NA a ser numèric
  .(min_diff = min_val)
}, by = OASISID]

print(unique(diffs[order(min_diff)][1:10]))

# S'afegeixen sufixos i es creen intervals de cerca
setDT(health_history)
cols_to_rename_health <- setdiff(names(health_history), "OASISID")
setnames(health_history, cols_to_rename_health, paste0(cols_to_rename_health, "_health"))

health_history[, days_min_health := days_to_visit_health - 365]
health_history[, days_max_health := days_to_visit_health + 365]

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara amyloid_centiloid
amyloid_centiloid <- fread("path/OASIS3_amyloid_centiloid.csv")

# Es renombren i s'extreuen variables
setnames(amyloid_centiloid, "subject_id", "OASISID")
amyloid_centiloid[, days_to_visit_amyloid := as.numeric(gsub(".*_d(\\d+).*", "\\1", oasis_session_id))]

cols_to_rename_amyloid <- setdiff(names(amyloid_centiloid), c("OASISID", "days_to_visit_amyloid"))
setnames(amyloid_centiloid, cols_to_rename_amyloid, paste0(cols_to_rename_amyloid, "_amyloid"))

amyloid_centiloid[, days_min_amyloid := days_to_visit_amyloid]
amyloid_centiloid[, days_max_amyloid := days_to_visit_amyloid]

# Es fusiona amb health_history
setDT(health_history)
setDT(amyloid_centiloid)
setkey(amyloid_centiloid, OASISID, days_min_amyloid, days_max_amyloid)
setkey(health_history, OASISID, days_min_health, days_max_health)

merged_1 <- foverlaps(health_history, amyloid_centiloid,
                      by.x = c("OASISID", "days_min_health", "days_max_health"),
                      by.y = c("OASISID", "days_min_amyloid", "days_max_amyloid"),
                      nomatch = NA)
merged_1[, diff := abs(days_to_visit_health - days_to_visit_amyloid)]
merged_1 <- merged_1[order(OASISID, days_to_visit_health, diff)]
merged_1 <- merged_1[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(amyloid_centiloid$oasis_session_id_amyloid))
sum(!is.na(merged_1$oasis_session_id_amyloid))
length(unique(merged_1$oasis_session_id_amyloid))

# Assignació única per amiloide
merged_1_aux <- merged_1[
  , .SD[which.min(diff)], 
  by = .(oasis_session_id_amyloid)
]

sum(!is.na(amyloid_centiloid$oasis_session_id_amyloid))
sum(!is.na(merged_1_aux$oasis_session_id_amyloid))
length(unique(merged_1_aux$oasis_session_id_amyloid))

# S'uneixen les files sense assignació
health_missing_amyloid <- health_history[!paste(OASISID, days_to_visit_health) %in% 
                                           paste(merged_1_aux$OASISID, merged_1_aux$days_to_visit_health)]

cols_amyloid <- setdiff(names(merged_1_aux), names(health_missing_amyloid))
health_missing_amyloid[, (cols_amyloid) := NA]

merged_1_final <- rbindlist(list(merged_1_aux, health_missing_amyloid), use.names = TRUE, fill = TRUE)

# Comprovació
sum(!is.na(amyloid_centiloid$oasis_session_id_amyloid))
sum(!is.na(merged_1_final$oasis_session_id_amyloid))
length(unique(merged_1_final[!is.na(oasis_session_id_amyloid), oasis_session_id_amyloid]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara CT
CT <- fread("path/OASIS3_CT_json.csv")
setnames(CT, "subject_id", "OASISID")
CT[, days_to_visit_CT := as.numeric(gsub(".*_d(\\d+).*", "\\1", session))]

cols_to_rename_CT <- setdiff(names(CT), c("OASISID", "days_to_visit_CT"))
setnames(CT, cols_to_rename_CT, paste0(cols_to_rename_CT, "_CT"))

CT[, days_min_CT := days_to_visit_CT]
CT[, days_max_CT := days_to_visit_CT]

# Es fusiona amb merged_1_final (base de salut)
setDT(CT)
setkey(CT, OASISID, days_min_CT, days_max_CT)
setkey(merged_1_final, OASISID, days_min_health, days_max_health)

merged_2 <- foverlaps(merged_1_final, CT, 
                      by.x = c("OASISID", "days_min_health", "days_max_health"), 
                      by.y = c("OASISID", "days_min_CT", "days_max_CT"),
                      nomatch = NA)

merged_2[, diff_CT := abs(days_to_visit_health - days_to_visit_CT)]
merged_2 <- merged_2[order(OASISID, days_to_visit_health, diff_CT)]
merged_2 <- merged_2[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(CT$session_CT))
sum(!is.na(merged_2$session_CT))
length(unique(merged_2[!is.na(session_CT), session_CT]))

# Assignació única per CT
merged_2_aux <- merged_2[, .SD[which.min(diff_CT)], by = .(session_CT)]

# S'uneixen les files sense assignació
missing_CT <- merged_1_final[!paste(OASISID, days_to_visit_health) %in% 
                               paste(merged_2_aux$OASISID, merged_2_aux$days_to_visit_health)]
cols_CT <- setdiff(names(merged_2_aux), names(missing_CT))
missing_CT[, (cols_CT) := NA]

merged_2_final <- rbindlist(list(merged_2_aux, missing_CT), use.names = TRUE, fill = TRUE)

# Comprovació
sum(!is.na(CT$session_CT))
sum(!is.na(merged_2_final$session_CT))
length(unique(merged_2_final[!is.na(session_CT), session_CT]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara Freesurfer
FS <- fread("path/OASIS3_Freesurfer_output.csv")
setnames(FS, "Subject", "OASISID")
FS[, days_to_visit_FS := as.numeric(gsub(".*_d(\\d+).*", "\\1", MR_session))]

cols_to_rename_FS <- setdiff(names(FS), c("OASISID", "days_to_visit_FS"))
setnames(FS, cols_to_rename_FS, paste0(cols_to_rename_FS, "_FS"))

FS[, days_min_FS := days_to_visit_FS]
FS[, days_max_FS := days_to_visit_FS]

# Es fusiona amb merged_2_final (base amb CT i salut)
setDT(FS)
setkey(FS, OASISID, days_min_FS, days_max_FS)
setkey(merged_2_final, OASISID, days_min_health, days_max_health)

merged_3 <- foverlaps(merged_2_final, FS, 
                      by.x = c("OASISID", "days_min_health", "days_max_health"), 
                      by.y = c("OASISID", "days_min_FS", "days_max_FS"),
                      nomatch = NA)

merged_3[, diff_FS := abs(days_to_visit_health - days_to_visit_FS)]
merged_3 <- merged_3[order(OASISID, days_to_visit_health, diff_FS)]
merged_3 <- merged_3[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(FS$MR_session_FS))
sum(!is.na(merged_3$MR_session_FS))
length(unique(merged_3[!is.na(MR_session_FS), MR_session_FS]))

# Assignació única per FS
merged_3_aux <- merged_3[, .SD[which.min(diff_FS)], by = .(MR_session_FS)]

# S'uneixen les files sense assignació
missing_FS <- merged_2_final[!paste(OASISID, days_to_visit_health) %in% 
                               paste(merged_3_aux$OASISID, merged_3_aux$days_to_visit_health)]
cols_FS <- setdiff(names(merged_3_aux), names(missing_FS))
missing_FS[, (cols_FS) := NA]

merged_3_final <- rbindlist(list(merged_3_aux, missing_FS), use.names = TRUE, fill = TRUE)

# Comprovació
sum(!is.na(FS$MR_session_FS))
sum(!is.na(merged_3_final$MR_session_FS))
length(unique(merged_3_final[!is.na(MR_session_FS), MR_session_FS]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara MR
MR <- fread("path/OASIS3_MR_json.csv")
setnames(MR, "subject_id", "OASISID")
MR[, days_to_visit_MR := as.numeric(gsub(".*_d(\\d+).*", "\\1", label))]

cols_to_rename_MR <- setdiff(names(MR), c("OASISID", "days_to_visit_MR"))
setnames(MR, cols_to_rename_MR, paste0(cols_to_rename_MR, "_MR"))

MR[, days_min_MR := days_to_visit_MR]
MR[, days_max_MR := days_to_visit_MR]

setDT(MR)
setkey(MR, OASISID, days_min_MR, days_max_MR)
setkey(merged_3_final, OASISID, days_min_health, days_max_health)

merged_4 <- foverlaps(merged_3_final, MR, 
                      by.x = c("OASISID", "days_min_health", "days_max_health"), 
                      by.y = c("OASISID", "days_min_MR", "days_max_MR"),
                      nomatch = NA)

merged_4[, diff_MR := abs(days_to_visit_health - days_to_visit_MR)]
merged_4 <- merged_4[order(OASISID, days_to_visit_health, diff_MR)]
merged_4 <- merged_4[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(MR$label_MR))
sum(!is.na(merged_4$label_MR))
length(unique(merged_4[!is.na(label_MR), label_MR]))

# Assignació única per MR
merged_4_aux <- merged_4[, .SD[which.min(diff_MR)], by = .(label_MR)]

# S'uneixen les files sense assignació
missing_MR <- merged_3_final[!paste(OASISID, days_to_visit_health) %in% 
                               paste(merged_4_aux$OASISID, merged_4_aux$days_to_visit_health)]
cols_MR <- setdiff(names(merged_4_aux), names(missing_MR))
missing_MR[, (cols_MR) := NA]

merged_4_final <- rbindlist(list(merged_4_aux, missing_MR), use.names = TRUE, fill = TRUE)

# Comprovació
sum(!is.na(MR$label_MR))
sum(!is.na(merged_4_final$label_MR))
length(unique(merged_4_final[!is.na(label_MR), label_MR]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara PET
PET <- fread("path/OASIS3_PET_json.csv")
setnames(PET, "subject_id", "OASISID")
PET[, days_to_visit_PET := as.numeric(gsub(".*_d(\\d+).*", "\\1", session_id))]

cols_to_rename_PET <- setdiff(names(PET), c("OASISID", "days_to_visit_PET"))
setnames(PET, cols_to_rename_PET, paste0(cols_to_rename_PET, "_PET"))

PET[, days_min_PET := days_to_visit_PET]
PET[, days_max_PET := days_to_visit_PET]

setDT(PET)
setkey(PET, OASISID, days_min_PET, days_max_PET)
setkey(merged_4_final, OASISID, days_min_health, days_max_health)

merged_5 <- foverlaps(merged_4_final, PET, 
                      by.x = c("OASISID", "days_min_health", "days_max_health"), 
                      by.y = c("OASISID", "days_min_PET", "days_max_PET"),
                      nomatch = NA)

merged_5[, diff_PET := abs(days_to_visit_health - days_to_visit_PET)]
merged_5 <- merged_5[order(OASISID, days_to_visit_health, diff_PET)]
merged_5 <- merged_5[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(PET$session_id_PET))
sum(!is.na(merged_5$session_id_PET))
length(unique(merged_5[!is.na(session_id_PET), session_id_PET]))

# Assignació única per PET
merged_5_aux <- merged_5[, .SD[which.min(diff_PET)], by = .(session_id_PET)]

# S'uneixen les files sense assignació
missing_PET <- merged_4_final[!paste(OASISID, days_to_visit_health) %in% 
                                paste(merged_5_aux$OASISID, merged_5_aux$days_to_visit_health)]
cols_PET <- setdiff(names(merged_5_aux), names(missing_PET))
missing_PET[, (cols_PET) := NA]

merged_5_final <- rbindlist(list(merged_5_aux, missing_PET), use.names = TRUE, fill = TRUE)

# Comprovació
sum(!is.na(PET$session_id_PET))
sum(!is.na(merged_5_final$session_id_PET))
length(unique(merged_5_final[!is.na(session_id_PET), session_id_PET]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara PUP
PUP <- fread("path/PUP_braak.csv")
PUP[, OASISID := gsub("_.*", "", `PUP_PUPTIMECOURSEDATA ID`)]
PUP[, days_to_visit_PUP := as.numeric(gsub(".*_d(\\d+).*", "\\1", `PUP_PUPTIMECOURSEDATA ID`))]

cols_to_rename_PUP <- setdiff(names(PUP), c("OASISID", "days_to_visit_PUP"))
setnames(PUP, cols_to_rename_PUP, paste0(cols_to_rename_PUP, "_PUP"))

PUP[, days_min_PUP := days_to_visit_PUP]
PUP[, days_max_PUP := days_to_visit_PUP]

setDT(PUP)
setkey(PUP, OASISID, days_min_PUP, days_max_PUP)
setkey(merged_5_final, OASISID, days_min_health, days_max_health)

merged_6 <- foverlaps(merged_5_final, PUP, 
                      by.x = c("OASISID", "days_min_health", "days_max_health"), 
                      by.y = c("OASISID", "days_min_PUP", "days_max_PUP"),
                      nomatch = NA)

merged_6[, diff_PUP := abs(days_to_visit_health - days_to_visit_PUP)]
merged_6 <- merged_6[order(OASISID, days_to_visit_health, diff_PUP)]
merged_6 <- merged_6[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(PUP$`PUP_PUPTIMECOURSEDATA ID`))
sum(!is.na(merged_6$`PUP_PUPTIMECOURSEDATA ID_PUP`))
length(unique(merged_6[!is.na(`PUP_PUPTIMECOURSEDATA ID_PUP`), `PUP_PUPTIMECOURSEDATA ID_PUP`]))

# Assignació única per observació de PUP
merged_6_aux <- merged_6[, .SD[which.min(diff_PUP)], by = .(`PUP_PUPTIMECOURSEDATA ID_PUP`)]

# S'uneixen les files sense assignació
missing_PUP <- merged_5_final[!paste(OASISID, days_to_visit_health) %in% 
                                paste(merged_6_aux$OASISID, merged_6_aux$days_to_visit_health)]
cols_PUP <- setdiff(names(merged_6_aux), names(missing_PUP))
missing_PUP[, (cols_PUP) := NA]

merged_6_final <- rbindlist(list(merged_6_aux, missing_PUP), use.names = TRUE, fill = TRUE)

# Comprovació
sum(!is.na(PUP$`PUP_PUPTIMECOURSEDATA ID`))
sum(!is.na(merged_6_final$`PUP_PUPTIMECOURSEDATA ID_PUP`))
length(unique(merged_6_final[!is.na(`PUP_PUPTIMECOURSEDATA ID_PUP`), `PUP_PUPTIMECOURSEDATA ID_PUP`]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara psychometrics
psycho <- fread("path/OASIS3_UDSc1_cognitive_assessments.csv")

cols_to_rename_psycho <- setdiff(names(psycho), "OASISID")
setnames(psycho, cols_to_rename_psycho, paste0(cols_to_rename_psycho, "_psycho"))

psycho[, days_min_psycho := days_to_visit_psycho]
psycho[, days_max_psycho := days_to_visit_psycho]

setDT(psycho)
setkey(psycho, OASISID, days_min_psycho, days_max_psycho)
setkey(merged_6_final, OASISID, days_min_health, days_max_health)

merged_7 <- foverlaps(merged_6_final, psycho, 
                      by.x = c("OASISID", "days_min_health", "days_max_health"), 
                      by.y = c("OASISID", "days_min_psycho", "days_max_psycho"),
                      nomatch = NA)

merged_7[, diff_psycho := abs(days_to_visit_health - days_to_visit_psycho)]
merged_7 <- merged_7[order(OASISID, days_to_visit_health, diff_psycho)]
merged_7 <- merged_7[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(psycho$OASIS_session_label_psycho))
sum(!is.na(merged_7$OASIS_session_label_psycho))
length(unique(merged_7[!is.na(OASIS_session_label_psycho), OASIS_session_label_psycho]))

# Assignació única per sessió psicomètrica
merged_7_aux <- merged_7[, .SD[which.min(diff_psycho)], by = .(OASIS_session_label_psycho)]

# S'uneixen les files sense assignació
missing_psycho <- merged_6_final[!paste(OASISID, days_to_visit_health) %in% 
                                   paste(merged_7_aux$OASISID, merged_7_aux$days_to_visit_health)]
cols_psycho <- setdiff(names(merged_7_aux), names(missing_psycho))
missing_psycho[, (cols_psycho) := NA]

merged_7_final <- rbindlist(list(merged_7_aux, missing_psycho), use.names = TRUE, fill = TRUE)

# Comprovació
sum(!is.na(psycho$OASIS_session_label_psycho))
sum(!is.na(merged_7_final$OASIS_session_label_psycho))
length(unique(merged_7_final[!is.na(OASIS_session_label_psycho), OASIS_session_label_psycho]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara UDSa1
UDSa1 <- fread("path/OASIS3_UDSa1_participant_demo.csv")

cols_to_rename_UDSa1 <- setdiff(names(UDSa1), "OASISID")
setnames(UDSa1, cols_to_rename_UDSa1, paste0(cols_to_rename_UDSa1, "_UDSa1"))

UDSa1[, days_min_UDSa1 := days_to_visit_UDSa1]
UDSa1[, days_max_UDSa1 := days_to_visit_UDSa1]

setDT(UDSa1)
setkey(UDSa1, OASISID, days_min_UDSa1, days_max_UDSa1)
setkey(merged_7, OASISID, days_min_health, days_max_health)

merged_8 <- foverlaps(merged_7_final, UDSa1, 
                      by.x = c("OASISID", "days_min_health", "days_max_health"), 
                      by.y = c("OASISID", "days_min_UDSa1", "days_max_UDSa1"),
                      nomatch = NA)
merged_8[, diff := abs(days_to_visit_health - days_to_visit_UDSa1)]
merged_8 <- merged_8[order(OASISID, days_to_visit_health, diff)]
merged_8_final <- merged_8[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(UDSa1$OASIS_session_label_UDSa1))
sum(!is.na(merged_8_final$OASIS_session_label_UDSa1))
length(unique(merged_8_final[!is.na(OASIS_session_label_UDSa1), OASIS_session_label_UDSa1]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara UDSa2
UDSa2 <- fread("path/OASIS3_UDSa2_cs_demo.csv")

cols_to_rename_UDSa2 <- setdiff(names(UDSa2), "OASISID")
setnames(UDSa2, cols_to_rename_UDSa2, paste0(cols_to_rename_UDSa2, "_UDSa2"))

UDSa2[, days_min_UDSa2 := days_to_visit_UDSa2]
UDSa2[, days_max_UDSa2 := days_to_visit_UDSa2]

setDT(UDSa2)
setkey(UDSa2, OASISID, days_min_UDSa2, days_max_UDSa2)
setkey(merged_8, OASISID, days_min_health, days_max_health)

merged_9 <- foverlaps(merged_8_final, UDSa2, 
                      by.x = c("OASISID", "days_min_health", "days_max_health"), 
                      by.y = c("OASISID", "days_min_UDSa2", "days_max_UDSa2"),
                      nomatch = NA)
merged_9[, diff := abs(days_to_visit_health - days_to_visit_UDSa2)]
merged_9 <- merged_9[order(OASISID, days_to_visit_health, diff)]
merged_9_final <- merged_9[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(UDSa2$OASIS_session_label_UDSa2))
sum(!is.na(merged_9_final$OASIS_session_label_UDSa2))
length(unique(merged_9_final[!is.na(OASIS_session_label_UDSa2), OASIS_session_label_UDSa2]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara UDSa3
UDSa3 <- fread("path/OASIS3_UDSa3.csv")

cols_to_rename_UDSa3 <- setdiff(names(UDSa3), "OASISID")
setnames(UDSa3, cols_to_rename_UDSa3, paste0(cols_to_rename_UDSa3, "_UDSa3"))

UDSa3[, days_min_UDSa3 := days_to_visit_UDSa3]
UDSa3[, days_max_UDSa3 := days_to_visit_UDSa3]

setDT(UDSa3)
setkey(UDSa3, OASISID, days_min_UDSa3, days_max_UDSa3)
setkey(merged_9_final, OASISID, days_min_health, days_max_health)

merged_10 <- foverlaps(merged_9_final, UDSa3, 
                       by.x = c("OASISID", "days_min_health", "days_max_health"), 
                       by.y = c("OASISID", "days_min_UDSa3", "days_max_UDSa3"),
                       nomatch = NA)

merged_10[, diff_UDSa3 := abs(days_to_visit_health - days_to_visit_UDSa3)]
merged_10 <- merged_10[order(OASISID, days_to_visit_health, diff_UDSa3)]
merged_10 <- merged_10[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(UDSa3$OASIS_session_label_UDSa3))
sum(!is.na(merged_10$OASIS_session_label_UDSa3))
length(unique(merged_10[!is.na(OASIS_session_label_UDSa3), OASIS_session_label_UDSa3]))

# Assignació única per sessió d’UDSa3
merged_10_aux <- merged_10[, .SD[which.min(diff_UDSa3)], by = .(OASIS_session_label_UDSa3)]

# S'uneixen les files sense assignació
missing_UDSa3 <- merged_9_final[!paste(OASISID, days_to_visit_health) %in% 
                                  paste(merged_10_aux$OASISID, merged_10_aux$days_to_visit_health)]
cols_UDSa3 <- setdiff(names(merged_10_aux), names(missing_UDSa3))
missing_UDSa3[, (cols_UDSa3) := NA]

merged_10_final <- rbindlist(list(merged_10_aux, missing_UDSa3), use.names = TRUE, fill = TRUE)

# Comprovació
sum(!is.na(UDSa3$OASIS_session_label_UDSa3))
sum(!is.na(merged_10_final$OASIS_session_label_UDSa3))
length(unique(merged_10_final[!is.na(OASIS_session_label_UDSa3), OASIS_session_label_UDSa3]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara UDSa4d
UDSa4d <- fread("path/OASIS3_UDSa4D_med_codes.csv")

cols_to_rename_UDSa4d <- setdiff(names(UDSa4d), "OASISID")
setnames(UDSa4d, cols_to_rename_UDSa4d, paste0(cols_to_rename_UDSa4d, "_UDSa4d"))

UDSa4d[, days_min_UDSa4d := days_to_visit_UDSa4d]
UDSa4d[, days_max_UDSa4d := days_to_visit_UDSa4d]

setDT(UDSa4d)
setkey(UDSa4d, OASISID, days_min_UDSa4d, days_max_UDSa4d)
setkey(merged_10_final, OASISID, days_min_health, days_max_health)

merged_11 <- foverlaps(merged_10_final, UDSa4d, 
                       by.x = c("OASISID", "days_min_health", "days_max_health"), 
                       by.y = c("OASISID", "days_min_UDSa4d", "days_max_UDSa4d"),
                       nomatch = NA)

merged_11[, diff_UDSa4d := abs(days_to_visit_health - days_to_visit_UDSa4d)]
merged_11 <- merged_11[order(OASISID, days_to_visit_health, diff_UDSa4d)]
merged_11 <- merged_11[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(UDSa4d$OASIS_session_label_UDSa4d))
sum(!is.na(merged_11$OASIS_session_label_UDSa4d))
length(unique(merged_11[!is.na(OASIS_session_label_UDSa4d), OASIS_session_label_UDSa4d]))

# Assignació única per UDSa4d
merged_11_aux <- merged_11[, .SD[which.min(diff_UDSa4d)], by = .(OASIS_session_label_UDSa4d)]

# S'uneixen les files sense assignació
missing_UDSa4d <- merged_10_final[!paste(OASISID, days_to_visit_health) %in% 
                                    paste(merged_11_aux$OASISID, merged_11_aux$days_to_visit_health)]
cols_UDSa4d <- setdiff(names(merged_11_aux), names(missing_UDSa4d))
missing_UDSa4d[, (cols_UDSa4d) := NA]

# Dataset final amb totes les files de salut
merged_11_final <- rbindlist(list(merged_11_aux, missing_UDSa4d), use.names = TRUE, fill = TRUE)

# Comprovació
sum(!is.na(UDSa4d$OASIS_session_label_UDSa4d))
sum(!is.na(merged_11_final$OASIS_session_label_UDSa4d))
length(unique(merged_11_final[!is.na(OASIS_session_label_UDSa4d), OASIS_session_label_UDSa4d]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara UDSa4g
UDSa4g <- fread("path/OASIS3_UDSa4G_med_names.csv")

cols_to_rename_UDSa4g <- setdiff(names(UDSa4g), "OASISID")
setnames(UDSa4g, cols_to_rename_UDSa4g, paste0(cols_to_rename_UDSa4g, "_UDSa4g"))

UDSa4g[, days_min_UDSa4g := days_to_visit_UDSa4g]
UDSa4g[, days_max_UDSa4g := days_to_visit_UDSa4g]

setDT(UDSa4g)
setkey(UDSa4g, OASISID, days_min_UDSa4g, days_max_UDSa4g)
setkey(merged_11_final, OASISID, days_min_health, days_max_health)

merged_12 <- foverlaps(merged_11_final, UDSa4g, 
                       by.x = c("OASISID", "days_min_health", "days_max_health"), 
                       by.y = c("OASISID", "days_min_UDSa4g", "days_max_UDSa4g"),
                       nomatch = NA)

merged_12[, diff_UDSa4g := abs(days_to_visit_health - days_to_visit_UDSa4g)]
merged_12 <- merged_12[order(OASISID, days_to_visit_health, diff_UDSa4g)]
merged_12 <- merged_12[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(UDSa4g$OASIS_session_label_UDSa4g))
sum(!is.na(merged_12$OASIS_session_label_UDSa4g))
length(unique(merged_12[!is.na(OASIS_session_label_UDSa4g), OASIS_session_label_UDSa4g]))

# Assignació única
merged_12_aux <- merged_12[, .SD[which.min(diff_UDSa4g)], by = .(OASIS_session_label_UDSa4g)]

missing_UDSa4g <- merged_11_final[!paste(OASISID, days_to_visit_health) %in% 
                                    paste(merged_12_aux$OASISID, merged_12_aux$days_to_visit_health)]
cols_UDSa4g <- setdiff(names(merged_12_aux), names(missing_UDSa4g))
missing_UDSa4g[, (cols_UDSa4g) := NA]

merged_12_final <- rbindlist(list(merged_12_aux, missing_UDSa4g), use.names = TRUE, fill = TRUE)

# Comprovació
sum(!is.na(UDSa4g$OASIS_session_label_UDSa4g))
sum(!is.na(merged_12_final$OASIS_session_label_UDSa4g))
length(unique(merged_12_final[!is.na(OASIS_session_label_UDSa4g), OASIS_session_label_UDSa4g]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara UDSb1
UDSb1 <- fread("path/OASIS3_UDSb1_physical_eval.csv")

cols_to_rename_UDSb1 <- setdiff(names(UDSb1), "OASISID")
setnames(UDSb1, cols_to_rename_UDSb1, paste0(cols_to_rename_UDSb1, "_UDSb1"))

UDSb1[, days_min_UDSb1 := days_to_visit_UDSb1]
UDSb1[, days_max_UDSb1 := days_to_visit_UDSb1]

setDT(UDSb1)
setkey(UDSb1, OASISID, days_min_UDSb1, days_max_UDSb1)
setkey(merged_12_final, OASISID, days_min_health, days_max_health)

merged_13 <- foverlaps(merged_12_final, UDSb1, 
                       by.x = c("OASISID", "days_min_health", "days_max_health"), 
                       by.y = c("OASISID", "days_min_UDSb1", "days_max_UDSb1"),
                       nomatch = NA)

merged_13[, diff_UDSb1 := abs(days_to_visit_health - days_to_visit_UDSb1)]
merged_13 <- merged_13[order(OASISID, days_to_visit_health, diff_UDSb1)]
merged_13_final <- merged_13[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(UDSb1$OASIS_session_label_UDSb1))
sum(!is.na(merged_13_final$OASIS_session_label_UDSb1))
length(unique(merged_13_final[!is.na(OASIS_session_label_UDSb1), OASIS_session_label_UDSb1]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara UDSb2
UDSb2 <- fread("path/OASIS3_UDSb2_his_cvd.csv")

cols_to_rename_UDSb2 <- setdiff(names(UDSb2), "OASISID")
setnames(UDSb2, cols_to_rename_UDSb2, paste0(cols_to_rename_UDSb2, "_UDSb2"))

UDSb2[, days_min_UDSb2 := days_to_visit_UDSb2]
UDSb2[, days_max_UDSb2 := days_to_visit_UDSb2]

setDT(UDSb2)
setkey(UDSb2, OASISID, days_min_UDSb2, days_max_UDSb2)
setkey(merged_13_final, OASISID, days_min_health, days_max_health)

merged_14 <- foverlaps(merged_13, UDSb2, 
                       by.x = c("OASISID", "days_min_health", "days_max_health"), 
                       by.y = c("OASISID", "days_min_UDSb2", "days_max_UDSb2"),
                       nomatch = NA)
merged_14[, diff := abs(days_to_visit_health - days_to_visit_UDSb2)]
merged_14 <- merged_14[order(OASISID, days_to_visit_health, diff)]
merged_14_final <- merged_14[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(UDSb2$OASIS_session_label_UDSb2))
sum(!is.na(merged_14_final$OASIS_session_label_UDSb2))
length(unique(merged_14_final[!is.na(OASIS_session_label_UDSb2), OASIS_session_label_UDSb2]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara UDSb3
UDSb3 <- fread("path/OASIS3_UDSb3.csv")

cols_to_rename_UDSb3 <- setdiff(names(UDSb3), "OASISID")
setnames(UDSb3, cols_to_rename_UDSb3, paste0(cols_to_rename_UDSb3, "_UDSb3"))

UDSb3[, days_min_UDSb3 := days_to_visit_UDSb3]
UDSb3[, days_max_UDSb3 := days_to_visit_UDSb3]

setDT(UDSb3)
setkey(UDSb3, OASISID, days_min_UDSb3, days_max_UDSb3)
setkey(merged_14_final, OASISID, days_min_health, days_max_health)

merged_15 <- foverlaps(merged_14_final, UDSb3, 
                       by.x = c("OASISID", "days_min_health", "days_max_health"), 
                       by.y = c("OASISID", "days_min_UDSb3", "days_max_UDSb3"),
                       nomatch = NA)

merged_15[, diff_UDSb3 := abs(days_to_visit_health - days_to_visit_UDSb3)]
merged_15 <- merged_15[order(OASISID, days_to_visit_health, diff_UDSb3)]
merged_15 <- merged_15[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(UDSb3$OASIS_session_label_UDSb3))
sum(!is.na(merged_15$OASIS_session_label_UDSb3))
length(unique(merged_15[!is.na(OASIS_session_label_UDSb3), OASIS_session_label_UDSb3]))

# Assignació única per sessió
merged_15_aux <- merged_15[, .SD[which.min(diff_UDSb3)], by = .(OASIS_session_label_UDSb3)]

# S'uneixen les files sense assignació
missing_UDSb3 <- merged_14_final[!paste(OASISID, days_to_visit_health) %in% 
                                   paste(merged_15_aux$OASISID, merged_15_aux$days_to_visit_health)]
cols_UDSb3 <- setdiff(names(merged_15_aux), names(missing_UDSb3))
missing_UDSb3[, (cols_UDSb3) := NA]

merged_15_final <- rbindlist(list(merged_15_aux, missing_UDSb3), use.names = TRUE, fill = TRUE)

sum(!is.na(UDSb3$OASIS_session_label_UDSb3))
sum(!is.na(merged_15_final$OASIS_session_label_UDSb3))
length(unique(merged_15_final[!is.na(OASIS_session_label_UDSb3), OASIS_session_label_UDSb3]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara UDSb4
UDSb4 <- fread("path/OASIS3_UDSb4_cdr.csv")

cols_to_rename_UDSb4 <- setdiff(names(UDSb4), "OASISID")
setnames(UDSb4, cols_to_rename_UDSb4, paste0(cols_to_rename_UDSb4, "_UDSb4"))

UDSb4[, days_min_UDSb4 := days_to_visit_UDSb4]
UDSb4[, days_max_UDSb4 := days_to_visit_UDSb4]

setDT(UDSb4)
setkey(UDSb4, OASISID, days_min_UDSb4, days_max_UDSb4)
setkey(merged_15_final, OASISID, days_min_health, days_max_health)

merged_16 <- foverlaps(merged_15_final, UDSb4,
                       by.x = c("OASISID", "days_min_health", "days_max_health"),
                       by.y = c("OASISID", "days_min_UDSb4", "days_max_UDSb4"),
                       nomatch = NA)

merged_16[, diff_UDSb4 := abs(days_to_visit_health - days_to_visit_UDSb4)]
merged_16 <- merged_16[order(OASISID, days_to_visit_health, diff_UDSb4)]
merged_16 <- merged_16[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(UDSb4$OASIS_session_label_UDSb4))
sum(!is.na(merged_16$OASIS_session_label_UDSb4))
length(unique(merged_16[!is.na(OASIS_session_label_UDSb4), OASIS_session_label_UDSb4]))

merged_16_aux <- merged_16[, .SD[which.min(diff_UDSb4)], by = .(OASIS_session_label_UDSb4)]

missing_UDSb4 <- merged_15_final[!paste(OASISID, days_to_visit_health) %in%
                                   paste(merged_16_aux$OASISID, merged_16_aux$days_to_visit_health)]
cols_UDSb4 <- setdiff(names(merged_16_aux), names(missing_UDSb4))
missing_UDSb4[, (cols_UDSb4) := NA]

merged_16_final <- rbindlist(list(merged_16_aux, missing_UDSb4), use.names = TRUE, fill = TRUE)

sum(!is.na(UDSb4$OASIS_session_label_UDSb4))
sum(!is.na(merged_16_final$OASIS_session_label_UDSb4))
length(unique(merged_16_final[!is.na(OASIS_session_label_UDSb4), OASIS_session_label_UDSb4]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara UDSb5
UDSb5 <- fread("path/OASIS3_UDSb5_npiq.csv")

cols_to_rename_UDSb5 <- setdiff(names(UDSb5), "OASISID")
setnames(UDSb5, cols_to_rename_UDSb5, paste0(cols_to_rename_UDSb5, "_UDSb5"))

UDSb5[, days_min_UDSb5 := days_to_visit_UDSb5]
UDSb5[, days_max_UDSb5 := days_to_visit_UDSb5]

setDT(UDSb5)
setkey(UDSb5, OASISID, days_min_UDSb5, days_max_UDSb5)
setkey(merged_16_final, OASISID, days_min_health, days_max_health)

merged_17 <- foverlaps(merged_16_final, UDSb5,
                       by.x = c("OASISID", "days_min_health", "days_max_health"),
                       by.y = c("OASISID", "days_min_UDSb5", "days_max_UDSb5"),
                       nomatch = NA)

merged_17[, diff_UDSb5 := abs(days_to_visit_health - days_to_visit_UDSb5)]
merged_17 <- merged_17[order(OASISID, days_to_visit_health, diff_UDSb5)]
merged_17_final <- merged_17[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(UDSb5$OASIS_session_label_UDSb5))
sum(!is.na(merged_17_final$OASIS_session_label_UDSb5))
length(unique(merged_17_final[!is.na(OASIS_session_label_UDSb5), OASIS_session_label_UDSb5]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara UDSb6
UDSb6 <- fread("path/OASIS3_UDSb6_gds.csv")

cols_to_rename_UDSb6 <- setdiff(names(UDSb6), "OASISID")
setnames(UDSb6, cols_to_rename_UDSb6, paste0(cols_to_rename_UDSb6, "_UDSb6"))

UDSb6[, days_min_UDSb6 := days_to_visit_UDSb6]
UDSb6[, days_max_UDSb6 := days_to_visit_UDSb6]

setDT(UDSb6)
setkey(UDSb6, OASISID, days_min_UDSb6, days_max_UDSb6)
setkey(merged_17_final, OASISID, days_min_health, days_max_health)

merged_18 <- foverlaps(merged_17_final, UDSb6,
                       by.x = c("OASISID", "days_min_health", "days_max_health"),
                       by.y = c("OASISID", "days_min_UDSb6", "days_max_UDSb6"),
                       nomatch = NA)

merged_18[, diff_UDSb6 := abs(days_to_visit_health - days_to_visit_UDSb6)]
merged_18 <- merged_18[order(OASISID, days_to_visit_health, diff_UDSb6)]
merged_18 <- merged_18[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(UDSb6$OASIS_session_label_UDSb6))
sum(!is.na(merged_18$OASIS_session_label_UDSb6))
length(unique(merged_18[!is.na(OASIS_session_label_UDSb6), OASIS_session_label_UDSb6]))

merged_18_aux <- merged_18[, .SD[which.min(diff_UDSb6)], by = .(OASIS_session_label_UDSb6)]

missing_UDSb6 <- merged_17_final[!paste(OASISID, days_to_visit_health) %in%
                                   paste(merged_18_aux$OASISID, merged_18_aux$days_to_visit_health)]
cols_UDSb6 <- setdiff(names(merged_18_aux), names(missing_UDSb6))
missing_UDSb6[, (cols_UDSb6) := NA]

merged_18_final <- rbindlist(list(merged_18_aux, missing_UDSb6), use.names = TRUE, fill = TRUE)

sum(!is.na(UDSb6$OASIS_session_label_UDSb6))
sum(!is.na(merged_18_final$OASIS_session_label_UDSb6))
length(unique(merged_18_final[!is.na(OASIS_session_label_UDSb6), OASIS_session_label_UDSb6]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara UDSb6
UDSb7 <- fread("path/OASIS3_UDSb7_faq_fas.csv")

cols_to_rename_UDSb7 <- setdiff(names(UDSb7), "OASISID")
setnames(UDSb7, cols_to_rename_UDSb7, paste0(cols_to_rename_UDSb7, "_UDSb7"))

UDSb7[, days_min_UDSb7 := days_to_visit_UDSb7]
UDSb7[, days_max_UDSb7 := days_to_visit_UDSb7]

setDT(UDSb7)
setkey(UDSb7, OASISID, days_min_UDSb7, days_max_UDSb7)
setkey(merged_18, OASISID, days_min_health, days_max_health)

merged_19 <- foverlaps(merged_18_final, UDSb7, 
                       by.x = c("OASISID", "days_min_health", "days_max_health"), 
                       by.y = c("OASISID", "days_min_UDSb7", "days_max_UDSb7"),
                       nomatch = NA)
merged_19[, diff := abs(days_to_visit_health - days_to_visit_UDSb7)]
merged_19 <- merged_19[order(OASISID, days_to_visit_health, diff)]
merged_19_final <- merged_19[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(UDSb7$OASIS_session_label_UDSb7))
sum(!is.na(merged_19_final$OASIS_session_label_UDSb7))
length(unique(merged_19_final[!is.na(OASIS_session_label_UDSb7), OASIS_session_label_UDSb7]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara UDSb8
UDSb8 <- fread("path/OASIS3_UDSb8_neuro_exam.csv")

cols_to_rename_UDSb8 <- setdiff(names(UDSb8), "OASISID")
setnames(UDSb8, cols_to_rename_UDSb8, paste0(cols_to_rename_UDSb8, "_UDSb8"))

UDSb8[, days_min_UDSb8 := days_to_visit_UDSb8]
UDSb8[, days_max_UDSb8 := days_to_visit_UDSb8]

setDT(UDSb8)
setkey(UDSb8, OASISID, days_min_UDSb8, days_max_UDSb8)
setkey(merged_19, OASISID, days_min_health, days_max_health)

merged_20 <- foverlaps(merged_19_final, UDSb8, 
                       by.x = c("OASISID", "days_min_health", "days_max_health"), 
                       by.y = c("OASISID", "days_min_UDSb8", "days_max_UDSb8"),
                       nomatch = NA)
merged_20[, diff := abs(days_to_visit_health - days_to_visit_UDSb8)]
merged_20 <- merged_20[order(OASISID, days_to_visit_health, diff)]
merged_20_final <- merged_20[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(UDSb8$OASIS_session_label_UDSb8))
sum(!is.na(merged_20_final$OASIS_session_label_UDSb8))
length(unique(merged_20_final[!is.na(OASIS_session_label_UDSb8), OASIS_session_label_UDSb8]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara UDSb9
UDSb9 <- fread("path/OASIS3_UDSb9_symptoms.csv")

cols_to_rename_UDSb9 <- setdiff(names(UDSb9), "OASISID")
setnames(UDSb9, cols_to_rename_UDSb9, paste0(cols_to_rename_UDSb9, "_UDSb9"))

UDSb9[, days_min_UDSb9 := days_to_visit_UDSb9]
UDSb9[, days_max_UDSb9 := days_to_visit_UDSb9]

setDT(UDSb9)
setkey(UDSb9, OASISID, days_min_UDSb9, days_max_UDSb9)
setkey(merged_20, OASISID, days_min_health, days_max_health)

merged_21 <- foverlaps(merged_20_final, UDSb9, 
                       by.x = c("OASISID", "days_min_health", "days_max_health"), 
                       by.y = c("OASISID", "days_min_UDSb9", "days_max_UDSb9"),
                       nomatch = NA)
merged_21[, diff := abs(days_to_visit_health - days_to_visit_UDSb9)]
merged_21 <- merged_21[order(OASISID, days_to_visit_health, diff)]
merged_21_final <- merged_21[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(UDSb9$OASIS_session_label_UDSb9))
sum(!is.na(merged_21_final$OASIS_session_label_UDSb9))
length(unique(merged_21_final[!is.na(OASIS_session_label_UDSb9), OASIS_session_label_UDSb9]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara UDSd1
UDSd1 <- fread("path/OASIS3_UDSd1_diagnoses.csv")

cols_to_rename_UDSd1 <- setdiff(names(UDSd1), "OASISID")
setnames(UDSd1, cols_to_rename_UDSd1, paste0(cols_to_rename_UDSd1, "_UDSd1"))

UDSd1[, days_min_UDSd1 := days_to_visit_UDSd1]
UDSd1[, days_max_UDSd1 := days_to_visit_UDSd1]

setDT(UDSd1)
setkey(UDSd1, OASISID, days_min_UDSd1, days_max_UDSd1)
setkey(merged_21, OASISID, days_min_health, days_max_health)

merged_22 <- foverlaps(merged_21_final, UDSd1, 
                       by.x = c("OASISID", "days_min_health", "days_max_health"), 
                       by.y = c("OASISID", "days_min_UDSd1", "days_max_UDSd1"),
                       nomatch = NA)
merged_22[, diff := abs(days_to_visit_health - days_to_visit_UDSd1)]
merged_22 <- merged_22[order(OASISID, days_to_visit_health, diff)]
merged_22_final <- merged_22[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(UDSd1$OASIS_session_label_UDSd1))
sum(!is.na(merged_22_final$OASIS_session_label_UDSd1))
length(unique(merged_22_final[!is.na(OASIS_session_label_UDSd1), OASIS_session_label_UDSd1]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara UDSd2
UDSd2 <- fread("path/OASIS3_UDSd2_med_conditions.csv")

cols_to_rename_UDSd2 <- setdiff(names(UDSd2), "OASISID")
setnames(UDSd2, cols_to_rename_UDSd2, paste0(cols_to_rename_UDSd2, "_UDSd2"))

UDSd2[, days_min_UDSd2 := days_to_visit_UDSd2]
UDSd2[, days_max_UDSd2 := days_to_visit_UDSd2]

setDT(UDSd2)
setkey(UDSd2, OASISID, days_min_UDSd2, days_max_UDSd2)
setkey(merged_22, OASISID, days_min_health, days_max_health)

merged_23 <- foverlaps(merged_22_final, UDSd2, 
                       by.x = c("OASISID", "days_min_health", "days_max_health"), 
                       by.y = c("OASISID", "days_min_UDSd2", "days_max_UDSd2"),
                       nomatch = NA)
merged_23[, diff := abs(days_to_visit_health - days_to_visit_UDSd2)]
merged_23 <- merged_23[order(OASISID, days_to_visit_health, diff)]
merged_23_final <- merged_23[, .SD[1], by = .(OASISID, days_to_visit_health)]

sum(!is.na(UDSd2$OASIS_session_label_UDSd2))
sum(!is.na(merged_23_final$OASIS_session_label_UDSd2))
length(unique(merged_23_final[!is.na(OASIS_session_label_UDSd2), OASIS_session_label_UDSd2]))

# ─────────────────────────────────────────────────────────────
# Es carrega i prepara demographics
demo <- fread("path/OASIS3_demographics.csv")

cols_to_rename_demo <- setdiff(names(demo), "OASISID")
setnames(demo, cols_to_rename_demo, paste0(cols_to_rename_demo, "_demo"))

OASIS3_merged <- merge(merged_23_final, demo, by = "OASISID", all.x = TRUE)

OASIS3_merged <- OASIS3_merged %>% select(-starts_with("days_min"), -starts_with("days_max"))

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
fwrite(OASIS3_merged, file = "path/OASIS3_merged.csv", sep = ",", na = "NA")
