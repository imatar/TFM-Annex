# ─────────────────────────────────────────────────────────────
# Llibreries
library(dplyr)
library(readr)

# ─────────────────────────────────────────────────────────────
# Es carrega el fitxer
df <- read_csv("path/OASIS3_PUP.csv")

# ─────────────────────────────────────────────────────────────
# Es defineixen les regions per etapa Braak
braak12 <- c("PET_fSUVR_L_CTX_ENTORHINAL", "PET_fSUVR_R_CTX_ENTORHINAL")
braak34 <- c("PET_fSUVR_L_CTX_FUSIFORM", "PET_fSUVR_R_CTX_FUSIFORM",
             "PET_fSUVR_L_CTX_PARAHPCMPL", "PET_fSUVR_R_CTX_PARAHPCMPL",
             "PET_fSUVR_L_CTX_INFRTMP", "PET_fSUVR_R_CTX_INFTMP",
             "PET_fSUVR_L_CTX_MIDTMP", "PET_fSUVR_R_CTX_MIDTMP",
             "PET_fSUVR_L_CTX_INSULA", "PET_fSUVR_R_CTX_INSULA")
braak56 <- c("PET_fSUVR_L_CTX_PRECUNEUS", "PET_fSUVR_R_CTX_PRECUNEUS",
             "PET_fSUVR_L_CTX_POSTCNG", "PET_fSUVR_R_CTX_POSTCNG",
             "PET_fSUVR_L_CTX_SUPERFRN", "PET_fSUVR_R_CTX_SUPERFRN",
             "PET_fSUVR_L_CTX_SUPERTMP", "PET_fSUVR_R_CTX_SUPERTMP",
             "PET_fSUVR_L_CTX_INFRPRTL", "PET_fSUVR_R_CTX_INFPRTL")

# ─────────────────────────────────────────────────────────────
# Es calculen els Braak stages com mitjanes SUVr
df <- df %>%
  rowwise() %>%
  mutate(
    Bk12 = mean(c_across(all_of(braak12)), na.rm = TRUE),
    Bk34 = mean(c_across(all_of(braak34)), na.rm = TRUE),
    Bk56 = mean(c_across(all_of(braak56)), na.rm = TRUE)
  ) %>%
  ungroup()

# ─────────────────────────────────────────────────────────────
# Es guarda el fitxer
write_csv(df, "path/PUP_braak.csv")