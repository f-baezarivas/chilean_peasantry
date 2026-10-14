### CAF 2021: PEASANT HOUSEHOLDS AND COMBINATION OF THE HOUSEHOLD AND PRODUCTIVE DATASETS ###
# 1. Adds the (imputed) peasant classification to the household and member datasets (caf21_hog, caf21_mie).
# 2. Combines the household and productive datasets (caf21). They share no identifier, so they are matched on all
#    the variables available in both, keeping only unambiguous matches.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr)

if (!exists("caf21_hog")) source("data_process/04_caf21.R")


# PEASANT HOUSEHOLDS -----------------------------------------------------------

### Imputed classification
# Produced by 04_01_caf21_imputaciones.R. As it runs slowly, use the output.
caf21_imp <- readRDS("output/caf21_imp.rds")

# Households classified as peasant in at least 9 of the 10 imputations are peasant households
caf21_hog <- caf21_hog %>%
  left_join(caf21_imp %>%
              transmute(id_hog, hog_afc_est = if_else(coalesce(hog_afc_est_mis_sum >= 0.9, FALSE), 1, 0)),
            by = "id_hog")

caf21_mie <- caf21_mie %>%
  left_join(select(caf21_hog, id_hog, hog_afc_est, n_mie_rec), by = "id_hog")


# COMBINATION OF HOUSEHOLD AND PRODUCTIVE DATASETS (CAF21) ---------------------

### Matching on the variables shared by both datasets (only extended questionnaires)
caf21 <- caf21_hog %>%
  select(-g2) %>% # The complete version comes from caf21_ce
  inner_join(caf21_ce, by = c("com", "g3", "hp280_2", paste0("us61_", 1:19),
                              "ga161", "ga172", "ga182", "ga180", "ga185", "ga186", "ga199", paste0("ga", 201:209),
                              "tr244", "tr250"),
             relationship = "many-to-many") %>% # Ambiguous matches are dropped below
  rename(hog_afc_est = hog_afc_est.y, # From the productive questionnaire (gold standard)
         hog_afc_est_hp = hog_afc_est.x) # From the household questionnaire (imputed)

### Only unambiguous matches
# Households or productive units matched more than once are dropped (about 9 thousand), so that all matches are correct.
# Hence, the number of peasant households is estimated with caf21_pre; household analyses use caf21_hog, productive
# analyses caf21_ce, and multivariate analyses this combined dataset.
caf21 <- caf21 %>%
  add_count(id_hog, name = "n_hog") %>%
  add_count(guid_ce, name = "n_ce") %>%
  filter(n_hog == 1, n_ce == 1) %>%
  select(-n_hog, -n_ce)

### Abbreviated questionnaires
caf21 <- caf21 %>%
  bind_rows(add_com_0721(caf21_ca)) %>%
  mutate(cues = case_when(substr(guid_ce, 1, 2) == "CE" ~ "Extendido",
                          substr(guid_ce, 1, 2) == "CA" ~ "Abreviado"),
         n_rub_aut = case_when(cues == "Extendido" ~ n_rub_agr_aut + n_rub_gan_aut + n_rub_pos_aut,
                               cues == "Abreviado" ~ n_rub_agr_aut + n_rub_gan_aut),
         n_rub_com = case_when(cues == "Extendido" ~ n_rub_agr_com + n_rub_gan_com + n_rub_pos_com,
                               cues == "Abreviado" ~ n_rub_agr_com + n_rub_gan_com)) %>%
  add_sup_pct()


# SAVING ------------------------------------------------------------------

saveRDS(caf21_hog, "output/caf21_hog.rds")
saveRDS(caf21_mie, "output/caf21_mie.rds")
saveRDS(caf21, "output/caf21.rds")
