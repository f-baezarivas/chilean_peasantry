### CAF 2021: PEASANT HOUSEHOLDS AND COMBINATION OF THE HOUSEHOLD AND PRODUCTIVE DATASETS ###
# 1. Adds the peasant classification of the household questionnaire (observed or imputed, hog_afc_est_hp) to caf21_hog.
# 2. Combines the household and productive datasets (caf21). They share no identifier, so they are matched on all
#    the variables available in both, keeping only unambiguous matches.
# 3. Final peasant classification of households (hog_afc_est in caf21_hog and caf21_mie): the classification of the
#    productive questionnaire for matched households, and that of the household questionnaire for the rest.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr)

if (!exists("caf21_hog")) source("data_process/04_caf21.R")


# PEASANT HOUSEHOLDS -----------------------------------------------------------

### Classification of the household questionnaire (hog_afc_est_hp)
# Observed for households hiring no workers (peasant) or permanent workers (non-peasant); imputed for the 17,438
# households hiring only temporary workers, whose number of days hired is unknown (hog_afc_est_mis is missing).
# The imputation is produced by 04_01_caf21_imputaciones.R. As it runs slowly, its output is used (it is not re-run).
caf21_imp <- readRDS("output/caf21_imp.rds")

# Households classified as peasant in at least 9 of the 10 imputations are peasant households (observed households
# keep their observed value in every imputation). 115,132 peasant households with this classification.
# This is not the final classification of caf21_hog: see "Final classification" below.
caf21_hog <- caf21_hog %>%
  left_join(caf21_imp %>%
              transmute(id_hog, hog_afc_est_hp = if_else(coalesce(hog_afc_est_mis_sum >= 0.9, FALSE), 1, 0)),
            by = "id_hog")


# COMBINATION OF HOUSEHOLD AND PRODUCTIVE DATASETS (CAF21) ---------------------

### Variables shared by both datasets used for matching (only extended questionnaires)
# 40 variables available in the same format in both datasets (the thesis used 31; ga201-ga209 were added later):
#   - Municipality (1): com. In caf21_ce, the municipality where most of the unit's land is located
#   - Main agricultural, livestock or forestry activity generating the most income (1): g3, 45 categories in both
#     datasets. g2 (main activity) is excluded: it has 73 categories in caf21_ce but only 18 in caf21_hog
#   - Sales bracket (1): hp280_2
#   - Land use, hectares (19): us61_1 to us61_19
#   - Livestock, heads by species (16): cattle (ga161), sheep (ga172), pigs (ga182), goats (ga180),
#     poultry (ga185, ga186), beehives (ga199) and other livestock (ga201 to ga209)
#   - Hired labor (2): permanent (tr244) and temporary (tr250) workers
vars_match <- c("com", "g3", "hp280_2", paste0("us61_", 1:19),
                "ga161", "ga172", "ga182", "ga180", "ga185", "ga186", "ga199", paste0("ga", 201:209),
                "tr244", "tr250")

### Matching on vars_match
caf21 <- caf21_hog %>%
  select(-g2) %>% # The complete version comes from caf21_ce
  inner_join(caf21_ce%>%
               filter(g1==1), # Only natural persons: households can only match their own productive unit
             by = vars_match,
             relationship = "many-to-many") # Ambiguous matches are dropped below
# caf21 keeps both classifications: hog_afc_est from the productive questionnaire (gold standard) and hog_afc_est_hp
# from the household questionnaire (observed or imputed), used to validate the latter (Table S8)

### Only unambiguous matches
# Only one-to-one matches are kept: 952 households matched more than one unit or shared their unit with another
# household, and 7,201 found no match, so 121,454 of the 129,607 households (93.7%) are matched.
# Hence (analysis rules in master.R):
# - the number of peasant households is estimated with caf21_pre (both questionnaires);
# - household analyses use caf21_hog;
# - productive analyses use caf21_ce;
# - analyses relating household and farm variables use this combined dataset (caf21), with cues == "Extendido"
#   and the classification of the productive questionnaire (hog_afc_est). Matched non-peasant units are biased
#   towards smaller units (82.8% matched, analysis/comparacion_emparejadas.R), so peasant/non-peasant comparisons
#   of productive resources use caf21_ce.
# All characterization is compared with 2007 households with cues_21 == "Extendido" (03_caf07.R).
caf21 <- caf21 %>%
  add_count(id_hog, name = "n_hog") %>%
  add_count(guid_ce, name = "n_ce") %>%
  filter(n_hog == 1, n_ce == 1) %>%
  select(-n_hog, -n_ce)



# PEASANT HOUSEHOLDS: FINAL CLASSIFICATION -------------------------------------

### Final classification of caf21_hog (hog_afc_est), with its source (hog_afc_fuente)
# Decided on 24-11-2026. Each household gets the best classification available:
#   - Matched one-to-one (121,454): classification of the productive questionnaire, which has all the variables of
#     the definition (natural person, no hired administrator, less than 240 days of hired work per year)
#   - Unmatched (8,153: 7,201 without a match and 952 ambiguous): classification of the household questionnaire,
#     observed (6,623) or imputed if the household hired only temporary workers (1,530)
# The imputation is thus the final classification of only 1,530 households. In matched pairs the household
# classification agrees with the productive one in 95.1% of cases (98.7% observed, 71.5% imputed; Table S8), which is
# why the productive classification is preferred wherever it is available.
caf21_hog <- caf21_hog %>%
  left_join(select(caf21, id_hog, hog_afc_est), by = "id_hog") %>% # caf21: one-to-one matches only (UAC not yet appended)
  mutate(hog_afc_fuente = factor(case_when(!is.na(hog_afc_est) ~ "Cuestionario productivo",
                                           !is.na(hog_afc_est_mis) ~ "Cuestionario de hogar, observada",
                                           TRUE ~ "Cuestionario de hogar, imputada"),
                                 levels = c("Cuestionario productivo", "Cuestionario de hogar, observada",
                                            "Cuestionario de hogar, imputada")),
         hog_afc_est = coalesce(hog_afc_est, hog_afc_est_hp))
# Check: count(caf21_hog, hog_afc_fuente) should give 121,454 / 6,623 / 1,530

caf21_mie <- caf21_mie %>%
  left_join(select(caf21_hog, id_hog, hog_afc_est, n_mie_rec), by = "id_hog")


# COMBINED DATASET: ABBREVIATED QUESTIONNAIRES ---------------------------------

### Abbreviated questionnaires
# UAC are appended so that caf21 covers all productive units, but they have no household variables: filter
# cues == "Extendido" for characterization, and use cues == "Abreviado" only to describe UAC separately.
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
