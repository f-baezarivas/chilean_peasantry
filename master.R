######## ***La doble reducción del campesinado chileno en el siglo XXI*** ###################
### El código reproduce los resultados del artículo y del material suplementario
# Este trabajo se enmarca en la tesis doctoral de Fernando Baeza
# Baeza, F. (2026). Hacia una comprensión de la heterogeneidad del campesinado chileno desde una perspectiva geográfica: las estrategias productivas de los hogares campesinos y los mercados regionales.

# DATA PROCESSING --------------------------------------------------------------

# Run only once if there are no output files
source("data_process/01_dpa.R") # Political-administrative division codes
source("data_process/02_casen.R") # CASEN 2000-2022: persons (casen_mie) and households (casen)
source("data_process/03_caf07.R") # CAF 2007: households (caf07) and members (caf07_mie)
source("data_process/04_caf21.R") # CAF 2021: productive units (caf21_ce, caf21_ca, caf21_pre), households and members
# source("data_process/04_01_caf21_imputaciones.R") # Imputation of the peasant classification (slow; not run by default)
source("data_process/04_02_caf21_combinacion.R") # CAF 2021: peasant households and combined dataset (caf21)


# ANALYSIS RULES ---------------------------------------------------------------
# Decided on 23-11-2026. Every analysis script follows these rules (supplementary material, section S6).
#
# 1. COUNTS (number of peasant households and their territorial distribution) use the full census universes:
#    - CAF 2007: caf07, hog_afc_est == 1 (220,038)
#    - CAF 2021: caf21_pre, hog_afc_est == 1, both questionnaires (149,258 = 114,996 extended + 34,262 abbreviated)
#    The count harmonized with the CAF 2007 minimums (144,464; -34.3%) is a sensitivity check
#    (analysis/revisor_caf21.R, block 3).
#
# 2. CHARACTERIZATION (any variable other than the count of households) compares the same segment in both
#    censuses: units that answered, or would have answered, the CAF 2021 extended questionnaire (2 ha or more, or
#    sales). The abbreviated questionnaire (UAC) has no household, management or labor variables, so keeping the
#    small non-commercial 2007 units would attenuate the decreases (and inflate the increases) of the productive
#    and income variables (Table S10).
#    - CAF 2007: caf07 / caf07_mie, hog_afc_est == 1 & cues_21 == "Extendido" (176,464 households).
#      cues_21 is built in 03_caf07.R: "Extendido" if sup_tot >= 2 or n_rub_com >= 1 (commercial production lines
#      according to the cut-off points in helpers/caf.R, a proxy for sales, which CAF 2007 did not record).
#    - CAF 2021, productive variables: caf21_ce (extended questionnaire only), hog_afc_est == 1 (114,996)
#    - CAF 2021, household and member variables: caf21_hog and caf21_mie (household questionnaire, which only
#      extended units answered), hog_afc_est == 1. Since 24-11-2026 hog_afc_est is the final classification:
#      productive questionnaire for matched households, household questionnaire (observed or imputed) for the rest
#      (source in hog_afc_fuente; 04_02_caf21_combinacion.R)
#    - CAF 2021, variables relating household and farm: caf21 with cues == "Extendido" (one-to-one matches), using
#      the classification of the productive questionnaire (hog_afc_est; hog_afc_est_hp is the household one)
#    The number of peasant workers is a characterization variable: in 2021 it is only known for extended units, so
#    it is compared with the workers of 2007 households with cues_21 == "Extendido".
#
# 3. UAC (caf21_pre or caf21 with cues == "Abreviado") are described separately, with the variables of the
#    abbreviated questionnaire (land use, presence of livestock species, production lines, location).
#
# 4. The full 2007 universe (including cues_21 == "Abreviado") is only characterized in the sensitivity analysis of
#    the supplementary material (Table S10; analysis/revisor_caf21.R, block 4).
#
# 5. CASEN is not affected by these rules.


# ANALYSIS ---------------------------------------------------------------------

source("analysis/01_methods.R") # Section 4: households for magnitude and characterization, unit of observation, matching and imputation, non-response

