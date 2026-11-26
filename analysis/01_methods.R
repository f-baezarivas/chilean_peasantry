### SECTION 4: DATA AND METHODS. DIFFERENCES BETWEEN CAF 2007 AND CAF 2021 ###
# Reproduces the figures of the last paragraphs of section 4 of the article:
#   1. Self-consumption units (UAC) and the number of households used for magnitude and for characterization
#   2. Unit of observation: farm (explotacion, 2007) vs. agricultural production unit (UPA, 2021)
#   3. CAF 2021 particularity 1: separate household and productive datasets (matching and imputation)
#   4. CAF 2021 particularity 2: non-response (no logro) and the projection of the number and characteristics of
#      households
# The analysis rules applied here (counts with full universes, characterization with the CAF 2021
# extended-questionnaire segment) are documented in master.R.
# Detailed checks behind the supplementary material (section S6) are kept as annexes:
#   - analysis/revisor_caf21.R: harmonized 2021 universe, duplicate households, sensitivity of 2007 (Table S10)
#   - analysis/comparacion_emparejadas.R: matched units vs. their source datasets
#
# Requires the output files of data_process/ (master.R), the CAF 2021 .csv files cached in input/caf21 and the
# municipal non-response file input/caf21/no_logro_comunal.xlsx (not in the public repository; see block 4).
# Results are printed and saved in output/01_methods.rds.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, readr, readxl)

source("data_process/01_dpa.R") # dpa_cod: official (cod_com_2018) and harmonized (cod_com_0721) municipalities

caf07 <- readRDS("output/caf07.rds")
caf21_pre <- readRDS("output/caf21_pre.rds")
caf21_ce <- readRDS("output/caf21_ce.rds")
caf21_hog <- readRDS("output/caf21_hog.rds")
caf21 <- readRDS("output/caf21.rds")

pct <- function(x) round(100 * mean(x), 1)
var_pct <- function(t1, t0) round(100 * (t1 - t0) / t0, 1)

# Reads a productive-questionnaire file of CAF 2021 (UTF-8, decimal comma), as in data_process/04_caf21.R
read_caf21 <- function(file) {
  read_delim(file.path("input/caf21", paste0(file, ".csv")), delim = ";", locale = locale(decimal_mark = ","),
             trim_ws = TRUE, show_col_types = FALSE, name_repair = "minimal") %>%
    select(which(names(.) != "")) %>%
    rename_with(tolower)
}

methods <- list()


# 1. SELF-CONSUMPTION UNITS (UAC) AND NUMBER OF HOUSEHOLDS ---------------------
# In 2007, units below the inclusion minimums (surface, livestock or several production lines) were not censused.
# In 2021 all units with agricultural activity were registered, but those with less than 2 ha and no sales (UAC)
# answered an abbreviated questionnaire without household, management or labor variables. UAC with production in
# at least one production line are counted as peasant households (caf21_pre, hog_afc_est == 1).
#
# Rules (master.R):
#   - Magnitude: full universes. 2007: all peasant households of caf07. 2021: caf21_pre, both questionnaires.
#   - Characterization: same segment in both censuses. 2007: peasant households that would have answered the 2021
#     extended questionnaire (cues_21 == "Extendido": sup_tot >= 2 or at least one commercial production line,
#     n_rub_com >= 1; built in data_process/03_caf07.R). 2021: extended questionnaire, i.e. caf21_ce for productive
#     variables and caf21_hog / caf21_mie for household variables (final classification hog_afc_est).

### 2007: peasant households by the questionnaire they would have answered in 2021
# Expected: 220,038 = 176,464 "Extendido" + 43,574 "Abreviado" (19.8%)
n07 <- caf07 %>%
  filter(hog_afc_est == 1) %>%
  count(cues_21) %>%
  mutate(pct = round(100 * n / sum(n), 1))

### 2021: units of the productive questionnaires by questionnaire and peasant classification
# Expected: extended 114,996 peasant; abbreviated 34,262 peasant (UAC with production) and 2,666 without production
# (36,928 abbreviated questionnaires); 149,258 peasant households in total (UAC = 23.0%)
n21 <- caf21_pre %>%
  count(cues, hog_afc_est) %>%
  arrange(desc(cues), desc(hog_afc_est))

n21_afc <- caf21_pre %>%
  filter(hog_afc_est == 1) %>%
  count(cues) %>%
  mutate(pct = round(100 * n / sum(n), 1))

### Number of households for magnitude and for characterization (rows "N" of Table 1)
n07_tot <- sum(caf07$hog_afc_est == 1)
n07_ext <- sum(caf07$hog_afc_est == 1 & caf07$cues_21 == "Extendido")
n21_tot <- sum(caf21_pre$hog_afc_est == 1)
n21_ext <- sum(caf21_pre$hog_afc_est == 1 & caf21_pre$cues == "Extendido") # = caf21_ce
n21_hog <- sum(caf21_hog$hog_afc_est == 1)

tab_n <- tibble(
  uso = c("Magnitud: hogares campesinos (universos completos)",
          "Caracterización, variables productivas",
          "Caracterización, variables del hogar",
          "Excluidos de la caracterización"),
  base_2007 = c("caf07", "caf07, cues_21 == Extendido", "caf07, cues_21 == Extendido", "caf07, cues_21 == Abreviado"),
  n_2007 = c(n07_tot, n07_ext, n07_ext, n07_tot - n07_ext),
  base_2021 = c("caf21_pre (extendido + UAC)", "caf21_ce (extendido)", "caf21_hog (clasificación final)",
                "caf21_pre, UAC (cues == Abreviado)"),
  n_2021 = c(n21_tot, n21_ext, n21_hog, n21_tot - n21_ext)) %>%
  mutate(var_abs = if_else(row_number() <= 3, n_2021 - n_2007, NA),
         var_pct = if_else(row_number() <= 3, var_pct(n_2021, n_2007), NA),
         pct_excluidos_2007 = if_else(row_number() == 4, round(100 * n_2007 / n07_tot, 1), NA),
         pct_excluidos_2021 = if_else(row_number() == 4, round(100 * n_2021 / n21_tot, 1), NA))
# Expected: magnitude 220,038 -> 149,258 (-70,780; -32.1%). Characterization: 176,464 in 2007; 114,996 (productive)
# and 115,110 (household) in 2021. Excluded: 43,574 (19.8%) in 2007 and 34,262 UAC (23.0%) in 2021.

print(n07); print(n21); print(as.data.frame(tab_n))

methods$n <- list(n07 = n07, n21 = n21, n21_afc = n21_afc, tabla_n = tab_n)


# 2. UNIT OF OBSERVATION: PLOTS AND MUNICIPALITIES PER PRODUCTIVE UNIT ----------
# In 2007 the unit was the farm, whose plots had to be in a single municipality (a producer with land in two
# municipalities had two farms). In 2021 the UPA groups all the plots of a producer, wherever they are.
# Section 8 of CAF 2021 has one row per plot and municipality (a plot extending over several municipalities appears
# once per municipality), for both questionnaires (GUID CE... and CA...).
# Expected (% of units): peasant UPA (extended) 85.6% one plot, 14.4% two or more, 0.9% in more than one
# municipality; peasant UAC 99.8%, 0.2%, 0.0%; non-peasant UPA 71.0%, 29.0%, 9.4%.
# The article reports the 0.9% of peasant UPA of the extended questionnaire.

grupos <- caf21_pre %>%
  transmute(guid = guid_ce,
            grupo = case_when(cues == "Extendido" & hog_afc_est == 1 ~ "UPA campesinas (extendido)",
                              cues == "Abreviado" & hog_afc_est == 1 ~ "UAC campesinas (abreviado)",
                              cues == "Extendido" & hog_afc_est == 0 ~ "UPA no campesinas (extendido)",
                              cues == "Abreviado" & hog_afc_est == 0 ~ "UAC sin producción (abreviado)"))

predios <- read_caf21("seccion_8") %>%
  group_by(guid) %>%
  summarise(n_pre = n_distinct(establecimiento, predio),
            n_com = n_distinct(cut_comuna)) %>%
  inner_join(grupos, by = "guid")

tab_predios <- predios %>%
  filter(grupo != "UAC sin producción (abreviado)") %>%
  group_by(grupo) %>%
  summarise(n = n(),
            pct_un_predio = pct(n_pre == 1),
            pct_dos_o_mas = pct(n_pre >= 2),
            pct_mas_de_una_comuna = pct(n_com >= 2)) %>%
  arrange(match(grupo, c("UPA campesinas (extendido)", "UAC campesinas (abreviado)", "UPA no campesinas (extendido)")))

print(as.data.frame(tab_predios))

methods$predios <- tab_predios
rm(predios)


# 3. CAF 2021 PARTICULARITY 1: SEPARATE HOUSEHOLD AND PRODUCTIVE DATASETS -----
# The INE published the productive dataset (UPA, extended questionnaire) and the household dataset without a common
# identifier (INE, 2022b). Only UPA of natural persons have a household record. Both datasets are matched in
# data_process/04_02_caf21_combinacion.R on 40 variables recorded in the same format, keeping one-to-one matches.
# The household dataset records neither a hired administrator nor the number of temporary days hired, so households
# hiring only temporary workers cannot be classified from it (hog_afc_est_mis is missing). Final classification of
# caf21_hog (hog_afc_est, source in hog_afc_fuente): productive questionnaire for matched households, household
# questionnaire for the rest, observed or imputed (multiple imputation, 04_01_caf21_imputaciones.R).

### 3.1 Matching
# Same variables as in data_process/04_02_caf21_combinacion.R (vars_match)
vars_match <- c("com", "g3", "hp280_2", paste0("us61_", 1:19),
                "ga161", "ga172", "ga182", "ga180", "ga185", "ga186", "ga199", paste0("ga", 201:209),
                "tr244", "tr250")

pares <- caf21_hog %>%
  select(id_hog, all_of(vars_match)) %>%
  inner_join(caf21_ce %>% filter(g1 == 1) %>% select(guid_ce, all_of(vars_match)),
             by = vars_match, relationship = "many-to-many") %>%
  add_count(id_hog, name = "n_hog") %>%
  add_count(guid_ce, name = "n_ce")

n_natural <- sum(caf21_ce$g1 == 1) # UPA of natural persons (= households in caf21_hog)
n_unicos <- sum(pares$n_hog == 1 & pares$n_ce == 1)
n_con_pareja <- n_distinct(pares$id_hog)

tab_emparejamiento <- tibble(
  categoria = c("UPA de persona natural (hogares)", "Sin correspondencia", "Correspondencia ambigua (descartadas)",
                "Emparejadas uno a uno"),
  n = c(n_natural, nrow(caf21_hog) - n_con_pareja, n_con_pareja - n_unicos, n_unicos)) %>%
  mutate(pct = round(100 * n / n_natural, 1))
# Expected: 129,607 / 7,201 (5.6%) / 952 (0.7%) / 121,454 (93.7%)

# Matched share by peasant classification of the productive questionnaire
# Expected: peasant 109,359 of 114,996 (95.1%); non-peasant 12,095 of 14,611 (82.8%)
tab_emparejadas_afc <- caf21_ce %>%
  filter(g1 == 1) %>%
  mutate(emparejada = guid_ce %in% caf21$guid_ce[caf21$cues == "Extendido"]) %>%
  group_by(hog_afc_est) %>%
  summarise(n = n(), emparejadas = sum(emparejada), pct = pct(emparejada))

if (n_unicos != sum(caf21$cues == "Extendido")) warning("One-to-one matches differ from caf21: check vars_match")

print(as.data.frame(tab_emparejamiento)); print(as.data.frame(tab_emparejadas_afc))
rm(pares)

### 3.2 Households that cannot be classified with the household dataset
# Expected: 17,438 households hiring only temporary workers (13.5% of 129,607)
n_sin_clas <- sum(is.na(caf21_hog$hog_afc_est_mis))
tab_sin_clas <- tibble(n_hogares = nrow(caf21_hog), n_solo_temporal = n_sin_clas,
                       pct = round(100 * n_sin_clas / nrow(caf21_hog), 1))
print(tab_sin_clas)

### 3.3 Source of the final classification of caf21_hog
# Expected: productive questionnaire 121,454; household questionnaire observed 6,623; imputed 1,530
tab_fuente <- caf21_hog %>%
  group_by(hog_afc_fuente) %>%
  summarise(n = n(), campesinos = sum(hog_afc_est == 1))
print(as.data.frame(tab_fuente))

### 3.4 Validation: household-questionnaire classification vs. productive classification in matched pairs
# Justifies using the productive classification wherever a match exists (Table S8 of the supplementary material).
# Expected: agreement 95.1% overall; 98.7% among observed (105,546 pairs) and 71.5% among imputed (15,908);
# among imputed, sensitivity 78.1% and specificity 36.3%.
emp <- caf21 %>%
  filter(cues == "Extendido") %>%
  mutate(clasificacion_hogar = if_else(is.na(hog_afc_est_mis), "Imputada", "Observada"))

validacion <- function(d) {
  summarise(d,
            pares = n(),
            coincidencia = pct(hog_afc_est_hp == hog_afc_est),
            sensibilidad = pct(hog_afc_est_hp[hog_afc_est == 1] == 1), # Peasant (productive) classified as peasant
            especificidad = pct(hog_afc_est_hp[hog_afc_est == 0] == 0), # Non-peasant classified as non-peasant
            falsos_positivos = sum(hog_afc_est_hp == 1 & hog_afc_est == 0),
            falsos_negativos = sum(hog_afc_est_hp == 0 & hog_afc_est == 1))
}

tab_validacion <- bind_rows(emp %>% group_by(clasificacion_hogar) %>% validacion(),
                            emp %>% validacion() %>% mutate(clasificacion_hogar = "Total"))
print(as.data.frame(tab_validacion))
rm(emp)

### 3.5 Number of peasant households according to each dataset
# Expected: productive dataset 114,996 (reference); household dataset 115,110 with the final classification
# (115,132 with the household-questionnaire classification, observed or imputed, for all households)
tab_bases <- tibble(
  base = c("Base de actividad (cuestionario productivo)", "Base de hogar, clasificación final",
           "Base de hogar, clasificación del cuestionario de hogar (observada o imputada)"),
  hogares_campesinos = c(sum(caf21_ce$hog_afc_est == 1), sum(caf21_hog$hog_afc_est == 1),
                         sum(caf21_hog$hog_afc_est_hp == 1)))
print(as.data.frame(tab_bases))

methods$combinacion <- list(emparejamiento = tab_emparejamiento, emparejadas_afc = tab_emparejadas_afc,
                            sin_clasificacion = tab_sin_clas, fuente = tab_fuente, validacion = tab_validacion,
                            bases = tab_bases)


# 4. CAF 2021 PARTICULARITY 2: NON-RESPONSE (NO LOGRO) --------------------------
# The INE estimated non-response as a share of the managed surface, not of the number of units: 8.66% nationally
# (INE, 2022c, 2022d). The sensitivity of the results is assessed by projecting the number and characteristics of
# peasant households under the assumption that the share of surface not achieved equals the share of units not
# censused (a pessimistic assumption, since most of the surface not achieved was natural formations and native forest;
# the smaller size of peasant units works in the opposite direction, see section S6.4 of the supplementary material).
# Each 2021 unit is weighted by 1 / (1 - share not achieved in its municipality).

### 4.1 Municipal non-response
# input/caf21/no_logro_comunal.xlsx, built from the municipal tables of the INE technical notes: % of the managed
# surface achieved (logro) and not achieved (no_logro), 0-100, by CAF 2021 harmonized municipality (cod_com_0721,
# merged municipalities included): 313 of the 315 cod_com_0721, all but 12202 (Antartica) and 99999. Nine urban
# municipalities of Santiago have 0 in both columns (no agricultural surface). Units in municipalities without
# information are not projected (weight 1).
# The file is not in the public repository (input/caf21 is not tracked).
no_logro_com <- read_excel("input/caf21/no_logro_comunal.xlsx") %>%
  transmute(cod_com_0721, pct_no_logro = no_logro, peso_no_logro = 1 / (1 - no_logro / 100))

# Municipalities with high non-response. Expected: 103 harmonized municipalities above 20%, 53 above 30% and 11
# above 50%. Counted as official municipalities, 54 above 30% (8113 merges Chiguayante and Hualqui, 36.6%), as in
# section S6.4 of the supplementary material.
tab_no_logro_com <- no_logro_com %>%
  left_join(count(dpa_cod, cod_com_0721, name = "n_com_2018"), by = "cod_com_0721") %>%
  summarise(comunas_0721 = n(),
            sobre_20 = sum(pct_no_logro > 20),
            sobre_30 = sum(pct_no_logro > 30),
            sobre_50 = sum(pct_no_logro > 50),
            comunas_2018_sobre_30 = sum(n_com_2018[pct_no_logro > 30]),
            comunas_2018_sobre_50 = sum(n_com_2018[pct_no_logro > 50]))
print(as.data.frame(tab_no_logro_com))

# Adds the municipal weight to a dataset (municipality in variable com, harmonized codes)
add_peso <- function(df, com) {
  df %>%
    left_join(select(no_logro_com, cod_com_0721, peso_no_logro), by = setNames("cod_com_0721", com)) %>%
    mutate(peso_no_logro = coalesce(peso_no_logro, 1))
}

### 4.2 Projection of the number of peasant households (magnitude)
# Full 2021 universe (caf21_pre, extended questionnaire and UAC) compared with the full 2007 universe.
proy_pre <- caf21_pre %>%
  filter(hog_afc_est == 1) %>%
  add_peso("cod_com_0721")

tab_no_logro <- tibble(
  escenario = c("Observado", "Proyectado con el no logro comunal"),
  hogares_2021 = c(n21_tot, round(sum(proy_pre$peso_no_logro)))) %>%
  mutate(hogares_2007 = n07_tot,
         var_abs = hogares_2021 - hogares_2007,
         var_pct = var_pct(hogares_2021, hogares_2007))
print(as.data.frame(tab_no_logro))

### 4.3 Projection of the characterization
# Comparable segment (master.R): 2007 households with cues_21 == "Extendido"; 2021 extended questionnaire (caf21_ce
# for productive variables, caf21_hog for household variables), weighted by municipal non-response. The projection
# only changes the composition across municipalities: units not censused are assumed to resemble those censused in
# their municipality. A few key variables are shown here; results scripts can project any variable with the weights
# saved in output/01_methods.rds (pesos).
c07 <- caf07 %>% filter(hog_afc_est == 1, cues_21 == "Extendido")
ce_p <- caf21_ce %>% filter(hog_afc_est == 1) %>% add_peso("com")
hog_p <- caf21_hog %>% filter(hog_afc_est == 1) %>% add_peso("com")

wmean <- function(x, w) weighted.mean(x, w, na.rm = TRUE)

tab_carac_proy <- tibble(
  variable = c("Hogares caracterizados, variables productivas", "Hogares caracterizados, variables del hogar",
               "Superficie total, media (ha)", "Integrantes del hogar, media", "Edad del productor, media",
               "Productora mujer (%)"),
  caf_2007 = c(nrow(c07), nrow(c07), mean(c07$sup_tot, na.rm = TRUE), mean(c07$n_mie, na.rm = TRUE),
               mean(c07$edad_jh, na.rm = TRUE), 100 * mean(c07$sex_jh == "Femenino", na.rm = TRUE)),
  caf_2021_observado = c(nrow(ce_p), nrow(hog_p), mean(ce_p$sup_tot, na.rm = TRUE), mean(hog_p$n_mie, na.rm = TRUE),
                         mean(hog_p$edad_jh, na.rm = TRUE), 100 * mean(hog_p$sex_jh == "Femenino", na.rm = TRUE)),
  caf_2021_proyectado = c(sum(ce_p$peso_no_logro), sum(hog_p$peso_no_logro),
                          wmean(ce_p$sup_tot, ce_p$peso_no_logro), wmean(hog_p$n_mie, hog_p$peso_no_logro),
                          wmean(hog_p$edad_jh, hog_p$peso_no_logro),
                          100 * wmean(hog_p$sex_jh == "Femenino", hog_p$peso_no_logro))) %>%
  mutate(cambio_observado = caf_2021_observado - caf_2007,
         cambio_proyectado = caf_2021_proyectado - caf_2007,
         across(where(is.double), ~ round(.x, 2)))
print(as.data.frame(tab_carac_proy))
rm(c07, ce_p, hog_p)

methods$no_logro <- list(comunas = tab_no_logro_com, proyeccion = tab_no_logro, caracterizacion = tab_carac_proy,
                         pesos = no_logro_com)
rm(proy_pre)


# SAVING ------------------------------------------------------------------

saveRDS(methods, "output/01_methods.rds")

