### CASEN PROCESSING ###
# Builds person- and household-level variables from CASEN 2000, 2006, 2015 and 2022. 
# Identifies peasant (AFC) workers and households, and pools all years into two datasets:
#   - casen_mie: persons (one row per household member)
#   - casen: households (one row per household)
# Both keep only the variables created here, plus the survey year (año) 
# And the sample design variables (varunit = PSU, varstrat = stratum, expr = expansion factor).


if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, stringr, purrr, haven, archive)

source("data_process/01_dpa.R")
source("helpers/hogar.R") # Household structure helpers

### CASEN source files
# Downloaded from the Observatorio Social (https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen)
# and cached in casen_dir. We use the SPSS versions consistent with the historical series: the newer
# re-processed releases of 2006, 2015 and 2022 change variable names, codes and the poverty methodology.
casen_dir <- "input/casen"
casen_url <- "https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen"
casen_files <- c(casen2000 = "2000/Casen2000.rar",
                 casen2000_ingresos = "2000/ingreso_ajustados_casen2000_spss.zip", # Adjusted incomes, disaggregated
                 casen2006 = "2006/casen2006.zip",
                 casen2006_raking = "2006/Casen2006_factor_raking_deciles_y_quintil SPSS.sav.zip", # Expansion factor, Census 2017 calibration
                 casen2015 = "2015/casen_2015_spss.rar",
                 casen2015_raking = "2015/Casen2015_factor_raking_deciles_y_quintil SPSS.sav.zip",
                 casen2022 = "2022/Base de datos Casen 2022 SPSS_18 marzo 2024.sav.zip")


# HELPERS ----------------------------------------------------------------------

# Reads a CASEN dataset, downloading and extracting it into casen_dir the first time
read_casen <- function(name) {
  sav <- file.path(casen_dir, paste0(name, ".sav"))
  if (!file.exists(sav)) {
    dir.create(casen_dir, showWarnings = FALSE, recursive = TRUE)
    zip <- file.path(tempdir(), basename(casen_files[[name]]))
    options(timeout = max(1200, getOption("timeout")))
    download.file(paste0(casen_url, "/", URLencode(casen_files[[name]])), zip, mode = "wb")
    content <- archive(zip)$path
    i <- which(grepl("[.]sav$", content, ignore.case = TRUE) & !grepl("__MACOSX", content))
    stopifnot(length(i) == 1)
    archive_extract(zip, dir = casen_dir, files = i)
    file.rename(file.path(casen_dir, content[i]), sav)
    unlink(zip)
  }
  read_sav(sav) %>%
    zap_formats() %>%
    zap_labels() %>%
    zap_label() %>%
    rename_with(tolower)
}

### Category levels shared across years
lev_sfdt <- c("Menor de 15 años", "Ocupado", "Desocupado", "Inactivo")
lev_nedu <- c("Hasta básica completa", "Media incompleta", "Media completa", "Superior")
lev_cise <- c("Empleador o patrón", "Trabajador por cuenta propia", "Asalariado", "Familiar no remunerado")
lev_ciiu <- c("Agricultura, ganadería, silvicultura y pesca", "Minería, industria y EGA",
              "Construcción", "Comercio", "Transporte, almacenamiento, alojamiento y servicio de comidas",
              "Servicios", "Trabajadora de casa particular")
lev_ciuo <- c("Directores y gerentes", "Fuerzas armadas", "Profesionales, científicos e intelectuales", "Técnicos y profesionales de nivel medio",
              "Empleados de oficina", "Trabajadores de servicios y vendedores de comercio", "Agricultores y trabajdores calificados agropecuarios y pesqueros",
              "Oficiales, operarios y artesanos", "Operadores de instalaciones y máquinas", "Trabajadores no calificados")
lev_pue <- c("No pertenece", "Pertenece")
lev_nac <- c("Chileno", "Extranjero")

### Recoding functions (inputs are the year-specific source variables)
recode_jh <- function(pco1) {
  factor(case_when(pco1 == 1 ~ "Productor o jefe de hogar",
                   pco1 != 1 ~ "Otro miembro del hogar"),
         levels = c("Productor o jefe de hogar", "Otro miembro del hogar"))
}

recode_rur <- function(zona) {
  factor(case_when(zona == 1 ~ "Urbano",
                   zona == 2 ~ "Rural"),
         levels = c("Urbano", "Rural"))
}

recode_sfdt <- function(edad, activ) {
  factor(case_when(edad < 15 ~ "Menor de 15 años",
                   activ == 1 ~ "Ocupado",
                   activ == 2 ~ "Desocupado",
                   activ == 3 ~ "Inactivo"),
         levels = lev_sfdt)
}

# educ codes for each educational level differ across years
recode_nedu <- function(edad, educ, basica, media_inc, media_com, superior) {
  factor(case_when(edad >= 15 & educ %in% basica ~ lev_nedu[1],
                   edad >= 15 & educ %in% media_inc ~ lev_nedu[2],
                   edad >= 15 & educ %in% media_com ~ lev_nedu[3],
                   edad >= 15 & educ %in% superior ~ lev_nedu[4]),
         levels = lev_nedu)
}

# 1-digit ISCO codes (0 = armed forces)
recode_ciuo <- function(oficio1) {
  lab <- lev_ciuo[c(2, 1, 3:10)] # labels ordered by code 0-9
  factor(lab[match(oficio1, 0:9)], levels = lev_ciuo)
}

recode_cont <- function(sfdt, cise, firmado) {
  asal <- sfdt == "Ocupado" & cise == "Asalariado"
  factor(case_when(asal & firmado ~ "Tiene contrato firmado",
                   asal ~ "No tiene contrato firmado"),
         levels = c("Tiene contrato firmado", "No tiene contrato firmado"))
}

recode_temp <- function(x) {
  factor(case_when(x == 1 ~ "Permanente",
                   x > 1 & x <= 5 ~ "Temporal, a plazo u otra"),
         levels = c("Permanente", "Temporal, a plazo u otra"))
}

recode_conm <- function(conm_min) {
  factor(case_when(conm_min > 30 ~ "Commuter",
                   conm_min <= 30 ~ "No commuter"),
         levels = c("No commuter", "Commuter"))
}

### Identification of peasant (AFC) workers
# Only independent workers (own-account or unpaid family workers) can be AFC.
# ciiu_afc: agricultural/forestry activity (rama_core), or agro-processing/trade activities (rama_ext) when the occupation is AFC.
# ciuo_afc: agricultural occupations (ocup_core), or ambiguous occupations (ocup_cond) when the activity is agricultural.
classify_afc <- function(cise, rama, ocup, rama_core, ocup_core, ocup_cond, rama_ext) {
  ind <- cise %in% c("Trabajador por cuenta propia", "Familiar no remunerado")
  ciiu0 <- ind & rama %in% rama_core
  ciuo <- ind & (ocup %in% ocup_core | (ocup %in% ocup_cond & ciiu0))
  ciiu <- ciiu0 | (ind & rama %in% rama_ext & ciuo)
  tibble(cise_ind = as.numeric(ind),
         ciiu_afc = as.numeric(ciiu),
         ciuo_afc = as.numeric(ciuo))
}


# YEAR-SPECIFIC HARMONIZATION --------------------------------------------------
# Each function loads one CASEN round and creates the variables whose source questions or codes
# differ across years. Monetary values are adjusted to December 2023. pobre and decil keep the
# original codes, and jparc (part-time by choice, 1 = yes, 2 = no) is an intermediate input.

prep_casen00 <- function() {
  ing <- read_casen("casen2000_ingresos") %>%
    transmute(id_per = paste(segmento, folio, o, sep = "-"),
              ing_ven_agr = yvpaaj * 2.246,
              ing_con_agr = yac2aj * 2.246)

  read_casen("casen2000") %>%
    mutate(id_hog = paste(segmento, folio, sep = "-"),
           id_per = paste(segmento, folio, o, sep = "-"),
           jh = recode_jh(pco1),
           rur = recode_rur(z),
           nedu = recode_nedu(edad, educ, 0:3, 4:5, 6:7, 8:9),
           pue = factor(case_when(etnia > 0 ~ "Pertenece",
                                  etnia == 0 ~ "No pertenece"), levels = lev_pue),
           sfdt = recode_sfdt(edad, activ),
           cise = factor(case_when(sfdt == "Ocupado" & o10 == 1 ~ "Empleador o patrón",
                                   sfdt == "Ocupado" & o10 == 2 ~ "Trabajador por cuenta propia",
                                   sfdt == "Ocupado" & o10 %in% c(3:7, 9) ~ "Asalariado",
                                   sfdt == "Ocupado" & o10 == 8 ~ "Familiar no remunerado"), levels = lev_cise),
           ciiu = factor(case_when(sfdt == "Ocupado" & rama == 1 ~ lev_ciiu[1],
                                   sfdt == "Ocupado" & rama %in% 2:4 ~ lev_ciiu[2],
                                   sfdt == "Ocupado" & rama == 5 ~ lev_ciiu[3],
                                   sfdt == "Ocupado" & rama == 6 ~ lev_ciiu[4],
                                   sfdt == "Ocupado" & rama == 7 ~ lev_ciiu[5],
                                   sfdt == "Ocupado" & rama %in% 8:9 ~ lev_ciiu[6],
                                   sfdt == "Ocupado" & o10 %in% 6:7 ~ lev_ciiu[7]), levels = lev_ciiu),
           ciuo = recode_ciuo(if_else(sfdt == "Ocupado", oficio - 1, NA)), # oficio is coded 1-10 (11 = no answer)
           cont = recode_cont(sfdt, cise, o11 <= 3),
           ytrab = yopraj * 2.246,
           htrab = na_if(o14, 999),
           temp = recode_temp(o15),
           jparc = o29,
           classify_afc(cise, rama = o9, ocup = o8,
                        rama_core = c(1110, 1120, 1130),
                        ocup_core = c(6000:6112, 6114:6129, 9200:9211),
                        ocup_cond = c(6113, 6210, 8331),
                        rama_ext = c(3111:3113, 3131:3134, 6100, 6200)),
           cod_com_res = comu,
           tot_hog = as.numeric(o %in% 1),
           pobre = corte,
           decil = daur,
           varunit = segmento,
           varstrat = estrato) %>%
    left_join(ing, by = "id_per")
}

prep_casen06 <- function() {
  read_casen("casen2006") %>%
    select(-expr) %>%
    left_join(read_casen("casen2006_raking") %>%
                select(seg, f, o, expr = expr_c2017_nm),
              by = c("seg", "f", "o")) %>%
    mutate(id_hog = paste(seg, f, sep = "-"),
           id_per = paste(seg, f, o, sep = "-"),
           jh = recode_jh(pco1),
           rur = recode_rur(z),
           nedu = recode_nedu(edad, educ, 0:2, 3:4, 5:6, 7:9),
           nac = factor(case_when(t7 < 3 ~ "Chileno",
                                  t7 == 3 ~ "Extranjero"), levels = lev_nac),
           pue = factor(case_when(t4 == 90 ~ "No pertenece",
                                  t4 < 10 ~ "Pertenece"), levels = lev_pue),
           sfdt = recode_sfdt(edad, activ),
           cise = factor(case_when(sfdt == "Ocupado" & o19 == 1 ~ "Empleador o patrón",
                                   sfdt == "Ocupado" & o19 == 2 ~ "Trabajador por cuenta propia",
                                   sfdt == "Ocupado" & o19 %in% c(3:7, 9) ~ "Asalariado",
                                   sfdt == "Ocupado" & o19 == 8 ~ "Familiar no remunerado"), levels = lev_cise),
           ciiu = factor(case_when(sfdt == "Ocupado" & rama == 1 ~ lev_ciiu[1],
                                   sfdt == "Ocupado" & rama %in% 2:4 ~ lev_ciiu[2],
                                   sfdt == "Ocupado" & rama == 5 ~ lev_ciiu[3],
                                   sfdt == "Ocupado" & rama == 6 ~ lev_ciiu[4],
                                   sfdt == "Ocupado" & rama == 7 ~ lev_ciiu[5],
                                   sfdt == "Ocupado" & rama %in% 8:9 ~ lev_ciiu[6],
                                   sfdt == "Ocupado" & o19 %in% 6:7 ~ lev_ciiu[7]), levels = lev_ciiu),
           ciuo = recode_ciuo(if_else(sfdt == "Ocupado", oficio, NA)),
           cont = recode_cont(sfdt, cise, o20 == 1),
           ytrab = yopraj * 2.008,
           htrab = na_if(o15, 999),
           temp = recode_temp(o16),
           jparc = o26,
           classify_afc(cise, rama = c_o12, ocup = c_o11,
                        rama_core = c(1110, 1120, 1130),
                        ocup_core = c(6000:6112, 6114:6130, 9200:9211),
                        ocup_cond = c(6113, 6210, 8331),
                        rama_ext = c(3111:3113, 3131:3134, 6100, 6200)),
           cod_com_res = comuna,
           tot_hog = as.numeric(pco1 %in% 1),
           pobre = corte,
           decil = daur_15,
           varunit = seg,
           varstrat = estrato)
}

prep_casen15 <- function() {
  read_casen("casen2015") %>%
    select(-expr) %>%
    left_join(read_casen("casen2015_raking") %>%
                select(folio, o, expr = expr_c2017_nm),
              by = c("folio", "o")) %>%
    mutate(id_hog = as.character(folio),
           id_per = paste(folio, o, sep = "-"),
           jh = recode_jh(pco1),
           rur = recode_rur(zona),
           nedu = recode_nedu(edad, educ, 1:2, 3:4, 5:6, 7:12),
           nac = factor(case_when(r1b %in% 1:2 ~ "Chileno",
                                  r1b == 3 ~ "Extranjero"), levels = lev_nac),
           pue = factor(case_when(r3 == 10 ~ "No pertenece",
                                  r3 < 10 ~ "Pertenece"), levels = lev_pue),
           sfdt = recode_sfdt(edad, activ),
           cise = factor(case_when(o15 %in% 3:8 ~ "Asalariado", # Includes domestic service and armed forces
                                   o15 == 2 ~ "Trabajador por cuenta propia",
                                   o15 == 1 ~ "Empleador o patrón",
                                   o15 == 9 ~ "Familiar no remunerado"), levels = lev_cise),
           ciiu = factor(case_when(rama1 %in% 1:2 ~ lev_ciiu[1],
                                   rama1 %in% 3:5 ~ lev_ciiu[2],
                                   rama1 == 6 ~ lev_ciiu[3],
                                   rama1 == 7 ~ lev_ciiu[4],
                                   rama1 %in% 8:9 ~ lev_ciiu[5],
                                   rama1 %in% c(10:15, 17) ~ lev_ciiu[6],
                                   rama1 == 16 ~ lev_ciiu[7]), levels = lev_ciiu),
           ciuo = recode_ciuo(oficio1),
           cont = recode_cont(sfdt, cise, o17 == 1),
           ytrab = yoprcor * 1.381,
           htrab = na_if(o10, 999),
           temp = recode_temp(o12),
           conm_min = if_else(sfdt == "Ocupado" & o25a_hr >= 0 & o25a_min >= 0 & o25a_min < 60, o25a_hr * 60 + o25a_min, NA),
           conm = recode_conm(conm_min),
           jparc = o26,
           classify_afc(cise, rama = as.integer(rama4), ocup = as.integer(oficio4),
                        rama_core = 110:150, # Agriculture, livestock and forestry (excludes fishing)
                        ocup_core = c(6100:6112, 6114:6130, 9210:9211),
                        ocup_cond = c(6113, 6210, 8331),
                        rama_ext = c(1511, 1513, 1520, 1551, 1552, 1554, 5121, 5220, 5252)),
           ing_con_agr = y17 * 1.381,
           ing_ven_agr = y9 * 1.381,
           cod_com_res = as.integer(comuna),
           tot_hog = as.numeric(pco1 %in% 1),
           pobre = pobreza,
           decil = dautr)
}

prep_casen22 <- function() {
  read_casen("casen2022") %>%
    mutate(id_hog = as.character(folio),
           id_per = paste(folio, id_persona, sep = "-"),
           jh = recode_jh(pco1_a),
           rur = recode_rur(area),
           nedu = recode_nedu(edad, educ, 1:2, 3:4, 5:6, 7:12),
           nac = factor(case_when(lugar_nac == 0 ~ "Chileno",
                                  lugar_nac == 1 ~ "Extranjero"), levels = lev_nac),
           pue = factor(case_when(r3 == 11 ~ "No pertenece",
                                  r3 < 11 ~ "Pertenece"), levels = lev_pue),
           sfdt = recode_sfdt(edad, activ),
           est_tra = factor(case_when(edad < 15 ~ "Menor de 15 años",
                                      activ %in% 1:2 & e3 == 1 ~ "Estudia y trabaja", # Or looks for work
                                      activ %in% 1:2 & e3 == 2 ~ "Sólo trabaja",
                                      activ == 3 & e3 == 1 ~ "Sólo estudia",
                                      activ == 3 & e3 == 2 ~ "No estudia ni trabaja"),
                            levels = c("Menor de 15 años", "Estudia y trabaja", "Sólo trabaja", "Sólo estudia", "No estudia ni trabaja")),
           cise = factor(case_when(sfdt == "Ocupado" & o15 %in% 3:8 ~ "Asalariado", # Includes domestic service and armed forces
                                   sfdt == "Ocupado" & o15 == 2 ~ "Trabajador por cuenta propia",
                                   sfdt == "Ocupado" & o15 == 1 ~ "Empleador o patrón",
                                   sfdt == "Ocupado" & o15 == 9 ~ "Familiar no remunerado"), levels = lev_cise),
           ciiu = factor(case_when(rama1 < 0 ~ NA,
                                   rama1 == 1 ~ lev_ciiu[1],
                                   rama1 %in% 2:5 ~ lev_ciiu[2],
                                   rama1 == 6 ~ lev_ciiu[3],
                                   rama1 == 7 ~ lev_ciiu[4],
                                   rama1 %in% 8:9 ~ lev_ciiu[5],
                                   rama1 %in% 10:20 ~ lev_ciiu[6],
                                   rama4 >= 9701 ~ lev_ciiu[7]), levels = lev_ciiu),
           ciuo = recode_ciuo(oficio1_08),
           cont = recode_cont(sfdt, cise, o19 == 1),
           sii = factor(case_when(sfdt == "Ocupado" & cise %in% c("Trabajador por cuenta propia", "Empleador o patrón") & o16 == 1 ~ "Registrada en el SII",
                                  sfdt == "Ocupado" & cise %in% c("Trabajador por cuenta propia", "Empleador o patrón") & o16 %in% c(2, 88) ~ "No registrada en el SII"),
                        levels = c("No registrada en el SII", "Registrada en el SII")),
           ytrab = (yoprcor + y0901) * 1.039,
           htrab = if_else(o10 > 0, o10, NA),
           temp = recode_temp(o12),
           conm_min = if_else(sfdt == "Ocupado" & o28a_hr >= 0 & o28a_min >= 0, o28a_hr * 60 + o28a_min, NA),
           conm = recode_conm(conm_min),
           jparc = o29,
           classify_afc(cise, rama = rama4, ocup = oficio4_08,
                        rama_core = 100:200, # Agriculture, livestock and forestry (excludes fishing)
                        ocup_core = c(6110:6112, 6114:6130, 6310:6330, 9210:9213),
                        ocup_cond = c(6113, 8341),
                        rama_ext = c(1001:1003, 1101, 1102, 4801:4803, 4809)),
           # Self-reported activity description mentions agriculture, livestock or peasantry
           glosa_afc = as.numeric(cise_ind == 1 & coalesce(str_detect(o9a, regex("ganade|agric|agríc|campes", ignore_case = TRUE)), FALSE)),
           ing_con_agr = yac2 * 1.039,
           ing_ven_agr = coalesce(y0901 * 1.039, 0),
           cod_com_res = as.integer(str_sub(estrato, 1, str_length(estrato) - 2)),
           tot_hog = as.numeric(pco1_a %in% 1),
           pobre = pobreza,
           decil = dautr)
}

# Household-level variables only available in CASEN 2022
hog_extra_casen22 <- function(mie) {
  sii <- mie %>%
    filter(tra_hog_afc == "Trabajador campesino", !is.na(sii)) %>%
    group_by(id_hog) %>%
    summarise(sii = factor(if_else(any(sii == "Registrada en el SII"), "Registrada en el SII", "No registrada en el SII"),
                           levels = c("No registrada en el SII", "Registrada en el SII")))

  ing <- mie %>%
    filter(tot_hog == 1) %>%
    transmute(id_hog,
              ing_ven = y0901h, # Household sales income of independent workers (should be similar to ing_ven_agr)
              ing_sub = ysubh, # Household monetary subsidies
              ing_rem = yfa1h + yfa2h, # Remittances
              ing_pen = y2803ch, # Household old-age pensions
              ing_imp = ing_sub + ing_rem + ing_pen,
              ing_tot = ymonecorh,
              pct_ing_imp = ing_imp / ing_tot)

  full_join(sii, ing, by = "id_hog")
}


# COMMON PROCESSING ------------------------------------------------------------

### Person-level variables computed the same way in every year
add_common_vars <- function(mie) {
  mie <- mie %>%
    mutate(sex = factor(case_when(sexo == 1 ~ "Masculino",
                                  sexo == 2 ~ "Femenino"),
                        levels = c("Femenino", "Masculino")),
           edad_rec = recode_edad_rec(edad),
           edad_cat = recode_edad_cat(edad),
           sfdt_pet = case_when(edad < 15 ~ 0,
                                edad >= 15 ~ 1),
           sfdt_ocu = as.numeric(sfdt %in% "Ocupado"),
           ytrab_h = ytrab / htrab,
           afc_amp = as.numeric(ciuo_afc == 1 | ciiu_afc == 1),
           afc_est = as.numeric(ciuo_afc == 1 & ciiu_afc == 1),
           # Labor time (in full-time equivalents) devoted to work, to the own farm (pre) and off-farm (ext). Unemployed counted as inactive.
           jor_ocu = case_when(sfdt == "Ocupado" & htrab > 33 ~ 1,
                               sfdt == "Ocupado" & htrab <= 33 & jparc == 1 ~ 1,
                               sfdt == "Ocupado" & htrab <= 33 & htrab > 22 & jparc == 2 ~ 0.75,
                               sfdt == "Ocupado" & htrab <= 22 & htrab > 11 & jparc == 2 ~ 0.5,
                               sfdt == "Ocupado" & htrab <= 11 & jparc == 2 ~ 0.25,
                               sfdt %in% c("Inactivo", "Desocupado") ~ 0),
           jor_pre = case_when(afc_amp == 1 & htrab > 33 ~ 1,
                               afc_amp == 1 & htrab <= 33 & htrab > 22 ~ 0.75,
                               afc_amp == 1 & htrab <= 22 & htrab > 11 ~ 0.5,
                               afc_amp == 1 & htrab <= 11 ~ 0.25,
                               afc_amp == 0 | sfdt %in% c("Inactivo", "Desocupado") ~ 0),
           jor_ext = case_when(afc_amp == 0 ~ jor_ocu,
                               afc_amp == 1 ~ jor_ocu - jor_pre),
           jor_ina = case_when(sfdt %in% c("Inactivo", "Desocupado") ~ 1,
                               sfdt == "Ocupado" ~ 1 - jor_ocu),
           tot_per = 1) %>%
    left_join(select(dpa_cod, cod_com_2010, cod_com_res_2018 = cod_com_2018, nom_com_res = nom_com),
              by = c("cod_com_res" = "cod_com_2010"))

  # Agricultural income is not available in CASEN 2006
  if ("ing_con_agr" %in% names(mie)) {
    mie <- mie %>%
      mutate(con_agr = as.numeric(coalesce(ing_con_agr >= 25000 & edad >= 15, FALSE)), # Self-consumption
             ven_agr = as.numeric(coalesce(ing_ven_agr > 0, FALSE))) # Sales
  }
  mie
}

### Household-level variables aggregated from members
build_hog <- function(mie) {
  # Household head characteristics, poverty and income decile
  jefe <- mie %>%
    filter(jh == "Productor o jefe de hogar") %>%
    transmute(id_hog, rur,
              sex_jh = sex,
              edad_jh = edad,
              edad_rec_jh = edad_rec,
              nedu_jh = nedu,
              pue_jh = pue,
              pobre = case_when(pobre %in% 1:2 ~ 1,
                                pobre == 3 ~ 0),
              decil = factor(decil))

  # Mean years of schooling (members aged 25+)
  esc <- mie %>%
    filter(edad >= 25) %>%
    group_by(id_hog) %>%
    summarise(esc = mean(esc, na.rm = TRUE))

  # Age structure
  edad <- mie %>%
    group_by(id_hog) %>%
    summarise(n_menores = sum(edad_rec %in% "Menor"),
              n_adultos = sum(edad_rec %in% "Adulto"),
              n_ancianos = sum(edad_rec %in% "Anciano"))

  # Occupational structure and allocation of family labor (members aged 15+)
  trabajo <- mie %>%
    filter(sfdt %in% c("Ocupado", "Desocupado", "Inactivo")) %>%
    group_by(id_hog) %>%
    summarise(n_ocupados = sum(sfdt == "Ocupado"),
              n_desocupados = sum(sfdt == "Desocupado"),
              n_inactivos = sum(sfdt == "Inactivo"),
              across(c(jor_pre, jor_ext, jor_ina, jor_ocu), \(x) sum(x, na.rm = TRUE)))

  # Sample design variables
  diseno <- mie %>%
    filter(tot_hog == 1) %>%
    select(id_hog, varunit, varstrat, expr)

  mie %>%
    distinct(id_hog, hog_afc_amp, hog_afc_est) %>%
    left_join(jefe, by = "id_hog") %>%
    left_join(esc, by = "id_hog") %>%
    left_join(edad, by = "id_hog") %>%
    left_join(trabajo, by = "id_hog") %>%
    left_join(diseno, by = "id_hog") %>%
    mutate(n_mie = n_menores + n_adultos + n_ancianos) %>%
    add_estructura_hogar() %>%
    mutate(jor_fam_tot = jor_ocu + jor_ina,
           pct_jor_ocu = 100 * (jor_ocu / jor_fam_tot),
           pct_jor_pre = 100 * (jor_pre / jor_fam_tot),
           pct_jor_ext = 100 * (jor_ext / jor_fam_tot),
           pct_jor_ina = 100 * (jor_ina / jor_fam_tot))
}

### Person-level variables kept in the final dataset
mie_vars <- c("id_hog", "id_per", "jh", "sex", "edad", "edad_rec", "edad_cat", "esc", "nedu", "rur", "nac", "pue",
              "sfdt", "est_tra", "sfdt_pet", "sfdt_ocu", "cise", "ciiu", "ciuo", "cont", "sii", "ytrab", "htrab", "ytrab_h",
              "temp", "conm_min", "conm", "cise_ind", "ciiu_afc", "ciuo_afc", "afc_amp", "afc_est", "glosa_afc",
              "ing_con_agr", "ing_ven_agr", "con_agr", "ven_agr", "jor_ocu", "jor_pre", "jor_ext", "jor_ina",
              "hog_afc_amp", "hog_afc_est", "tra_hog_afc", "cod_com_res", "cod_com_res_2018", "nom_com_res",
              "tot_per", "tot_hog", "varunit", "varstrat", "expr")

### Full pipeline for one CASEN round
process_casen <- function(prep, hog_extra = NULL) {
  mie <- add_common_vars(prep()) %>%
    mutate(across(c(varunit, varstrat), as.character)) # Stored as text in some years

  # Peasant households: at least one AFC member
  mie <- mie %>%
    group_by(id_hog) %>%
    mutate(hog_afc_amp = as.numeric(any(afc_amp == 1)),
           hog_afc_est = as.numeric(any(afc_est == 1))) %>%
    ungroup() %>%
    mutate(tra_hog_afc = factor(case_when(sfdt == "Ocupado" & hog_afc_amp == 0 ~ "Hogar no campesino",
                                          sfdt == "Ocupado" & afc_amp == 0 ~ "Trabajador no campesino, hogar campesino",
                                          sfdt == "Ocupado" & afc_amp == 1 ~ "Trabajador campesino"),
                                levels = c("Hogar no campesino", "Trabajador no campesino, hogar campesino", "Trabajador campesino")))

  hog <- build_hog(mie)
  if (!is.null(hog_extra)) hog <- left_join(hog, hog_extra(mie), by = "id_hog")

  list(mie = select(mie, any_of(mie_vars)),
       hog = relocate(hog, varunit, varstrat, expr, .after = last_col()))
}


# POOLED DATASETS ----------------------------------------------------------------

casen_list <- list(`2000` = process_casen(prep_casen00),
                   `2006` = process_casen(prep_casen06),
                   `2015` = process_casen(prep_casen15),
                   `2022` = process_casen(prep_casen22, hog_extra = hog_extra_casen22))

casen_mie <- map(casen_list, "mie") %>%
  bind_rows(.id = "año") %>%
  mutate(año = as.integer(año))

casen <- map(casen_list, "hog") %>%
  bind_rows(.id = "año") %>%
  mutate(año = as.integer(año))

rm(casen_list)


# SAVING ------------------------------------------------------------------

saveRDS(casen_mie, "output/casen_mie.rds")
saveRDS(casen, "output/casen.rds")

# Survey designs: strata are year-specific, so each year is analyzed with its own design, e.g.
# survey::svydesign(ids = ~varunit, strata = ~varstrat, weights = ~expr, data = filter(casen, año == 2022))
