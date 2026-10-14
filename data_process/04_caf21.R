### CAF 2021 PROCESSING ###
# Builds the datasets of the 2021 Agricultural and Forestry Census (CAF 2021). CAF 2021 has two kinds of
# datasets that share no common identifier: the household questionnaire (GUID = HP...) and the productive
# questionnaires (GUID = CE... for the extended questionnaire, CA... for the abbreviated one).
#   - caf21_ce: productive units (extended questionnaires), with the identification of peasant households
#   - caf21_ca: productive units (abbreviated questionnaire), with an alternativ identification of peasant households.
#   - caf21_pre: all productive units (caf21_ce + caf21_ca). The base for estimating magnitudes, with fewer variables
#   - caf21_hog: households (household questionnaire). Peasant households are identified in 04_02_caf21_combinacion.R
#   - caf21_mie: household members
# Then 04_01_caf21_imputaciones.R imputes the peasant classification for households without complete information,
# and 04_02_caf21_combinacion.R combines the household and productive datasets (caf21).

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, readr, purrr)

source("data_process/01_dpa.R")
source("helpers/hogar.R") # Household structure helpers
source("helpers/caf.R") # CAF helpers

caf21_dir <- "C:/Users/fbaez/OneDrive - Universidad Católica de Chile/Bases de datos/Censo Agropecuario/2021/Resultados finales" # Original CAF 2021 .csv files (INE)

# Each file is read only once (several are used in more than one step)
caf21_files <- new.env()
read_caf21 <- function(file) {
  if (is.null(caf21_files[[file]])) {
    caf21_files[[file]] <- read_delim(file.path(caf21_dir, paste0(file, ".csv")),
                                      delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","),
                                      trim_ws = TRUE, show_col_types = FALSE) %>%
      rename_with(tolower)
  }
  caf21_files[[file]]
}

# Municipality codes. CAF 2021 uses harmonized municipalities (cod_com_0721), where small municipalities were merged.
add_com_0721 <- function(df) {
  df %>%
    left_join(select(dpa_cod, cod_com_2008, cod_com_2018, nom_com), by = c("com" = "cod_com_2018")) %>%
    mutate(cod_com_2018 = com,
           cod_com_0721 = com) %>%
    left_join(com_0721, by = "cod_com_0721") %>%
    mutate(cod_com_2018_dom = coalesce(cod_com_2018_dom, cod_com_2018)) # Dominant municipality, for spatial variables
}


# PRODUCTIVE UNITS: VARIABLES BY UNIT (SECTION 8) -----------------------------
# Section 8 has one row per plot. These variables are used for both questionnaires.

sec8 <- read_caf21("seccion_8")

### Land use (hectares)
us61 <- sec8 %>%
  mutate(across(starts_with("us61_"), as.double),
         us61_19 = as.double(us60)) %>%
  group_by(guid) %>%
  summarise(across(c(sprintf("us61_%02d", 1:18), "us61_19"), \(x) sum(x, na.rm = TRUE))) %>%
  rename_with(\(x) sub("us61_0", "us61_", x))

### Municipality: the one where most of the unit's land is located
comuna <- sec8 %>%
  mutate(us60 = as.double(us60)) %>%
  group_by(guid, cut_comuna) %>%
  summarise(us60 = sum(us60, na.rm = TRUE), .groups = "drop_last") %>%
  filter(rank(-us60, ties.method = "first") == 1) %>%
  ungroup() %>%
  select(guid, com = cut_comuna)

### Land tenure: regime with the largest area
tenencia <- sec8 %>%
  mutate(across(pr58_1:pr58_8, as.double)) %>%
  group_by(guid) %>%
  summarise(across(pr58_1:pr58_8, \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(cols = pr58_1:pr58_8, names_to = "reg", values_to = "sup") %>%
  group_by(guid) %>%
  filter(sup > 0, rank(-sup, ties.method = "first") == 1) %>%
  ungroup() %>%
  mutate(reg_ten = factor(lev_reg_ten[match(reg, paste0("pr58_", 1:7))], levels = lev_reg_ten)) %>%
  recode_tenencia() %>%
  select(guid, reg_ten, for_ten, pro_ten)


# PRODUCTIVE UNITS: EXTENDED QUESTIONNAIRE (CAF21_CE) --------------------------
# Built on section 1, which identifies each productive unit once (it also includes the abbreviated questionnaires,
# which are excluded in caf21_pre).

### Variables for the identification of peasant households and for the combination with the household dataset
sec1 <- read_caf21("seccion_1") %>%
  select(guid, g1, g2, g3, g4, sup_upa, ad29) # ad29: hired administrator

venta <- read_caf21("seccion_16_venta") %>%
  transmute(guid,
            hp280_2 = case_when(hp280_2 >= 1 & hp280_2 <= 14 ~ hp280_2, # Farm sales brackets
                                hp280_1 == 2 ~ 15,
                                hp280_1 == 3 ~ 16))

us62 <- sec8 %>%
  mutate(across(starts_with("us62"), ~ case_when(.x == -77 ~ NA,
                                                 is.na(.x) ~ 0,
                                                 .x == 1 ~ 1))) %>%
  group_by(guid) %>%
  summarise(across(starts_with("us62"), \(x) sum(x, na.rm = TRUE))) %>%
  mutate(across(starts_with("us62"), ~ ifelse(.x >= 1, 1, 0)))

# Livestock (heads), by species
sum_guid <- function(file, vars) {
  read_caf21(file) %>%
    group_by(guid) %>%
    summarise(across(all_of(vars), \(x) sum(x, na.rm = TRUE)))
}
ganado <- list(sum_guid("seccion_10_bovinos", "ga161"),
               sum_guid("seccion_10_ovinos", "ga172"),
               sum_guid("seccion_10_porcinos", "ga182"),
               sum_guid("seccion_10_caprinos", "ga180"),
               sum_guid("seccion_10_otro_ganado", c(paste0("ga", 201:209), "ga211")),
               sum_guid("seccion_10_aves", paste0("ga", 185:193)),
               sum_guid("seccion_10_apicultura", "ga199")) %>%
  reduce(left_join, by = "guid", .init = select(sec1, guid)) %>%
  mutate(across(starts_with("ga"), ~ replace_na(.x, 0)))

# Hired labor
trabajo <- read_caf21("seccion_14_trabajo") %>%
  mutate(tra_per = factor(case_when(tr244 == 1 ~ "Contrató trabajadores permanentes",
                                    tr244 %in% 2:3 ~ "No contrató trabajadores permanentes"),
                          levels = c("No contrató trabajadores permanentes", "Contrató trabajadores permanentes")),
         tra_tem = factor(case_when(tr250 == 1 ~ "Contrató trabajadores temporales",
                                    tr250 %in% 2:3 ~ "No contrató trabajadores temporales"),
                          levels = c("No contrató trabajadores temporales", "Contrató trabajadores temporales")),
         across(tr255_1:tr255_12, ~ pmax(replace_na(.x, 0), 0)), # Days of temporary work by month
         n_tra_tem = case_when(tra_tem == "No contrató trabajadores temporales" ~ 0,
                               tra_tem == "Contrató trabajadores temporales" ~ tr253_13),
         jor_tra_tem = case_when(tra_tem == "No contrató trabajadores temporales" ~ 0,
                                 tra_tem == "Contrató trabajadores temporales" ~ rowSums(pick(starts_with("tr255_")), na.rm = TRUE))) %>%
  select(guid, tr244, tr250, tra_per, tra_tem, n_tra_tem, jor_tra_tem) %>%
  distinct(guid, .keep_all = TRUE)

caf21_ce <- sec1 %>%
  left_join(venta, by = "guid") %>%
  left_join(us61, by = "guid") %>%
  left_join(us62, by = "guid") %>%
  left_join(ganado, by = "guid") %>%
  left_join(trabajo, by = "guid") %>%
  left_join(comuna, by = "guid") %>%
  rename(guid_ce = guid) %>%
  mutate(hog_afc_est = case_when(ad29 %in% 2:3 & # No hired administrator
                                   tra_per == "No contrató trabajadores permanentes" & # No permanent workers
                                   jor_tra_tem < 240 ~ 1, # Less than 240 days of temporary work per year (one full-time equivalent)
                                 TRUE ~ 0))

rm(venta, us62, ganado, trabajo)

### Land: availability and area
caf21_ce <- caf21_ce %>%
  mutate(sup_tot = us61_19,
         sup_apr = pmax(sup_tot - (us61_16 + us61_17 + us61_18), 0), # Usable land
         sup_cul = us61_1 + us61_2 + us61_3 + us61_4 + us61_5 + us61_6 + us61_7 + us61_8 + us61_9,
         sup_pra = us61_10 + us61_13 + us61_14,
         sup_bos = us61_11 + us61_12,
         sup_bar = us61_15,
         across(c(sup_cul, sup_pra, sup_bos, sup_bar), ~ .x * 100 / sup_apr, .names = "pct_{.col}"))

### Crops by species, production line (rub) and irrigation system (section 9)
# Each row has the dry-land (sec) and irrigated (rie) area of a species with a given irrigation system (sis_rie).
leer_seccion_9 <- function(file) {
  read_caf21(file) %>%
    select(-ends_with("_glo")) %>%
    mutate(across(where(is.numeric), ~ na_if(.x, -77)),
           across(where(is.character), ~ na_if(.x, "-77")))
}

# Questions with separate dry-land and irrigated areas
cultivo_sec_rie <- function(file, rub, esp, sis, sec, rie) {
  leer_seccion_9(file) %>%
    mutate(across(all_of(c(sis, sec, rie, if (is.character(esp)) esp)), as.double),
           sis_rie = case_when(is.na(.data[[sis]]) ~ 0,
                               .data[[sis]] > 0 ~ .data[[sis]]),
           esp = if (is.character(esp)) .data[[esp]] else esp,
           rub = rub) %>%
    group_by(guid, esp, sis_rie, rub) %>%
    summarise(sec = sum(.data[[sec]], na.rm = TRUE),
              rie = sum(.data[[rie]], na.rm = TRUE),
              .groups = "drop")
}

# Questions with a single area, where the irrigation system code cod_sec means dry land
cultivo_sup <- function(file, rub, esp, sis, sup, cod_sec) {
  leer_seccion_9(file) %>%
    mutate(across(all_of(c(sis, sup, if (is.character(esp)) esp)), as.double),
           sec = case_when(.data[[sis]] == cod_sec ~ .data[[sup]],
                           .data[[sis]] != cod_sec ~ 0),
           rie = case_when(.data[[sis]] != cod_sec ~ .data[[sup]],
                           .data[[sis]] == cod_sec ~ 0),
           sis_rie = case_when(is.na(.data[[sis]]) ~ 0,
                               .data[[sis]] > 0 & .data[[sis]] != cod_sec ~ .data[[sis]],
                               .data[[sis]] == cod_sec ~ 0),
           esp = if (is.character(esp)) .data[[esp]] else esp,
           rub = rub) %>%
    group_by(guid, esp, sis_rie, rub) %>%
    summarise(sec = sum(sec, na.rm = TRUE),
              rie = sum(rie, na.rm = TRUE),
              .groups = "drop")
}

# Forestry: no irrigation
cultivo_forestal <- function(file, rub, esp, sup) {
  leer_seccion_9(file) %>%
    mutate(across(all_of(c(esp, sup)), as.double),
           sis_rie = 12,
           esp = .data[[esp]],
           rub = rub) %>%
    group_by(guid, esp, sis_rie, rub) %>%
    summarise(sec = sum(.data[[sup]], na.rm = TRUE),
              rie = 0,
              .groups = "drop")
}

cultivos <- bind_rows(
  cultivo_sec_rie("seccion_9_cereales", "Cereales", "ss64", "ss67", "ss66", "ss65"),
  cultivo_sec_rie("seccion_9_leguminosas_tuberculos", "Leguminosas y tubérculos", "ss69", "ss72", "ss71", "ss70"),
  cultivo_sec_rie("seccion_9_industriales", "Industriales", "ss74", "ss77", "ss76", "ss75"),
  cultivo_sup("seccion_9_hortalizas", "Hortalizas, hongos y hierbas", "ss79", "ss83", "ss81", 12),
  cultivo_sup("seccion_9_hortalizas", "Hortalizas, hongos y hierbas", "ss79", "ss88", "ss86", 12),
  cultivo_sup("seccion_9_frutales", "Frutales", "ss92", "ss94", "ss93", 11),
  cultivo_sup("seccion_9_frutales", "Frutales", "ss92", "ss98", "ss97", 11),
  cultivo_sec_rie("seccion_9_vinas", "Viñas", "ss101", "ss104", "ss103", "ss102"),
  cultivo_sup("seccion_9_flores", "Flores", "ss106", "ss110", "ss108", 12),
  cultivo_sup("seccion_9_flores", "Flores", "ss106", "ss115", "ss113", 12),
  cultivo_sup("seccion_9_semilleros", "Semilleros", "ss119", "ss123", "ss121", 11),
  cultivo_sup("seccion_9_semilleros", "Semilleros", "ss119", "ss128", "ss126", 11),
  cultivo_sup("seccion_9_viveros", "Viveros", 1, "ss135", "ss133", 11),
  cultivo_sec_rie("seccion_9_forrajeras", "Forrajeras", "ss138", "ss141", "ss140", "ss139"),
  cultivo_forestal("seccion_9_forestal", "Forestal", "ss144", "ss145"),
  cultivo_forestal("seccion_9_bosque_nativo", "Bosque nativo", "ss146", "ss146_ha"),
  cultivo_sec_rie("seccion_9_praderas", "Praderas", 1, "ss149", "ss148", "ss147")) %>%
  mutate(across(c(sec, rie), ~ replace_na(.x, 0)),
         sis_rie = factor(case_when(sis_rie == 0 ~ "Sin información",
                                    sis_rie %in% 1:3 ~ "Tradicional",
                                    sis_rie %in% 4:5 ~ "Microrriego",
                                    sis_rie %in% 6:10 ~ "Aspersión",
                                    sis_rie == 11 & rub %in% c("Hortalizas, hongos y hierbas", "Flores") ~ "Microrriego",
                                    sis_rie == 11 & rub %in% c("Cereales", "Leguminosas y tubérculos", "Viñas", "Forrajeras", "Praderas") ~ "Sin información",
                                    sis_rie == 12 & rub %in% c("Frutales", "Semilleros") ~ "Sin información",
                                    sis_rie == 13 & rub == "Hortalizas, hongos y hierbas" ~ "Sin información"),
                          levels = c("Sin riego", "Tradicional", "Microrriego", "Aspersión", "Sin información"))) %>%
  pivot_longer(cols = c(sec, rie), names_to = "sec_rie", values_to = "sup") %>%
  mutate(sis_rie = case_when(sec_rie == "sec" ~ "Sin riego",
                             sec_rie == "rie" ~ sis_rie)) %>%
  filter(sup > 0)

### Irrigation: availability and technology
caf21_ce <- caf21_ce %>%
  left_join(cultivos %>%
              group_by(guid, sis_rie) %>%
              summarise(sup = sum(sup, na.rm = TRUE), .groups = "drop") %>%
              mutate(sis = case_when(sis_rie == "Sin información" ~ "si",
                                     sis_rie == "Sin riego" ~ "sec",
                                     sis_rie == "Tradicional" ~ "tra",
                                     sis_rie == "Microrriego" ~ "mic",
                                     sis_rie == "Aspersión" ~ "asp")) %>%
              select(-sis_rie) %>%
              pivot_wider(names_from = "sis", values_from = "sup", names_prefix = "sup_rie_") %>%
              rename(guid_ce = guid, sup_sec = sup_rie_sec) %>%
              mutate(across(starts_with("sup_"), ~ replace_na(.x, 0)),
                     sup_rie = sup_rie_tra + sup_rie_mic + sup_rie_asp + sup_rie_si,
                     sup_rie_tec = sup_rie_mic + sup_rie_asp,
                     rie = factor(case_when(sup_rie == 0 ~ "Sin riego",
                                            sup_rie > 0 ~ "Con riego"),
                                  levels = c("Sin riego", "Con riego"))),
            by = "guid_ce") %>%
  mutate(con_rie = rie == "Con riego",
         sup_rie = pmin(sup_rie, sup_apr), # Irrigated area cannot exceed usable land
         sup_rie_tra = pmin(sup_rie_tra, sup_apr),
         sup_rie_tec = pmin(sup_rie_tec, sup_apr),
         pct_rie = if_else(con_rie, sup_rie * 100 / sup_apr, NA),
         pct_rie_tec = if_else(con_rie, sup_rie_tec * 100 / sup_apr, NA),
         pct_rie_tra = if_else(con_rie, sup_rie_tra * 100 / sup_apr, NA)) %>%
  select(-con_rie)

### Irrigation: main water source and water rights (source with the most formal rights)
caf21_ce <- caf21_ce %>%
  left_join(read_caf21("seccion_12") %>%
              select(guid_ce = guid, starts_with("ar22"), -ends_with("_glo")) %>%
              mutate(across(starts_with("ar228"), ~ replace_na(.x, 0)),
                     n_fue_rie = rowSums(pick(starts_with("ar228_")), na.rm = TRUE),
                     llu = case_when(ar228_1 == 1 ~ 6), # Rain
                     pot = case_when(ar228_2 == 1 ~ 7), # Drinking water
                     rio = case_when(ar228_3 == 1 ~ ar229_3),
                     est = case_when(ar228_4 == 1 ~ ar229_4),
                     nor = case_when(ar228_5 == 1 ~ ar229_5),
                     pro = case_when(ar228_6 == 1 ~ ar229_6)) %>%
              select(guid_ce, n_fue_rie, llu:pro) %>%
              pivot_longer(cols = llu:pro, names_to = "fue", values_to = "pro") %>%
              filter(pro > 0) %>%
              group_by(guid_ce) %>%
              filter(rank(pro) < 2) %>%
              ungroup() %>%
              distinct(guid_ce, .keep_all = TRUE) %>%
              mutate(fue_rie = factor(c(pro = "Subterráneas", nor = "Subterráneas", rio = "Cauces", est = "Cauces",
                                        llu = "Lluvias", pot = "Agua potable")[fue],
                                      levels = c("Subterráneas", "Cauces", "Lluvias", "Agua potable")),
                     pro_rie = factor(c("Propia inscrita", "Arrendada", "De hecho", "Otra", NA, "Lluvias", "Agua potable")[pro],
                                      levels = c("Propia inscrita", "Arrendada", "De hecho", "Otra", "Lluvias", "Agua potable"))) %>%
              select(guid_ce, n_fue_rie, fue, fue_rie, pro_rie),
            by = "guid_ce")

### Land tenure
caf21_ce <- caf21_ce %>%
  left_join(rename(tenencia, guid_ce = guid), by = "guid_ce")

### Capital: machinery and equipment (animal traction, mechanized and stationary)
# Value of machinery (ac234_*): brackets recoded to their midpoint, in millions of pesos
val_maq <- c(0.75, 9, 4.5, 11.5, 43.5, 110, 225, 500, 1050, 1400)

caf21_ce <- caf21_ce %>%
  left_join(read_caf21("seccion_13_maquinaria") %>%
              distinct(guid, .keep_all = TRUE) %>%
              select(guid_ce = guid, starts_with("ac"), -ends_with("glo")) %>%
              mutate(usa_maq_ani = factor(case_when(ac230 == 1 ~ "Utiliza",
                                                    ac230 %in% 2:3 ~ "No utiliza"),
                                          levels = c("Utiliza", "No utiliza")),
                     usa_maq_mec = factor(case_when(ac231 == 1 ~ "Utiliza",
                                                    ac231 %in% 2:3 ~ "No utiliza"),
                                          levels = c("Utiliza", "No utiliza")),
                     pro_maq_mec = factor(case_when(usa_maq_mec == "Utiliza" & ac232_1 == 1 ~ "Propia", # Owns at least one machine
                                                    usa_maq_mec == "Utiliza" & ac232_1 == 2 ~ "Ajena"), # All machinery used belongs to others
                                          levels = c("Propia", "Ajena")),
                     across(starts_with("ac233_"), ~ case_when(usa_maq_mec == "No utiliza" | pro_maq_mec == "Ajena" ~ 0,
                                                               pro_maq_mec == "Propia" & .x <= 0 ~ 0,
                                                               pro_maq_mec == "Propia" & .x > 0 ~ .x)),
                     n_maq_mec = rowSums(pick(starts_with("ac233_")), na.rm = TRUE),
                     across(starts_with("ac234_"), ~ case_when(.x < 0 ~ 0, .default = val_maq[match(.x, 1:10)]), .names = "{.col}_cont"),
                     val_maq_mec = rowSums(pick(ends_with("_cont")), na.rm = TRUE),
                     tec_dig = factor(case_when(ac236_1 == 1 | ac236_2 == 1 | ac236_3 == 1 | ac236_4 == 1 | ac236_5 == 1 | ac236_6 == 1 | ac236_7 == 1 ~ "Utiliza",
                                                ac236_8 == 1 ~ "No utiliza"),
                                      levels = c("Utiliza", "No utiliza"))) %>%
              select(guid_ce, ends_with("ani"), ends_with("mec"), tec_dig),
            by = "guid_ce", multiple = "first") %>%
  left_join(read_caf21("seccion_13_maquinaria_estacionaria") %>%
              distinct(guid, .keep_all = TRUE) %>%
              transmute(guid_ce = guid,
                        usa_maq_fij = factor(case_when(ac239 == 1 ~ "Utiliza",
                                                       ac239 %in% 2:3 ~ "No utiliza"),
                                             levels = c("Utiliza", "No utiliza")),
                        pro_maq_fij = factor(case_when(usa_maq_fij == "Utiliza" & ac240_1 == 1 ~ "Propia",
                                                       usa_maq_fij == "Utiliza" & ac240_1 == 2 ~ "Ajena"),
                                             levels = c("Propia", "Ajena"))),
            by = "guid_ce", multiple = "first")

### Management practices, fertilizers, herbicides and pesticides
caf21_ce <- caf21_ce %>%
  left_join(read_caf21("seccion_11") %>%
              distinct(guid, .keep_all = TRUE) %>%
              mutate(pra_man = factor(case_when(pm212_1 == 1 | pm212_2 == 1 | pm212_3 == 1 | pm212_4 == 1 | pm212_5 == 1 | pm212_6 == 1 ~ "Realiza",
                                                pm212_7 == 1 ~ "No realiza"), # Mostly incorporation of organic matter, crop and livestock rotation
                                      levels = c("Realiza", "No realiza")),
                     cer_pra = factor(case_when(pm223_1 == 1 | pm223_2 == 1 | pm223_3 == 1 ~ "Tiene certificaciones",
                                                pm223_4 == 1 ~ "No tiene certificaciones"),
                                      levels = c("Tiene certificaciones", "No tiene certificaciones")),
                     fert = factor(case_when(pm213_3 == 1 | pm213_4 == 1 | pm213_5 == 1 | pm213_6 == 1 ~ "Fertilizantes caseros",
                                             pm213_1 == 1 | pm213_2 == 1 ~ "Fertilizantes comerciales",
                                             pm213_7 == 1 ~ "No utiliza fertilizantes"),
                                   levels = c("No utiliza fertilizantes", "Fertilizantes caseros", "Fertilizantes comerciales")),
                     plag = factor(case_when(pm214 <= 3 ~ "Utiliza plaguicidas",
                                             pm214 == 4 ~ "No utiliza plaguicidas"),
                                   levels = c("No utiliza plaguicidas", "Utiliza plaguicidas")),
                     herb = factor(case_when(pm215 == 1 ~ "Utiliza herbicidas",
                                             pm215 == 2 ~ "No utiliza herbicidas"),
                                   levels = c("No utiliza herbicidas", "Utiliza herbicidas"))) %>%
              select(guid_ce = guid, pra_man, cer_pra, fert, plag, herb, starts_with("pm21"), -ends_with("_glo")),
            by = "guid_ce", multiple = "first")

### Insurance, credit and technical assistance
caf21_ce <- caf21_ce %>%
  left_join(read_caf21("seccion_3") %>%
              transmute(guid_ce = guid,
                        segu = factor(case_when(sa24_11 == 1 ~ "No contrata seguros",
                                                if_any(paste0("sa24_", 1:10), ~ .x == 1) ~ "Contrata seguros"),
                                      levels = c("Contrata seguros", "No contrata seguros")),
                        cred = factor(case_when(sa25_4 == 1 ~ "Sin créditos",
                                                sa25_1 == 1 | sa25_2 == 1 | sa25_3 == 1 ~ "Con créditos"),
                                      levels = c("Con créditos", "Sin créditos")),
                        asis = factor(case_when(sa26_1 == 1 ~ "Recibió asistencia técnica estatal",
                                                sa26_2 == 1 ~ "No recibió asistencia técnica estatal"),
                                      levels = c("Recibió asistencia técnica estatal", "No recibió asistencia técnica estatal"))),
            by = "guid_ce", multiple = "first")

### Production lines: main line (the one that generated the most income)
caf21_ce <- caf21_ce %>%
  mutate(rub_pri = factor(case_when(g2 %in% c(1, 4) ~ "Cereales",
                                    g2 == 2 ~ "Leguminosas",
                                    g2 %in% c(3, 9, 10, 15) ~ "Industriales",
                                    g2 %in% c(5:7, 13, 14) ~ "Hortalizas, hongos y hierbas",
                                    g2 == 8 ~ "Tubérculos",
                                    g2 == 11 ~ "Forrajeras",
                                    g2 == 12 ~ "Flores",
                                    g2 == 16 ~ "Viñas",
                                    g2 %in% 17:24 ~ "Frutales",
                                    g2 == 25 ~ "Viveros",
                                    g2 == 26 ~ "Bovinos",
                                    g2 == 27 ~ "Equinos",
                                    g2 == 28 ~ "Otros animales mayores",
                                    g2 == 29 ~ "Ovinos",
                                    g2 == 30 ~ "Caprinos",
                                    g2 == 31 ~ "Porcinos",
                                    g2 == 32 ~ "Aves de corral",
                                    g2 == 33 ~ "Colmenares",
                                    g2 == 34 ~ "Otras aves",
                                    g2 == 35 ~ "Agrícola y ganadero",
                                    g2 %in% c(36:38, 43, 44, 47, 55, 62) ~ "Actividades conexas a lo silvoagropecuario",
                                    g2 == 39 ~ "Semilleros",
                                    g2 == 40 ~ "Forestal",
                                    g2 == 41 ~ "Producción de leña y otros madereros",
                                    g2 == 42 ~ "Recolección de productos forestales no madereros",
                                    g2 %in% c(45, 46, 48:54) ~ "Elaboración de alimentos y bebidas",
                                    g2 %in% 56:61 ~ "Otras actividades no productivas",
                                    g2 %in% c(63:68, 70, 71) ~ "Actividades productivas no silvoagropecuarias",
                                    g2 == 72 ~ "Sin actividad generadora de ingresos"),
                          levels = c("Hortalizas, hongos y hierbas", "Frutales", "Leguminosas", "Tubérculos", "Viñas", "Flores", "Semilleros", "Viveros",
                                     "Cereales", "Industriales", "Forrajeras", "Forestal",
                                     "Bovinos", "Equinos", "Otros animales mayores", "Ovinos", "Caprinos", "Porcinos", "Aves de corral", "Otras aves", "Colmenares",
                                     "Agrícola y ganadero",
                                     "Elaboración de alimentos y bebidas", "Producción de leña y otros madereros", "Recolección de productos forestales no madereros",
                                     "Actividades conexas a lo silvoagropecuario", "Actividades productivas no silvoagropecuarias", "Otras actividades no productivas", "Sin actividad generadora de ingresos")))

### Production lines: crops (area and share irrigated)
abr_rub <- c("Bosque nativo" = "bos", "Cereales" = "cer", "Flores" = "flo", "Forestal" = "for", "Forrajeras" = "forr",
             "Frutales" = "fru", "Hortalizas, hongos y hierbas" = "hor", "Industriales" = "ind", "Leguminosas y tubérculos" = "leg",
             "Praderas" = "pra_mej", "Semilleros" = "sem", "Viñas" = "vin", "Viveros" = "viv")

caf21_ce <- caf21_ce %>%
  left_join(cultivos %>%
              group_by(guid, rub, sec_rie) %>%
              summarise(sup = sum(sup, na.rm = TRUE), .groups = "drop") %>%
              mutate(rub = abr_rub[rub]) %>%
              pivot_wider(names_from = c("rub", "sec_rie"), values_from = "sup", names_prefix = "sup_") %>%
              mutate(across(starts_with("sup_"), ~ replace_na(.x, 0)),
                     pct_bos_rie = if_else(sup_bos_sec > 0, 0, NA),
                     pct_cer_rie = pct_rie(sup_cer_rie, sup_cer_sec),
                     pct_flo_rie = pct_rie(sup_flo_rie, sup_flo_sec),
                     pct_for_rie = if_else(sup_for_sec > 0, 0, NA),
                     pct_forr_rie = pct_rie(sup_forr_rie, sup_forr_sec),
                     pct_fru_rie = pct_rie(sup_fru_rie, sup_fru_sec),
                     pct_hor_rie = pct_rie(sup_hor_rie, sup_hor_sec),
                     pct_ind_rie = pct_rie(sup_ind_rie, sup_ind_sec),
                     pct_leg_rie = pct_rie(sup_leg_rie, sup_leg_sec),
                     pct_pra_mej_rie = pct_rie(sup_pra_mej_rie, sup_pra_mej_sec),
                     pct_sem_rie = pct_rie(sup_sem_rie, sup_sem_sec),
                     pct_vin_rie = pct_rie(sup_vin_rie, sup_vin_sec),
                     pct_viv_rie = pct_rie(sup_viv_rie, sup_viv_sec)) %>%
              select(guid_ce = guid, starts_with("pct")),
            by = "guid_ce") %>%
  mutate(sup_cer = us61_1,
         sup_leg = us61_2,
         sup_ind = us61_3,
         sup_hor = us61_4,
         sup_fru = us61_5,
         sup_vin = us61_6,
         sup_flo = us61_7,
         sup_sem = us61_8,
         sup_viv = us61_9,
         sup_forr = us61_10,
         sup_for = us61_11,
         sup_bos = us61_12,
         sup_pra_mej = us61_13,
         sup_pra_nat = us61_14,
         pct_pra_nat_rie = 0,
         # Share irrigated is missing when the unit has no area in the production line
         across(paste0("pct_", c("bos", "cer", "flo", "fru", "hor", "ind", "leg", "pra_mej", "pra_nat", "sem", "vin", "viv"), "_rie"),
                ~ if_else(get(sub("pct_(.*)_rie", "sup_\\1", cur_column())) > 0, .x, NA)),
         # pct_sup_bos keeps the land use definition (plantations and native forest)
         across(paste0("sup_", setdiff(names(umbrales_agr), "bos")), ~ .x * 100 / sup_apr, .names = "pct_{.col}")) %>%
  clasificar_rubros(umbrales_agr, "sup_")

### Production lines: livestock and poultry
caf21_ce <- caf21_ce %>%
  mutate(gan_bov = ga161,
         gan_ovi = ga172,
         gan_cap = ga180,
         gan_por = ga182,
         gan_equ = ga201,
         gan_con = ga207,
         gan_gal = ga185,
         gan_pav = ga186,
         gan_pat = ga187,
         gan_gan = ga188,
         gan_col = ga199,
         gan_ani_may = ga202 + ga203 + ga204 + ga205 + ga206 + ga208 + ga209,
         gan_oav = ga189 + ga191 + ga193) %>%
  clasificar_rubros(umbrales_gan, "gan_") %>%
  add_rubro_dummies(paste0("sup_", c("hor", "flo", "sem", "viv", "ind", "vin", "leg", "fru", "cer", "forr", "for", "pra_mej"), "_cat"), "agr") %>%
  add_rubro_dummies(paste0("gan_", names(umbrales_gan), "_cat"), "gan")

### Sales: main destination of production and farm sales
caf21_ce <- caf21_ce %>%
  mutate(des_pro = factor(case_when(g4 %in% c(11, 12, 14) ~ "Autoconsumo, trueque o sin ventas",
                                    g4 == 3 ~ "En el predio - consumidor final",
                                    g4 %in% c(1, 4:8) ~ "Intermediarios, industrias y cooperativas",
                                    g4 %in% c(2, 9, 10) ~ "Mercado minorista y mayorista y supermercados"),
                          levels = c("Autoconsumo, trueque o sin ventas", "En el predio - consumidor final",
                                     "Mercado minorista y mayorista y supermercados", "Intermediarios, industrias y cooperativas")),
         ven_pre = factor(case_when(hp280_2 == 16 ~ "Sin ventas",
                                    hp280_2 == 1 ~ "(0 - 1.5]", # Millions of pesos
                                    hp280_2 == 2 ~ "(1.5 - 3]",
                                    hp280_2 == 3 ~ "(3 - 6]",
                                    hp280_2 == 4 ~ "(6 - 17]",
                                    hp280_2 == 5 ~ "(17 - 70]",
                                    hp280_2 >= 6 & hp280_2 < 15 ~ "(70 y más]"), # Higher brackets are grouped (very low frequencies)
                          levels = c("Sin ventas", "(0 - 1.5]", "(1.5 - 3]", "(3 - 6]", "(6 - 17]", "(17 - 70]", "(70 y más]")))


# PRODUCTIVE UNITS: ABBREVIATED QUESTIONNAIRE (CAF21_CA) -----------------------
# The abbreviated questionnaire only records land use, tenure and self-consumption production.
# All of its units with some production are classified as peasant households.

caf21_ca <- read_caf21("seccion_1") %>%
  filter(substr(guid, 1, 2) == "CA") %>%
  transmute(guid_ce = guid, sup_tot = as.double(sup_upa)) %>%
  left_join(rename(comuna, guid_ce = guid), by = "guid_ce") %>%
  left_join(rename(tenencia, guid_ce = guid), by = "guid_ce") %>%
  left_join(rename(us61, guid_ce = guid), by = "guid_ce") %>%
  mutate(sup_cer = us61_1,
         sup_leg = us61_2,
         sup_ind = us61_3,
         sup_hor = us61_4,
         sup_fru = us61_5,
         sup_vin = us61_6,
         sup_flo = us61_7,
         sup_sem = us61_8,
         sup_viv = us61_9,
         sup_forr = us61_10,
         sup_for = us61_11,
         sup_bos = us61_12,
         sup_pra_mej = us61_13,
         sup_pra_nat = us61_14,
         sup_apr = sup_tot - (us61_16 + us61_17 + us61_18),
         across(paste0("sup_", names(umbrales_agr)), ~ .x * 100 / sup_apr, .names = "pct_{.col}"),
         across(paste0("sup_", names(umbrales_agr)), ~ case_when(.x == 0 ~ "Sin producción",
                                                                  .x > 0 ~ "Autoconsumo"), .names = "{.col}_cat")) %>%
  # Livestock: only whether the unit has each species (ga152_*), counted as self-consumption
  left_join(read_caf21("seccion_10") %>%
              filter(substr(guid, 1, 2) == "CA") %>%
              select(guid_ce = guid, starts_with("ga152"), -ends_with("_glo")) %>%
              mutate(across(starts_with("ga152"), ~ case_when(.x == 1 ~ "Autoconsumo",
                                                              is.na(.x) ~ "Sin producción"), .names = "{.col}_cat")) %>%
              select(guid_ce, ends_with("_cat")),
            by = "guid_ce") %>%
  mutate(gan_bov_cat = ga152_1_cat,
         gan_ovi_cat = ga152_2_cat,
         gan_cap_cat = ga152_3_cat,
         gan_por_cat = ga152_4_cat,
         gan_gal_cat = ga152_5_cat,
         gan_col_cat = ga152_6_cat,
         gan_equ_cat = ga152_7_cat,
         gan_con_cat = ga152_13_cat,
         gan_ani_may_cat = if_else(if_any(paste0("ga152_", c(8:12, 14:16), "_cat"), ~ .x %in% "Autoconsumo"), "Autoconsumo", "Sin producción"),
         across(c(gan_bov_cat, gan_ovi_cat, gan_cap_cat, gan_por_cat, gan_gal_cat, gan_col_cat, gan_equ_cat, gan_con_cat, gan_ani_may_cat),
                ~ factor(.x, levels = c("Sin producción", "Autoconsumo"))),
         hog_afc_est = as.numeric(if_any(c(paste0("sup_", c("hor", "leg", "fru", "cer", "ind", "vin", "flo", "sem", "viv", "pra_mej"), "_cat"),
                                           paste0("gan_", c("bov", "ovi", "cap", "por", "gal", "col", "ani_may"), "_cat")),
                                         ~ .x %in% "Autoconsumo"))) %>%
  add_rubro_dummies(paste0("sup_", c("hor", "flo", "sem", "viv", "ind", "vin", "leg", "fru", "cer", "forr", "for", "pra_mej"), "_cat"), "agr") %>%
  add_rubro_dummies(paste0("gan_", c("bov", "ovi", "cap", "por", "gal", "col", "equ", "con", "ani_may"), "_cat"), "gan")

# Only variables shared with caf21_ce
caf21_ca <- select(caf21_ca, any_of(names(caf21_ce)))


# PRODUCTIVE UNITS: ALL (CAF21_PRE) --------------------------------------------
# All productive units, the base for estimating magnitudes (fewer variables than caf21)

caf21_pre <- caf21_ce %>%
  filter(is.na(g1) | g1 != -77) %>% # Excludes the abbreviated questionnaires included in caf21_ce
  bind_rows(caf21_ca) %>%
  mutate(cues = case_when(substr(guid_ce, 1, 2) == "CE" ~ "Extendido",
                          substr(guid_ce, 1, 2) == "CA" ~ "Abreviado")) %>%
  add_com_0721()

rm(sec1, sec8, us61, comuna, tenencia, cultivos)


# HOUSEHOLDS (CAF21_HOG) -------------------------------------------------------
# Household questionnaire (GUID = HP...). Together with caf21_mie, the only datasets with this identifier.

caf21_hog <- read_caf21("gestion") %>%
  mutate(reg = cut_region, com = cut_comuna) %>%
  left_join(read_caf21("seccion_15_hogar_oa"), by = "guid") %>%
  left_join(read_caf21("actividad_agricola"), by = "guid") %>%
  select(-c(starts_with("cut_"), starts_with("region."), starts_with("comuna."), ends_with("_glo"))) %>%
  mutate(across(starts_with("us61_"), as.double),
         across(starts_with("pr"), as.double),
         across(starts_with(c("ac", "hp", "pr", "us", "ga")), ~ na_if(.x, -77))) %>%
  rename(id_hog = guid) %>%
  add_com_0721()


# MEMBERS (CAF21_MIE) ----------------------------------------------------------

caf21_mie <- read_caf21("seccion_15_hogar") %>%
  select(-ends_with("_glo")) %>%
  mutate(across(starts_with("hp"), ~ case_when(.x <= 0 ~ 0, .x > 0 ~ .x))) %>%
  mutate(reg = cut_region, com = cut_comuna,
         id_hog = guid,
         id_per = paste(guid, hp261, hp262, hp263, sep = "-"),
         jh = factor(case_when(hp261 == 1 ~ "Productor o jefe de hogar",
                               hp261 != 1 ~ "Otro miembro del hogar"),
                     levels = c("Productor o jefe de hogar", "Otro miembro del hogar")),
         sex = factor(case_when(hp262 == 1 ~ "Masculino",
                                hp262 == 2 ~ "Femenino"),
                      levels = c("Femenino", "Masculino")),
         edad = hp263,
         edad_rec = recode_edad_rec(edad),
         edad_cat = recode_edad_cat(edad),
         nedu = factor(case_when(edad >= 15 & hp265 %in% c(1, 2, 6) ~ "Hasta básica completa",
                                 edad >= 15 & hp265 == 3 & hp266 == 2 ~ "Media incompleta",
                                 edad >= 15 & hp265 == 3 & hp266 == 1 ~ "Media completa",
                                 edad >= 15 & hp265 == 4 ~ "Superior"),
                       levels = c("Hasta básica completa", "Media incompleta", "Media completa", "Superior")),
         # Not available: nationality, rurality, employment status in detail, occupation and industry
         pue_rec = factor(case_when(hp268 >= 9 ~ "No pertenece", # Don't know/no answer coded as "No pertenece", for comparability with 2007
                                    hp268 == 1 ~ "Mapuche",
                                    hp268 %in% 2:8 ~ "Pertenece a otro pueblo"),
                          levels = c("No pertenece", "Mapuche", "Pertenece a otro pueblo")),
         pue = factor(case_when(pue_rec == "No pertenece" ~ "No pertenece",
                                pue_rec %in% c("Mapuche", "Pertenece a otro pueblo") ~ "Pertenece"),
                      levels = c("No pertenece", "Pertenece")),
         sfdt = factor(case_when(edad < 15 ~ "Menor de 15 años",
                                 edad >= 15 & (hp261 == 1 | hp270 == 1 | hp274 == 1) ~ "Ocupado", # Works on the farm or elsewhere
                                 edad >= 15 & hp261 != 1 & hp270 == 2 & hp274 == 2 ~ "Inactivo"), # Works neither on nor off the farm (potentially unemployed included)
                       levels = c("Menor de 15 años", "Ocupado", "Desocupado", "Inactivo")),
         sfdt_pet = as.numeric(edad >= 15 & !is.na(edad)),
         sfdt_ocu = case_when(sfdt == "Ocupado" ~ 1,
                              sfdt %in% c("Inactivo", "Desocupado") ~ 0),
         afc_est = case_when(sfdt == "Ocupado" & (hp261 == 1 | hp270 == 1) ~ 1,
                             sfdt == "Ocupado" & hp261 != 1 & hp270 != 1 ~ 0,
                             sfdt %in% c("Desocupado", "Inactivo") ~ 0),
         # Farm work: days per week (hp273_a, days per month) and hours per week (hp273_b, hours per day)
         dtrab = case_when(afc_est == 1 & hp273_a >= 20 ~ 5,
                           afc_est == 1 & hp273_a < 20 ~ hp273_a / 4,
                           afc_est == 0 ~ 0),
         htrab = case_when(afc_est == 1 & hp273_b >= 8 ~ 8 * dtrab,
                           afc_est == 1 & hp273_b < 8 ~ hp273_b * dtrab,
                           afc_est == 0 ~ 0),
         temp = factor(case_when(afc_est == 1 & hp272 > 6 ~ "Permanente", # More than 6 months
                                 afc_est == 1 & hp272 > 0 & hp272 <= 6 ~ "Temporal, a plazo u otra"),
                       levels = c("Permanente", "Temporal, a plazo u otra")),
         ytrab_pri = factor(case_when(sfdt == "Ocupado" & afc_est == 0 ~ "Extra-predial",
                                      sfdt == "Ocupado" & afc_est == 1 & hp275 == 1 ~ "Extra-predial",
                                      sfdt == "Ocupado" & afc_est == 1 & (hp274 == 2 | hp275 == 2) ~ "Predial"),
                            levels = c("Extra-predial", "Predial")),
         # Labor time devoted to the farm (pre), off-farm (ext) and inactivity (ina), in full-time equivalents
         # Missing for children; unemployed counted as inactive
         .menor = sfdt %in% "Menor de 15 años",
         jor_pre = case_when(.menor ~ NA,
                             sfdt == "Ocupado" & afc_est == 1 & htrab > 33 ~ 1,
                             sfdt == "Ocupado" & afc_est == 1 & htrab <= 33 & htrab > 22 ~ 0.75,
                             sfdt == "Ocupado" & afc_est == 1 & htrab <= 22 & htrab > 11 ~ 0.5,
                             sfdt == "Ocupado" & afc_est == 1 & htrab <= 11 ~ 0.25,
                             TRUE ~ 0),
         jor_ext = case_when(.menor ~ NA,
                             sfdt == "Ocupado" & afc_est == 0 & hp274 == 1 ~ 1,
                             sfdt == "Ocupado" & afc_est == 1 & hp274 == 1 ~ 1 - jor_pre,
                             TRUE ~ 0),
         jor_ocu = case_when(.menor ~ NA,
                             sfdt == "Ocupado" ~ jor_pre + jor_ext,
                             TRUE ~ 0),
         jor_ina = case_when(.menor ~ NA,
                             sfdt %in% c("Inactivo", "Desocupado") ~ 1,
                             sfdt == "Ocupado" ~ 1 - jor_ocu,
                             TRUE ~ 0),
         # Annual days of farm work
         jor_afc = hp272 * case_when(hp273_a >= 20 ~ 20, hp273_a < 20 ~ hp273_a) *
           case_when(hp273_b == 0 ~ 0, hp273_b > 0 & hp273_b <= 4 ~ 0.5, hp273_b > 4 & hp273_b <= 6 ~ 0.75, hp273_b > 6 ~ 1),
         jor_afc = if_else(sfdt %in% "Menor de 15 años", NA, jor_afc),
         tot_per = 1) %>%
  select(-.menor)


# HOUSEHOLDS: STRUCTURE AND PEASANT CLASSIFICATION -----------------------------

### Producer characteristics
jefe <- caf21_mie %>%
  filter(jh == "Productor o jefe de hogar") %>%
  select(id_hog, sex, edad, edad_rec, nedu, pue) %>%
  mutate(sex_jh = sex,
         edad_jh = edad,
         edad_rec_jh = edad_rec,
         nedu_jh = nedu,
         pue_jh = pue)

### Age and occupational structure (unemployed are not identified in CAF 2021)
edad <- caf21_mie %>%
  group_by(id_hog) %>%
  summarise(n_menores = sum(edad_rec %in% "Menor"),
            n_adultos = sum(edad_rec %in% "Adulto"),
            n_ancianos = sum(edad_rec %in% "Anciano"),
            n_mie = n()) # Includes members without age

trabajo <- caf21_mie %>%
  filter(sfdt %in% c("Ocupado", "Desocupado", "Inactivo")) %>%
  group_by(id_hog) %>%
  summarise(n_ocupados = sum(sfdt == "Ocupado"),
            n_inactivos = sum(sfdt == "Inactivo"),
            across(c(jor_pre, jor_ext, jor_ina, jor_ocu), \(x) sum(x, na.rm = TRUE)))

caf21_hog <- caf21_hog %>%
  left_join(jefe, by = "id_hog") %>%
  left_join(edad, by = "id_hog") %>%
  left_join(trabajo, by = "id_hog") %>%
  mutate(n_desocupados = 0) %>%
  add_estructura_hogar() %>%
  mutate(jor_fam_tot = jor_ocu + jor_ina,
         pct_jor_ocu = 100 * (jor_ocu / jor_fam_tot),
         pct_jor_ext = 100 * (jor_ext / jor_fam_tot),
         pct_jor_pre = pct_jor_ocu - pct_jor_ext,
         pct_jor_ina = 100 * (jor_ina / jor_fam_tot))

rm(jefe, edad, trabajo)

### Production lines: post-productive activities (food and beverages, crafts, agrotourism, gathering, firewood and others)
caf21_hog <- caf21_hog %>%
  mutate(across(starts_with("hp278"), ~ case_when(is.na(.x) ~ "Sin producción",
                                                  .x %in% 2:3 ~ "Autoconsumo",
                                                  .x == 1 ~ "Comercialización"), .names = "{.col}_cat"),
         rub_otr = case_when(is.na(hp277_7) & is.na(hp277_8) & is.na(hp277_9) & is.na(hp277_10) ~ "Sin producción",
                             hp278_7 %in% 2:3 | hp278_8 %in% 2:3 | hp278_9 %in% 2:3 | hp278_10 %in% 2:3 ~ "Autoconsumo",
                             hp278_7 == 1 | hp278_8 == 1 | hp278_9 == 1 | hp278_10 == 1 ~ "Comercialización"),
         rub_ali = hp278_1_cat,
         rub_beb = hp278_2_cat,
         rub_art = hp278_3_cat,
         rub_agr = hp278_4_cat,
         rub_rec = hp278_5_cat,
         rub_len = hp278_6_cat,
         across(c(rub_ali, rub_beb, rub_art, rub_agr, rub_rec, rub_len), ~ case_when(.x %in% c("Autoconsumo", "Comercialización") ~ 1,
                                                                                    .x == "Sin producción" ~ 0),
                .names = "{.col}_aut"),
         across(c(rub_ali, rub_beb, rub_art, rub_agr, rub_rec, rub_len), ~ case_when(.x == "Comercialización" ~ 1,
                                                                                    .x %in% c("Autoconsumo", "Sin producción") ~ 0),
                .names = "{.col}_com"),
         n_rub_pos_aut = rowSums(pick(rub_ali_aut, rub_beb_aut, rub_art_aut, rub_agr_aut, rub_rec_aut, rub_len_aut), na.rm = TRUE),
         n_rub_pos_com = rowSums(pick(rub_ali_com, rub_beb_com, rub_art_com, rub_agr_com, rub_rec_com, rub_len_com), na.rm = TRUE)) %>%
  select(-c(hp278_1_cat:hp278_10_cat))

### Share of farm income in total household income (reported by the producer)
caf21_hog <- caf21_hog %>%
  left_join(caf21_mie %>%
              filter(jh == "Productor o jefe de hogar") %>%
              transmute(id_hog, pct_ing_pre = recode_pct_ing_pre(hp276)),
            by = "id_hog")

### Peasant households according to the household questionnaire (hog_afc_est_mis)
# Households hiring permanent workers are not peasant households; those hiring neither permanent nor temporary workers are.
# Households hiring only temporary workers are left missing, because the number of days hired is unknown. Their
# classification is imputed in 04_01_caf21_imputaciones.R.
caf21_hog <- caf21_hog %>%
  mutate(hog_afc_est_mis = factor(case_when(tr244 == 1 ~ 0,
                                            tr244 %in% 2:3 & tr250 %in% 2:3 ~ 1,
                                            tr244 %in% 2:3 & tr250 == 1 ~ NA)))

rm(caf21_files)

# SAVING ------------------------------------------------------------------

saveRDS(caf21_ce, "output/caf21_ce.rds")
saveRDS(caf21_ca, "output/caf21_ca.rds")
saveRDS(caf21_pre, "output/caf21_pre.rds")
