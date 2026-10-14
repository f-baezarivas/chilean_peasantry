### CAF 2007 PROCESSING ###
# Builds household- and member-level datasets from the 2007 Agricultural and Forestry Census (CAF 2007),
# identifying peasant (AFC) households:
#   - caf07: households (one row per farm unit, dir_folio)
#   - caf07_mie: household members

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, haven, readxl)

source("data_process/01_dpa.R")
source("helpers/hogar.R") # Household structure helpers
source("helpers/caf.R") # CAF helpers

caf07_dir <- "C:/Users/fbaez/OneDrive - Universidad Católica de Chile/Bases de datos/Censo Agropecuario/2007" # Original CAF 2007 .sav files (INE)

read_caf07 <- function(file) {
  read_sav(file.path(caf07_dir, paste0(file, ".sav"))) %>%
    zap_formats() %>%
    zap_labels() %>%
    zap_label() %>%
    rename_with(tolower)
}


# HOUSEHOLDS -------------------------------------------------------------------

### Identification of peasant households
caf07 <- read_caf07("DIRECTORIO") %>%
  left_join(read_caf07("EMPLEO"), by = "dir_folio") %>%
  left_join(read_caf07("TOTALES"), by = "dir_folio") %>%
  left_join(read_excel(file.path(caf07_dir, "VBP-folio (únicos).xlsx")) %>% select(dir_folio, vbp_uf), by = "dir_folio") %>%
  mutate(id_hog = dir_folio,
         hog_afc_est = case_when(dir_condicion_juridica <= 3 & # 1-Individual producer; 2-Successions and de facto partnerships; 3-Commoner with individual use
                                   dir_administrador == 0 & # Not managed by a hired administrator
                                   te <= 3 & # 1-Farm with activity; 2-Forestry farm; 3-Farm without land
                                   numero_2006_total == 0 & # No permanent workers hired from outside the household
                                   (abril_2006_total + julio_2006_total + octubre_2006_total + enero_2007_total) < 4 ~ 1, # Up to 3 temporary workers (less than one full-time equivalent)
                                 TRUE ~ 0),
         hog_afc_amp = hog_afc_est,
         vbp_cat = factor(case_when(vbp_uf <= 100 ~ "Menos o igual a 100 UF",
                                    vbp_uf > 100 & vbp_uf <= 200 ~ "Entre 100 y 200 UF",
                                    vbp_uf > 200 & vbp_uf <= 600 ~ "Entre 200 y 600 UF",
                                    vbp_uf > 600 & vbp_uf <= 1200 ~ "Entre 600 y 1.200 UF",
                                    vbp_uf > 1200 & vbp_uf <= 2400 ~ "Entre 1.200 y 2.400 UF",
                                    vbp_uf > 2400 ~ "Más de 2.400 UF"),
                          levels = c("Menos o igual a 100 UF", "Entre 100 y 200 UF", "Entre 200 y 600 UF",
                                     "Entre 600 y 1.200 UF", "Entre 1.200 y 2.400 UF", "Más de 2.400 UF"))) %>%
  # Municipalities, including the version harmonized with CAF 2021 (no new municipalities between 2008 and 2018)
  left_join(select(dpa_cod, cod_com_2008, cod_com_2018, nom_com, cod_com_0721, nom_com_0721),
            by = c("dir_comuna" = "cod_com_2008")) %>%
  mutate(cod_com_2008 = dir_comuna)

### Producer characteristics (recorded only for natural persons, dir_sexo 0-1)
caf07 <- caf07 %>%
  mutate(sex_jh = factor(case_when(dir_sexo == 1 ~ "Masculino",
                                   dir_sexo == 0 ~ "Femenino"),
                         levels = c("Femenino", "Masculino")),
         edad_jh = dir_edad,
         edad_rec_jh = recode_edad_rec(dir_edad),
         nedu_jh = factor(case_when(dir_edad >= 15 & dir_educacion_p %in% c(1, 5) ~ "Hasta básica completa",
                                    dir_edad >= 15 & dir_educacion_p == 2 & dir_estudios == 0 ~ "Media incompleta",
                                    dir_edad >= 15 & dir_educacion_p == 2 & dir_estudios == 1 ~ "Media completa",
                                    dir_edad >= 15 & dir_educacion_p %in% 3:4 ~ "Superior"),
                          levels = c("Hasta básica completa", "Media incompleta", "Media completa", "Superior")),
         pue_rec = factor(case_when(dir_comunidad_indiijena_productor == 1000 ~ "No pertenece",
                                    dir_comunidad_indiijena_productor == 6 ~ "Mapuche",
                                    dir_comunidad_indiijena_productor %in% c(1:5, 7:9) ~ "Pertenece a otro pueblo"),
                          levels = c("No pertenece", "Mapuche", "Pertenece a otro pueblo")),
         pue_jh = factor(case_when(pue_rec == "No pertenece" ~ "No pertenece",
                                   pue_rec %in% c("Mapuche", "Pertenece a otro pueblo") ~ "Pertenece"),
                         levels = c("No pertenece", "Pertenece")),
         across(c(sex_jh, edad_jh, edad_rec_jh, nedu_jh, pue_rec, pue_jh), \(x) if_else(dir_sexo <= 1, x, NA)))


# MEMBERS ----------------------------------------------------------------------

caf07_mie <- read_caf07("MIEMBROS") %>%
  mutate(id_hog = dir_folio,
         id_per = mie_id,
         jh = factor(case_when(parentesco == 1 ~ "Productor o jefe de hogar",
                               parentesco != 1 ~ "Otro miembro del hogar"),
                     levels = c("Productor o jefe de hogar", "Otro miembro del hogar")),
         sex = factor(case_when(sexo == 1 ~ "Masculino",
                                sexo == 2 ~ "Femenino"),
                      levels = c("Femenino", "Masculino")),
         edad_rec = recode_edad_rec(edad),
         edad_cat = recode_edad_cat(edad),
         nedu = factor(case_when(edad >= 15 & educacion %in% c(1:3, 10) ~ "Hasta básica completa",
                                 edad >= 15 & educacion == 4 ~ "Media incompleta",
                                 edad >= 15 & educacion == 5 ~ "Media completa",
                                 edad >= 15 & educacion %in% 6:9 ~ "Superior"),
                       levels = c("Hasta básica completa", "Media incompleta", "Media completa", "Superior")),
         # Not available: nationality, rurality (must be derived from the district), ethnicity (only for the producer)
         sfdt = factor(case_when(edad < 15 ~ "Menor de 15 años",
                                 edad >= 15 & (dedicacion %in% 2:3 | otra_act_ded %in% 6:9) ~ "Ocupado", # Works on the farm or elsewhere
                                 edad >= 15 & dedicacion == 1 & otra_act_ded == 5 ~ "Desocupado", # Does not work on the farm and is looking for work
                                 edad >= 15 & dedicacion == 1 & otra_act_ded %in% 2:4 ~ "Inactivo"), # Student, housework or retired
                       levels = c("Menor de 15 años", "Ocupado", "Desocupado", "Inactivo")),
         .ocu = sfdt %in% "Ocupado",
         cise = factor(case_when(.ocu & dedicacion %in% 0:1 & otra_act_ded == 6 ~ "Asalariado",
                                 .ocu & dedicacion %in% 2:3 & otra_act_ded == 6 & sueldo %in% c(0, 1, 3) & otro_ing %in% c(0, 1, 3) ~ "Asalariado",
                                 .ocu & dedicacion %in% 0:1 & otra_act_ded >= 7 ~ "Trabajador por cuenta propia",
                                 .ocu & dedicacion %in% 2:3 & sueldo %in% c(0, 1, 3) & otro_ing %in% c(0, 1, 3) & otra_act_ded >= 7 ~ "Trabajador por cuenta propia",
                                 .ocu & dedicacion %in% 2:3 & (sueldo == 2 | otro_ing == 2) ~ "Trabajador por cuenta propia",
                                 .ocu & dedicacion %in% 2:3 & sueldo %in% 0:1 & otro_ing %in% 0:1 ~ "Familiar no remunerado"),
                       levels = c("Trabajador por cuenta propia", "Asalariado", "Familiar no remunerado")),
         # Not available: occupation (ciuo) and industry (ciiu)
         sfdt_pet = as.numeric(edad >= 15 & !is.na(edad)),
         sfdt_ocu = case_when(sfdt == "Ocupado" ~ 1,
                              sfdt %in% c("Inactivo", "Desocupado") ~ 0),
         est_tra = factor(case_when(edad < 15 ~ "Menor de 15 años",
                                    sfdt %in% c("Ocupado", "Desocupado") & otra_act_ded %in% 1:2 ~ "Estudia y trabaja", # Or looks for work
                                    sfdt %in% c("Ocupado", "Desocupado") & otra_act_ded >= 3 ~ "Sólo trabaja", # Or looks for work
                                    sfdt == "Inactivo" & otra_act_ded %in% 1:2 ~ "Sólo estudia",
                                    sfdt == "Inactivo" & otra_act_ded %in% 3:4 ~ "No estudia ni trabaja"),
                          levels = c("Menor de 15 años", "Estudia y trabaja", "Sólo trabaja", "Sólo estudia", "No estudia ni trabaja")),
         cise_ind = case_when(cise %in% c("Trabajador por cuenta propia", "Familiar no remunerado") ~ 1,
                              cise == "Asalariado" ~ 0),
         # Peasant workers: strict (works on the farm) and broad (works on the farm or receives income from it)
         afc_est = case_when(sfdt == "Ocupado" & dedicacion %in% 2:3 ~ 1,
                             sfdt == "Ocupado" & dedicacion %in% 0:1 ~ 0,
                             sfdt %in% c("Desocupado", "Inactivo") ~ 0),
         afc_amp = case_when(sfdt == "Ocupado" & (dedicacion %in% 2:3 | sueldo %in% 2:3 | otro_ing %in% 2:3) ~ 1,
                             sfdt == "Ocupado" & dedicacion %in% 0:1 & sueldo %in% 0:1 & otro_ing %in% 0:1 ~ 0,
                             sfdt %in% c("Desocupado", "Inactivo") ~ 0),
         ytrab_pri = factor(case_when(sfdt == "Ocupado" & afc_est == 0 ~ "Extra-predial",
                                      sfdt == "Ocupado" & afc_est == 1 & sueldo == 3 ~ "Extra-predial",
                                      sfdt == "Ocupado" & afc_est == 1 & sueldo == 2 ~ "Predial"),
                            levels = c("Extra-predial", "Predial")),
         # Labor time devoted to the farm (pre), off-farm (ext) and inactivity (ina), in full-time equivalents.
         # dedicacion: 1 = does not work on the farm, 2 = full time, 3 = part time. Unemployed counted as inactive.
         jor_pre_07 = case_when(afc_est == 1 & dedicacion == 2 ~ 1,
                                afc_est == 1 & dedicacion == 3 ~ 0.5,
                                afc_est == 0 ~ 0),
         jor_ext_07 = case_when(sfdt == "Ocupado" & dedicacion == 1 & otra_act_ded >= 5 ~ 1,
                                sfdt == "Ocupado" & dedicacion == 1 & otra_act_ded < 5 ~ 0,
                                sfdt == "Ocupado" & dedicacion == 3 & otra_act_ded >= 5 ~ 0.5,
                                sfdt == "Ocupado" & dedicacion == 3 & otra_act_ded < 5 ~ 0,
                                sfdt == "Ocupado" & dedicacion == 2 ~ 0,
                                sfdt %in% c("Inactivo", "Desocupado") ~ 0),
         jor_ocu_07 = jor_pre_07 + jor_ext_07,
         jor_ina_07 = case_when(sfdt %in% c("Inactivo", "Desocupado") ~ 1,
                                sfdt == "Ocupado" & dedicacion == 3 & otra_act_ded < 5 ~ 0.5,
                                sfdt == "Ocupado" & dedicacion == 3 & otra_act_ded >= 5 ~ 0,
                                sfdt == "Ocupado" & dedicacion == 1 & otra_act_ded < 5 ~ 1,
                                sfdt == "Ocupado" & dedicacion == 1 & otra_act_ded >= 5 ~ 0,
                                sfdt == "Ocupado" & dedicacion == 2 ~ 0),
         tot_per = 1) %>%
  select(-.ocu)


# HOUSEHOLD STRUCTURE ----------------------------------------------------------

### Age and occupational structure
edad <- caf07_mie %>%
  group_by(id_hog) %>%
  summarise(n_menores = sum(edad_rec %in% "Menor"),
            n_adultos = sum(edad_rec %in% "Adulto"),
            n_ancianos = sum(edad_rec %in% "Anciano"),
            n_mie = n()) # Includes members without age

trabajo <- caf07_mie %>%
  filter(sfdt %in% c("Ocupado", "Desocupado", "Inactivo")) %>%
  group_by(id_hog) %>%
  summarise(n_ocupados = sum(sfdt == "Ocupado"),
            n_desocupados = sum(sfdt == "Desocupado"),
            n_inactivos = sum(sfdt == "Inactivo"))

### Allocation of family labor (members aged 15+ with information on farm work)
jornadas <- caf07_mie %>%
  filter(sfdt %in% c("Ocupado", "Desocupado", "Inactivo"), dedicacion > 0) %>%
  group_by(id_hog) %>%
  summarise(across(c(jor_pre_07, jor_ext_07, jor_ina_07, jor_ocu_07), \(x) sum(x, na.rm = TRUE)))

caf07 <- caf07 %>%
  left_join(edad, by = "id_hog") %>%
  left_join(trabajo, by = "id_hog") %>%
  add_estructura_hogar() %>%
  left_join(jornadas, by = "id_hog") %>%
  mutate(jor_fam_tot_07 = jor_pre_07 + jor_ext_07 + jor_ina_07,
         pct_jor_pre_07 = 100 * (jor_pre_07 / jor_fam_tot_07),
         pct_jor_ext_07 = 100 * (jor_ext_07 / jor_fam_tot_07),
         pct_jor_ina_07 = 100 * (jor_ina_07 / jor_fam_tot_07),
         pct_jor_ocu_07 = 100 * (jor_ocu_07 / jor_fam_tot_07),
         # Hired temporary labor
         n_tra_tem = abril_2006_total + julio_2006_total + octubre_2006_total + enero_2007_total,
         tra_tem = factor(case_when(abril_2006_total > 0 | julio_2006_total > 0 | octubre_2006_total > 0 | enero_2007_total > 0 ~ "Contrató trabajadores temporales",
                                    abril_2006_total == 0 & julio_2006_total == 0 & octubre_2006_total == 0 & enero_2007_total == 0 ~ "No contrató trabajadores temporales"),
                          levels = c("No contrató trabajadores temporales", "Contrató trabajadores temporales")))

rm(edad, trabajo, jornadas)


# PRODUCTIVE VARIABLES ---------------------------------------------------------

### Land: availability and area
caf07 <- caf07 %>%
  left_join(read_caf07("USO_SUELO"), by = "dir_folio") %>%
  mutate(sup_tot = tot_superficie,
         sup_apr = sup_tot - (matorrales_superficie + infraestructura_superficie + terrenoeo_superficie), # Usable land
         sup_cul = cultivosap_superficie,
         sup_pra = forrajeraspr_superficie + praderasme_superficie + praderasna_superficie,
         sup_bos = plantacionesfo_superficie + bosquesna_superficie,
         sup_bar = barbecho_superficie,
         across(c(sup_apr, sup_cul, sup_pra, sup_bos, sup_bar), ~ .x * 100 / sup_apr, .names = "pct_{.col}")) %>%
  add_sup_pct()

### Irrigation: availability and technology
caf07 <- caf07 %>%
  left_join(read_caf07("RIEGO"), by = "dir_folio") %>%
  mutate(rie = factor(case_when(tot_rie == 0 ~ "Sin riego",
                                tot_rie > 0 ~ "Con riego"),
                      levels = c("Sin riego", "Con riego")),
         con_rie = rie == "Con riego",
         sup_rie = if_else(con_rie, pmin(tot_rie, sup_apr), NA), # Irrigated area cannot exceed usable land
         sup_rie_tra = if_else(con_rie, pmin(tendido + surco + otro, sup_apr), NA), # Traditional
         sup_rie_asp = if_else(con_rie, aspersion + carrete, NA), # Sprinkler
         sup_rie_mic = if_else(con_rie, goteo + microaspersion, NA), # Micro-irrigation
         sup_rie_tec = if_else(con_rie, pmin(aspersion + carrete + goteo + microaspersion, sup_apr), NA), # Technified
         pct_rie_tra = if_else(con_rie, sup_rie_tra * 100 / sup_apr, NA),
         pct_rie_tec = if_else(con_rie, sup_rie_tec * 100 / sup_apr, NA),
         pct_rie = if_else(con_rie, sup_rie * 100 / sup_apr, NA)) %>%
  select(-con_rie)

### Irrigation: main water source and water rights (source with the most formal rights)
fuentes_rie <- c(pozo = "Subterráneas", vertiente = "Cauces", rio = "Cauces", estero = "Cauces",
                 embalse_estacional = "Detenidas", embalse_interanual = "Detenidas", lago = "Detenidas", laguna = "Detenidas",
                 otra = "Otras")

caf07 <- caf07 %>%
  left_join(caf07 %>%
              mutate(n_fue_rie = rowSums(across(pozo:otra, ~ case_when(.x == 0 ~ 0, .x > 0 ~ 1)), na.rm = TRUE)) %>%
              filter(n_fue_rie > 0) %>%
              select(id_hog, pozo:otra, n_fue_rie) %>%
              pivot_longer(cols = pozo:otra, names_to = "fue", values_to = "pro") %>%
              filter(pro > 0) %>%
              group_by(id_hog) %>%
              filter(rank(pro) < 2) %>%
              ungroup() %>%
              distinct(id_hog, .keep_all = TRUE) %>%
              mutate(fue_rie = factor(fuentes_rie[fue], levels = c("Subterráneas", "Cauces", "Detenidas", "Otras")),
                     pro_rie = factor(case_when(pro %in% 1:3 ~ "Propia inscrita",
                                                pro == 4 ~ "Arrendada",
                                                pro == 5 ~ "De hecho",
                                                pro == 6 ~ "Otra"),
                                      levels = c("Propia inscrita", "Arrendada", "De hecho", "Otra"))) %>%
              select(id_hog, n_fue_rie, fue_rie, pro_rie),
            by = "id_hog")

### Land tenure: regime of the plot with the largest area managed by the producer
caf07 <- caf07 %>%
  left_join(read_caf07("PREDIOS") %>%
              group_by(dir_folio) %>%
              filter(rank(-pre_manejo_propio) < 2) %>%
              ungroup() %>%
              distinct(dir_folio, .keep_all = TRUE),
            by = "dir_folio") %>%
  mutate(reg_ten = factor(lev_reg_ten[match(tenencia, c(1, 2, 4, 5, 3, 6, 7))], levels = lev_reg_ten)) %>%
  recode_tenencia()

### Capital: machinery and equipment (animal traction, mechanized and stationary)
# Columns are reordered so that each type of machinery forms a contiguous block of variables
maq <- read_caf07("MAQUINARIA") %>%
  select(-ends_with("comprada")) %>%
  relocate(dir_folio, aradosan_uso:sembradorasan_ajena, # Animal traction
           acondicionadorasme_uso:tractoresme90_ajena, # Mechanized
           autocargadorfo_uso, autocargadorfo_ajena, autoclavesfo_uso, autoclavesfo_ajena,
           cosechadorafo_uso, cosechadorafo_ajena, descortezadorafo_uso, descortezadorafo_ajena,
           desmalezadorasfo_uso, desmalezadorasfo_ajena, gruafo_uso, gruafo_ajena,
           motosierrasfo_uso, motosierrasfo_ajena, taladoraapifo_uso, taladoraapifo_ajena,
           taladoraautfo_uso, taladoraautfo_ajena, tractorcfo_uso, tractorcfo_ajena,
           tractorgfo_uso, tractorgfo_ajena, trineumáticofo_uso, trineumáticofo_ajena,
           lavadorases_uso:equipoomes_ajena, # Stationary
           aserraderosperfo_uso, aserraderosperfo_ajena, aserraderosporfo_uso, aserraderosporfo_ajena,
           astilladorafo_uso, astilladorafo_ajena, subsoladorasfo_uso, subsoladorasfo_ajena,
           torresfo_uso, torresfo_ajena, trituradorfo_uso, trituradorfo_ajena) %>%
  mutate(usa_maq_ani = rowSums(across(aradosan_uso:sembradorasan_ajena), na.rm = TRUE) > 0,
         usa_maq_mec = rowSums(across(acondicionadorasme_uso:trineumáticofo_ajena), na.rm = TRUE) > 0,
         usa_maq_fij = rowSums(across(lavadorases_uso:trituradorfo_ajena), na.rm = TRUE) > 0) %>%
  select(dir_folio:trituradorfo_ajena, starts_with("usa_")) %>%
  relocate(dir_folio, ends_with("uso"), ends_with("ajena"), starts_with("usa_"))

# Machinery is "Ajena" if the household uses machinery owned by others, and "Propia" otherwise
pro_maq <- function(usa, ajena, uso) {
  factor(case_when(rowSums(ajena, na.rm = TRUE) > 0 & usa ~ "Ajena",
                   rowSums(uso, na.rm = TRUE) > 0 & usa ~ "Propia"),
         levels = c("Propia", "Ajena"))
}
usa_maq <- function(usa) factor(if_else(usa, "Utiliza", "No utiliza"), levels = c("Utiliza", "No utiliza"))

maq <- maq %>%
  mutate(pro_maq_ani = pro_maq(usa_maq_ani, pick(aradosan_ajena:sembradorasan_ajena), pick(aradosan_uso:sembradorasan_uso)),
         pro_maq_mec = pro_maq(usa_maq_mec, pick(acondicionadorasme_ajena:trineumáticofo_ajena), pick(acondicionadorasme_uso:trineumáticofo_uso)),
         pro_maq_fij = pro_maq(usa_maq_fij, pick(lavadorases_ajena:trituradorfo_ajena), pick(lavadorases_uso:trituradorfo_uso)),
         n_maq_ani = if_else(pro_maq_ani %in% "Propia", rowSums(pick(aradosan_uso:sembradorasan_uso), na.rm = TRUE), 0),
         n_maq_mec = if_else(pro_maq_mec %in% "Propia", rowSums(pick(acondicionadorasme_uso:trineumáticofo_uso), na.rm = TRUE), 0),
         n_maq_fij = if_else(pro_maq_fij %in% "Propia", rowSums(pick(lavadorases_uso:trituradorfo_uso), na.rm = TRUE), 0),
         across(starts_with("usa_maq_"), usa_maq)) %>%
  select(dir_folio, ends_with("ani"), ends_with("mec"), ends_with("fij"))

caf07 <- left_join(caf07, maq, by = "dir_folio")
rm(maq)

### Insurance, credit, technical assistance and market links
otras <- read_caf07("OTRAS_CONSULTAS")

caf07 <- caf07 %>%
  left_join(otras %>%
              transmute(dir_folio,
                        segu = factor(case_when(seguroagricola == 0 ~ "No contrata seguros",
                                                seguroagricola == 1 ~ "Contrata seguros"),
                                      levels = c("Contrata seguros", "No contrata seguros")),
                        cred = factor(case_when(creditoindap == 1 | creditobancoe == 1 | creditootrob == 1 | lineacredito == 1 ~ "Con créditos",
                                                creditoindap == 0 & creditobancoe == 0 & creditootrob == 0 & lineacredito == 0 ~ "Sin créditos"),
                                      levels = c("Con créditos", "Sin créditos")),
                        asis = factor(case_when(asistenciai == 1 | gtt == 1 | instrumentoscorfo == 1 | fia == 1 ~ "Recibió asistencia técnica estatal",
                                                asistenciai == 0 & gtt == 0 & instrumentoscorfo == 0 & fia == 0 ~ "No recibió asistencia técnica estatal"),
                                      levels = c("Recibió asistencia técnica estatal", "No recibió asistencia técnica estatal")),
                        ocon_exporta, ocon_vende, ocon_contrato, # Exports, sells to agroindustry, contract farming
                        porcentaje), # Share of farm income in total income
            by = "dir_folio")

### Production lines: crops (main production line not available in CAF 2007)
caf07 <- caf07 %>%
  left_join(read_caf07("CULTIVO") %>%
              select(dir_folio, ends_with("secano"), ends_with("riego")) %>% # Makes secano and riego columns contiguous for the ranges below
              transmute(dir_folio,
                        sup_cer_sec = rowSums(pick(trigob_secano:alpiste_secano), na.rm = TRUE) + otroscereales_secano,
                        sup_cer_rie = rowSums(pick(trigob_riego:alpiste_riego), na.rm = TRUE) + otroscereales_riego,
                        sup_cer = sup_cer_sec + sup_cer_rie,
                        sup_leg_sec = rowSums(pick(papa_secano:chicharo_secano), na.rm = TRUE),
                        sup_leg_rie = rowSums(pick(papa_riego:chicharo_riego), na.rm = TRUE),
                        sup_leg = sup_leg_sec + sup_leg_rie,
                        sup_ind_sec = rowSums(pick(raps_secano:soya_secano), na.rm = TRUE) + otrosindustriales2_secano,
                        sup_ind_rie = rowSums(pick(raps_riego:soya_riego), na.rm = TRUE) + otrosindustriales2_riego,
                        sup_ind = sup_ind_sec + sup_ind_rie),
            by = "dir_folio") %>%
  mutate(sup_hor_rie = (tot_hortalizas_invernadero / 10000) + (tot_hortalizas_aire_libre - tot_hortalizas_no_regadas), # Greenhouses in m2
         sup_hor_sec = tot_hortalizas_no_regadas,
         sup_hor = sup_hor_rie + sup_hor_sec,
         sup_fru_rie = (tot_fru_formacion + tot_fru_produccion) - tot_fru_no_regadas,
         sup_fru_sec = tot_fru_no_regadas,
         sup_fru = sup_fru_rie + sup_fru_sec,
         sup_vin_rie = tot_vin_riego + tot_parron_riego,
         sup_vin_sec = tot_vin_secano,
         sup_vin = sup_vin_rie + sup_vin_sec,
         sup_flo_rie = (tot_flores_invernadero / 10000) + (tot_flores_aire_libre - tot_flores_no_regadas),
         sup_flo_sec = tot_flores_no_regadas,
         sup_flo = sup_flo_rie + sup_flo_sec,
         sup_sem_rie = tot_semillero - tot_semillero_no_regado,
         sup_sem_sec = tot_semillero_no_regado,
         sup_sem = sup_sem_rie + sup_sem_sec,
         # Nurseries (sup_viv) are not available; they are included in forestry
         sup_forr_rie = tot_forraj_anual_riego + tot_forraj_perm_riego,
         sup_forr_sec = tot_forraj_anual_secano + tot_forraj_perm_secano,
         sup_forr = sup_forr_rie + sup_forr_sec,
         sup_for_rie = tot_fore - tot_fore_no_regada,
         sup_for_sec = tot_fore_no_regada,
         sup_for = sup_for_rie + sup_for_sec,
         sup_bos_rie = 0,
         sup_bos_sec = tot_bos,
         sup_bos = tot_bos,
         sup_pra_mej_rie = NA,
         sup_pra_mej_sec = NA,
         sup_pra_mej = praderasme_superficie,
         sup_pra_nat = praderasna_superficie,
         # Share of irrigated area by production line
         pct_bos_rie = if_else(sup_bos_sec > 0, 0, NA),
         pct_for_rie = if_else(sup_for_sec > 0, 0, NA),
         pct_cer_rie = pct_rie(sup_cer_rie, sup_cer_sec),
         pct_flo_rie = pct_rie(sup_flo_rie, sup_flo_sec),
         pct_forr_rie = pct_rie(sup_forr_rie, sup_forr_sec),
         pct_fru_rie = pct_rie(sup_fru_rie, sup_fru_sec),
         pct_hor_rie = pct_rie(sup_hor_rie, sup_hor_sec),
         pct_ind_rie = pct_rie(sup_ind_rie, sup_ind_sec),
         pct_leg_rie = pct_rie(sup_leg_rie, sup_leg_sec),
         pct_pra_mej_rie = pct_rie(sup_pra_mej_rie, sup_pra_mej_sec),
         pct_sem_rie = pct_rie(sup_sem_rie, sup_sem_sec),
         pct_vin_rie = pct_rie(sup_vin_rie, sup_vin_sec)) %>%
  clasificar_rubros(umbrales_agr[c("hor", "flo", "sem", "ind", "vin", "leg", "fru", "cer", "forr", "for", "bos", "pra_mej", "pra_nat")], "sup_")

### Production lines: livestock and poultry
caf07 <- caf07 %>%
  mutate(gan_bov = tot_gan_bovinos,
         gan_ovi = tot_gan_ovinos,
         gan_cap = tot_gan_caprinos) %>%
  left_join(read_caf07("GANADERIA") %>%
              transmute(dir_folio,
                        gan_por = cerdos,
                        gan_equ = caballares,
                        gan_con = conejos,
                        gan_gal = gallosp,
                        gan_pav = pavos,
                        gan_pat = patos,
                        gan_gan = gansos,
                        gan_col = colmenasm + colmenasr,
                        gan_ani_may = alpacas + llamas + mulares + asnales + jabalies + ciervos + huarizos,
                        gan_oav = avestruces + emues + codornices + faisanes),
            by = "dir_folio") %>%
  clasificar_rubros(umbrales_gan, "gan_")

### Dummies and number of production lines (self-consumption or commercial)
caf07 <- caf07 %>%
  add_rubro_dummies(paste0("sup_", c("hor", "flo", "sem", "ind", "vin", "leg", "fru", "cer", "forr", "for", "pra_mej"), "_cat"), "agr") %>%
  add_rubro_dummies(paste0("gan_", names(umbrales_gan), "_cat"), "gan")

### Production lines: post-productive activities (destination not recorded, counted as self-consumption)
caf07 <- caf07 %>%
  left_join(otras %>%
              transmute(dir_folio,
                        rub_agr = case_when(agroturismo == 0 ~ "Sin producción",
                                            agroturismo == 1 ~ "Autoconsumo o comercialización"),
                        rub_len = if_else(coalesce(produccionc == 1 | produccionl == 1 | produccionrollizosp == 1 | produccionrollizosa == 1 | produccionmaderaa == 1, FALSE),
                                          "Autoconsumo o comercialización", "Sin producción"),
                        rub_otr = if_else(coalesce(lombricultura == 1 | acuicultura == 1 | helicicultura == 1 | produccionm == 1 | cultivohongos == 1 | polinizacion == 1, FALSE),
                                          "Autoconsumo o comercialización", "Sin producción")) %>%
              mutate(across(c(rub_agr, rub_len, rub_otr), ~ case_when(.x == "Autoconsumo o comercialización" ~ 1,
                                                                      .x == "Sin producción" ~ 0),
                            .names = "{.col}_aut"),
                     n_rub_pos_aut = rowSums(pick(ends_with("_aut")), na.rm = TRUE)),
            by = "dir_folio") %>%
  mutate(n_rub_aut = n_rub_agr_aut + n_rub_gan_aut + n_rub_pos_aut,
         n_rub_com = n_rub_agr_com + n_rub_gan_com) # Post-productive activities counted as self-consumption (conservative)

rm(otras)

### Sales
# Main destination of production and farm income are not available. Available: exports, sales to agroindustry and
# contract farming (ocon_exporta, ocon_vende, ocon_contrato), and gross value of production (vbp_uf, vbp_cat)
caf07 <- caf07 %>%
  mutate(pct_ing_pre = recode_pct_ing_pre(porcentaje))


# MEMBERS: HOUSEHOLD VARIABLES -------------------------------------------------

caf07_mie <- caf07_mie %>%
  left_join(select(caf07, id_hog, pue = dir_comunidad_indiijena_explotacion, hog_afc_est, n_mie_rec), by = "id_hog")



# SAVING ------------------------------------------------------------------

saveRDS(caf07, "output/caf07.rds")
saveRDS(caf07_mie, "output/caf07_mie.rds")
