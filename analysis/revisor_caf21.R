### CAF 2021: CHECKS REQUESTED BY THE REVIEWER ###
# 1. Plots and municipalities per productive unit (section 8)
# 2. Possible duplicates between productive units and households (household members)
# 3. 2021 universe harmonized with the CAF 2007 minimums
# 4. Sensitivity of the 2007 characterization to the 2021 extended-questionnaire criterion
# Uses the output files of data_process/ and the CAF 2021 .csv files cached in input/caf21.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, readr, purrr)

caf21_pre <- readRDS("output/caf21_pre.rds")
caf21_hog <- readRDS("output/caf21_hog.rds")
caf21_mie <- readRDS("output/caf21_mie.rds")
caf07 <- readRDS("output/caf07.rds")

# Reads a productive-questionnaire file of CAF 2021 (UTF-8, decimal comma), as in data_process/04_caf21.R
read_caf21 <- function(file) {
  read_delim(file.path("input/caf21", paste0(file, ".csv")), delim = ";", locale = locale(decimal_mark = ","),
             trim_ws = TRUE, show_col_types = FALSE, name_repair = "minimal") %>%
    select(which(names(.) != "")) %>%
    rename_with(tolower)
}

pct <- function(x) round(100 * mean(x), 1)

# Groups of productive units: peasant and non-peasant (extended questionnaire) and peasant (abbreviated)
grupos <- caf21_pre %>%
  transmute(guid = guid_ce, cues, hog_afc_est,
            grupo = case_when(cues == "Extendido" & hog_afc_est == 1 ~ "UPA campesinas (extendido)",
                              cues == "Extendido" & hog_afc_est == 0 ~ "UPA no campesinas (extendido)",
                              cues == "Abreviado" & hog_afc_est == 1 ~ "UAC campesinas (abreviado)",
                              cues == "Abreviado" & hog_afc_est == 0 ~ "UAC sin producción (abreviado)"))


# 1. PLOTS AND MUNICIPALITIES PER PRODUCTIVE UNIT ------------------------------
# Section 8 has one row per plot and municipality: a plot (predio, within an establishment) that extends over
# several municipalities appears once per municipality. It covers both questionnaires (GUID CE... and CA...).

sec8 <- read_caf21("seccion_8")

predios <- sec8 %>%
  group_by(guid) %>%
  summarise(n_pre = n_distinct(establecimiento, predio),
            n_com = n_distinct(cut_comuna)) %>%
  inner_join(grupos, by = "guid")

res_predios <- predios %>%
  filter(grupo != "UAC sin producción (abreviado)") %>%
  group_by(grupo) %>%
  summarise(n = n(),
            pct_un_predio = pct(n_pre == 1),
            pct_dos_o_mas = pct(n_pre >= 2),
            pct_mas_de_una_comuna = pct(n_com >= 2))

print(res_predios)

rm(sec8)


# 2. POSSIBLE DUPLICATES BETWEEN PRODUCTIVE UNITS AND HOUSEHOLDS ----------------
# Pairs of households (caf21_mie) with (a) two or more members, (b) the same municipality, (c) the same
# composition (multiset of sex and age of all members) and (d) a different producer (sex and age of the producer
# differ). Members with missing sex or age enter the composition with an "NA" value. This is an upper bound of
# duplicates: identical compositions can also be different households.

composicion <- caf21_mie %>%
  group_by(id_hog) %>%
  summarise(n_mie = n(),
            comp = paste(sort(paste(hp262, hp263, sep = "-")), collapse = "|"),
            productor = paste(hp262[hp261 == 1], hp263[hp261 == 1], sep = "-", collapse = "|")) %>%
  left_join(select(caf21_hog, id_hog, com, hog_afc_est), by = "id_hog") %>%
  filter(n_mie >= 2) %>% # (a)
  group_by(com, comp) %>% # (b) and (c)
  mutate(n_grupo = n(),
         n_productores = n_distinct(productor)) %>%
  ungroup()

# Pairs: all pairs within each group meet (a)-(c); those with the same producer are subtracted for (d)
pares <- composicion %>%
  filter(n_grupo >= 2) %>%
  count(com, comp, productor, name = "m") %>%
  group_by(com, comp) %>%
  summarise(n = sum(m), pares_iguales = sum(choose(m, 2)), .groups = "drop") %>%
  summarise(pares_abc = sum(choose(n, 2)),
            pares_abcd = sum(choose(n, 2) - pares_iguales))

# Households involved: in a group with at least two different producers, every household has a pair meeting (d)
n_hog <- nrow(caf21_hog)
n_hog_afc <- sum(caf21_hog$hog_afc_est == 1)

res_duplicados <- composicion %>%
  summarise(hog_abc = sum(n_grupo >= 2),
            hog_abcd = sum(n_productores >= 2),
            hog_abcd_afc = sum(n_productores >= 2 & hog_afc_est == 1)) %>%
  mutate(pct_hog_abcd = round(100 * hog_abcd / n_hog, 2), # Over the 129,607 households
         pct_hog_abcd_afc = round(100 * hog_abcd_afc / n_hog, 2),
         pct_hog_abcd_afc_sobre_afc = round(100 * hog_abcd_afc / n_hog_afc, 2)) %>% # Over peasant households
  bind_cols(pares)

print(as.data.frame(res_duplicados))


# 3. 2021 UNIVERSE HARMONIZED WITH THE CAF 2007 MINIMUMS ----------------------
# A unit is "censable in 2007" if it meets at least one minimum (Manual del empadronador CAF 2007, p. 20) or
# produces in two or more production lines (p. 21). Mapping of each line to CAF 2021 variables:
#   - 0.1 ha in annual crops, vegetables, fodder crops, vineyards or fruit trees: sum of us61_1 (cereals),
#     us61_2 (legumes and tubers), us61_3 (industrial crops), us61_4 (vegetables), us61_5 (fruit trees),
#     us61_6 (vineyards), us61_7 (flowers), us61_8 (seed crops), us61_9 (nurseries) and us61_10 (fodder crops).
#     Flowers, seed crops and nurseries are counted as annual crops
#   - Any area in greenhouses: protected area of vegetables (ss86, section 9) and flowers (ss113), any type of cover
#     (greenhouse, tunnel, roof, anti-aphid mesh, growth chamber). Extended questionnaire only
#   - 0.5 ha in natural grasslands, forest plantations or native forest: sum of us61_14 (natural grasslands),
#     us61_13 (improved grasslands), us61_11 (plantations) and us61_12 (native forest)
#   - 2 large animals: cattle (ga161), horses (ga201), mules and donkeys (ga202, ga203), camelids (ga204, ga205,
#     ga206) and deer (ga208). Age is not recorded for every species, so the total number of heads is used
#   - 10 small animals: sheep (ga172), pigs (ga182), goats (ga180) and wild boars (ga209)
#   - 50 geese, turkeys or rabbits: ga188, ga186 and ga207
#   - 100 hens, chickens, ducks or quails: ga185, ga187 and ga190
#   - 10 ostriches or emus: ga189 and ga191
#   - 10 beehives: ga199
#   Codes ga189-ga193 and ga202-ga209 were identified from the order of the CAF 2021 questionnaire and the size of
#   their totals; ga192 and ga193 (other birds) are not used.
#   Each threshold applies to the sum of the lines in its group.
#   - Two or more production lines: n_rub_agr_aut + n_rub_gan_aut >= 2 (crops and livestock, as in caf21_pre)
# The abbreviated questionnaire records land use (us61_*) but only the presence of each species (ga152_*), not the
# number of heads, so its livestock minimums cannot be checked: units with livestock only enter through the rule
# of two or more lines.

bajo_cubierta <- bind_rows(read_caf21("seccion_9_hortalizas") %>% transmute(guid, sup = ss86),
                           read_caf21("seccion_9_flores") %>% transmute(guid, sup = ss113)) %>%
  filter(sup > 0) %>%
  distinct(guid) %>%
  transmute(guid_ce = guid, invernadero = TRUE)

ga_vars <- c("ga161", "ga172", "ga180", "ga182", "ga185", "ga186", "ga187", "ga188", "ga189", "ga190", "ga191",
             "ga199", paste0("ga", c(201:209)))

censable <- caf21_pre %>%
  left_join(bajo_cubierta, by = "guid_ce") %>%
  mutate(across(any_of(ga_vars), ~ coalesce(.x, 0)),
         invernadero = coalesce(invernadero, FALSE),
         min_cultivos = (us61_1 + us61_2 + us61_3 + us61_4 + us61_5 + us61_6 + us61_7 + us61_8 + us61_9 + us61_10) >= 0.1,
         min_bosque = (us61_11 + us61_12 + us61_13 + us61_14) >= 0.5,
         min_mayores = (ga161 + ga201 + ga202 + ga203 + ga204 + ga205 + ga206 + ga208) >= 2,
         min_menores = (ga172 + ga182 + ga180 + ga209) >= 10,
         min_gansos = (ga188 + ga186 + ga207) >= 50,
         min_gallinas = (ga185 + ga187 + ga190) >= 100,
         min_avestruces = (ga189 + ga191) >= 10,
         min_colmenas = ga199 >= 10,
         dos_rubros = (n_rub_agr_aut + n_rub_gan_aut) >= 2,
         censable_07 = coalesce(min_cultivos | invernadero | min_bosque | min_mayores | min_menores | min_gansos |
                                  min_gallinas | min_avestruces | min_colmenas | dos_rubros, FALSE))

n_afc_07 <- sum(caf07$hog_afc_est == 1) # 220,038

res_universo <- censable %>%
  filter(hog_afc_est == 1) %>%
  group_by(cues) %>%
  summarise(n_campesinas = n(),
            n_censables = sum(censable_07),
            n_fuera = sum(!censable_07)) %>%
  bind_rows(summarise(., cues = "Total", across(where(is.numeric), sum))) %>%
  mutate(pct_fuera = round(100 * n_fuera / n_campesinas, 1),
         caida_vs_2007 = if_else(cues == "Total", n_afc_07 - n_censables, NA),
         pct_caida_vs_2007 = if_else(cues == "Total", round(100 * (n_afc_07 - n_censables) / n_afc_07, 1), NA))

print(res_universo)

# Minimums met by peasant units (% of units), to see which criteria drive the result
print(censable %>%
        filter(hog_afc_est == 1) %>%
        group_by(cues) %>%
        summarise(across(c(min_cultivos, invernadero, min_bosque, min_mayores, min_menores, min_gansos, min_gallinas,
                           min_avestruces, min_colmenas, dos_rubros), pct)) %>%
        pivot_longer(-cues, names_to = "criterio", values_to = "pct") %>%
        pivot_wider(names_from = cues, values_from = pct))


# 4. SENSITIVITY OF THE 2007 CHARACTERIZATION ----------------------------------
# Peasant households of 2007 that would have answered the extended questionnaire in 2021: sup_tot >= 2 or at least
# one production line with commercial destination according to the cut-off points (n_rub_com >= 1; crops and
# livestock, umbrales_agr and umbrales_gan in helpers/caf.R).

caf07_afc <- caf07 %>%
  filter(hog_afc_est == 1) %>%
  mutate(extendido = coalesce(sup_tot >= 2, FALSE) | coalesce(n_rub_agr_com + n_rub_gan_com >= 1, FALSE))

resumen_07 <- function(df) {
  df %>%
    summarise(n = n(),
              n_mie = mean(n_mie, na.rm = TRUE),
              edad_jh = mean(edad_jh, na.rm = TRUE),
              pct_mujer_jh = round(100 * mean(sex_jh == "Femenino", na.rm = TRUE), 1),
              sup_tot_media = mean(sup_tot, na.rm = TRUE),
              sup_tot_mediana = median(sup_tot, na.rm = TRUE),
              n_rub_aut = mean(n_rub_aut, na.rm = TRUE),
              pct_ing_pre_0_25 = round(100 * mean(pct_ing_pre == "0% - 25%", na.rm = TRUE), 1),
              pct_ing_pre_25_50 = round(100 * mean(pct_ing_pre == "25% - 50%", na.rm = TRUE), 1),
              pct_ing_pre_50_75 = round(100 * mean(pct_ing_pre == "50% - 75%", na.rm = TRUE), 1),
              pct_ing_pre_75_100 = round(100 * mean(pct_ing_pre == "75% - 100%", na.rm = TRUE), 1))
}

res_07 <- bind_rows(caf07_afc %>% resumen_07() %>% mutate(grupo = "Todos los hogares campesinos"),
                    caf07_afc %>% filter(extendido) %>% resumen_07() %>% mutate(grupo = "Marcados (cuestionario extendido)"),
                    caf07_afc %>% filter(!extendido) %>% resumen_07() %>% mutate(grupo = "No marcados")) %>%
  relocate(grupo) %>%
  mutate(across(where(is.double), ~ round(.x, 2)))

print(as.data.frame(t(data.frame(res_07, row.names = "grupo"))))
