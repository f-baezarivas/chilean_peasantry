### Matched natural-person units (caf21, Extendido) vs. their source datasets (caf21_ce g1 == 1, caf21_hog)
suppressMessages({library(dplyr); library(tidyr); library(purrr)})

ce  <- readRDS("output/caf21_ce.rds") %>% filter(g1 == 1)
hog <- readRDS("output/caf21_hog.rds")
emp <- readRDS("output/caf21.rds") %>% filter(cues == "Extendido")

lab_afc <- function(x) factor(if_else(x == 1, "Campesinos", "No campesinos"), levels = c("Campesinos", "No campesinos"))

g3_grp <- function(g3) case_when(g3 == -77 ~ "No aplica (-77)",
                                 g3 %in% 1:25 ~ "Agrícola (1-25)",
                                 g3 %in% 26:35 ~ "Pecuaria (26-35)",
                                 g3 %in% 36:43 ~ "Apoyo y forestal (36-43)",
                                 g3 == 44 ~ "No tiene (44)",
                                 g3 == 45 ~ "No responde (45)")

prep_ce <- function(df) df %>%
  transmute(grupo = lab_afc(hog_afc_est), sup_tot, sup_apr, sup_cul, sup_pra, sup_bos, sup_bar,
            sup_noa = us61_16 + us61_17 + us61_18, ga161, ga172,
            ven = coalesce(as.character(ven_pre), "Sin dato"), g3 = g3_grp(g3))

# Household side: classification from the household questionnaire in both sources
prep_hog <- function(df, afc) df %>%
  transmute(grupo = lab_afc({{ afc }}), n_mie, sex_jh, edad_jh,
            nedu = coalesce(as.character(nedu_jh), "Sin dato"))

stats_ce <- function(d) {
  num <- tibble(
    var = c("N UPA", "Sup. total, media (ha)", "Sup. total, mediana (ha)", "Sup. aprovechable, media (ha)", "Sup. aprovechable, mediana (ha)",
            "Cultivos, media (ha)", "Praderas (forr.+mej.+nat.), media (ha)", "Plantaciones y bosque nativo, media (ha)",
            "Barbecho, media (ha)", "No aprovechable, media (ha)",
            "Bovinos, cabezas media", "Bovinos, % UPA con", "Ovinos, cabezas media", "Ovinos, % UPA con"),
    val = c(nrow(d), mean(d$sup_tot), median(d$sup_tot), mean(d$sup_apr), median(d$sup_apr),
            mean(d$sup_cul), mean(d$sup_pra), mean(d$sup_bos), mean(d$sup_bar), mean(d$sup_noa),
            mean(d$ga161), 100 * mean(d$ga161 > 0), mean(d$ga172), 100 * mean(d$ga172 > 0)))
  ven <- d %>% count(var = paste0("Ventas ", ven)) %>% mutate(val = 100 * n / sum(n)) %>% select(-n)
  g3  <- d %>% count(var = paste0("g3: ", g3)) %>% mutate(val = 100 * n / sum(n)) %>% select(-n)
  bind_rows(num, ven, g3)
}

stats_hog <- function(d) {
  num <- tibble(var = c("N hogares", "Tamaño del hogar, media", "Tamaño del hogar, mediana", "Productor mujer, %",
                        "Edad productor, media", "Edad productor, mediana"),
                val = c(nrow(d), mean(d$n_mie, na.rm = TRUE), median(d$n_mie, na.rm = TRUE),
                        100 * mean(d$sex_jh == "Femenino", na.rm = TRUE),
                        mean(d$edad_jh, na.rm = TRUE), median(d$edad_jh, na.rm = TRUE)))
  edu <- d %>% count(var = paste0("Educ. productor: ", nedu)) %>% mutate(val = 100 * n / sum(n)) %>% select(-n)
  bind_rows(num, edu)
}

comparar <- function(a, b, f) {
  bind_rows(a %>% group_by(grupo) %>% group_modify(~ f(.x)) %>% mutate(fuente = "Emparejadas"),
            b %>% group_by(grupo) %>% group_modify(~ f(.x)) %>% mutate(fuente = "Origen")) %>%
    ungroup() %>%
    pivot_wider(names_from = fuente, values_from = val, values_fill = 0) %>%
    mutate(Dif = Emparejadas - Origen)
}

tab_ce  <- comparar(prep_ce(emp), prep_ce(ce), stats_ce)
tab_hog <- comparar(prep_hog(emp, hog_afc_est_hp), prep_hog(hog, hog_afc_est), stats_hog)

fmt <- function(tab) {
  orden <- unique(tab$var)
  tab %>%
    mutate(across(c(Emparejadas, Origen, Dif), ~ formatC(.x, format = "f", digits = 1, big.mark = ".", decimal.mark = ","))) %>%
    pivot_wider(names_from = grupo, values_from = c(Emparejadas, Origen, Dif), names_glue = "{grupo}|{.value}") %>%
    select(var, starts_with("Campesinos"), starts_with("No campesinos")) %>%
    arrange(match(var, orden))
}
out <- list(ce = fmt(tab_ce), hog = fmt(tab_hog))

### Matched share by region
reg_nom <- c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", "O'Higgins", "Maule", "Biobío", "Araucanía",
             "Los Lagos", "Aysén", "Magallanes", "Metropolitana", "Los Ríos", "Arica y Parinacota", "Ñuble")
reg_orden <- c(15, 1:5, 13, 6, 7, 16, 8, 9, 14, 10:12)
pct_emp <- function(num, den) {
  full_join(count(num, reg, grupo, name = "e"), count(den, reg, grupo, name = "o"), by = c("reg", "grupo")) %>%
    bind_rows(full_join(count(num, reg, name = "e"), count(den, reg, name = "o"), by = "reg") %>% mutate(grupo = "Total")) %>%
    mutate(pct = 100 * coalesce(e, 0L) / o)
}
reg_ce  <- pct_emp(emp %>% transmute(reg = com %/% 1000, grupo = lab_afc(hog_afc_est)),
                   ce  %>% transmute(reg = com %/% 1000, grupo = lab_afc(hog_afc_est))) %>% mutate(base = "UPA")
reg_hog <- pct_emp(emp %>% transmute(reg, grupo = lab_afc(hog_afc_est_hp)),
                   hog %>% transmute(reg, grupo = lab_afc(hog_afc_est))) %>% mutate(base = "Hogares")
out$reg <- bind_rows(reg_ce, reg_hog) %>%
  mutate(col = paste(base, grupo, sep = "|"), reg = factor(reg_nom[reg], levels = reg_nom[reg_orden])) %>%
  select(reg, col, pct, o) %>%
  bind_rows(bind_rows(reg_ce, reg_hog) %>% group_by(base, grupo) %>%
              summarise(pct = 100 * sum(coalesce(e, 0L)) / sum(o), o = sum(o), .groups = "drop") %>%
              transmute(reg = "Total", col = paste(base, grupo, sep = "|"), pct, o)) %>%
  mutate(reg = factor(reg, levels = c(reg_nom[reg_orden], "Total"))) %>%
  arrange(reg)

saveRDS(out, "output/comparacion_emparejadas.rds")
options(width = 250)
print(as.data.frame(out$ce)); print(as.data.frame(out$hog))
print(as.data.frame(out$reg %>% select(-o) %>% pivot_wider(names_from = col, values_from = pct) %>%
                      mutate(across(-reg, ~ round(.x, 1)))))
print(as.data.frame(out$reg %>% select(-pct) %>% pivot_wider(names_from = col, values_from = o)))
