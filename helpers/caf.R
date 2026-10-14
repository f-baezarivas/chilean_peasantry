### CAF HELPERS ###
# Functions shared by the CAF 2007 and CAF 2021 processing scripts.

library(dplyr)

### Production lines (rubros): self-consumption or commercial production
# Thresholds (hectares for crops, heads for livestock) up to which production is considered self-consumption,
# based on self-consumption units and the CAF 2007 cut-off points.
umbrales_agr <- c(hor = 0.25, flo = 0.25, sem = 0.25, viv = 0.25,
                  ind = 0.5, vin = 0.5, leg = 0.5, fru = 0.5,
                  cer = 1, forr = 1, "for" = 1, bos = 1, pra_mej = 1,
                  pra_nat = 2)
umbrales_gan <- c(bov = 10, equ = 10, ani_may = 10,
                  ovi = 20, cap = 20, por = 20, col = 20, oav = 20, con = 20, pav = 20, pat = 20, gan = 20,
                  gal = 50)

lev_rubro <- c("Sin producción", "Autoconsumo", "Comercialización")

cat_rubro <- function(x, umbral) {
  factor(case_when(x == 0 ~ "Sin producción",
                   x > 0 & x <= umbral ~ "Autoconsumo",
                   x > umbral ~ "Comercialización"),
         levels = lev_rubro)
}

# Adds {prefijo}{rubro}_cat for each rubro in umbrales (e.g. sup_hor -> sup_hor_cat)
clasificar_rubros <- function(df, umbrales, prefijo) {
  for (r in names(umbrales)) df[[paste0(prefijo, r, "_cat")]] <- cat_rubro(df[[paste0(prefijo, r)]], umbrales[[r]])
  df
}

# Adds dummies for each classified rubro ({cat}_aut: any production; {cat}_com: commercial production)
# and their totals n_rub_{grupo}_aut and n_rub_{grupo}_com
add_rubro_dummies <- function(df, cats, grupo) {
  for (v in cats) {
    df[[paste0(v, "_aut")]] <- case_when(df[[v]] %in% c("Autoconsumo", "Comercialización") ~ 1,
                                         df[[v]] == "Sin producción" ~ 0)
    df[[paste0(v, "_com")]] <- case_when(df[[v]] == "Comercialización" ~ 1,
                                         df[[v]] %in% c("Autoconsumo", "Sin producción") ~ 0)
  }
  df[[paste0("n_rub_", grupo, "_aut")]] <- rowSums(df[paste0(cats, "_aut")], na.rm = TRUE)
  df[[paste0("n_rub_", grupo, "_com")]] <- rowSums(df[paste0(cats, "_com")], na.rm = TRUE)
  df
}

### Land tenure regime
lev_reg_ten <- c("Propiedad inscrita", "Propiedad no inscrita", "Arriendo", "Mediería", "Goce o regalía", "Cesión", "Ocupación")

recode_tenencia <- function(df) {
  df %>%
    mutate(for_ten = factor(case_when(reg_ten %in% c("Propiedad inscrita", "Arriendo") ~ "Tenencia formal",
                                      reg_ten %in% lev_reg_ten[-c(1, 3)] ~ "Tenencia precaria"),
                            levels = c("Tenencia formal", "Tenencia precaria")),
           pro_ten = factor(case_when(reg_ten %in% c("Propiedad inscrita", "Propiedad no inscrita") ~ "Propietario",
                                      reg_ten %in% lev_reg_ten[-(1:2)] ~ "No propietario"),
                            levels = c("Propietario", "No propietario")))
}

### Share of farm income in total household income
recode_pct_ing_pre <- function(x) {
  factor(case_when(x == 1 ~ "75% - 100%",
                   x == 2 ~ "50% - 75%",
                   x == 3 ~ "25% - 50%",
                   x == 4 ~ "0% - 25%"),
         levels = c("0% - 25%", "25% - 50%", "50% - 75%", "75% - 100%"))
}

### Municipality percentile of farm land (only for peasant households)
add_sup_pct <- function(df) {
  df %>%
    group_by(cod_com_0721) %>%
    mutate(sup_tot_pct = if_else(hog_afc_est == 1, 100 * ecdf(sup_tot)(sup_tot), NA),
           sup_apr_pct = if_else(hog_afc_est == 1, 100 * ecdf(sup_apr)(sup_apr), NA)) %>%
    ungroup()
}

### Share of irrigated area
pct_rie <- function(rie, sec) rie * 100 / (rie + sec)
