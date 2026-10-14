### HOUSEHOLD STRUCTURE HELPERS ###
# Recodes shared by the CASEN and CAF processing scripts.

library(dplyr)

recode_edad_rec <- function(edad) {
  factor(case_when(edad < 15 ~ "Menor",
                   edad >= 15 & edad < 65 ~ "Adulto",
                   edad >= 65 ~ "Anciano"),
         levels = c("Menor", "Adulto", "Anciano"))
}

recode_edad_cat <- function(edad) {
  factor(case_when(edad >= 15 & edad <= 29 ~ "15-29",
                   edad >= 30 & edad <= 44 ~ "30-44",
                   edad >= 45 & edad <= 59 ~ "45-59",
                   edad >= 60 ~ "60 y más"))
}

# Counts recoded as "0 x", "1 x", "2 x", "3 o más x"
recode_n <- function(n, sing, plur) {
  lev <- c(paste("0", plur), paste("1", sing), paste("2", plur), paste("3 o más", plur))
  factor(case_when(n == 0 ~ lev[1],
                   n == 1 ~ lev[2],
                   n == 2 ~ lev[3],
                   n >= 3 ~ lev[4]),
         levels = lev)
}

# Demographic and occupational structure of the household.
# Requires n_mie, n_menores, n_adultos, n_ancianos, n_ocupados, n_desocupados and n_inactivos.
add_estructura_hogar <- function(hog) {
  hog %>%
    mutate(n_mie_rec = factor(case_when(n_mie == 1 ~ "Unipersonal",
                                        n_mie == 2 ~ "Bipersonal",
                                        n_mie == 3 ~ "Pluripersonal: 3 personas",
                                        n_mie == 4 ~ "Pluripersonal: 4 personas",
                                        n_mie >= 5 ~ "Ampliado: 5 o más personas"),
                              levels = c("Unipersonal", "Bipersonal", "Pluripersonal: 3 personas",
                                         "Pluripersonal: 4 personas", "Ampliado: 5 o más personas")),
           n_menores_rec = recode_n(n_menores, "menor", "menores"),
           n_adultos_rec = recode_n(n_adultos, "adulto", "adultos"),
           n_ancianos_rec = recode_n(n_ancianos, "anciano", "ancianos"),
           n_dependientes = n_menores + n_ancianos,
           n_dependientes_rec = recode_n(n_dependientes, "dependiente", "dependientes"),
           n_pet = n_adultos + n_ancianos,
           n_pet_rec = recode_n(n_pet, "miembro en edad de trabajar", "miembros en edad de trabajar"),
           dep_dem = 100 * (n_dependientes / n_adultos),
           dep_dem_rec = factor(case_when(dep_dem <= 50 ~ "Activo",
                                          dep_dem > 50 & dep_dem <= 100 ~ "Compensado",
                                          dep_dem > 100 ~ "Dependiente"),
                                levels = c("Activo", "Compensado", "Dependiente")),
           n_ocupados_rec = recode_n(n_ocupados, "miembro ocupado", "miembros ocupados"),
           n_inactivos_rec = recode_n(n_inactivos, "miembro inactivo", "miembros inactivos"),
           par_lab = 100 * ((n_ocupados + n_desocupados) / (n_ocupados + n_desocupados + n_inactivos)),
           dep_lab = 100 * (n_ocupados / (n_mie - n_ocupados)),
           dep_lab_rec = factor(case_when(dep_lab <= 50 ~ "Alta",
                                          dep_lab > 50 & dep_lab <= 100 ~ "Media",
                                          dep_lab > 100 ~ "Baja"),
                                levels = c("Baja", "Media", "Alta")),
           jor_fam_dis = n_adultos * 260) # 260 working days per year
}
