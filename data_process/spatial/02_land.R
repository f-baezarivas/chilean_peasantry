### LAND USE ###
# Imports the land-use layers by region from IDE Minagri, based on the Catastro y Actualización de los Recursos
# Vegetacionales y Uso de la Tierra (CONAF). Keeps agricultural land and grasslands, harmonizes the regional
# layers and combines them into one national layer, in UTM 19S (EPSG 32719). Exported to output/ as a GeoPackage
# (land_gpkg). Building it takes time: it is built once and then read from output/.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, sf, archive)

### Land use source files
# One .rar file per region, downloaded from IDE Minagri (https://ide.minagri.gob.cl/descarga-de-capas-shp/planificacion-catastral/)
# and extracted into land_dir (about 9.5 GB compressed). Only continental regions: the island layers (Isla de
# Pascua, Robinson Crusoe and Santa Clara, Alejandro Selkirk) are excluded. Each region has a different year.
land_dir <- "input/land"
land_gpkg <- "output/land_use.gpkg"
land_url <- "https://ide.minagri.gob.cl/wp-content/uploads/DESCARGAS/CAPAS/PLANIFICACION_CATASTRAL/CATASTRO_CONAF"
land_files <- tibble::tribble(
  ~reg, ~year,       ~file,
  15,   "2015",      "15.Regin-de-Arica-y-Parinacota",
  1,    "2016",      "catastro_rv_r01_2016_v2",
  2,    "2019",      "CBN_REG02_2019",
  3,    "2018",      "03.Region-de-Atacama",
  4,    "2014",      "04.Regin-de-Coquimbo",
  5,    "2019",      "cbn_05reg_2019_v2",
  13,   "2019",      "cbn_rm_2019_21feb24",
  6,    "2020",      "cbn_06reg_2020",
  7,    "2024",      "cbn_07reg_2024",
  16,   "2024",      "cbn_16reg_2024",
  8,    "2024",      "cbn_08reg_2024",
  9,    "2024",      "cbn_09reg_2024",
  14,   "2024",      "cbn_14reg_2024",
  10,   "2018",      "cbn_10reg_2018",
  11,   "2020-2022", "cbn_r11reg_2020_2022",
  12,   "2017-2019", "cbn_2017_2019_r12")

# Returns the shapefile of a region, downloading and extracting it into land_dir the first time
land_shp <- function(file) {
  dir <- file.path(land_dir, file)
  if (!dir.exists(dir)) {
    dir.create(land_dir, showWarnings = FALSE, recursive = TRUE)
    rar <- file.path(tempdir(), paste0(file, ".rar"))
    options(timeout = max(3600, getOption("timeout")))
    download.file(paste0(land_url, "/", file, ".rar"), rar, mode = "wb")
    archive_extract(rar, dir = paste0(dir, ".part"), files = which(archive(rar)$size > 0)) # Skips directory entries
    file.rename(paste0(dir, ".part"), dir)
    unlink(rar)
  }
  shp <- list.files(dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
  stopifnot(length(shp) == 1)
  shp
}

# Reverts text encoded twice as UTF-8 (e.g. "AgrÃ­cola" in regions 11 and 12)
fix_utf8 <- function(x) {
  y <- iconv(x, from = "UTF-8", to = "latin1")
  Encoding(y) <- "UTF-8"
  ifelse(grepl("\u00c3", x) & !is.na(y) & validUTF8(y), y, x)
}

# Reads the agricultural land and grasslands of a regional layer, with the same variables in all regions.
# The land-use codes are the same in all regions: id_uso 2 is agricultural land (id_subuso 1 farmland,
# 2 crop-grassland rotation), and id_uso 3, id_subuso 1, id_estruc 3 and 4 are annual and perennial grasslands.
land_read <- function(shp) {
  layer <- st_layers(shp)$name
  st_read(shp, quiet = TRUE, query = paste0(
    "SELECT * FROM \"", layer, "\" WHERE CAST(id_uso AS integer) = 2 OR (CAST(id_uso AS integer) = 3 AND ",
    "CAST(id_subuso AS integer) = 1 AND CAST(id_estruc AS integer) IN (3, 4))")) %>%
    rename_with(tolower) %>%
    rename(any_of(c(codcom = "codcomun", uso_tierra = "uso_tierr"))) %>% # Region 8 (2015) and region 9 (2017)
    select(codreg, codcom, id_uso, id_subuso, id_estruc, uso, subuso, uso_tierra, estructura, superf_ha) %>%
    mutate(across(codreg:id_estruc, as.integer),
           across(uso:estructura, ~ trimws(fix_utf8(.x))),
           superf_ha = as.numeric(superf_ha),
           source = tools::file_path_sans_ext(basename(shp))) %>%
    st_zm() %>% # Regions 3 and 15 are XYZ
    st_transform(32719) %>% # Some regions are in UTM 18S
    st_cast("MULTIPOLYGON")
}

### National land-use layer
# Built from the regional shapefiles only when land_gpkg does not exist yet; otherwise uso_suelo is read from it
land_new <- !file.exists(land_gpkg)
if (land_new) land_use <- do.call(rbind, lapply(sapply(land_files$file, land_shp), land_read))

### Agricultural land use
# Land-use categories from uso_tierra, which is written differently across regions.
# The GeoPackage keeps neither factor levels nor units, so both are restored when uso_suelo is read from it.
land_cats <- c("Agrícola", "Rotación cultivo-pradera", "Praderas anuales", "Praderas perennes")
if (land_new) {
  uso_suelo <- land_use %>%
    mutate(uso_cat = factor(case_when(grepl("rotaci", uso_tierra, ignore.case = TRUE) ~ "Rotación cultivo-pradera",
                                      grepl("uso agr", uso_tierra, ignore.case = TRUE) ~ "Agrícola",
                                      grepl("anual", uso_tierra, ignore.case = TRUE) ~ "Praderas anuales",
                                      grepl("perenne", uso_tierra, ignore.case = TRUE) ~ "Praderas perennes"),
                            levels = land_cats)) %>%
    rename(cod_com = codcom, cod_reg = codreg)
  uso_suelo$area <- st_area(uso_suelo) %>%
    replace(is.na(.), 0)
} else {
  uso_suelo <- st_read(land_gpkg, as_tibble = TRUE, quiet = TRUE) %>%
    mutate(uso_cat = factor(uso_cat, levels = land_cats),
           area = units::set_units(area, m^2))
}


# SAVING ------------------------------------------------------------------

# The regional shapefiles are deleted once land_gpkg is saved, since they are only needed to build it
if (land_new) {
  st_write(uso_suelo, land_gpkg, layer = "land_use", delete_dsn = TRUE, quiet = TRUE)
  unlink(file.path(land_dir, land_files$file), recursive = TRUE)
}
