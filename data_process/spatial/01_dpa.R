### DPA SHAPEFILE PROCESSING ###
# Imports the 2020 political-administrative division (DPA) at the municipality level and adds the municipality
# codes in dpa_cod, to link it with the census datasets. Builds two layers, in UTM 19S (EPSG 32719):
#   - dpa_com: municipalities of continental Chile
#   - dpa_cl: boundary of continental Chile
# Both are exported to output/ as GeoPackages (shapefiles would truncate the variable names).

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, sf, archive)

source("data_process/01_dpa.R") # Loads dpa_cod with common identifiers for municipalities

### DPA 2020 source files
# Downloaded from SUBDERE (dpa_url). The zip file contains one .rar file per level (municipality, province,
# region); only the municipality shapefile is extracted and cached in dpa_dir.
dpa_dir <- "input/dpa"
dpa_url <- "https://proactiva.subdere.gov.cl/bitstream/handle/123456789/521/DivisionPoliticoAdministrativa2020.zip?sequence=1&isAllowed=y"
dpa_shp <- file.path(dpa_dir, "COMUNAS_2020.shp")

if (!file.exists(dpa_shp)) {
  zip <- file.path(tempdir(), "DivisionPoliticoAdministrativa2020.zip")
  options(timeout = max(1200, getOption("timeout")))
  download.file(dpa_url, zip, mode = "wb")
  rar <- unzip(zip, files = "DivisionPoliticoAdministrativa2020/COMUNA.rar", exdir = tempdir())
  content <- archive(rar)$path
  i <- grep("COMUNAS_2020[.]", content)
  archive_extract(rar, dir = tempdir(), files = i)
  file.copy(file.path(tempdir(), content[i]), dpa_dir, overwrite = TRUE)
  unlink(c(zip, rar))
}

### Municipalities
# Excludes Antártica (12202), Juan Fernández (5104) and Isla de Pascua (5201)
dpa_com <- st_read(dsn = dpa_shp, as_tibble = TRUE, quiet = TRUE) %>%
  st_transform(32719) %>%
  mutate(cod_com_2018 = as.double(CUT_COM)) %>%
  select(geometry, cod_com_2018) %>%
  left_join(y = dpa_cod, by = "cod_com_2018") %>%
  filter(cod_com_2018 != 12202 & cod_com_2018 != 5104 & cod_com_2018 != 5201)

### Boundary of continental Chile
dpa_cl <- dpa_com %>%
  st_make_valid() %>%
  mutate(zona = "Chile") %>%
  group_by(zona) %>%
  summarise() %>%
  ungroup()


# SAVING ------------------------------------------------------------------

st_write(dpa_com, "output/dpa_com.gpkg", delete_dsn = TRUE, quiet = TRUE)
st_write(dpa_cl, "output/dpa_cl.gpkg", delete_dsn = TRUE, quiet = TRUE)
