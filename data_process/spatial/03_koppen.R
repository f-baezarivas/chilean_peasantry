### KÖPPEN CLIMATE ZONES ###
# Imports the Köppen-Geiger climate zones of Chile and intersects them with the municipalities in dpa_com,
# in UTM 19S (EPSG 32719). Exported to output/ as a GeoPackage.
# Original shapefile from Sarricolea et al (2017): https://doi.org/10.1080/17445647.2016.1259592, which is an updated Köppen–Geiger climate classification for continental Chile  
# from Pliscoff et al (2014): (https://doi.org/10.1016/j.ecolmodel.2014.06.003)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, sf)

dpa_com <- st_read("output/dpa_com.gpkg", quiet = TRUE) # Built in spatial/01_dpa.R

### Köppen source file
# Downloaded from the IDE Chile geoportal (koppen_url) and cached in koppen_dir
koppen_dir <- "input/koppen"
koppen_url <- "https://geoportal.cl/geoportal/catalog/download/8e85a12a-151d-31c0-9e7e-e356f92ece84"
koppen_shp <- file.path(koppen_dir, "layer_koppen_20220309023154.shp")

if (!file.exists(koppen_shp)) {
  zip <- file.path(tempdir(), "layer_koppen_20220309023154.zip")
  download.file(koppen_url, zip, mode = "wb")
  unzip(zip, exdir = koppen_dir)
  unlink(zip)
}

### Climate zones
# Groups the climate types by their main code, without the precipitation and coastal-influence suffixes
koppen <- st_read(koppen_shp, as_tibble = TRUE, quiet = TRUE) %>%
  rename_with(tolower) %>%
  st_transform(32719) %>%
  st_make_valid() %>% # Most polygons are invalid in the source layer
  mutate(code = sub(" .*", "", koppen_fin),
         tipo2 = factor(case_when(code %in% c("BWh", "BWk") ~ "Desértico",
                                  code == "BSk" ~ "Semiárido",
                                  code %in% c("Csb", "Csc") ~ "Mediterráneo",
                                  code %in% c("Cfb", "Cfc") ~ "Templado lluvioso",
                                  code == "ET" ~ "Tundra",
                                  code == "EF" ~ "Glacial"),
                        levels = c("Desértico", "Semiárido", "Mediterráneo", "Templado lluvioso", "Tundra", "Glacial")),
         tipo3 = factor(case_when(code == "BWh" ~ "Desértico cálido",
                                  code == "BWk" ~ "Desértico frío",
                                  code == "BSk" ~ "Semiárido frío",
                                  code == "Csb" ~ "Mediterráneo oceánico",
                                  code == "Csc" ~ "Mediterráneo frío",
                                  code == "Cfb" ~ "Lluvioso oceánico",
                                  code == "Cfc" ~ "Lluvioso frío",
                                  code == "ET" ~ "Tundra",
                                  code == "EF" ~ "Glacial"),
                        levels = c("Desértico cálido", "Desértico frío", "Semiárido frío", "Mediterráneo oceánico", "Mediterráneo frío",
                                   "Lluvioso oceánico", "Lluvioso frío", "Tundra", "Glacial"))) %>%
  select(koppen_fin, tipo2, tipo3, tipo4 = denominaci, pp_mm, tmed, alt_min, alt_max) %>%
  st_intersection(y = select(dpa_com, cod_reg = cod_reg_2018, cod_com = cod_com_2018)) %>%
  relocate(cod_reg, cod_com)

# SAVING ------------------------------------------------------------------

st_write(koppen, "output/koppen.gpkg", delete_dsn = TRUE, quiet = TRUE)
