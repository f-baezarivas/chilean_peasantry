### SPEI ###
# Imports the Standardized Precipitation-Evapotranspiration Index (SPEI; Vicente-Serrano et al., 2010) over
# 12 months for continental Chile, and intersects it with the boundary of continental Chile in dpa_cl, in UTM 19S
# (EPSG 32719). Exported to output/ as a GeoPackage.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, sf, terra)

dpa_cl <- st_read("output/dpa_cl.gpkg", quiet = TRUE) # Built in spatial/01_dpa.R

### SPEI source file
# Raster downloaded from Climate Engine (https://app.climateengine.org/climateEngine), based on ERA5-Ag on a
# 0.1° grid. Potential evapotranspiration follows Hargreaves, and the climatic water balance is fitted to a
# log-logistic distribution (Beguería et al., 2014). The reference period is 1980-2009, the 30 years before the
# beginning of the megadrought in 2010 (Garreaud et al., 2017). Cells without data have the value 0.
spei_tif <- "input/spei/SPEI1-12.speih.tif"

### SPEI by cell in continental Chile
# Only the cells on the boundary are intersected with dpa_cl; the cells within it are kept whole
spei_cells <- rast(spei_tif) %>%
  setNames("spei12") %>%
  as.polygons(aggregate = FALSE) %>%
  st_as_sf() %>%
  st_transform(32719)
within <- lengths(st_within(spei_cells, dpa_cl)) > 0
boundary <- !within & lengths(st_intersects(spei_cells, dpa_cl)) > 0
spei <- bind_rows(spei_cells[within, ],
                  st_intersection(spei_cells[boundary, ], st_geometry(dpa_cl)))


# SAVING ------------------------------------------------------------------

st_write(spei, "output/spei.gpkg", delete_dsn = TRUE, quiet = TRUE)
