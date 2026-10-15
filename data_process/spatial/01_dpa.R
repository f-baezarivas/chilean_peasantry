### DPA SHAPEFILE PROCESSING ###
# Import a shapefile containing the country's political-administrative divisions at the commune level.
# Add a key variable with commune values in `dpa_cod` for use with census variables.
# Export shapefiles, including one with national boundaries.

library(sf)

source("data_process/01_dpa.R") #Loads dpa_cod with common identifiers for comuna

#Comunas
dpa_com<-st_read(dsn = "C:/Users/fbaez/OneDrive - Universidad Católica de Chile/Capas/DPA/2020/COMUNA/COMUNAS_2020.shp", as_tibble = TRUE, quiet = FALSE, drivers = "ESRI Shapefile")%>%
  st_transform(32719)%>%
  mutate(cod_com_2018=as.double(CUT_COM))%>%
  select(geometry, cod_com_2018)%>%
  left_join(y=dpa_cod, by="cod_com_2018")%>%
  filter(cod_com_2018!=12202 & cod_com_2018!=5104 & cod_com_2018!=5201)

#Límites Chile continental
dpa_cl<-dpa_com%>%
  st_make_valid()%>%
  filter(cod_com_2018!=12202 & cod_com_2018!=5104 & cod_com_2018!=5201)%>%
  mutate(zona="Chile")%>%
  group_by(zona)%>%
  summarise()%>%
  ungroup()
