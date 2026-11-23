
# 0.1. DPA ----------------------------------------------------------------


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

# 0.4. Suelos ---------------------------------------------------

#Uso suelo
#Catastro y Actualización de los Recursos Vegetacionales y Uso de la Tierra (CONAF). Descargado de IDE Minagri

#shapes_full<-list.files(path = "C:/Users/fbaez/OneDrive - Universidad Católica de Chile/Capas/Uso suelo/CONAF", pattern = "\\.shp$", full.names = T)
#No funcionó el loop pegándolas todas porque hay bases que no tienen los mismos nombres. Eso lo descubrimos con esta función
#sapply(shapes_full, function(shp) {
#  length(intersect(
#    names(st_read(dsn = shp, as_tibble = TRUE, quiet = TRUE) %>% rename_all(tolower)),
#    names(suelo_1)
#  ))
#})
#shapes<-shapes_full[-c(6,11)]#No consideramos las dos bases que tienen nombres distintos

#uso_suelo <- do.call(rbind, lapply(shapes, function(shp) {
#  st_read(dsn = shp, as_tibble = TRUE, quiet = TRUE, drivers = "ESRI Shapefile") %>%
#    st_transform(32719) %>%
#    rename_all(tolower) %>%
#    select(codreg, codcom, id_uso, id_subuso, id_estruc, uso_tierra, uso, subuso, estructura)
#}))

#uso_suelo<-uso_suelo%>%
#  rbind(st_read(dsn = shapes_full[6], as_tibble = TRUE, quiet = TRUE, drivers = "ESRI Shapefile") %>%
#          st_transform(32719) %>%
#          rename_all(tolower) %>%
#          select(codreg, codcom=codcomun, id_uso, id_subuso, id_estruc, uso_tierra, uso, subuso, estructura))%>%
#  rbind(st_read(dsn = shapes_full[11], as_tibble = TRUE, quiet = TRUE, drivers = "ESRI Shapefile") %>%
#          st_transform(32719) %>%
#          rename_all(tolower) %>%
#          select(codreg, codcom, id_uso, id_subuso, id_estruc, uso_tierra=uso_tierr, uso, subuso, estructura))%>%
#  mutate(across(c(codreg:id_estruc), ~ as.integer(.x)))

#Una selección de terrenos agrícolas y praderas
#uso_suelo_agr<-uso_suelo%>%
#  filter(uso=="Terrenos AgrÃ­colas" | uso=="Terrenos Agricolas" | uso=="Terrenos Agrícolas" |
#           uso_tierra=="Pradera Perenne" | uso_tierra=="Praderas Anuales" | uso_tierra=="Praderas Perenne" | uso_tierra=="Praderas Perennes")

#Tenemos el problema de que algunos casos tienen dimensión XY y otros XYZ. Necesitamos que todos sean sólo XY para poder guardarlo con st_write.
#Los shp de las regiones 3 y 15 eran XYZ, todos los demás XY. A los casos con dimension XYZ eliminamos la dimensión Z 

#uso_suelo_agr<-uso_suelo_agr%>%
#  filter(codreg!=3 & codreg!=15)%>%
#  rbind(suelo_agr%>%
#          filter(codreg==3 | codreg==15)%>%
#          st_zm(drop=T, what="ZM"))

#st_write(uso_suelo_agr, dsn="C:/Users/fbaez/OneDrive - Universidad Católica de Chile/Capas/Uso suelo/CONAF/uso_suelo_agricola_chile.shp")

uso_suelo<-st_read(dsn="C:/Users/fbaez/OneDrive - Universidad Católica de Chile/Capas/Uso suelo/CONAF/uso_suelo_agricola_chile.shp",
                   as_tibble = TRUE, quiet = TRUE, drivers = "ESRI Shapefile")%>%
  mutate(uso_cat=factor(case_when(uso_tierra=="Pradera Perenne" | uso_tierra=="Praderas Perenne" | uso_tierra=="Praderas Perennes" ~ "Praderas perennes",
                                  uso_tierra=="Praderas Anuales" ~ "Praderas anuales",
                                  uso_tierra=="RotaciÃ³n Cultivo-Pradera" | uso_tierra=="Rotación Cultivo-Pradera" ~ "Rotación cultivo-pradera",
                                  uso_tierra=="Terreno de Uso AgrÃ­cola" | uso_tierra=="Terreno de Uso Agrícola" | uso_tierra=="Terrenos de Uso Agrícola" ~ "Agrícola"),
                        levels=c("Agrícola", "Rotación cultivo-pradera", "Praderas anuales", "Praderas perennes")))%>%
  rename(cod_com=codcom, cod_reg=codreg)
uso_suelo$area<-st_area(uso_suelo)%>%
  replace(is.na(.), 0)


# 0.5. Zonas climáticas Koppen --------------------------------------------

koppen<-st_read(dsn="C:/Users/fbaez/OneDrive - Universidad Católica de Chile/Capas/Zonas climáticas/layer_koppen_20220309023154.shp",
                as_tibble = TRUE, quiet = TRUE, drivers = "ESRI Shapefile")%>%
  st_transform(32719) %>%
  rename_all(tolower)%>%
  mutate(tipo2=factor(case_when(koppen_fin == "BSk" ~ "Semiárido", koppen_fin == "BSk (s)" ~ "Semiárido", koppen_fin == "BSk (s) (i)" ~ "Semiárido", koppen_fin == "BSk (w)" ~ "Semiárido",
                                koppen_fin == "BWh" ~ "Desértico", koppen_fin == "BWh (s)" ~ "Desértico", koppen_fin == "BWk" ~ "Desértico", koppen_fin == "BWk (s)" ~ "Desértico", koppen_fin == "BWk (w)" ~ "Desértico",
                                koppen_fin == "Cfb" ~ "Templado lluvioso", koppen_fin == "Cfb (i)" ~ "Templado lluvioso", koppen_fin == "Cfb (s)" ~ "Templado lluvioso", 
                                koppen_fin == "Cfb (s) (i)" ~ "Templado lluvioso", koppen_fin == "Cfc" ~ "Templado lluvioso", koppen_fin == "Cfc (s)" ~ "Templado lluvioso",
                                koppen_fin == "Csb" ~ "Mediterráneo", koppen_fin == "Csb (h)" ~ "Mediterráneo", koppen_fin == "Csb (i)" ~ "Mediterráneo", koppen_fin == "Csc" ~ "Mediterráneo",
                                koppen_fin == "EF" ~ "Glacial", koppen_fin == "EF (s)" ~ "Glacial", koppen_fin == "EF (w)" ~ "Glacial", koppen_fin == "ET" ~ "Tundra",
                                koppen_fin == "ET (s)" ~ "Tundra", koppen_fin == "ET (w)" ~ "Tundra"),
                      levels=c("Desértico", "Semiárido", "Mediterráneo", "Templado lluvioso", "Tundra", "Glacial")),
         tipo3=factor(case_when(koppen_fin == "BSk" ~ "Semiárido frío", koppen_fin == "BSk (s)" ~ "Semiárido frío", koppen_fin == "BSk (s) (i)" ~ "Semiárido frío", koppen_fin == "BSk (w)" ~ "Semiárido frío",
                                koppen_fin == "BWh" ~ "Desértico cálido", koppen_fin == "BWh (s)" ~ "Desértico cálido", 
                                koppen_fin == "BWk" ~ "Desértico frío", koppen_fin == "BWk (s)" ~ "Desértico frío", koppen_fin == "BWk (w)" ~ "Desértico frío",
                                koppen_fin == "Cfb" ~ "Lluvioso oceánico", koppen_fin == "Cfb (i)" ~ "Lluvioso oceánico", koppen_fin == "Cfb (s)" ~ "Lluvioso oceánico", koppen_fin == "Cfb (s) (i)" ~ "Lluvioso oceánico",
                                koppen_fin == "Cfc" ~ "Lluvioso frío", koppen_fin == "Cfc (s)" ~ "Lluvioso frío",
                                koppen_fin == "Csb" ~ "Mediterráneo oceánico", koppen_fin == "Csb (h)" ~ "Mediterráneo oceánico", koppen_fin == "Csb (i)" ~ "Mediterráneo oceánico", koppen_fin == "Csc" ~ "Mediterráneo frío",
                                koppen_fin == "EF" ~ "Glacial", koppen_fin == "EF (s)" ~ "Glacial", koppen_fin == "EF (w)" ~ "Glacial",
                                koppen_fin == "ET" ~ "Tundra", koppen_fin == "ET (s)" ~ "Tundra", koppen_fin == "ET (w)" ~ "Tundra"),
                      levels=c("Desértico cálido", "Desértico frío", "Semiárido frío", "Mediterráneo oceánico", "Mediterráneo frío", "Lluvioso oceánico", "Lluvioso frío", "Tundra", "Glacial")))%>%
  select(koppen_fin, tipo2, tipo3, tipo4=denominaci, koppen_fin, pp_mm, tmed, alt_min, alt_max)%>%
  st_intersection(y=dpa_com)%>%
  mutate(cod_reg=as.integer(CUT_REG))%>%
  select(cod_reg, cod_com=com, koppen_fin:alt_max, geometry)

# 0.6. SPEI ---------------------------------------------------------


#Importamos los raster de SPI, SPEI y luego los transformamos a vector y lo intersectamos con Chile continental
#Tenemos dos versiones, de corto y mediano plazo (3 y 6 meses, respectivamente, comenzando en septiembre y terminando en noviembre y en febrero), de modo de incluir el período más crítico de crecimiento de cultivos y otro más extendido que se acerca más a la fecha de cosecha (Zambrano, 2018).
#SPEI es con la ecuación de Hargreaves. SPEI es transformado en log-logística (Beguería) y SPI en gamma (Zambrano).
#El período de comparación, de referencia del clima es desde 1980 a 2009. Son 30 años antes del inicio de la mega-sequía en 2010 (Garreaud)
#Los datos están disponibles en una grilla de 0.1° (9,6 km2) a partir de datos observados a nivel de superficie y modelados para interpolación a una grilla de 0,1°lat-lon. 
#El producto ERA5 es especialmente utilizado para estudios agroecológicos AgERA5 (https://confluence.ecmwf.int/pages/viewpage.action?pageId=278551004)

spei36_raster<-raster::raster('SPEI25-36.speih.tif' )
spei36_shp<-stars:::st_as_sf.stars(stars::st_as_stars(spei36_raster), point = FALSE, merge = TRUE, connect8 = TRUE)%>%
  st_transform(32719)%>%
  st_intersection(y=dpa_cl)%>%
  select(spei36=SPEI25.36.speih, geometry)
