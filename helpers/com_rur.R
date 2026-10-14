### MUNICIPALITIES CLASSIFICATION BY RURALITY ###
### Classifies municipalities according to population size and density, according to OECD and PNDR criteria.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, sf, zip)

source("helpers/capwords.R") # Capwords Helper



### From 2017 Census (Instituto Nacional de Estadisticas)
url <- "https://www.ine.gob.cl/docs/default-source/geodatos-abiertos/cartografia/censo-2017/divisi%C3%B3n-pol%C3%ADtico-administrativa/shp/distritos_2017.zip?sfvrsn=2f267142_3"
zipfile <- tempfile(fileext = ".zip")
download.file(url, zipfile, mode = "wb")
destino <- tempfile()
dir.create(destino)
unzip(zipfile, exdir = destino)
shp <- list.files(destino, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)

### Municipalities rurality according density
com17 <- st_read(dsn = shp[1], as_tibble = TRUE, quiet = FALSE)%>%
  st_drop_geometry()%>%
  mutate(dens_rec=case_when(Densidad<=150 ~ "menos_150",
                            Densidad>150 ~ "mas_150"),
         REGION=as.double(REGION),
         COMUNA=as.double(COMUNA))%>% 
  select(reg=REGION, com=COMUNA, nom_com=NOM_COMUNA, dis=CODIGO_C17, pop=T_POB, dens=Densidad, dens_rec)%>%
  group_by(reg, com, nom_com, dens_rec)%>% # Grouping districts
  summarise(pop=sum(pop))%>%
  pivot_wider(names_from=dens_rec, values_from = pop)%>% 
  mutate(mas_150=replace_na(mas_150, 0),
         menos_150=replace_na(menos_150,0),
         pop=mas_150 + menos_150,
         pct_rur=100*menos_150/pop,
         # Within urban, we distinguish "metropolitan areas" ("Metropolitana")
         urb_rec=factor(case_when((com>=13100 & com<13200) | com== 13201 | com==13401 | com==13604 ~ "Metropolitana", #Núcleo urbano Gran Santiago: provincia de Santiago, Puente Alto, San Bernardo, Padre Hurtado.
                                  com==8101 | com==8102 | com==8103 | com==8107 | com==8108 | com==8110 | com==8112 ~ "Metropolitana", #Núcleo urbano Gran Concepción: Concepción, Coronel, Chiguayante, Hualpén, Penco, San Pedro de la Paz, Talcahuano.
                                  com==5101 | com==5103 | com==5109 | com==5801 | com==5804 ~ "Metropolitana", #Núcleo urbano Gran Valparaíso: Valparaíso, Concón, Viña del Mar, Quilpué, Villa Alemana.
                                  pct_rur>=15 & pct_rur<50 ~ "Mixta",
                                  pct_rur>=50 ~ "Rural",
                                  pct_rur<15 | pop >= 50000~ "Urbana"),
                        levels=c("Metropolitana", "Urbana", "Mixta", "Rural")))%>%
  mutate(nom_com=tolower(nom_com), # capwords function
         nom_com=capwords(nom_com))

rm(destino, shp, url, zipfile)


