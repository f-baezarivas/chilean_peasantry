### LOADING POLITICAL-ADMINISTRATIVE DIVISION CODES ### 
# We load the political-administrative division codes to standardize the datasets. 
# In addition, we create versions of municipalities that align the 2007 and 2021 CAF datasets.
# In CAF2021 the municipalities with the fewest cases were merged. 
# For spatial analyses, we assign the characteristics of the dominant municipality —i.e., the municipality with the most peasant households in the 2007 CAF dataset.

library(dplyr)

dpa_cod <- readxl::read_excel("input/dpa_codes.xlsx") # This is a small dataset with old and new municipalities code, according to SUBDERE (https://www.subdere.gov.cl/sites/default/files/documentos/CUT_2018_v04.xls)

dpa_cod<-dpa_cod%>% 
  mutate( # cod_com_0721 has the harmonized municipalities codes for CAF2007 and CAF2021
         cod_com_0721=case_when(cod_com_2018==1101 | cod_com_2018==1107 ~ 1108, #Dominant: 1107
                                cod_com_2018==2102 | cod_com_2018==2103 ~ 2401, #2102
                                cod_com_2018==2201 | cod_com_2018==2202 ~ 2205, #2201
                                cod_com_2018==2301 | cod_com_2018==2302 ~ 2402, #2302
                                cod_com_2018==3101 | cod_com_2018==3102 ~ 3104, #3101
                                cod_com_2018==3201 | cod_com_2018==3202 ~ 3204, #3202
                                cod_com_2018==5101 | cod_com_2018==5103 | cod_com_2018==5109 ~ 5110, #5101
                                cod_com_2018==5604 | cod_com_2018==5605 ~ 5607, #5604
                                cod_com_2018==8102 | cod_com_2018==8106 ~ 8114, #8102
                                cod_com_2018==8103 | cod_com_2018==8105 ~ 8113, #8105
                                cod_com_2018==8110 | cod_com_2018==8112 ~ 8115, #8112
                                cod_com_2018==11202 | cod_com_2018==11203 ~ 11204, #11202
                                cod_com_2018==11302 | cod_com_2018==11303 ~ 11304, #11302
                                cod_com_2018==13110 | cod_com_2018==13132 | cod_com_2018==13113 | cod_com_2018==13114 | cod_com_2018==13115 | 
                                  cod_com_2018==13120 | cod_com_2018==13122 | cod_com_2018==13123 | cod_com_2018==13101 ~ 13133, #13115
                                cod_com_2018==13125 | cod_com_2018==13128 | cod_com_2018==13121  | cod_com_2018==13117  | cod_com_2018==13102  | cod_com_2018==13103 |
                                  cod_com_2018==13104  | cod_com_2018==13106  | cod_com_2018==13107 ~ 13134, #13125
                                cod_com_2018==13131 | cod_com_2018==13105 | cod_com_2018==13112 ~ 13135, #13112
                                TRUE ~ cod_com_2018),
         #nom_com_0721 has the harmonized municipalities names for CAF2007 and CAF2021
         nom_com_0721=case_when(cod_com_2018==1101 | cod_com_2018==1107 ~ "Iquique/Alto Hospicio", #Dominant: 1107
                                cod_com_2018==2102 | cod_com_2018==2103 ~ "Mejillones/Sierra Gorda", #2102
                                cod_com_2018==2201 | cod_com_2018==2202 ~ "Calama/Ollagüe", #2201
                                cod_com_2018==2301 | cod_com_2018==2302 ~ "Tocopilla/María Elena", #2302
                                cod_com_2018==3101 | cod_com_2018==3102 ~ "Copiapó/Caldera", #3101
                                cod_com_2018==3201 | cod_com_2018==3202 ~ "Chañaral/Diego de Almagro", #3202
                                cod_com_2018==5101 | cod_com_2018==5103 | cod_com_2018==5109 ~ "Valparaíso/Viña del Mar/Concón", #5101
                                cod_com_2018==5604 | cod_com_2018==5605 ~ "El Quisco/El Tabo", #5604
                                cod_com_2018==8102 | cod_com_2018==8106 ~ "Coronel/Lota", #8102
                                cod_com_2018==8103 | cod_com_2018==8105 ~ "Chiguayante/Hualqui", #8105
                                cod_com_2018==8110 | cod_com_2018==8112 ~ "Talcahuano/Hualpén", #8112
                                cod_com_2018==11202 | cod_com_2018==11203 ~ "Cisnes/Guaitecas", #11202
                                cod_com_2018==11302 | cod_com_2018==11303 ~ "Tortel/O'Higgins", #11302
                                cod_com_2018==13110 | cod_com_2018==13132 | cod_com_2018==13113 | cod_com_2018==13114 | cod_com_2018==13115 | 
                                  cod_com_2018==13120 | cod_com_2018==13122 | cod_com_2018==13123 | cod_com_2018==13101 ~ "Santiago oriente", #13115
                                cod_com_2018==13125 | cod_com_2018==13128 | cod_com_2018==13121  | cod_com_2018==13117  | cod_com_2018==13102  | cod_com_2018==13103 |
                                  cod_com_2018==13104  | cod_com_2018==13106  | cod_com_2018==13107 ~ "Santiago norte", #13125
                                cod_com_2018==13131 | cod_com_2018==13105 | cod_com_2018==13112 ~ "Santiago sur", #13112
                                TRUE ~ nom_com),
         #cod_com_2018_dom has the code of the dominant municipality of each harmonized municipality
         cod_com_2018_dom=case_when(cod_com_0721==1108 ~ 1107,
                                    cod_com_0721==2401 ~ 2102,
                                    cod_com_0721==2205 ~ 2201,
                                    cod_com_0721==2402 ~ 2302,
                                    cod_com_0721==3104 ~ 3101,
                                    cod_com_0721==3204 ~ 3202,
                                    cod_com_0721==5110 ~ 5101,
                                    cod_com_0721==5607 ~ 5604,
                                    cod_com_0721==8114 ~ 8102,
                                    cod_com_0721==8113 ~ 8105,
                                    cod_com_0721==8115 ~ 8112,
                                    cod_com_0721==11204 ~ 11202,
                                    cod_com_0721==11304 ~ 11302,
                                    cod_com_0721==13133 ~ 13115,
                                    cod_com_0721==13134 ~ 13125,
                                    cod_com_0721==13135 ~ 13112,
                                    TRUE ~ cod_com_2018))

# Lookup for datasets coded with harmonized municipalities (CAF 2021)
com_0721 <- dpa_cod %>%
  distinct(cod_com_0721, nom_com_0721, cod_com_2018_dom)

