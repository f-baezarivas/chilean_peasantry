### SECTION 5: REDUCTION ###

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, sf)

# Figure 1 ----------------------------------------------------------------

source()

dpa_com <- st_read("output/dpa_com.gpkg", quiet = TRUE) # Built in spatial/01_dpa.R
caf07 <- readRDS("output/caf07.rds")
caf21 <- readRDS("output/caf21.rds")
caf21_pre <- readRDS("output/caf21_pre.rds")


# Dataset -----------------------------------------------------------------


caf_afc_com<-dpa_com%>%
  select(cod_com_2018, cod_com_0721, nom_com_0721, nom_com)%>%
  group_by(cod_com_0721, nom_com_0721)%>%
  summarise(.groups = "drop")%>%
  left_join(y=caf21_pre%>%
              group_by(cod_com_0721)%>%
              summarise(caf_21=sum(hog_afc_est==1, na.rm=T))%>%
              ungroup(),
            by="cod_com_0721")%>%
  left_join(y=caf07%>%
              group_by(cod_com_0721)%>%
              summarise(caf_07=sum(hog_afc_est==1, na.rm=T))%>%
              ungroup(),
            by="cod_com_0721")%>%
  filter(!cod_com_0721 %in% c(5104, 5201, 12202))%>%
  mutate(across(starts_with("caf"), ~ replace_na(.x, 0)),
         var_0721=caf_21-caf_07,
         pct_var_0721=var_0721*100/caf_07)


# Maps --------------------------------------------------------------------


### N hogares

# 2007
caf_afc_com %>%
  mutate(caf_tramo = cut(caf_07,
                           breaks = c(-Inf, 0, 100, 250, 500, 1000, 2000, Inf),
                           labels = c("Sin hogares", "1–100", "101–250", "251–500",
                                      "501–1.000", "1.001–2.000", "> 2.000"))) %>%
  ggplot() +
  geom_sf(aes(fill = caf_tramo), alpha=0.7, color = NA) +
  scale_fill_manual(values = c("grey85", "#D9F0A3", "#ADDD8E", "#78C679",
                               "#41AB5D", "#238443", "#005A32"),
                    drop = FALSE) +
  labs(title = "2007",
       fill="") +
  theme_void()+
  theme(legend.position = "right")

# 2021
caf_afc_com %>%
  mutate(caf_tramo = cut(caf_21,
                           breaks = c(-Inf, 0, 100, 250, 500, 1000, 2000, Inf),
                           labels = c("Sin hogares", "1–100", "101–250", "251–500",
                                      "501–1.000", "1.001–2.000", "> 2.000"))) %>%
  ggplot() +
  geom_sf(aes(fill = caf_tramo), alpha=0.7, color = NA) +
  scale_fill_manual(values = c("grey85", "#D9F0A3", "#ADDD8E", "#78C679",
                               "#41AB5D", "#238443", "#005A32"),
                    drop = FALSE) +
  labs(title = "2021",
       fill="") +
  theme_void()+
  theme(legend.position = "right")



### Variación

caf_afc_com %>%
  filter(caf_07>0 & caf_21>0)%>%
  mutate(caf_media = (caf_21 + caf_07) / 2,
         var_tramo = cut(pct_var_0721,
                         breaks = c(-100, -50, -25, 0, 25, 50, 100, Inf),
                         labels = c("-100 a -50", "-50 a -25", "-25 a 0",
                                    "0 a 25", "25 a 50", "50 a 100", "> 100"),
                         right = FALSE, include.lowest = TRUE)) %>%
  ggplot() +
  geom_sf(aes(fill = var_tramo), alpha=0.6, color = NA) +
  scale_fill_manual(values = c("#67001F", "#D6604D", "#F4A582",
                               "#92C5DE", "#4393C3", "#2166AC", "#053061"),
                    drop = FALSE) +
  theme_void()+
  theme(legend.position = "right")+
  labs(title="Variación hogares 2007-2021 (%)",
       fill="")


# 2007
caf_afc_com %>%
  mutate(reg=trunc(cod_com_0721/1000))%>%
  filter(reg %in% c(5,13))%>%
  mutate(caf_tramo = cut(caf_07,
                         breaks = c(-Inf, 0, 100, 250, 500, 1000, 2000, Inf),
                         labels = c("Sin hogares", "1–100", "101–250", "251–500",
                                    "501–1.000", "1.001–2.000", "> 2.000"))) %>%
  ggplot() +
  geom_sf(aes(fill = caf_tramo), alpha=0.7, color = NA) +
  geom_sf(data=dpa_com%>%filter(cod_reg_2018 %in% c(5,13))%>%group_by(cod_reg_2018)%>%summarise(.groups="drop"),
          color="gray20", fill=NA)+
  scale_fill_manual(values = c("grey85", "#D9F0A3", "#ADDD8E", "#78C679",
                               "#41AB5D", "#238443", "#005A32"),
                    drop = FALSE) +
  labs(title = "2007",
       fill="") +
  theme_void()+
  theme(legend.position = "right")



# 2021. Regiones V y Metro
caf_afc_com %>%
  mutate(reg=trunc(cod_com_0721/1000))%>%
  filter(reg %in% c(5,13))%>%
  mutate(caf_tramo = cut(caf_21,
                         breaks = c(-Inf, 0, 100, 250, 500, 1000, 2000, Inf),
                         labels = c("Sin hogares", "1–100", "101–250", "251–500",
                                    "501–1.000", "1.001–2.000", "> 2.000"))) %>%
  ggplot() +
  geom_sf(aes(fill = caf_tramo), alpha=0.7, color = NA) +
  geom_sf(data=dpa_com%>%filter(cod_reg_2018 %in% c(5,13))%>%group_by(cod_reg_2018)%>%summarise(.groups="drop"),
          color="gray20", fill=NA)+
  scale_fill_manual(values = c("grey85", "#D9F0A3", "#ADDD8E", "#78C679",
                               "#41AB5D", "#238443", "#005A32"),
                    drop = FALSE) +
  labs(title = "2021",
       fill="") +
  theme_void()+
  theme(legend.position = "right")


### Variación. Regiones V y Metro

caf_afc_com %>%
  mutate(reg=trunc(cod_com_0721/1000))%>%
  filter(reg %in% c(5,13))%>%
  filter(caf_07>0 & caf_21>0)%>%
  mutate(caf_media = (caf_21 + caf_07) / 2,
         var_tramo = cut(pct_var_0721,
                         breaks = c(-100, -50, -25, 0, 25, 50, 100, Inf),
                         labels = c("-100 a -50", "-50 a -25", "-25 a 0",
                                    "0 a 25", "25 a 50", "50 a 100", "> 100"),
                         right = FALSE, include.lowest = TRUE)) %>%
  ggplot() +
  geom_sf(aes(fill = var_tramo), alpha=0.6, color = NA) +
  geom_sf(data=dpa_com%>%filter(cod_reg_2018 %in% c(5,13))%>%group_by(cod_reg_2018)%>%summarise(.groups="drop"),
          color="gray20", fill=NA)+
  scale_fill_manual(values = c("#67001F", "#D6604D", "#F4A582",
                               "#92C5DE", "#4393C3", "#2166AC", "#053061"),
                    drop = FALSE) +
  theme_void()+
  theme(legend.position = "right")+
  labs(title="Variación hogares 2007-2021 (%)",
       fill="")

