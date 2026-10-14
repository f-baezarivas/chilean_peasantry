######## ***La doble reducción del campesinado chileno en el siglo XXI*** ###################
### El código reproduce los resultados del artículo y del material suplementario
# Este trabajo se enmarca en la tesis doctoral de Fernando Baeza
# Baeza, F. (2026). Hacia una comprensión de la heterogeneidad del campesinado chileno desde una perspectiva geográfica: las estrategias productivas de los hogares campesinos y los mercados regionales.

# DATA PROCESSING --------------------------------------------------------------
source("data_process/01_dpa.R") # Political-administrative division codes
source("data_process/02_casen.R") # CASEN 2000-2022: persons (casen_mie) and households (casen)
source("data_process/03_caf07.R") # CAF 2007: households (caf07) and members (caf07_mie)
source("data_process/04_caf21.R") # CAF 2021: productive units (caf21_ce, caf21_ca, caf21_pre), households and members
# source("data_process/04_01_caf21_imputaciones.R") # Imputation of the peasant classification (slow; not run by default)
source("data_process/04_02_caf21_combinacion.R") # CAF 2021: peasant households and combined dataset (caf21)
