### CAF 2021: IMPUTATION OF THE PEASANT CLASSIFICATION OF HOUSEHOLDS ###
# In the household questionnaire, households hiring only temporary workers cannot be classified as peasant
# households, because the number of days hired is unknown (hog_afc_est_mis is missing). Their classification is
# imputed with a multiple imputation model (mice), so that peasant households can be characterized with all
# household variables.
#
# Not run by default: it is slow, and its results may not exactly reproduce those of the original imputation.
# Output: output/caf21_imp.rds (id_hog, 10 imputations, observed classification and share of imputations classifying the household as peasant),
# used by 04_02_caf21_combinacion.R.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, mice)

if (!exists("caf21_hog")) source("data_process/04_caf21.R")

### Predictors
# Selected with a logistic regression in the gold standard (productive questionnaire), among the variables shared with
# the household questionnaire that are theoretically relevant, excluding non-significant ones:
# summary(glm(hog_afc_est ~ us61_1 + us61_2 + us61_4 + us61_5 + us61_6 + us61_10 + us61_11 + ga161 + ga172 + ga186 + ga199,
#             data = filter(caf21_ce, tr244 > 1 & tr250 == 1), family = binomial()))
vars_imp <- c("hp280_2",
              "us61_1", "us61_2", "us61_4", "us61_5", "us61_6", "us61_10", "us61_11",
              "ga161", "ga172", "ga185", "ga186", "ga199",
              "nedu_jh", "jor_pre", "n_ocupados",
              "hog_afc_est_mis")

datos_imp <- select(caf21_hog, all_of(vars_imp))

### Imputation model
# Only predictors with information in at least 25% of observations (minpuc) and a minimum correlation of 0.1 (quickpred default).
# 10 imputed datasets (m), 5 iterations each (mice default).
imp <- mice(data = datos_imp,
            pred = quickpred(datos_imp, minpuc = 0.25),
            m = 10,
            seed = 123)

### Diagnostics: convergence of means and standard deviations across the 10 solutions
# plot(imp, "hog_afc_est_mis")
# densityplot(imp, ~hog_afc_est_mis)
# xyplot(imp, hog_afc_est_mis ~ jor_pre | .imp, pch = c(1, 20), cex = c(1, 1.5))
# stripplot(imp, hog_afc_est_mis ~ .imp, cex = c(2, 4), pch = c(1, 20), jitter = FALSE)

### The 10 solutions and the share of solutions classifying each household as peasant
caf21_imp <- complete(imp, "broad") %>%
  select(starts_with("hog_afc_est_mis.")) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.x))),
         id_hog = caf21_hog$id_hog, # complete() keeps the row order of the data
         hog_afc_est_mis = as.numeric(as.character(caf21_hog$hog_afc_est_mis)), # Observed classification
         hog_afc_est_mis_sum = rowMeans(pick(starts_with("hog_afc_est_mis.")))) %>%
  relocate(id_hog)

dir.create("output", showWarnings = FALSE)
saveRDS(caf21_imp, "output/caf21_imp.rds")

rm(datos_imp, imp)
