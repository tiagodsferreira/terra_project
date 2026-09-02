# CONTENTS
#   1. Setup & data import
#   2. Model specifications (final models: 1D, 2D, 5D)
#   3. CFA - Mothers (fit indices & loadings)
#   4. CFA - Fathers (fit indices & loadings)
#   5. Omega internal consistency (total only, final models)
#   6. Comparison tables
##############################################################################

library(haven)
library(lavaan)
library(semTools)
library(dplyr)
library(psych)

# 1. SETUP & DATA IMPORT ------------------------------------------------------
data <- read_sav("../data/data_clean.sav")

# Create mother and father data frames with standardised item names (item1...item15)
df_mother <- data %>%
  select(id_crianca, 
         neps1.1, neps2R.1, neps3.1, neps4R.1, neps5.1,
         neps6R.1, neps7.1, neps8R.1, neps9.1, neps10R.1,
         neps11.1, neps12R.1, neps13.1, neps14R.1, neps15.1) %>%
  rename(id = id_crianca, 
         item1 = neps1.1, item2 = neps2R.1, item3 = neps3.1,
         item4 = neps4R.1, item5 = neps5.1, item6 = neps6R.1, 
         item7 = neps7.1, item8 = neps8R.1, item9 = neps9.1,
         item10 = neps10R.1, item11 = neps11.1, item12 = neps12R.1,
         item13 = neps13.1, item14 = neps14R.1, item15 = neps15.1)

df_father <- data %>%
  select(id_crianca,
         neps1.2, neps2R.2, neps3.2, neps4R.2, neps5.2,
         neps6R.2, neps7.2, neps8R.2, neps9.2, neps10R.2,
         neps11.2, neps12R.2, neps13.2, neps14R.2, neps15.2) %>%
  rename(id = id_crianca,
         item1 = neps1.2, item2 = neps2R.2, item3 = neps3.2,
         item4 = neps4R.2, item5 = neps5.2, item6 = neps6R.2,
         item7 = neps7.2, item8 = neps8R.2, item9 = neps9.2,
         item10 = neps10R.2, item11 = neps11.2, item12 = neps12R.2,
         item13 = neps13.2, item14 = neps14R.2, item15 = neps15.2)


# 2. MODEL SPECIFICATIONS (FINAL MODELS) --------------------------------------

# 1D final model (10 items, with residual covariances)
model_1D <- '
  NEP_Uni =~ item2 + item3 + item4 + item5 + item8 + item10 + item12 + item13 + item14 + item15

  item3  ~~ item5
  item13 ~~ item15
  item8  ~~ item10
  item14 ~~ item15
  item5  ~~ item15
  item5  ~~ item13
'

# 5D final model (items reduced, residual covariances)
model_5D <- '
  Growth              =~ item1 + item11
  Antiantropocentrism =~ item2 + item12
  Fragility           =~ item3 + item8 + item13
  Rejection           =~ item4 + item14
  Change              =~ item5 + item10 + item15

  item5 ~~ item15
  item8 ~~ item13
'

# 2D final model (items reduced, one residual covariance)
model_2D <- '
  Ecocentrism      =~ item3 + item5 + item13 + item15
  Anthropocentrism =~ item2 + item4 + item8 + item10 + item12 + item14

  item8 ~~ item10
'


# 3. CFA - MOTHERS ------------------------------------------------------------
cat("\n========== MOTHERS ==========\n")

# Helper to extract fit measures
get_fit <- function(fit) {
  fitMeasures(fit, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                     "cfi.robust", "tli.robust", "rmsea.robust",
                     "srmr", "aic", "bic", "rmsea.ci.lower", "rmsea.ci.upper"))
}

cfa_mother_1 <- cfa(model_1D, data = df_mother, estimator = "MLR", missing = "fiml", std.lv = TRUE)
cfa_mother_2 <- cfa(model_2D, data = df_mother, estimator = "MLR", missing = "fiml", std.lv = TRUE)
cfa_mother_5 <- cfa(model_5D, data = df_mother, estimator = "MLR", missing = "fiml", std.lv = TRUE)

fit_mother_1 <- get_fit(cfa_mother_1)
fit_mother_2 <- get_fit(cfa_mother_2)
fit_mother_5 <- get_fit(cfa_mother_5)

cat("\n--- Fit Indices (Mothers) ---\n")
fit_table_mother <- rbind(`1D` = fit_mother_1, `2D` = fit_mother_2, `5D` = fit_mother_5)
print(round(fit_table_mother, 3))


# 4. CFA - FATHERS ------------------------------------------------------------
cat("\n========== FATHERS ==========\n")

cfa_father_1 <- cfa(model_1D, data = df_father, estimator = "MLR", missing = "fiml", std.lv = TRUE)
cfa_father_2 <- cfa(model_2D, data = df_father, estimator = "MLR", missing = "fiml", std.lv = TRUE)
cfa_father_5 <- cfa(model_5D, data = df_father, estimator = "MLR", missing = "fiml", std.lv = TRUE)

fit_father_1 <- get_fit(cfa_father_1)
fit_father_2 <- get_fit(cfa_father_2)
fit_father_5 <- get_fit(cfa_father_5)

cat("\n--- Fit Indices (Fathers) ---\n")
fit_table_father <- rbind(`1D` = fit_father_1, `2D` = fit_father_2, `5D` = fit_father_5)
print(round(fit_table_father, 3))


# 5. OMEGA INTERNAL CONSISTENCY (TOTAL ONLY) ----------------------------------

# 1D items
uni_items <- paste0("item", c(2,3,4,5,8,10,12,13,14,15))

# 2D items and keys
bi_items <- paste0("item", c(2,3,4,5,8,10,12,13,14,15))
bi_keys <- make.keys(length(bi_items),
                     list(Ecocentrism = which(bi_items %in% paste0("item", c(3,5,13,15))),
                          Anthropocentrism = which(bi_items %in% paste0("item", c(2,4,8,10,12,14)))))

# 5D items and keys
dim5_items <- paste0("item", c(1,2,3,4,5,8,10,11,12,13,14,15))
dim5_keys <- make.keys(length(dim5_items),
                       list(Growth = which(dim5_items %in% paste0("item", c(1,11))),
                            Antiantropocentrism = which(dim5_items %in% paste0("item", c(2,12))),
                            Fragility = which(dim5_items %in% paste0("item", c(3,8,13))),
                            Rejection = which(dim5_items %in% paste0("item", c(4,14))),
                            Change = which(dim5_items %in% paste0("item", c(5,10,15)))))

cat("\n========== OMEGA TOTAL ==========\n")

# 1D
omega_1D_m <- omega(df_mother[, uni_items], nfactors = 1, plot = FALSE)
omega_1D_f <- omega(df_father[, uni_items], nfactors = 1, plot = FALSE)
cat("1D - Mothers: omega total =", round(omega_1D_m$omega.tot, 3), "\n")
cat("1D - Fathers: omega total =", round(omega_1D_f$omega.tot, 3), "\n")

# 2D
omega_2D_m <- omega(df_mother[, bi_items], nfactors = 2, keys = bi_keys, plot = FALSE)
omega_2D_f <- omega(df_father[, bi_items], nfactors = 2, keys = bi_keys, plot = FALSE)
cat("2D - Mothers: omega total =", round(omega_2D_m$omega.tot, 3), "\n")
cat("2D - Fathers: omega total =", round(omega_2D_f$omega.tot, 3), "\n")

# 5D
omega_5D_m <- omega(df_mother[, dim5_items], nfactors = 5, keys = dim5_keys, plot = FALSE)
omega_5D_f <- omega(df_father[, dim5_items], nfactors = 5, keys = dim5_keys, plot = FALSE)
cat("5D - Mothers: omega total =", round(omega_5D_m$omega.tot, 3), "\n")
cat("5D - Fathers: omega total =", round(omega_5D_f$omega.tot, 3), "\n")

##############################################################################

