# CONTENTS
#   1. Setup & data import
#   2. Dyadic invariance models (configural, metric, scalar, strict)
#   3. Model fit comparison (ΔCFI, ΔRMSEA, LRT)
#   4. Tests for factor mean differences (Wald tests)
##############################################################################

library(haven)
library(lavaan)
library(semTools)

# 1. SETUP & DATA IMPORT ------------------------------------------------------
data <- read_sav("../data/data_clean.sav")

# Variables are already numeric; no conversion needed.
# Dyadic items: mother = .1, father = .2 (same as original NEPS naming)

# 2. DYADIC INVARIANCE MODELS ------------------------------------------------

# -------- Configural model (all parameters free across parents) ----------
model_configural <- '
  # Factor definitions (mother)
  Eco_mother  =~ neps3.1  + neps5.1  + neps13.1 + neps15.1
  Ant_mother  =~ neps2R.1 + neps4R.1 + neps8R.1 + neps10R.1 + neps12R.1 + neps14R.1

  # Factor definitions (father)
  Eco_father  =~ neps3.2  + neps5.2  + neps13.2 + neps15.2
  Ant_father  =~ neps2R.2 + neps4R.2 + neps8R.2 + neps10R.2 + neps12R.2 + neps14R.2

  # Residual covariances between corresponding items (mother–father)
  neps3.1   ~~ neps3.2
  neps5.1   ~~ neps5.2
  neps13.1  ~~ neps13.2
  neps15.1  ~~ neps15.2
  neps2R.1  ~~ neps2R.2
  neps4R.1  ~~ neps4R.2
  neps8R.1  ~~ neps8R.2
  neps10R.1 ~~ neps10R.2
  neps12R.1 ~~ neps12R.2
  neps14R.1 ~~ neps14R.2

  # Within‑parent residual covariances (from final 2‑factor model)
  neps8R.1  ~~ neps10R.1
  neps8R.2  ~~ neps10R.2

  # Factor correlations (all free)
  Eco_mother ~~ Ant_mother + Eco_father + Ant_father
  Ant_mother ~~ Eco_father + Ant_father
  Eco_father ~~ Ant_father
'

fit_configural <- cfa(model_configural, data = data,
                      estimator = "MLR", missing = "fiml")

cat("\n--- Configural Model Fit ---\n")
fitMeasures(fit_configural, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                              "cfi.robust", "tli.robust", "rmsea.robust", "srmr",
                              "rmsea.ci.lower", "rmsea.ci.upper"))

# -------- Metric model (factor loadings equal across parents) ------------
model_metric <- '
  Eco_mother  =~ L1*neps3.1  + L2*neps5.1  + L3*neps13.1 + L4*neps15.1
  Ant_mother  =~ L5*neps2R.1 + L6*neps4R.1 + L7*neps8R.1 + L8*neps10R.1 +
                 L9*neps12R.1 + L10*neps14R.1

  Eco_father  =~ L1*neps3.2  + L2*neps5.2  + L3*neps13.2 + L4*neps15.2
  Ant_father  =~ L5*neps2R.2 + L6*neps4R.2 + L7*neps8R.2 + L8*neps10R.2 +
                 L9*neps12R.2 + L10*neps14R.2

  # Residual covariances (same as configural)
  neps3.1   ~~ neps3.2
  neps5.1   ~~ neps5.2
  neps13.1  ~~ neps13.2
  neps15.1  ~~ neps15.2
  neps2R.1  ~~ neps2R.2
  neps4R.1  ~~ neps4R.2
  neps8R.1  ~~ neps8R.2
  neps10R.1 ~~ neps10R.2
  neps12R.1 ~~ neps12R.2
  neps14R.1 ~~ neps14R.2

  neps8R.1  ~~ neps10R.1
  neps8R.2  ~~ neps10R.2

  # Factor correlations (free)
  Eco_mother ~~ Ant_mother + Eco_father + Ant_father
  Ant_mother ~~ Eco_father + Ant_father
  Eco_father ~~ Ant_father
'

fit_metric <- cfa(model_metric, data = data,
                  estimator = "MLR", missing = "fiml")

cat("\n--- Metric Model Fit ---\n")
fitMeasures(fit_metric, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                          "cfi.robust", "tli.robust", "rmsea.robust", "srmr",
                          "rmsea.ci.lower", "rmsea.ci.upper"))

# -------- Scalar model (loadings + intercepts equal) --------------------
model_scalar <- '
  Eco_mother  =~ L1*neps3.1  + L2*neps5.1  + L3*neps13.1 + L4*neps15.1
  Ant_mother  =~ L5*neps2R.1 + L6*neps4R.1 + L7*neps8R.1 + L8*neps10R.1 +
                 L9*neps12R.1 + L10*neps14R.1

  Eco_father  =~ L1*neps3.2  + L2*neps5.2  + L3*neps13.2 + L4*neps15.2
  Ant_father  =~ L5*neps2R.2 + L6*neps4R.2 + L7*neps8R.2 + L8*neps10R.2 +
                 L9*neps12R.2 + L10*neps14R.2

  # Intercepts (equal across parents)
  neps3.1   ~ I1*1
  neps5.1   ~ I2*1
  neps13.1  ~ I3*1
  neps15.1  ~ I4*1
  neps2R.1  ~ I5*1
  neps4R.1  ~ I6*1
  neps8R.1  ~ I7*1
  neps10R.1 ~ I8*1
  neps12R.1 ~ I9*1
  neps14R.1 ~ I10*1

  neps3.2   ~ I1*1
  neps5.2   ~ I2*1
  neps13.2  ~ I3*1
  neps15.2  ~ I4*1
  neps2R.2  ~ I5*1
  neps4R.2  ~ I6*1
  neps8R.2  ~ I7*1
  neps10R.2 ~ I8*1
  neps12R.2 ~ I9*1
  neps14R.2 ~ I10*1

  # Residual covariances (same)
  neps3.1   ~~ neps3.2
  neps5.1   ~~ neps5.2
  neps13.1  ~~ neps13.2
  neps15.1  ~~ neps15.2
  neps2R.1  ~~ neps2R.2
  neps4R.1  ~~ neps4R.2
  neps8R.1  ~~ neps8R.2
  neps10R.1 ~~ neps10R.2
  neps12R.1 ~~ neps12R.2
  neps14R.1 ~~ neps14R.2

  neps8R.1  ~~ neps10R.1
  neps8R.2  ~~ neps10R.2

  # Factor correlations (free)
  Eco_mother ~~ Ant_mother + Eco_father + Ant_father
  Ant_mother ~~ Eco_father + Ant_father
  Eco_father ~~ Ant_father

  # Factor means: mother = 0 (reference), father = free
  Eco_mother ~ 0*1
  Ant_mother ~ 0*1
  Eco_father ~ NA*1
  Ant_father ~ NA*1
'

fit_scalar <- cfa(model_scalar, data = data,
                  estimator = "MLR", missing = "fiml")

cat("\n--- Scalar Model Fit ---\n")
fitMeasures(fit_scalar, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                          "cfi.robust", "tli.robust", "rmsea.robust", "srmr",
                          "rmsea.ci.lower", "rmsea.ci.upper"))

# -------- Strict model (adds equal residual variances) -----------------
model_strict <- '
  Eco_mother  =~ L1*neps3.1  + L2*neps5.1  + L3*neps13.1 + L4*neps15.1
  Ant_mother  =~ L5*neps2R.1 + L6*neps4R.1 + L7*neps8R.1 + L8*neps10R.1 +
                 L9*neps12R.1 + L10*neps14R.1

  Eco_father  =~ L1*neps3.2  + L2*neps5.2  + L3*neps13.2 + L4*neps15.2
  Ant_father  =~ L5*neps2R.2 + L6*neps4R.2 + L7*neps8R.2 + L8*neps10R.2 +
                 L9*neps12R.2 + L10*neps14R.2

  # Intercepts (equal)
  neps3.1   ~ I1*1
  neps5.1   ~ I2*1
  neps13.1  ~ I3*1
  neps15.1  ~ I4*1
  neps2R.1  ~ I5*1
  neps4R.1  ~ I6*1
  neps8R.1  ~ I7*1
  neps10R.1 ~ I8*1
  neps12R.1 ~ I9*1
  neps14R.1 ~ I10*1

  neps3.2   ~ I1*1
  neps5.2   ~ I2*1
  neps13.2  ~ I3*1
  neps15.2  ~ I4*1
  neps2R.2  ~ I5*1
  neps4R.2  ~ I6*1
  neps8R.2  ~ I7*1
  neps10R.2 ~ I8*1
  neps12R.2 ~ I9*1
  neps14R.2 ~ I10*1

  # Residual variances (equal across parents)
  neps3.1   ~~ E1*neps3.1
  neps5.1   ~~ E2*neps5.1
  neps13.1  ~~ E3*neps13.1
  neps15.1  ~~ E4*neps15.1
  neps2R.1  ~~ E5*neps2R.1
  neps4R.1  ~~ E6*neps4R.1
  neps8R.1  ~~ E7*neps8R.1
  neps10R.1 ~~ E8*neps10R.1
  neps12R.1 ~~ E9*neps12R.1
  neps14R.1 ~~ E10*neps14R.1

  neps3.2   ~~ E1*neps3.2
  neps5.2   ~~ E2*neps5.2
  neps13.2  ~~ E3*neps13.2
  neps15.2  ~~ E4*neps15.2
  neps2R.2  ~~ E5*neps2R.2
  neps4R.2  ~~ E6*neps4R.2
  neps8R.2  ~~ E7*neps8R.2
  neps10R.2 ~~ E8*neps10R.2
  neps12R.2 ~~ E9*neps12R.2
  neps14R.2 ~~ E10*neps14R.2

  # Residual covariances (same as before, but now with constrained variances)
  neps3.1   ~~ neps3.2
  neps5.1   ~~ neps5.2
  neps13.1  ~~ neps13.2
  neps15.1  ~~ neps15.2
  neps2R.1  ~~ neps2R.2
  neps4R.1  ~~ neps4R.2
  neps8R.1  ~~ neps8R.2
  neps10R.1 ~~ neps10R.2
  neps12R.1 ~~ neps12R.2
  neps14R.1 ~~ neps14R.2

  neps8R.1  ~~ neps10R.1
  neps8R.2  ~~ neps10R.2

  # Factor correlations (free)
  Eco_mother ~~ Ant_mother + Eco_father + Ant_father
  Ant_mother ~~ Eco_father + Ant_father
  Eco_father ~~ Ant_father

  # Factor means (mother = 0, father free)
  Eco_mother ~ 0*1
  Ant_mother ~ 0*1
  Eco_father ~ mean_eco*1
  Ant_father ~ mean_ant*1
'

fit_strict <- cfa(model_strict, data = data,
                  estimator = "MLR", missing = "fiml")

cat("\n--- Strict Model Fit ---\n")
fitMeasures(fit_strict, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                          "cfi.robust", "tli.robust", "rmsea.robust", "srmr",
                          "rmsea.ci.lower", "rmsea.ci.upper"))

# 3. MODEL COMPARISON (LRT with Satorra‑Bentler correction) ------------------
comparacao <- lavTestLRT(fit_configural, fit_metric, fit_scalar, fit_strict,
                         method = "satorra.bentler.2010")
print(comparacao)

# Fit indices summary with ΔCFI and ΔRMSEA
tabela_fit <- data.frame(
  Modelo = c("Configural", "Metric", "Scalar", "Strict"),
  rbind(
    fitMeasures(fit_configural, c("chisq.scaled","df.scaled","cfi.robust",
                                  "tli.robust","rmsea.robust","srmr")),
    fitMeasures(fit_metric,    c("chisq.scaled","df.scaled","cfi.robust",
                                  "tli.robust","rmsea.robust","srmr")),
    fitMeasures(fit_scalar,    c("chisq.scaled","df.scaled","cfi.robust",
                                  "tli.robust","rmsea.robust","srmr")),
    fitMeasures(fit_strict,    c("chisq.scaled","df.scaled","cfi.robust",
                                  "tli.robust","rmsea.robust","srmr"))
  )
)
tabela_fit$Delta_CFI   <- c(NA, diff(tabela_fit$cfi.robust))
tabela_fit$Delta_RMSEA <- c(NA, diff(tabela_fit$rmsea.robust))

cat("\n--- Fit indices and Δ values ---\n")
print(tabela_fit)

# Decision criteria (Chen, 2007; Cheung & Rensvold, 2002):
#   Invariance is supported if ΔCFI <= -0.010 and ΔRMSEA <= 0.015
#   (LRT p‑value is also reported but is sensitive to sample size)

# 4. TESTS FOR FACTOR MEAN DIFFERENCES (Wald tests) --------------------------
# Check whether father means differ significantly from zero (mother reference)
cat("\n--- Wald tests for father factor means (from strict model) ---\n")
lavTestWald(fit_strict, constraints = "mean_eco == 0")
lavTestWald(fit_strict, constraints = "mean_ant == 0")


###############################################################################

