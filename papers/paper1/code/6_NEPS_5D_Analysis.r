# CONTENTS
#   1. Setup & data import
#   2. Variable lists (NEPS items)
#   3. Model specification (initial and final)
#   4. CFA - Mothers
#   5. CFA - Fathers
#   6. Internal consistency (Omega total - final model items only)
##############################################################################

library(haven)
library(lavaan)
library(semTools)
library(dplyr)
library(psych)

# 1. SETUP & DATA IMPORT ------------------------------------------------------
data <- read_sav("../data/data_clean.sav")

# 2. VARIABLE LISTS (NEPS ITEMS) ----------------------------------------------
varnames_mother <- c(
  "neps1.1", "neps2R.1", "neps3.1", "neps4R.1", "neps5.1",
  "neps6R.1", "neps7.1", "neps8R.1", "neps9.1", "neps10R.1",
  "neps11.1", "neps12R.1", "neps13.1", "neps14R.1", "neps15.1"
)

varnames_father <- c(
  "neps1.2", "neps2R.2", "neps3.2", "neps4R.2", "neps5.2",
  "neps6R.2", "neps7.2", "neps8R.2", "neps9.2", "neps10R.2",
  "neps11.2", "neps12R.2", "neps13.2", "neps14R.2", "neps15.2"
)

df_mother <- data %>%
  select(id_crianca, all_of(varnames_mother)) %>%
  rename(id = id_crianca, item1 = neps1.1, item2 = neps2R.1, item3 = neps3.1,
         item4 = neps4R.1, item5 = neps5.1, item6 = neps6R.1, item7 = neps7.1,
         item8 = neps8R.1, item9 = neps9.1, item10 = neps10R.1, item11 = neps11.1,
         item12 = neps12R.1, item13 = neps13.1, item14 = neps14R.1, item15 = neps15.1)

df_father <- data %>%
  select(id_crianca, all_of(varnames_father)) %>%
  rename(id = id_crianca, item1 = neps1.2, item2 = neps2R.2, item3 = neps3.2,
         item4 = neps4R.2, item5 = neps5.2, item6 = neps6R.2, item7 = neps7.2,
         item8 = neps8R.2, item9 = neps9.2, item10 = neps10R.2, item11 = neps11.2,
         item12 = neps12R.2, item13 = neps13.2, item14 = neps14R.2, item15 = neps15.2)

# 3. MODEL SPECIFICATION ------------------------------------------------------

# Initial model: 15 items, 5 correlated factors (all items load)
model_5_initial <- '
  Growth       =~ item1 + item6  + item11
  Antiantro    =~ item2 + item7  + item12
  Fragility    =~ item3 + item8  + item13
  Rejection    =~ item4 + item9  + item14
  Change       =~ item5 + item10 + item15
'

# Final model: as specified (items reduced and residual covariances added)
model_5_final <- '
  Growth              =~ item1 + item11
  Antiantropocentrism =~ item2 + item12
  Fragility           =~ item3 + item8 + item13
  Rejection           =~ item4 + item14
  Change              =~ item5 + item10 + item15

  item5 ~~ item15
  item8 ~~ item13
'

# 4. CFA - MOTHERS ------------------------------------------------------------

# Initial model
cfa_mother_5_init <- cfa(
  model     = model_5_initial,
  data      = df_mother,
  estimator = "MLR",
  missing   = "fiml",
  std.lv    = TRUE
)

cat("\n--- CFA Mothers - Initial Model (15 items, 5 factors) ---\n")
summary(cfa_mother_5_init, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Final model
cfa_mother_5 <- cfa(
  model     = model_5_final,
  data      = df_mother,
  estimator = "MLR",
  missing   = "fiml",
  std.lv    = TRUE
)

cat("\n--- CFA Mothers - Final Model Fit ---\n")
fitMeasures(cfa_mother_5, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                            "cfi.robust", "tli.robust", "rmsea.robust", "srmr"))

cat("\n--- Standardized Loadings (Mothers, Final) ---\n")
standardizedSolution(cfa_mother_5) %>%
  filter(op == "=~") %>%
  select(lhs, rhs, est.std, se, z, pvalue)

# 5. CFA - FATHERS ------------------------------------------------------------

# Initial model
cfa_father_5_init <- cfa(
  model     = model_5_initial,
  data      = df_father,
  estimator = "MLR",
  missing   = "fiml",
  std.lv    = TRUE
)

cat("\n--- CFA Fathers - Initial Model (15 items, 5 factors) ---\n")
summary(cfa_father_5_init, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Final model
cfa_father_5 <- cfa(
  model     = model_5_final,
  data      = df_father,
  estimator = "MLR",
  missing   = "fiml",
  std.lv    = TRUE
)

cat("\n--- CFA Fathers - Final Model Fit ---\n")
fitMeasures(cfa_father_5, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                            "cfi.robust", "tli.robust", "rmsea.robust", "srmr"))

cat("\n--- Standardized Loadings (Fathers, Final) ---\n")
standardizedSolution(cfa_father_5) %>%
  filter(op == "=~") %>%
  select(lhs, rhs, est.std, se, z, pvalue)

##############################################################################

