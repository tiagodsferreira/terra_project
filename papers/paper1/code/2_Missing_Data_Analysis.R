# CONTENTS
#   1. Setup & data import
#   2. Building DF_MAIN (variable selection)
#   3. Complete case analysis
#   4. Missing data visualization
#   5. Missing data patterns (mice / naniar)
#   6. Little's MCAR test
#   7. MAR check: missingness counts per row
#   8. MAR check: association with demographic/child variables
#   9. MAR check: logistic regression models (mother / father)
##############################################################################

# install.packages("naniar")
# install.packages("mice")
library(haven)
library(naniar)
library(mice)

# 1. SETUP & DATA IMPORT ----------------------------------------------------

data <- read_sav("../data/data_preped.sav")

sum(duplicated(data$id_crianca))      # sanity check: no duplicate child ids

# 2. BUILDING DF_MAIN (VARIABLE SELECTION) -----------------------------------

varnames <- c(
  "id_crianca", "child_age", "gender_child",
  # -- mother block --
  "escolaridade.1", "situ_prof.1", "n_agreg_fam.1", "n_criancas_agreg.1",
  "rend_mens_liq.1", "horas_trab_sem.1", "mother_age",
  "neps1.1", "neps2.1", "neps3.1", "neps4.1", "neps5.1", "neps6.1", "neps7.1",
  "neps8.1", "neps9.1", "neps10.1", "neps11.1", "neps12.1", "neps13.1",
  "neps14.1", "neps15.1",
  # -- father block --
  "escolaridade.2", "situ_prof.2", "n_agreg_fam.2", "n_criancas_agreg.2",
  "rend_mens_liq.2", "horas_trab_sem.2", "father_age",
  "neps1.2", "neps2.2", "neps3.2", "neps4.2", "neps5.2", "neps6.2", "neps7.2",
  "neps8.2", "neps9.2", "neps10.2", "neps11.2", "neps12.2", "neps13.2",
  "neps14.2", "neps15.2"
)

DF_MAIN <- data[, varnames]


# 3. COMPLETE CASE ANALYSIS --------------------------------------------------

varnames_mother <- paste0("neps", 1:15, ".1")
varnames_father <- paste0("neps", 1:15, ".2")

## 3.1 Complete cases, various slices -----------------------------------
sum(complete.cases(DF_MAIN[, -1]))                               # excluding id column
sum(complete.cases(DF_MAIN[, c(varnames_mother, varnames_father)]))

sum(complete.cases(DF_MAIN[, varnames_mother]))                  # mother NEPS only
sum(complete.cases(DF_MAIN[, varnames_father]))                  # father NEPS only

mean(complete.cases(DF_MAIN[, varnames_mother]))                 # proportion, mother
mean(complete.cases(DF_MAIN[, varnames_father]))                 # proportion, father

## 3.2 Rows with at least one missing value ----------------------------------
sum(!complete.cases(DF_MAIN[, c(varnames_mother, varnames_father)]))
sum(complete.cases(DF_MAIN[, c(varnames_mother, varnames_father)]))
mean(complete.cases(DF_MAIN[, c(varnames_mother, varnames_father)]))

## 3.3 Missingness per column -------------------------------------------
colSums(is.na(DF_MAIN[, c(varnames_mother, varnames_father)]))
colMeans(is.na(DF_MAIN[, c(varnames_mother, varnames_father)])) * 100


# 4. MISSING DATA VISUALIZATION ----------------------------------------------

miss_var_summary(DF_MAIN[, c(varnames_mother, varnames_father)])  # counts/% per variable
gg_miss_var(DF_MAIN[, c(varnames_mother, varnames_father)])       # quick plot
vis_miss(DF_MAIN[, c(varnames_mother, varnames_father)])          # heatmap overview


# 5. MISSING DATA PATTERNS ---------------------------------------------------

md.pattern(DF_MAIN[, c(varnames_mother, varnames_father)])        # pattern table
gg_miss_upset(DF_MAIN[, c(varnames_mother, varnames_father)])     # upset plot: intersections
vis_miss(DF_MAIN[, c(varnames_mother, varnames_father)], cluster = TRUE)  # clustered heatmap


# 6. LITTLE'S MCAR TEST -------------------------------------------------------
mcar_test(DF_MAIN[, c(varnames_mother, varnames_father)])
# [ATUALIZAR após rodar] Resultado da rodada anterior: χ²(225) = 222, p = .536
# -> não rejeitava MCAR. Conferir se ainda se mantém com esta base.


# 7. MAR CHECK: MISSINGNESS COUNTS PER ROW -----------------------------------
DF_MAIN$n_missing_neps <- rowSums(is.na(DF_MAIN[, c(varnames_mother, varnames_father)]))
DF_MAIN$n_missing_neps_mother <- rowSums(is.na(DF_MAIN[, varnames_mother]))
DF_MAIN$n_missing_neps_father <- rowSums(is.na(DF_MAIN[, varnames_father]))

table(DF_MAIN$n_missing_neps)
summary(DF_MAIN$n_missing_neps)


# 8. MAR CHECK: ASSOCIATION WITH DEMOGRAPHIC/CHILD VARIABLES -----------------

## 8.1 Continuous predictors (correlation) -----------------------------------
cor.test(DF_MAIN$n_missing_neps, DF_MAIN$mother_age, use = "complete.obs")
cor.test(DF_MAIN$n_missing_neps, DF_MAIN$father_age, use = "complete.obs")
cor.test(DF_MAIN$n_missing_neps, DF_MAIN$rend_mens_liq.1, use = "complete.obs")

## 8.2 Categorical predictors (group comparisons) ----------------------------
DF_MAIN$any_missing <- DF_MAIN$n_missing_neps > 0

t.test(mother_age ~ any_missing, data = DF_MAIN)
t.test(rend_mens_liq.1 ~ any_missing, data = DF_MAIN)

chisq.test(table(DF_MAIN$escolaridade.1, DF_MAIN$any_missing))
chisq.test(table(DF_MAIN$situ_prof.1, DF_MAIN$any_missing))


# 9. MAR CHECK: LOGISTIC REGRESSION MODELS (MOTHER / FATHER) -----------------
# Missingness tends to be all-or-nothing per parent, so mother and father
# missingness are modeled separately using each parent's own characteristics
# plus child-level variables.

DF_MAIN$mother_any_missing <- DF_MAIN$n_missing_neps_mother > 0
DF_MAIN$father_any_missing <- DF_MAIN$n_missing_neps_father > 0

table(DF_MAIN$mother_any_missing)
table(DF_MAIN$father_any_missing)

## 9.1 Mother missingness model ----------------------------------------------
mar_mother <- glm(
  mother_any_missing ~ mother_age + escolaridade.1 + situ_prof.1 +
    n_agreg_fam.1 + n_criancas_agreg.1 + rend_mens_liq.1 +
    horas_trab_sem.1 + child_age + gender_child,
  data = DF_MAIN, family = binomial
)
summary(mar_mother)

## 9.2 Father missingness model ----------------------------------------------
mar_father <- glm(
  father_any_missing ~ father_age + escolaridade.2 + situ_prof.2 +
    n_agreg_fam.2 + n_criancas_agreg.2 + rend_mens_liq.2 +
    horas_trab_sem.2 + child_age + gender_child,
  data = DF_MAIN, family = binomial
)
summary(mar_father)

## 9.3 Participation counts (any response, even if some items missing) ------
DF_MAIN$mother_responded <- rowSums(!is.na(DF_MAIN[, varnames_mother])) > 0
DF_MAIN$father_responded <- rowSums(!is.na(DF_MAIN[, varnames_father])) > 0

sum(DF_MAIN$mother_responded)                                # mothers who answered >= 1 NEPS item
sum(DF_MAIN$father_responded)                                # fathers who answered >= 1 NEPS item

# Couple-level breakdown
table(mother = DF_MAIN$mother_responded, father = DF_MAIN$father_responded)

sum(DF_MAIN$mother_responded & DF_MAIN$father_responded)     # both responded (complete couples)
sum(DF_MAIN$mother_responded & !DF_MAIN$father_responded)    # only mother responded
sum(!DF_MAIN$mother_responded & DF_MAIN$father_responded)    # only father responded
sum(!DF_MAIN$mother_responded & !DF_MAIN$father_responded)   # neither responded

# Proportions (%)
mean(DF_MAIN$mother_responded & DF_MAIN$father_responded) * 100
mean(DF_MAIN$mother_responded & !DF_MAIN$father_responded) * 100
mean(!DF_MAIN$mother_responded & DF_MAIN$father_responded) * 100
mean(!DF_MAIN$mother_responded & !DF_MAIN$father_responded) * 100

##############################################################################