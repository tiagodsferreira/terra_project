# CONTENTS
#   1. Setup & data import
#   2. Descriptive & frequency statistics functions
#   3. Age descriptives (mother / father / child)
#   4. Gender distribution (mother / father / child)
#   5. Education distribution (mother / father)
#   6. Professional status distribution (mother / father)
##############################################################################

library(haven)
library(moments)

# 1. SETUP & DATA IMPORT ------------------------------------------------------

data_clean <- read_sav("../data/data_clean.sav")

cat("DATASET:", nrow(data_clean), "children\n")

# --- NEW: Information completeness for .1 and .2 columns ---------------------
# Identify all columns whose names end with ".1" or ".2"
cols_1 <- grep("\\.1$", names(data_clean), value = TRUE)
cols_2 <- grep("\\.2$", names(data_clean), value = TRUE)

# For each row, check if at least one column in the respective group is non‑NA
has_1 <- apply(data_clean[cols_1], 1, function(row) any(!is.na(row)))
has_2 <- apply(data_clean[cols_2], 1, function(row) any(!is.na(row)))

# Counts
n_has_1 <- sum(has_1)
n_has_2 <- sum(has_2)
n_both  <- sum(has_1 & has_2)

cat("\n--- Information completeness for .1 and .2 columns ---\n")
cat("Columns ending with .1:", paste(cols_1, collapse = ", "), "\n")
cat("Columns ending with .2:", paste(cols_2, collapse = ", "), "\n")
cat("Rows with at least one response in .1 columns:", n_has_1, "\n")
cat("Rows with at least one response in .2 columns:", n_has_2, "\n")
cat("Rows with information in both .1 and .2:", n_both, "\n\n")

# 2. DESCRIPTIVE & FREQUENCY STATISTICS FUNCTIONS -----------------------------

## 2.1 Continuous variables: N, Mean, SD, Skewness, Kurtosis ------------------
desc_stats <- function(x, label = NULL) {
  x <- na.omit(x)
  res <- c(
    N        = length(x),
    Mean     = mean(x),
    SD       = sd(x),
    Skewness = skewness(x),
    Kurtosis = kurtosis(x) - 3
  )
  if (!is.null(label)) cat("\n---", label, "---\n")
  print(res)
  invisible(res)
}

## 2.2 Categorical variables: absolute and percentage frequencies -------------
freq_stats <- function(x, label = NULL) {
  x <- na.omit(x)

  freq_abs  <- table(x)
  freq_perc <- round(prop.table(freq_abs) * 100, 2)

  res <- data.frame(
    Category = names(freq_abs),
    N        = as.vector(freq_abs),
    Percent  = as.vector(freq_perc)
  )

  if (!is.null(label)) cat("\n---", label, "---\n")
  print(res, row.names = FALSE)
  invisible(res)
}


# 3. AGE DESCRIPTIVES ----------------------------------------------------------

desc_stats(data_clean$mother_age, "Mother's age (years)")
desc_stats(data_clean$father_age, "Father's age (years)")
desc_stats(data_clean$child_age,  "Child's age (years)")


# 4. GENDER DISTRIBUTION -------------------------------------------------------

gender_vars <- list(
  Mother = "genero.1",
  Father = "genero.2",
  Child  = "gender_child"
)

for (label in names(gender_vars)) {
  var <- data_clean[[gender_vars[[label]]]]

  cat("\n------------------------------------------------------------\n")
  cat("GENDER -", toupper(label), "\n")
  cat(sprintf("valid n = %d\n", sum(!is.na(var))))
  print(table(var, useNA = "ifany"))
  cat("Proportion (%):\n")
  print(round(prop.table(table(var, useNA = "ifany")) * 100, 1))
}


# 5. EDUCATION DISTRIBUTION ----------------------------------------------------

freq_stats(data_clean$escolaridade.1, "Education - Mother")
freq_stats(data_clean$escolaridade.2, "Education - Father")


# 6. PROFESSIONAL STATUS DISTRIBUTION ------------------------------------------

freq_stats(data_clean$situ_prof.1, "Professional status - Mother")
freq_stats(data_clean$situ_prof.2, "Professional status - Father")

################################################################################