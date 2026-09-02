# CONTENTS
#   1. Setup & data import
#   2. Variable lists (items, subscales, dimensions) - using itemX nomenclature
#   3. Descriptive statistics function
#   4. Individual items - descriptive statistics
#   5. Composite scores - descriptive statistics
#      5.1 Total score
#      5.2 Bivariate structure (Eco / Anti-anthropocentrism)
#      5.3 Five-dimension structure
#   6. Compile & print descriptive results
#   7. Correlation matrix (mother lower triangle / father upper triangle)
#      7.1 Separate correlation matrices
#      7.2 Combine matrices
#      7.3 p-values
#      7.4 Combined matrix with significance stars
#      7.5 Print results
##############################################################################

library(haven)
library(moments)
library(Hmisc)

# 1. SETUP & DATA IMPORT ------------------------------------------------------
# Reads in the final cleaned dataset. Column names are renamed to item1...item15
# to match the nomenclature used in scripts 5, 6, and 7.

data <- read_sav("../data/data_clean.sav")
data <- as.data.frame(data)   # avoids RStudio bug with tbl_df/haven_labelled

cat("DATASET:", nrow(data), "children\n")

# Create mother and father data frames with standardised item names (item1...item15)
# This replicates the structure from scripts 5_NEPS_1D_Analysis.r, 
# 6_NEPS_5D_Analysis.r, and 7_NEPS_2D_Analysis.r.
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


# 2. VARIABLE LISTS (ITEMS, SUBSCALES, DIMENSIONS) ----------------------------
# All lists now use the standardised itemX nomenclature.

items_mother <- paste0("item", 1:15)
items_father <- paste0("item", 1:15)

# Bivariate structure: Ecocentrism (odd items) and Anthropocentrism (even items)
items_eco_mother   <- paste0("item", c(1, 3, 5, 7, 9, 11, 13, 15))
items_antro_mother <- paste0("item", c(2, 4, 6, 8, 10, 12, 14))
items_eco_father   <- paste0("item", c(1, 3, 5, 7, 9, 11, 13, 15))
items_antro_father <- paste0("item", c(2, 4, 6, 8, 10, 12, 14))

# Five-dimension structure (matches the initial 5D model in script 6)
dims_mother <- list(
  Growth      = c("item1", "item6",  "item11"),
  AntiAnthro  = c("item2", "item7",  "item12"),
  Fragility   = c("item3", "item8",  "item13"),
  Rejection   = c("item4", "item9",  "item14"),
  Change      = c("item5", "item10", "item15")
)

dims_father <- list(
  Growth      = c("item1", "item6",  "item11"),
  AntiAnthro  = c("item2", "item7",  "item12"),
  Fragility   = c("item3", "item8",  "item13"),
  Rejection   = c("item4", "item9",  "item14"),
  Change      = c("item5", "item10", "item15")
)


# 3. DESCRIPTIVE STATISTICS FUNCTION ------------------------------------------

desc_stats <- function(x) {
  x <- na.omit(x)
  c(
    N        = length(x),
    Mean     = mean(x),
    SD       = sd(x),
    Skewness = moments::skewness(x),
    Kurtosis = moments::kurtosis(x) - 3
  )
}

# 4. INDIVIDUAL ITEMS - DESCRIPTIVE STATISTICS --------------------------------

stats_items_mother <- as.data.frame(t(sapply(items_mother, function(col) desc_stats(df_mother[[col]]))))
stats_items_mother$Group <- "Mother"
stats_items_mother$Scale <- paste0("Item ", 1:15)

stats_items_father <- as.data.frame(t(sapply(items_father, function(col) desc_stats(df_father[[col]]))))
stats_items_father$Group <- "Father"
stats_items_father$Scale <- paste0("Item ", 1:15)


# 5. COMPOSITE SCORES - DESCRIPTIVE STATISTICS --------------------------------

## 5.1 Total score (unidimensional) -------------------------------------------
df_mother$total <- rowMeans(df_mother[, items_mother], na.rm = TRUE)
stats_1_mother <- as.data.frame(t(desc_stats(df_mother$total)))
stats_1_mother$Group <- "Mother"
stats_1_mother$Scale <- "NEP Total"

df_father$total <- rowMeans(df_father[, items_father], na.rm = TRUE)
stats_1_father <- as.data.frame(t(desc_stats(df_father$total)))
stats_1_father$Group <- "Father"
stats_1_father$Scale <- "NEP Total"

## 5.2 Bivariate structure (Eco / Anti-anthropocentrism) ----------------------
df_mother$eco       <- rowMeans(df_mother[, items_eco_mother],   na.rm = TRUE)
df_mother$antianthro <- rowMeans(df_mother[, items_antro_mother], na.rm = TRUE)

stats_2_mother <- rbind(
  data.frame(t(desc_stats(df_mother$eco)),        Group = "Mother", Scale = "Eco"),
  data.frame(t(desc_stats(df_mother$antianthro)), Group = "Mother", Scale = "AntiAnthro")
)

df_father$eco       <- rowMeans(df_father[, items_eco_father],   na.rm = TRUE)
df_father$antianthro <- rowMeans(df_father[, items_antro_father], na.rm = TRUE)

stats_2_father <- rbind(
  data.frame(t(desc_stats(df_father$eco)),        Group = "Father", Scale = "Eco"),
  data.frame(t(desc_stats(df_father$antianthro)), Group = "Father", Scale = "AntiAnthro")
)

## 5.3 Five‑dimension structure ------------------------------------------------
for (dim_name in names(dims_mother)) {
  df_mother[[paste0(dim_name, "_mother")]] <- rowMeans(df_mother[, dims_mother[[dim_name]]], na.rm = TRUE)
}

stats_5_mother <- do.call(rbind, lapply(names(dims_mother), function(dim_name) {
  data.frame(t(desc_stats(df_mother[[paste0(dim_name, "_mother")]])), Group = "Mother", Scale = dim_name)
}))

for (dim_name in names(dims_father)) {
  df_father[[paste0(dim_name, "_father")]] <- rowMeans(df_father[, dims_father[[dim_name]]], na.rm = TRUE)
}

stats_5_father <- do.call(rbind, lapply(names(dims_father), function(dim_name) {
  data.frame(t(desc_stats(df_father[[paste0(dim_name, "_father")]])), Group = "Father", Scale = dim_name)
}))


# 6. COMPILE & PRINT DESCRIPTIVE RESULTS --------------------------------------

# Ensure all dataframes have the same column names (ignore order)
# Define a function to standardize column names
standardize_cols <- function(df) {
  # Rename columns if necessary (in case of minor variations)
  names(df) <- gsub("^Skew$", "Skewness", names(df))
  names(df) <- gsub("^Kurt$", "Kurtosis", names(df))
  # Reorder to common order
  desired <- c("N", "Mean", "SD", "Skewness", "Kurtosis", "Group", "Scale")
  # Check which columns exist
  existing <- intersect(desired, names(df))
  # If any missing, add them as NA
  for (col in setdiff(desired, names(df))) {
    df[[col]] <- NA
  }
  # Reorder
  df <- df[, desired]
  return(df)
}

# Apply to all dataframes
stats_items_mother <- standardize_cols(stats_items_mother)
stats_items_father <- standardize_cols(stats_items_father)
stats_1_mother <- standardize_cols(stats_1_mother)
stats_1_father <- standardize_cols(stats_1_father)
stats_2_mother <- standardize_cols(stats_2_mother)
stats_2_father <- standardize_cols(stats_2_father)
stats_5_mother <- standardize_cols(stats_5_mother)
stats_5_father <- standardize_cols(stats_5_father)

# Now combine using bind_rows (from dplyr) which is order-agnostic
library(dplyr)
results_items <- bind_rows(stats_items_mother, stats_items_father)
results_composite <- bind_rows(stats_1_mother, stats_1_father,
                               stats_2_mother, stats_2_father,
                               stats_5_mother, stats_5_father)

# Final display order (already in desired order after standardization)
col_order <- c("Group", "Scale", "N", "Mean", "SD", "Skewness", "Kurtosis")
results_items <- results_items[, col_order]
results_composite <- results_composite[, col_order]

# Rounding function
round_df <- function(df) {
  df[, c("N", "Mean", "SD", "Skewness", "Kurtosis")] <-
    round(df[, c("N", "Mean", "SD", "Skewness", "Kurtosis")], 3)
  df
}

cat("\n========================================================\n")
cat("  INDIVIDUAL ITEMS\n")
cat("\n========================================================\n")
print(round_df(results_items[order(results_items$Group, results_items$Scale), ]))

cat("\n========================================================\n")
cat("  COMPOSITE SCORES\n")
cat("  (Total | Bivariate | 5 Dimensions)\n")
cat("\n========================================================\n")
print(round_df(results_composite))


# 7. CORRELATION MATRIX (MOTHER LOWER TRIANGLE / FATHER UPPER TRIANGLE) -------

## 7.1 Separate correlation matrices ------------------------------------------
mother_items_df <- df_mother[, items_mother]
father_items_df <- df_father[, items_father]

item_names <- paste0("NEP", 1:15)
colnames(mother_items_df) <- item_names
colnames(father_items_df) <- item_names

cor_mother <- cor(mother_items_df, use = "pairwise.complete.obs", method = "pearson")
cor_father <- cor(father_items_df, use = "pairwise.complete.obs", method = "pearson")

## 7.2 Combine matrices: mother lower triangle, father upper triangle --------
cor_combined <- matrix(NA, nrow = 15, ncol = 15)
rownames(cor_combined) <- item_names
colnames(cor_combined) <- item_names

for (i in 1:15) {
  for (j in 1:15) {
    if (i == j) {
      cor_combined[i, j] <- NA               # empty diagonal
    } else if (i > j) {
      cor_combined[i, j] <- cor_mother[i, j] # lower triangle = mother
    } else {
      cor_combined[i, j] <- cor_father[i, j] # upper triangle = father
    }
  }
}

## 7.3 p-values -----------------------------------------------------------
cor_mother_hmisc <- rcorr(as.matrix(mother_items_df), type = "pearson")
cor_father_hmisc <- rcorr(as.matrix(father_items_df), type = "pearson")

pval_mother <- cor_mother_hmisc$P
pval_father <- cor_father_hmisc$P

pval_combined <- matrix(NA, nrow = 15, ncol = 15)
rownames(pval_combined) <- item_names
colnames(pval_combined) <- item_names

for (i in 1:15) {
  for (j in 1:15) {
    if (i > j) {
      pval_combined[i, j] <- pval_mother[i, j]
    } else if (i < j) {
      pval_combined[i, j] <- pval_father[i, j]
    }
  }
}

## 7.4 Combined matrix with significance stars --------------------------------
sig_stars <- function(r, p) {
  if (is.na(r) || is.na(p)) return("-")
  stars <- ifelse(p < .001, "***",
                  ifelse(p < .01,  "**",
                         ifelse(p < .05,  "*", "")))
  paste0(formatC(round(r, 3), format = "f", digits = 3), stars)
}

cor_stars <- matrix("-", nrow = 15, ncol = 15)
rownames(cor_stars) <- item_names
colnames(cor_stars) <- item_names

for (i in 1:15) {
  for (j in 1:15) {
    if (i != j) {
      cor_stars[i, j] <- sig_stars(cor_combined[i, j], pval_combined[i, j])
    }
  }
}

## 7.5 Print results -----------------------------------------------------
cat("\n========================================================\n")
cat("  CORRELATION MATRIX\n")
cat("  Lower triangle = Mother | Upper triangle = Father\n")
cat("========================================================\n")
print(round(cor_combined, 3), na.print = "-")

cat("\n========================================================\n")
cat("  p-VALUE MATRIX\n")
cat("  Lower triangle = Mother | Upper triangle = Father\n")
cat("========================================================\n")
print(round(pval_combined, 3), na.print = "-")

cat("\n========================================================\n")
cat("  MATRIX WITH SIGNIFICANCE STARS (* p<.05  ** p<.01  *** p<.001)\n")
cat("  Lower triangle = Mother | Upper triangle = Father\n")
cat("========================================================\n")
print(cor_stars, quote = FALSE)
##############################################################################

