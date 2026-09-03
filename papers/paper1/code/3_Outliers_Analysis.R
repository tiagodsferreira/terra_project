# CONTENTS
#   1. Setup & data import
#   2. Variable lists (NEPS items only)
#   3. Outlier detection function (univariate Z-scores + multivariate Mahalanobis)
#   4. Run analysis (mother / father)
#   5. Print results
#   6. Create cleaned dataset (multivariate outliers set to NA)
##############################################################################

library(haven)
library(dplyr)
library(tidyr)

# 1. SETUP & DATA IMPORT ------------------------------------------------------


data <- read_sav("../data/data_preped.sav")


cat("DATASET:", nrow(data), "children\n")


# 2. VARIABLE LISTS (NEPS ITEMS ONLY) -----------------------------------------

varnames_mother <- paste0("neps", 1:15, ".1")
varnames_father <- paste0("neps", 1:15, ".2")


# 3. OUTLIER DETECTION FUNCTION -----------------------------------------------
# Univariate: Z-score, |Z| > 3
# Multivariate: Mahalanobis distance, p < .001

detect_outliers <- function(df, id_col, vars, group_name) {

  group_data <- df[, c(id_col, vars)]

  # Remove rows with all values missing
  complete_rows <- group_data[rowSums(is.na(group_data[, vars])) < length(vars), ]

  ids <- complete_rows[[id_col]]

  # -- Univariate: Z-score |Z| > 3 --------------------------------------------
  z_matrix <- scale(complete_rows[, vars])
  outlier_uni_flags <- abs(z_matrix) > 3
  outlier_uni_flags[is.na(outlier_uni_flags)] <- FALSE

  uni_details <- lapply(seq_len(nrow(complete_rows)), function(i) {
    vars_flag <- which(outlier_uni_flags[i, ])
    if (length(vars_flag) == 0) return(NULL)
    data.frame(
      id_crianca = ids[i],
      variable   = vars[vars_flag],
      z_score    = round(z_matrix[i, vars_flag], 3)
    )
  })
  uni_df <- do.call(rbind, Filter(Negate(is.null), uni_details))

  ids_uni <- if (!is.null(uni_df)) unique(uni_df$id_crianca) else character(0)

  # -- Multivariate: Mahalanobis distance (requires complete data) -----------
  complete_data <- complete_rows[complete.cases(complete_rows[, vars]), ]
  ids_complete  <- complete_data[[id_col]]

  mahal_ids_out <- character(0)
  mahal_df      <- NULL

  if (nrow(complete_data) > length(vars) + 1) {
    X      <- as.matrix(complete_data[, vars])
    center <- colMeans(X)
    S      <- cov(X)

    if (det(S) != 0) {
      D2    <- mahalanobis(X, center = center, cov = S)
      gl    <- length(vars)
      p_val <- pchisq(D2, df = gl, lower.tail = FALSE)

      mahal_df <- data.frame(
        id_crianca    = ids_complete,
        mahal_D2      = round(D2, 3),
        p_value       = round(p_val, 4),
        outlier_multi = p_val < .001
      )

      mahal_ids_out <- mahal_df$id_crianca[mahal_df$outlier_multi]
    } else {
      warning(paste("Singular covariance matrix for", group_name,
                    "- multivariate outliers not calculated."))
    }
  } else {
    warning(paste("Insufficient cases for Mahalanobis distance in", group_name))
  }

  # -- Summary per participant -------------------------------------------------
  all_ids_out <- unique(c(ids_uni, mahal_ids_out))

  if (length(all_ids_out) == 0) {
    summary_df <- data.frame(
      id_crianca    = character(0),
      outlier_uni   = logical(0),
      outlier_multi = logical(0),
      type          = character(0)
    )
  } else {
    summary_df <- data.frame(
      id_crianca    = all_ids_out,
      outlier_uni   = all_ids_out %in% ids_uni,
      outlier_multi = all_ids_out %in% mahal_ids_out
    ) %>%
      mutate(type = case_when(
        outlier_uni & outlier_multi ~ "Univariate + Multivariate",
        outlier_uni                 ~ "Univariate",
        outlier_multi               ~ "Multivariate"
      )) %>%
      arrange(id_crianca)
  }

  list(
    group        = group_name,
    uni_details  = uni_df,
    mahal_df     = mahal_df,
    summary      = summary_df
  )
}


# 4. RUN ANALYSIS --------------------------------------------------------------

res_mother <- detect_outliers(data, "id_crianca", varnames_mother, "Mother")
res_father <- detect_outliers(data, "id_crianca", varnames_father, "Father")


# 5. PRINT RESULTS --------------------------------------------------------------

print_results <- function(res) {
  cat("GROUP:", res$group, "\n")

  # -- Univariate --
  cat("[ UNIVARIATE OUTLIERS - |Z| > 3 ]\n\n")
  if (is.null(res$uni_details) || nrow(res$uni_details) == 0) {
    cat("  No univariate outliers detected.\n\n")
  } else {
    print(res$uni_details, row.names = FALSE)
    cat("\n")
  }

  # -- Multivariate --
  cat("[ MULTIVARIATE OUTLIERS - Mahalanobis (p < .001) ]\n\n")
  if (is.null(res$mahal_df)) {
    cat("  Analysis not performed (insufficient data or singular matrix).\n\n")
  } else {
    out_m <- res$mahal_df[res$mahal_df$outlier_multi, ]
    if (nrow(out_m) == 0) {
      cat("  No multivariate outliers detected.\n\n")
    } else {
      print(out_m[, c("id_crianca", "mahal_D2", "p_value")], row.names = FALSE)
      cat("\n")
    }
  }

  # -- Final summary --
  cat("[ SUMMARY - child IDs flagged as outliers (", res$group, ") ]\n\n")
  if (nrow(res$summary) == 0) {
    cat("  No outliers detected.\n\n")
  } else {
    print(res$summary, row.names = FALSE)
    cat("\n")
  }
}

print_results(res_mother)
print_results(res_father)


# 6. CREATE CLEANED DATASET (OUTLIERS REMOVED) --------------------------------
# Decision rule: only participants that were multivariate (Mahalanobis) outliers
# AND univariate were excluded.

ids_remove_mother <- c("1.1.fmar2602", 
                       "10.14.raze2704", 
                       "10.14.vcoe0204",
                       "11.19.cval2004",
                       "12.24.enav3003",
                       "19.38.cmou0404",
                       "3.5.isan2010",
                       "8.12.mroc2508")

ids_remove_father <- c("11.20.vcas2508",
                       "12.22.rlem1007",
                       "12.24.enav3003",
                       "2.4.bcal1507",
                       "5.6.msil0811")

# All columns belonging to each respondent (same logic as in the data prep script)
cols_mother <- names(data)[grepl("\\.1$", names(data))]
cols_father <- names(data)[grepl("\\.2$", names(data))]

data_clean <- data

# Set ALL mother columns to NA for flagged mother outliers
data_clean[data_clean$id_crianca %in% ids_remove_mother, cols_mother] <- NA

# Set ALL father columns to NA for flagged father outliers
data_clean[data_clean$id_crianca %in% ids_remove_father, cols_father] <- NA

# Children where BOTH parents were flagged: drop the row completely
ids_remove_both <- intersect(ids_remove_mother, ids_remove_father)
data_clean <- data_clean[!(data_clean$id_crianca %in% ids_remove_both), ]

# 7. REMOVE ROWS WITH NO VALID NEPS ITEMS FROM EITHER PARENT ------------------

all_neps_vars <- c(varnames_mother, varnames_father)

# Identify rows where ALL NEPS items (mother + father) are NA
rows_all_na <- apply(data_clean[, all_neps_vars], 1, function(x) all(is.na(x)))

# Remove those rows
data_clean <- data_clean[!rows_all_na, ]

# Verification
cat("\n----------------------------------------\n")
cat("VERIFICATION\n")
cat("Original cases                   :", nrow(data), "\n")
cat("Final file cases                 :", nrow(data_clean), "\n")
cat("Mother columns set to NA         :", sum(data$id_crianca %in% setdiff(ids_remove_mother, ids_remove_both)), "\n")
cat("Father columns set to NA         :", sum(data$id_crianca %in% setdiff(ids_remove_father, ids_remove_both)), "\n")
cat("Children fully removed (both)    :", length(ids_remove_both), "\n")
cat("Additional rows removed (no NEPS):", nrow(data) - nrow(data_clean) - length(ids_remove_both), "\n")
cat("----------------------------------------\n")

write_sav(data_clean, "../data/data_clean.sav")

##############################################################################
