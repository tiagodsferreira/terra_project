# CONTENTS
#   1. Setup & data import
#   2. Study criteria exclusion
#   3. Reverse-code items
#   4. Save prepared data
##############################################################################

# 1. SETUP & DATA IMPORT ----------------------------------------------------

#install.packages("haven")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("lubridate")
#install.packages("naniar")
#install.packages("mice")
#install.packages("moments")
#install.packages("Hmisc")
#install.packages("lavaan")
#install.packages("semTools")
#install.packages("psych")

library(haven)
library(dplyr)
library(tidyr)
library(lubridate)
library(naniar)
library(mice)
library(moments)
library(Hmisc)
library(lavaan)
library(semTools)
library(psych)

data <- read_sav("../data/data.sav")


# 2. STUDY CRITERIA EXCLUSION -----------------------------------------------

cols_mother <- names(data)[grepl("\\.1$", names(data))]
cols_father <- names(data)[grepl("\\.2$", names(data))]

## 25.50.moli2702 — non‑binary person tagged as .2 (father)
idx_nb <- which(data$id_crianca == "25.50.moli2702")
data[idx_nb, cols_father] <- NA

## 1.1.epar2202 — same‑sex couple
idx_same_sex <- which(data$id_crianca == "11.21.evic1503")
data[idx_same_sex, cols_mother] <- NA
data[idx_same_sex, cols_father] <- NA

# 3. REVERSE-CODE ITEMS -----------------------------------------------------

even_itens <- c(2, 4, 6, 8, 10, 12, 14)

for (i in even_itens) {
  data[[paste0("neps", i, "R.1")]] <- 6 - data[[paste0("neps", i, ".1")]]
  data[[paste0("neps", i, "R.2")]] <- 6 - data[[paste0("neps", i, ".2")]]
}

itens_cols_mother <- c(
  "neps1.1",  "neps2R.1", "neps3.1",  "neps4R.1",  "neps5.1",
  "neps6R.1", "neps7.1",  "neps8R.1", "neps9.1",   "neps10R.1",
  "neps11.1", "neps12R.1","neps13.1", "neps14R.1", "neps15.1"
)

itens_cols_father <- c(
  "neps1.2",  "neps2R.2", "neps3.2",  "neps4R.2",  "neps5.2",
  "neps6R.2", "neps7.2",  "neps8R.2", "neps9.2",   "neps10R.2",
  "neps11.2", "neps12R.2","neps13.2", "neps14R.2", "neps15.2"
)


# 4. SAVE PREPARED DATA -----------------------------------------------------

write_sav(data, "../data/data_preped.sav")

###############################################################################
