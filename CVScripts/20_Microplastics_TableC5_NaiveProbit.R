#### Microplastics: IOP Paper ####
## Function: Naive probit
## Author: PK
## Last change: 08/09/25
# Changes:
# - This is script 05 but no bootstrapping



# ***********************************************************
# Replication Information: ####
# ***********************************************************
# This script performs in-text specification.
# Session information is saved to a separate file 'session_info.txt'

# ─ Session info ─────────────────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.0 (2025-04-11 ucrt)
# os       Windows 11 x64 (build 26200)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United Kingdom.utf8
# ctype    English_United Kingdom.utf8
# tz       Europe/London
# date     2026-03-23
# rstudio  2023.06.2+561 Mountain Hydrangea (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/earpkin/AppData/Local/Temp/RtmpcBhVDh/file692053785875". Did you mean command "create-project"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────────────────────
# package      * version date (UTC) lib source
# abind          1.4-8   2024-09-12 [1] CRAN (R 4.5.0)
# AER          * 1.2-14  2024-09-28 [1] CRAN (R 4.5.0)
# backports      1.5.0   2024-05-23 [1] CRAN (R 4.5.0)
# betareg      * 3.2-3   2025-05-10 [1] CRAN (R 4.5.0)
# biglm        * 0.9-3   2024-06-12 [1] CRAN (R 4.5.0)
# boot         * 1.3-31  2024-08-28 [1] CRAN (R 4.5.0)
# broom          1.0.8   2025-03-28 [1] CRAN (R 4.5.0)
# car          * 3.1-3   2024-09-27 [1] CRAN (R 4.5.0)
# carData      * 3.0-5   2022-01-06 [1] CRAN (R 4.5.0)
# cli            3.6.5   2025-04-23 [1] CRAN (R 4.5.0)
# data.table   * 1.17.2  2025-05-12 [1] CRAN (R 4.5.0)
# DBI          * 1.2.3   2024-06-02 [1] CRAN (R 4.5.0)
# DCchoice     * 0.2.0   2023-07-10 [1] CRAN (R 4.5.0)
# dichromat      2.0-0.1 2022-05-02 [1] CRAN (R 4.5.0)
# dplyr        * 1.1.4   2023-11-17 [1] CRAN (R 4.5.0)
# evaluate       1.0.3   2025-01-10 [1] CRAN (R 4.5.0)
# farver         2.1.2   2024-05-13 [1] CRAN (R 4.5.0)
# flexmix        2.3-20  2025-02-28 [1] CRAN (R 4.5.0)
# forcats      * 1.0.0   2023-01-29 [1] CRAN (R 4.5.0)
# Formula        1.2-5   2023-02-24 [1] CRAN (R 4.5.0)
# generics       0.1.4   2025-05-09 [1] CRAN (R 4.5.0)
# ggplot2      * 3.5.2   2025-04-09 [1] CRAN (R 4.5.0)
# ggpubr       * 0.6.0   2023-02-10 [1] CRAN (R 4.5.0)
# ggsignif       0.6.4   2022-10-13 [1] CRAN (R 4.5.0)
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.5.0)
# glue           1.8.0   2024-09-30 [1] CRAN (R 4.5.0)
# gridExtra      2.3     2017-09-09 [1] CRAN (R 4.5.0)
# gridtext       0.1.5   2022-09-16 [1] CRAN (R 4.5.0)
# gtable         0.3.6   2024-10-25 [1] CRAN (R 4.5.0)
# here         * 1.0.1   2020-12-13 [1] CRAN (R 4.5.0)
# hms            1.1.3   2023-03-21 [1] CRAN (R 4.5.0)
# Icens          1.72.0  2023-04-25 [1] Bioconductor
# interval       1.1-1.0 2023-08-24 [1] CRAN (R 4.5.0)
# janitor      * 2.2.1   2024-12-22 [1] CRAN (R 4.5.0)
# km.ci          0.5-6   2022-04-06 [1] CRAN (R 4.5.0)
# KMsurv         0.1-5   2012-12-03 [1] CRAN (R 4.5.0)
# knitr          1.50    2025-03-16 [1] CRAN (R 4.5.0)
# lattice        0.22-7  2025-04-02 [1] CRAN (R 4.5.0)
# lifecycle      1.0.4   2023-11-07 [1] CRAN (R 4.5.0)
# lmtest       * 0.9-40  2022-03-21 [1] CRAN (R 4.5.0)
# lubridate    * 1.9.4   2024-12-08 [1] CRAN (R 4.5.0)
# magrittr     * 2.0.3   2022-03-30 [1] CRAN (R 4.5.0)
# MASS         * 7.3-65  2025-02-28 [1] CRAN (R 4.5.0)
# Matrix       * 1.7-3   2025-03-11 [1] CRAN (R 4.5.0)
# MLEcens        0.1-7.1 2024-09-21 [1] CRAN (R 4.5.0)
# modeltools     0.2-24  2025-05-02 [1] CRAN (R 4.5.0)
# nnet           7.3-20  2025-01-01 [1] CRAN (R 4.5.0)
# perm           1.0-0.4 2023-08-24 [1] CRAN (R 4.5.0)
# pillar         1.10.2  2025-04-05 [1] CRAN (R 4.5.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.5.0)
# purrr        * 1.0.4   2025-02-05 [1] CRAN (R 4.5.0)
# R6             2.6.1   2025-02-15 [1] CRAN (R 4.5.0)
# RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.5.0)
# Rcpp           1.0.14  2025-01-12 [1] CRAN (R 4.5.0)
# readr        * 2.1.5   2024-01-10 [1] CRAN (R 4.5.0)
# rlang          1.1.6   2025-04-11 [1] CRAN (R 4.5.0)
# rprojroot      2.0.4   2023-11-05 [1] CRAN (R 4.5.0)
# rstatix      * 0.7.2   2023-02-01 [1] CRAN (R 4.5.0)
# rstudioapi     0.17.1  2024-10-22 [1] CRAN (R 4.5.0)
# sandwich     * 3.1-1   2024-09-15 [1] CRAN (R 4.5.0)
# scales       * 1.4.0   2025-04-24 [1] CRAN (R 4.5.0)
# sessioninfo  * 1.2.3   2025-02-05 [1] CRAN (R 4.5.0)
# snakecase      0.11.1  2023-08-27 [1] CRAN (R 4.5.0)
# snow         * 0.4-4   2021-10-27 [1] CRAN (R 4.5.0)
# speedglm     * 0.3-5   2023-05-06 [1] CRAN (R 4.5.0)
# stringi        1.8.7   2025-03-27 [1] CRAN (R 4.5.0)
# stringr      * 1.5.1   2023-11-14 [1] CRAN (R 4.5.0)
# survival     * 3.8-3   2024-12-17 [1] CRAN (R 4.5.0)
# survminer    * 0.5.0   2024-10-30 [1] CRAN (R 4.5.0)
# survMisc       0.5.6   2022-04-07 [1] CRAN (R 4.5.0)
# tibble       * 3.2.1   2023-03-20 [1] CRAN (R 4.5.0)
# tidyr        * 1.3.1   2024-01-24 [1] CRAN (R 4.5.0)
# tidyselect     1.2.1   2024-03-11 [1] CRAN (R 4.5.0)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.5.0)
# timechange     0.3.0   2024-01-18 [1] CRAN (R 4.5.0)
# tzdb           0.5.0   2025-03-15 [1] CRAN (R 4.5.0)
# vctrs          0.6.5   2023-12-01 [1] CRAN (R 4.5.0)
# withr          3.0.2   2024-10-28 [1] CRAN (R 4.5.0)
# xfun           0.52    2025-04-02 [1] CRAN (R 4.5.0)
# xml2           1.3.8   2025-03-14 [1] CRAN (R 4.5.0)
# xtable         1.8-4   2019-04-21 [1] CRAN (R 4.5.0)
# zoo          * 1.8-14  2025-04-10 [1] CRAN (R 4.5.0)
# 
# [1] C:/Users/earpkin/AppData/Local/Programs/R/R-4.5.0/library
# * ── Packages attached to the search path.


# Load libraries
library(tidyverse)
library(here)
library(DCchoice)
library(janitor)
library(data.table)
library(betareg)
library(boot)
library(AER)
library(snow)
library(speedglm)
library(sessioninfo)

# ***********************************************************
# Section 1: Import Data ####
# ***********************************************************

Data <- here("Data", "Microplastics_AllData_Wide_Anonymised.csv") %>%
  fread() %>%
  data.frame()

# Rescale belief variables consistent with main specification
Data$NewMEC <- ((Data$MeanExpectedFuture + Data$MeanExpectedCurrent) + 10.001)/20.002 
Data$MeanExpectations <- (Data$MeanExpectedFuture + Data$MeanExpectedCurrent) + 6

# Select columns needed for estimation
Data_Filtered <- Data %>%
  dplyr::select(
    CV,
    MEC,
    MEF,
    NewMEC,
    AdjustedMEC,
    MeanExpectations, 
    AgeDummy,
    EthnicityDummy,
    Gender_Dummy,
    Charity,
    Education_HigherEd,
    Q16_ClimateCurrentEnvironment,
    Q16_ClimateCurrentSelf,
    Q16_MicroplasticsCurrentEnvironment,
    Q16_MicroplasticsCurrentSelf,
    Uncertainty,
    LogBidIncome,
    Income_Annual
  )

# ***********************************************************
# Section 2: Define Functions ####
# ***********************************************************

# Convert z-values to two-tailed p-values
PvalueConverter <- function(ZValues) {
  2 * (1 - pnorm(abs(ZValues)))
}

# Format coefficient with significance stars and SE in parentheses
format_coef <- function(model, coef_name) {
  s <- summary(model)$coefficients
  if (!coef_name %in% rownames(s)) return("—")
  est <- s[coef_name, "Estimate"]
  se  <- s[coef_name, "Std. Error"]
  pv  <- s[coef_name, "Pr(>|z|)"]
  stars <- case_when(
    pv < 0.001 ~ "***", 
    pv < 0.01  ~ "**",
    pv < 0.05  ~ "*",   
    pv < 0.1   ~ ".",
    TRUE ~ ""
  )
  sprintf("%.3f%s (%.3f)", est, stars, se)
}

# Format EOP safely — returns dash if NA (e.g. factor model)
sprintf_safe <- function(x) {
  if (any(is.na(x))) return("—")
  sprintf("£%.0f (£%.0f)", x[1], x[2])
}

# Extract fit statistics and EOP for a model
diag_row <- function(model, eop) {
  c(
    AIC      = sprintf("%.3f", AIC(model)),
    LogLik   = sprintf("%.3f", logLik(model)),
    PseudoR2 = sprintf("%.3f", 1 - model$deviance / model$nulldev),
    EOP      = sprintf_safe(eop)
  )
}

# ***********************************************************
# Section 3: Stage 1 - Full Beta Regression ####
# ***********************************************************
# Re-estimate Stage 1 with identical specification to main bootstrap model (Script 05)
# This gives us person-level predicted means and variances for Model C

stage_1_full <- betareg(
  as.formula(
    AdjustedMEC ~ 
      AgeDummy + 
      EthnicityDummy +
      Gender_Dummy + 
      Charity +
      Education_HigherEd +
      Q16_ClimateCurrentEnvironment +
      Q16_ClimateCurrentSelf +
      Q16_MicroplasticsCurrentEnvironment + 
      Q16_MicroplasticsCurrentSelf |
      1 + as.factor(Uncertainty)
  ),
  data = Data_Filtered,
  type = "BC"
)

# Attach first-stage predictions to data
# pred_mean: fitted beta mean — person-level expected harmfulness
# pred_var:  fitted beta variance — person-level belief uncertainty
Data_Filtered$pred_mean <- predict(stage_1_full, type = "response")
Data_Filtered$pred_var  <- predict(stage_1_full, type = "variance")

# ***********************************************************
# Section 4: Second-Stage Models ####
# ***********************************************************

# Model A: Cameron (2005)-style — raw AdjustedMEC and continuous ordinal 
# uncertainty entered directly. Imposes equal spacing across 0/1/3/5 scale.
# Serves as reduced-form benchmark; γ expected to show attenuation bias.
model_A <- speedglm(
  CV ~ -1 + LogBidIncome + AdjustedMEC + Uncertainty,
  family = binomial(link = "probit"),
  data = Data_Filtered
)

# Model C: First-stage predictions inserted directly without bootstrapping.
# Point estimates should match Table 3 by construction; SEs underestimated
# as they ignore first-stage estimation uncertainty — motivating the bootstrap
# procedure in the main specification.
model_C <- speedglm(
  CV ~ -1 + LogBidIncome + I(pred_mean) + I(pred_var),
  family = binomial(link = "probit"),
  data = Data_Filtered
)

# ***********************************************************
# Section 5: EOP Calculations ####
# ***********************************************************

# Model A EOP: uses raw AdjustedMEC and continuous Uncertainty as inputs
EOP_A <- {
  B0 <- coef(model_A)["LogBidIncome"]
  D0 <- coef(model_A)["AdjustedMEC"]
  D1 <- coef(model_A)["Uncertainty"]
  A  <- D0 * Data_Filtered$AdjustedMEC + D1 * Data_Filtered$Uncertainty
  Y  <- Data_Filtered$Income_Annual
  EOP <- Y - Y * exp(-A / B0) * exp(1 / (2 * B0^2))
  c(Mean = mean(EOP, na.rm = TRUE), SD = sd(EOP, na.rm = TRUE))
}

# Model C EOP: uses first-stage predicted mean and variance as inputs
# Should be near-identical to Table 3 given same inputs entering the probit
EOP_C <- {
  B0 <- coef(model_C)["LogBidIncome"]
  D0 <- coef(model_C)["I(pred_mean)"]
  D1 <- coef(model_C)["I(pred_var)"]
  A  <- D0 * Data_Filtered$pred_mean + D1 * Data_Filtered$pred_var
  Y  <- Data_Filtered$Income_Annual
  EOP <- Y - Y * exp(-A / B0) * exp(1 / (2 * B0^2))
  c(Mean = mean(EOP, na.rm = TRUE), SD = sd(EOP, na.rm = TRUE))
}

# ***********************************************************
# Section 6: Assemble and Export Table C5 ####
# ***********************************************************

results <- data.frame(
  Variable = c(
    "Changes in household income (α)",
    "Changes in expected harm (β)",
    "Changes in variance of expected harm (γ)",
    "AIC", "Log-likelihood", "Pseudo-R²", "EOP (SD)"
  ),
  `Uncertainty as continuous predictor` = c(
    format_coef(model_A, "LogBidIncome"),
    format_coef(model_A, "AdjustedMEC"),
    format_coef(model_A, "Uncertainty"),
    diag_row(model_A, EOP_A)
  ),
  `Two-stage without bootstrapping` = c(
    format_coef(model_C, "LogBidIncome"),
    format_coef(model_C, "I(pred_mean)"),
    format_coef(model_C, "I(pred_var)"),
    diag_row(model_C, EOP_C)
  ),
  # Table 3 values hardcoded — bootstrapped SEs differ from single-run model_C
  `Two-stage bootstrapped (Table 3)` = c(
    "86.365*** (12.777)",
    "1.641*** (0.099)",
    "-13.833*** (2.518)",
    "1606.812", "-800.406", "0.262", "£340 (£198)"
  ),
  check.names = FALSE
)

print(results)

results %>%
  fwrite(
    sep = ",",
    here("Tables", "TableC5_NaiveProbit.txt")
  )

# ***********************************************************
# Section 7: Saving Session Information ####
# ***********************************************************

session_file_name <- paste0("session_info_", format(Sys.Date(), "%Y_%m_%d"), ".txt")
sessioninfo::session_info() %>%
  capture.output(file = here("Data", session_file_name))

# End Of Script # ********************************************