#### Microplastics: IOP Paper ####
## Function: In-text specification
## Author: PK
## Last change: 18/02/25
# Changes:
# - using factor uncertainty instead
# - new code to output the combined table
# - new code to report median (SD) of EOP
# - replace data$MEC with d$MEC line 217



# ***********************************************************
# Replication Information: ####
# ***********************************************************
# This script performs in-text specification.
# Session information is saved to a separate file 'session_info.txt'

# ─ Session info ──────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United Kingdom.utf8
# ctype    English_United Kingdom.utf8
# tz       Europe/London
# date     2025-02-18
# rstudio  2023.06.2+561 Mountain Hydrangea (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# abind          1.4-5   2016-07-21 [1] CRAN (R 4.4.0)
# AER          * 1.2-12  2024-02-03 [1] CRAN (R 4.4.0)
# betareg      * 3.2-0   2024-07-07 [1] CRAN (R 4.4.1)
# biglm        * 0.9-3   2024-06-12 [1] CRAN (R 4.4.2)
# boot         * 1.3-30  2024-02-26 [1] CRAN (R 4.4.1)
# car          * 3.1-2   2023-03-30 [1] CRAN (R 4.4.0)
# carData      * 3.0-5   2022-01-06 [1] CRAN (R 4.4.0)
# cli            3.6.3   2024-06-21 [1] CRAN (R 4.4.1)
# colorspace     2.1-0   2023-01-23 [1] CRAN (R 4.4.0)
# data.table   * 1.15.4  2024-03-30 [1] CRAN (R 4.4.0)
# DBI          * 1.2.3   2024-06-02 [1] CRAN (R 4.4.0)
# DCchoice     * 0.2.0   2023-07-10 [1] CRAN (R 4.4.0)
# dplyr        * 1.1.4   2023-11-17 [1] CRAN (R 4.4.0)
# fansi          1.0.6   2023-12-08 [1] CRAN (R 4.4.0)
# flexmix        2.3-19  2023-03-16 [1] CRAN (R 4.4.0)
# forcats      * 1.0.0   2023-01-29 [1] CRAN (R 4.4.0)
# Formula        1.2-5   2023-02-24 [1] CRAN (R 4.4.0)
# generics       0.1.3   2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2      * 3.5.1   2024-04-23 [1] CRAN (R 4.4.0)
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.4.0)
# glue           1.7.0   2024-01-09 [1] CRAN (R 4.4.0)
# gridtext       0.1.5   2022-09-16 [1] CRAN (R 4.4.0)
# gtable         0.3.5   2024-04-22 [1] CRAN (R 4.4.0)
# here         * 1.0.1   2020-12-13 [1] CRAN (R 4.4.0)
# hms            1.1.3   2023-03-21 [1] CRAN (R 4.4.0)
# Icens          1.72.0  2023-04-25 [1] Bioconductor
# interval       1.1-1.0 2023-08-24 [1] CRAN (R 4.4.0)
# janitor      * 2.2.0   2023-02-02 [1] CRAN (R 4.4.1)
# lattice        0.22-6  2024-03-20 [1] CRAN (R 4.4.1)
# lifecycle      1.0.4   2023-11-07 [1] CRAN (R 4.4.0)
# lmtest       * 0.9-40  2022-03-21 [1] CRAN (R 4.4.0)
# lubridate    * 1.9.3   2023-09-27 [1] CRAN (R 4.4.0)
# magrittr     * 2.0.3   2022-03-30 [1] CRAN (R 4.4.0)
# MASS         * 7.3-61  2024-06-13 [1] CRAN (R 4.4.1)
# Matrix       * 1.7-0   2024-04-26 [1] CRAN (R 4.4.1)
# MLEcens        0.1-7   2022-10-18 [1] CRAN (R 4.4.0)
# modeltools     0.2-23  2020-03-05 [1] CRAN (R 4.4.0)
# munsell        0.5.1   2024-04-01 [1] CRAN (R 4.4.0)
# nnet           7.3-19  2023-05-03 [1] CRAN (R 4.4.1)
# perm           1.0-0.4 2023-08-24 [1] CRAN (R 4.4.0)
# pillar         1.9.0   2023-03-22 [1] CRAN (R 4.4.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.4.0)
# purrr        * 1.0.2   2023-08-10 [1] CRAN (R 4.4.0)
# R6             2.5.1   2021-08-19 [1] CRAN (R 4.4.0)
# Rcpp           1.0.12  2024-01-09 [1] CRAN (R 4.4.0)
# D RcppParallel   5.1.7   2023-02-27 [1] CRAN (R 4.4.0)
# RcppZiggurat   0.1.6   2020-10-20 [1] CRAN (R 4.4.0)
# readr        * 2.1.5   2024-01-10 [1] CRAN (R 4.4.0)
# Rfast          2.1.0   2023-11-09 [1] CRAN (R 4.4.0)
# rlang          1.1.4   2024-06-04 [1] CRAN (R 4.4.0)
# rprojroot      2.0.4   2023-11-05 [1] CRAN (R 4.4.0)
# rstudioapi     0.16.0  2024-03-24 [1] CRAN (R 4.4.0)
# sandwich     * 3.1-0   2023-12-11 [1] CRAN (R 4.4.0)
# scales         1.3.0   2023-11-28 [1] CRAN (R 4.4.0)
# sessioninfo  * 1.2.2   2021-12-06 [1] CRAN (R 4.4.2)
# snakecase      0.11.1  2023-08-27 [1] CRAN (R 4.4.1)
# snow         * 0.4-4   2021-10-27 [1] CRAN (R 4.4.0)
# speedglm     * 0.3-5   2023-05-06 [1] CRAN (R 4.4.2)
# stringi        1.8.4   2024-05-06 [1] CRAN (R 4.4.0)
# stringr      * 1.5.1   2023-11-14 [1] CRAN (R 4.4.0)
# survival     * 3.7-0   2024-06-05 [1] CRAN (R 4.4.1)
# tibble       * 3.2.1   2023-03-20 [1] CRAN (R 4.4.0)
# tidyr        * 1.3.1   2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect     1.2.1   2024-03-11 [1] CRAN (R 4.4.0)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.4.0)
# timechange     0.3.0   2024-01-18 [1] CRAN (R 4.4.0)
# tzdb           0.4.0   2023-05-12 [1] CRAN (R 4.4.0)
# utf8           1.2.4   2023-10-22 [1] CRAN (R 4.4.0)
# vctrs          0.6.5   2023-12-01 [1] CRAN (R 4.4.0)
# withr          3.0.0   2024-01-16 [1] CRAN (R 4.4.0)
# xml2           1.3.6   2023-12-04 [1] CRAN (R 4.4.0)
# zoo          * 1.8-12  2023-04-13 [1] CRAN (R 4.4.0)
# 
# [1] C:/Users/earpkin/AppData/Local/Programs/R/R-4.4.1/library
# 
# D ── DLL MD5 mismatch, broken installation.


# Clear environment
rm(list = ls())

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
library(Rfast)

# ***********************************************************
# Section 1: Import Data ####
# ***********************************************************

# Load data from the specified path using data.table::fread
Data <- here("Data", "Microplastics_AllData_Wide_Anonymised.csv") %>%
  fread() %>%
  data.frame()



Data_Filtered <- Data %>% dplyr::select(c(
  "CV",
  "MEC",
  "MEF", 
  "AdjustedMEC",
  "AgeDummy",
  "EthnicityDummy",
  "Gender_Dummy",  
  "Charity",
  "Education_HigherEd",
  "Q16_ClimateCurrentEnvironment",
  "Q16_ClimateCurrentSelf",
  "Q16_MicroplasticsCurrentEnvironment", 
  "Q16_MicroplasticsCurrentSelf",
  "Q16_MicroplasticsTen", 
  "Q16_MicroplasticsTwentyFive", 
  "Q16_MicroplasticsFifty", 
  "Uncertainty",
  "LogBidIncome",
  "Income_Annual"
))


# ***********************************************************
# Section 2: Define Functions ####
# ***********************************************************

# Function to convert z-values to p-values
PvalueConverter <- function(ZValues) {
  2 * (1 - pnorm(abs(ZValues)))
}

# Function to add significance stars to an estimate based on p-value
PvalueLabeller <- function(estimate, p_values) {
  estimate_rounded <- round(estimate, 3)
  case_when(
    p_values < 0.001 ~ paste0(estimate_rounded, "***"),
    p_values < 0.01  ~ paste0(estimate_rounded, "**"),
    p_values < 0.05 ~ paste0(estimate_rounded, "*"),
    p_values < 0.1 ~ paste0(estimate_rounded, "."),
    TRUE ~ as.character(estimate_rounded)
  )
}

# Function to format p-values for paper
PvalueLabeller_Paper <- function(p_values) {
  case_when(
    p_values < 0.001 ~ "<0.001***",
    p_values < 0.01 ~ sprintf("%.3f***", p_values),
    p_values < 0.05 ~ sprintf("%.3f**", p_values),
    p_values < 0.1 ~ sprintf("%.3f*", p_values),
    TRUE ~ sprintf("%.3f", p_values)
  )
}

# Function to format model output for printing
ModelOutput <- function(Estimates, Identifier) {
  Estimates %>%
    data.frame() %>%
    dplyr::mutate(
      Variable = rownames(Estimates),
      Estimate = paste0(PvalueLabeller(Estimate, P.values), " (", round(Std..Error, 3), ")"),
      Model = Identifier
    ) %>%
    dplyr::select(Variable, Estimate, Model)
}

# Function to format model output for paper
ModelOutput_Paper <- function(Estimates, Identifier) {
  Estimates %>%
    data.frame() %>%
    dplyr::mutate(
      Variable = rownames(Estimates),
      Estimate = Estimates$Estimate,
      Std..Error = Estimates$Std..Error,
      z.value = Estimates$z.value,
      P.values = PvalueLabeller_Paper(P.values)
    ) %>%
    dplyr::select(Variable, Estimate, Std..Error, z.value, P.values)
}


summary_function <- function(EOP) {
  cbind(
    "2.5%" = EOP %>% quantile(c(0.025)) %>% round(2) %>% sprintf("%.2f", .) %>%  paste0("£", .),
    Median = EOP %>% median(na.rm = TRUE) %>% round(2) %>%  sprintf("%.2f", .) %>% paste0("£", .), 
    Mean = EOP %>% mean(na.rm = TRUE) %>% round(2) %>%  sprintf("%.2f", .) %>% paste0("£", .), 
    "97.5%" = EOP %>% quantile(c(0.975)) %>% round(2) %>% sprintf("%.2f", .) %>%  paste0("£", .))
  }



# ***********************************************************
# Section 3: Simulator Function ####
# ***********************************************************

# Function to simulate data and perform the two-stage modelling
Simulator <- function(data,
                      formula_stage_1,
                      formula_stage_2,
                      R = R) {
  
  # Function to perform a single bootstrap replicate
  boot.function <- function(data, indices) {
    d <- data[indices, ]
    
    # Stage 1: Beta regression of MEF on covariates
    stage_1 <- betareg(formula_stage_1, d, type = "BC")
    
    # Stage 2: Probit model of CV on LogBidIncome, predicted MEF, predicted variance and control variables
    stage_2 <- speedglm(
      paste0(
        "CV ~ ",
        formula_stage_2,
        " + I((predict(stage_1, type = 'response'))) + I(predict(stage_1, type = 'variance'))"
        # " + I((predict(stage_1, type = 'response') + MEC) / 2) + I(0 - predict(stage_1, type = 'variance'))"
      ) %>% as.formula(),
      family = binomial(link = "probit"),
      data = d
    )
    
    
    B0 <- stage_2$coefficients["LogBidIncome"] %>% as.numeric()
    Delta_0 <- stage_2$coefficients['I((predict(stage_1, type = "response")))'] %>% as.numeric()
    Delta_1 <- stage_2$coefficients['I(predict(stage_1, type = "variance"))'] %>% as.numeric()
    # Delta_1 <- stage_2$coefficients['I(0 - predict(stage_1, type = "variance"))'] %>% as.numeric()
    
    
    Means <- c(I((predict(stage_1, type = "response"))))
    # Means <- c(I((predict(stage_1, type = "response") + d$MEC)/2))
    Variances <- (betareg::predict(stage_1, type = "variance"))
    ## Define Y == gross monthly income * 12
    Y <- mean(Data_Filtered$Income_Annual)
    A <- (Delta_0 * mean(Means) + Delta_1 * mean(Variances)) %>% as.numeric()
    EOP <- (Y - Y * exp(-A / B0) * exp(1 / (2 * B0 ^ 2)))
    EOP
    
    # EOP <- (d$Income_Annual - (d$Income_Annual * exp(-A / B0))) *
    #   exp(0.5 * (B0^-2))
    
    return(EOP)
    
  }
  
  # Run the bootstrap
  boot.results <- boot(data = data,
                       statistic = boot.function,
                       R = R,
                       parallel = "snow")
  
  # Extracting the results
  # l <- length(boot.results$t0)
  # results <- boot.results$t0
  
  results <- boot.results$t %>% summary_function()
  
  ## Here just the raw data
  results %>% return() 
  # results %>% summary_function() %>% return()  
}

# ***********************************************************
# Section 4: Model Output ####
# ***********************************************************

# Define number of bootstrap iterations
# R <- 10
# R <- 1000
R <- 10000


# Define your formula for stage_1 and stage_2 models
Model1_stage1_formula <- as.formula(
  AdjustedMEC ~
    1 + ## intercept here
    AgeDummy + 
    EthnicityDummy +
    Gender_Dummy  + 
    Charity +
    Education_HigherEd +
    Q16_ClimateCurrentEnvironment +
    Q16_ClimateCurrentSelf +
    Q16_MicroplasticsCurrentEnvironment + 
    Q16_MicroplasticsCurrentSelf +
    Q16_MicroplasticsTen + 
    Q16_MicroplasticsTwentyFive + 
    Q16_MicroplasticsFifty |
    1 +  # intercept here
    as.factor(Uncertainty)
)



# Define your formula for stage_1 and stage_2 models
Model1_stage2_formula <- "-1 + LogBidIncome"


# Call the simulator function
Model1_simulation <- Simulator(data = Data_Filtered,
                               formula_stage_1 = Model1_stage1_formula,
                               formula_stage_2 = Model1_stage2_formula,
                               R = R
)  


# *****************************
# Section x: Export Data ####
# *****************************

# Data$EOP <- Model1_simulation
# 
Model1_simulation %>%
  data.frame() %>%
  fwrite(sep = ",",
         here("Data", "EOP_AtMeans.txt"))
# 



# ***********************************************************
# Section 6: Saving Session Information ####
# ***********************************************************

# Create file name for the session information
session_file_name <- paste0("session_info_", format(Sys.Date(), "%Y_%m_%d"), ".txt")

# Save the session information into a text file in the Data Folder
sessioninfo::session_info() %>%
  capture.output(file = here("Data", session_file_name))

# End Of Script # ********************************************