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

# R version 4.4.1 (2024-06-14 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 22631)
# Matrix products: default
# locale:
#   [1] LC_COLLATE=English_United Kingdom.utf8 
# [2] LC_CTYPE=English_United Kingdom.utf8   
# [3] LC_MONETARY=English_United Kingdom.utf8
# [4] LC_factor=C                           
# [5] LC_TIME=English_United Kingdom.utf8    
# 
# time zone: Europe/London
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] snow_0.4-4        AER_1.2-14        survival_3.6-4    sandwich_3.1-1   
# [5] lmtest_0.9-40     zoo_1.8-12        car_3.1-2         carData_3.0-5    
# [9] boot_1.3-30       betareg_3.2-1     DCchoice_0.2.0    here_1.0.1       
# [13] lubridate_1.9.3   forcats_1.0.0     stringr_1.5.1     purrr_1.0.2      
# [17] readr_2.1.5       tidyr_1.3.1       tibble_3.2.1      ggplot2_3.5.1    
# [21] tidyverse_2.0.0   dplyr_1.1.4       magrittr_2.0.3    data.table_1.16.0
# 
# loaded via a namespace (and not attached):
#   [1] utf8_1.2.4        generics_0.1.3    stringi_1.8.4     lattice_0.22-6   
# [5] hms_1.1.3         grid_4.4.1        timechange_0.3.0  Matrix_1.7-0     
# [9] rprojroot_2.0.4   nnet_7.3-19       Formula_1.2-5     Icens_1.76.0     
# [13] fansi_1.0.6       scales_1.3.0      modeltools_0.2-23 abind_1.4-8      
# [17] cli_3.6.3         rlang_1.1.4       munsell_0.5.1     splines_4.4.1    
# [21] withr_3.0.1       parallel_4.4.1    tools_4.4.1       flexmix_2.3-19   
# [25] tzdb_0.4.0        interval_1.1-1.0  colorspace_2.1-1  vctrs_0.6.5      
# [29] R6_2.5.1          stats4_4.4.1      lifecycle_1.0.4   MASS_7.3-60.2    
# [33] MLEcens_0.1-7.1   pkgconfig_2.0.3   pillar_1.9.0      gtable_0.3.5     
# [37] glue_1.7.0        tidyselect_1.2.1  rstudioapi_0.16.0 perm_1.0-0.4     
# [41] compiler_4.4.1    


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
library(glmx) ## for hetglm()


# ***********************************************************
# Section 1: Import Data ####
# ***********************************************************

# Load data from the specified path using data.table::fread
Data <- here("Data", "Microplastics_AllData_Wide_Anonymised.csv") %>%
  fread() %>%
  data.frame()


Data$NewMEC <- ((Data$MeanExpectedFuture + Data$MeanExpectedCurrent) + 10.001)/20.002 



# Select the specified columns
Data_Filtered <- Data %>%
  dplyr::select(
    CV,
    MEC,
    MEF,
    NewMEC,
    AdjustedMEC,
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
    Income_Annual,
    PaymentVehicle_Dummy
  )

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


# ***********************************************************
# Section 3: Verify ####
## speedglm() and hetglm() with constant scale produce equivalent results 
# ***********************************************************


# Stage 2: Probit model of CV on LogBidIncome, predicted MEF, predicted variance and control variables
GLM1 <- speedglm(
  paste0(
    "CV ~ -1 + LogBidIncome + AdjustedMEC + Uncertainty") %>% 
    as.formula(),
  family = binomial(link = "probit"),
  data = Data_Filtered
) 

GLM1 %>% summary()





# Stage 2: Probit model of CV on LogBidIncome, predicted MEF, predicted variance and control variables
GLM2 <- hetglm(
  formula = CV ~ -1 + LogBidIncome + AdjustedMEC + Uncertainty | 1,
  data = Data_Filtered
) 

GLM2 %>% summary()



# ***********************************************************
# Section 4: Check hetglm by payment vehicle ####
# ***********************************************************


## Before estimating, be aware that this is 15% 0s, 85% 1s
# > Data$PaymentVehicle_Dummy %>% tabyl()
# .    n   percent
# 0  228 0.1457801
# 1 1336 0.8542199


# Stage 2: Probit model of CV on LogBidIncome, predicted MEF, predicted variance and control variables
NaiveProbit_ByPaymentVehicle <- hetglm(
  formula = CV ~ -1 + LogBidIncome + AdjustedMEC + Uncertainty | PaymentVehicle_Dummy,
  data = Data_Filtered
) 

NaiveProbit_ByPaymentVehicle %>% summary()



# ***********************************************************
# Section 5: Calculate EOP for comparison ####
# ***********************************************************



# Extract relevant coefficients
B0 <- NaiveProbit_ByPaymentVehicle$coefficients$mean["LogBidIncome"] %>% as.numeric()
Delta_0 <-
  NaiveProbit_ByPaymentVehicle$coefficients$mean['AdjustedMEC'] %>% as.numeric()
Delta_1 <-
  NaiveProbit_ByPaymentVehicle$coefficients$mean['Uncertainty'] %>% as.numeric()


# Calculate means and variances from stage 1
Means <-
  Data_Filtered$AdjustedMEC %>% as.numeric()
Variances <-
  Data_Filtered$Uncertainty %>% as.numeric()


# Define Y as gross annual income
Y <- Data_Filtered$Income_Annual %>% as.numeric()
A <- ((
  Delta_0 * Means +
    (Delta_1 * (Variances))))

EOP <- (Y - Y * exp(-A / B0) * exp(1 / (2 * B0 ^ 2)))

# Add fit statistics for stage 1 and stage 2
fit_stats <- c(
  s2_AIC = AIC(NaiveProbit_ByPaymentVehicle),
  s2_LogLik = logLik(NaiveProbit_ByPaymentVehicle),
  s2_PseudoR2 = (1 - NaiveProbit_ByPaymentVehicle$deviance / NaiveProbit_ByPaymentVehicle$nulldev),
  S2_EOP_Mean = EOP %>% mean(),
  S2_EOP_SD = EOP %>% sd()
)


# ***********************************************************
# Section 4: Stage Two Outputs ####
# ***********************************************************


# Extract summary statistics
stage_2_summary <- summary(NaiveProbit_ByPaymentVehicle)
coef_table <- stage_2_summary$coefficients$mean

# Create significance stars
add_stars <- function(p_val) {
  case_when(
    p_val < 0.001 ~ "***",
    p_val < 0.01 ~ "**", 
    p_val < 0.05 ~ "*",
    p_val < 0.10 ~ ".",
    TRUE ~ ""
  )
}

# Format p-values 
format_pval <- function(p_val) {
  ifelse(p_val < 0.001, "<0.001", sprintf("%.3f", p_val))
}

# Create results table
results_table <- data.frame(
  Variable = c("Impact of changes in bid level on income",
               "Predicted impact of changes in expected harm", 
               "Predicted impact of variance in expected harm"),
  Coefficient = sprintf("%.3f", coef_table[, "Estimate"]),
  `Std Error` = sprintf("%.3f", coef_table[, "Std. Error"]),
  `z-value` = sprintf("%.3f", coef_table[, "z value"]),
  `P-value` = paste0(format_pval(coef_table[, "Pr(>|z|)"]), 
                     add_stars(coef_table[, "Pr(>|z|)"])),
  row.names = NULL,
  check.names = FALSE
)

# Add diagnostics
diagnostics <- data.frame(
  Variable = c("AIC", "Log-likelihood", "EOP (SD)"),
  Coefficient = c(sprintf("%.3f", fit_stats[["s2_AIC"]]),
                  sprintf("%.3f", fit_stats[["s2_LogLik"]]),
                  sprintf("£%.0f (£%.0f)", fit_stats[["S2_EOP_Mean"]], 
                          fit_stats[["S2_EOP_SD"]])),
  `Std Error` = rep("", 3),
  `z-value` = rep("", 3),
  `P-value` = rep("", 3),
  check.names = FALSE
)

# Combine tables
final_table <- rbind(results_table, diagnostics)

print(final_table)


# ***********************************************************
# Section 5: Export ####
# ***********************************************************



# Write results to text file in CVoutput folder
final_table %>%
  data.frame() %>%
  fwrite(
    sep = ",",
    here("CVoutput/Tables", 
         paste0("Table_X_HetglmProbitSecondStage.txt"))
  )



# ***********************************************************
# Section 6: Saving Session Information ####
# ***********************************************************

# # Create file name for the session information
# session_file_name <- paste0("session_info_", format(Sys.Date(), "%Y_%m_%d"), ".txt")
# 
# # Save the session information into a text file in the Data Folder
# sessioninfo::session_info() %>%
#   capture.output(file = here("Data", session_file_name))

# End Of Script # ********************************************