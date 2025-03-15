#### Microplastics: IOP Paper ####
## Function: In-text specification
## Author: PK
## Last change: 31/01/25
# Changes:
# - using factor uncertainty instead
# - new code to output the combined table
# - new code to report median (SD) of EOP



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
  "Income_Annual",
  "Speeders_Survey_TestDummy",
  "Order",
  "WaterBills",
  "Consequentiality",
  "VarianceUpperBound",
  "VarianceLowerBound"
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
    
    # Extract relevant coefficients
    B0 <- stage_2$coefficients["LogBidIncome"] %>% as.numeric()
    Delta_0 <- stage_2$coefficients['I((predict(stage_1, type = "response")))'] %>% as.numeric()
    Delta_1 <- stage_2$coefficients['I(predict(stage_1, type = "variance"))'] %>% as.numeric()
    # Delta_0 <- stage_2$coefficients['I((predict(stage_1, type = "response") + MEC)/2)'] %>% as.numeric()
    # Delta_1 <- stage_2$coefficients['I(0 - predict(stage_1, type = "variance"))'] %>% as.numeric()
    
    # Calculate means and variances from stage 1
    Means <- I((predict(stage_1, type = "response"))) %>% as.numeric()
    # Means <- I((predict(stage_1, type = "response") + d$MEC) / 2) %>% as.numeric()
    Variances <- (betareg::predict(stage_1, type = "variance"))  %>% as.numeric()
    
    # Define Y as gross annual income
    Y <- d$Income_Annual %>% as.numeric()
    A <- ((Delta_0 * Means +
             (Delta_1 * (Variances))
             # (Delta_1 * (0 - Variances))
    )) %>% as.numeric()
    # Calculate EOP
    EOP <- (Y - Y * exp(-A/B0) * exp(1/ (2 * B0 ^ 2) ))
    # EOP <- (Y - (Y * exp(-((Delta_0 * Means + (Delta_1 * (0 - Variances)))/B0)))) *
    #   exp(1/(2*B0^2))
    
    # Add fit statistics for stage 1 and stage 2
    fit_stats <- c(
      s1_AIC = AIC(stage_1),
      s1_LogLik = stage_1$loglik,
      s1_PseudoR2 = stage_1$pseudo.r.squared,
      s2_AIC = AIC(stage_2),
      s2_LogLik = logLik(stage_2),
      s2_PseudoR2 = (1 - stage_2$deviance/stage_2$nulldev),
      S2_EOP_Mean = EOP %>% mean(),
      S2_EOP_SD = EOP %>% sd()
    )
    
    # Return combined results from stages 1 and 2 along with fit statistics
    return(
      c(summary(stage_1)$coefficients$mean[, 1],
        summary(stage_1)$coefficients$precision[, 1],
        stage_2$coefficients,
        summary(stage_1)$coefficients$mean[, 2],
        summary(stage_1)$coefficients$precision[, 2],
        summary(stage_2)$coefficients[, 2],
        fit_stats)
    )
  }
  
  # Perform bootstrap simulation
  boot.results <- boot(
    data = data,
    statistic = boot.function,
    R = R,
    parallel = "snow"
  )
  
  # Define length based on number of coefficients returned from the boot function
  l <- length(boot.results$t0) - 8
  
  # Extract and format results from the simulation
  results <- cbind(
    Estimate = boot.results$t0[1:(l / 2)],
    `Std. Error` = boot.results$t[, (l / 2 + 1):l] %>% colMeans(),
    `z value` = boot.results$t0[1:(l / 2)] / (boot.results$t[, (l / 2 + 1):l] %>% colMeans())
  )
  # Round and add p-values to the results
  results_rounded <- results %>% round(3)
  results_rounded_withP <- cbind(
    results_rounded,
    "P values" = results[, 3] %>% PvalueConverter()
  ) %>% data.frame()
  # Simplify the extraction of the fit statistics
  fit_stats_means <- c(
    S1_AIC = boot.results$t[, l + 1] %>% mean() %>% round(3),
    S1_LogLik = boot.results$t[, l + 2] %>% mean() %>% round(3),
    S1_PseudoR2 = boot.results$t[, l + 3] %>% mean() %>% round(3),
    S2_AIC = boot.results$t[, l + 4] %>% mean() %>% round(3),
    S2_LogLik = boot.results$t[, l + 5] %>% mean() %>% round(3),
    S2_PseudoR2 = boot.results$t[, l + 6] %>% mean() %>% round(3),
    S2_EOP_Mean = paste0("£",
                         boot.results$t[, l + 7] %>% mean(na.rm = TRUE) %>% round(3),
                         " (£",
                         boot.results$t[, l + 8] %>% mean(na.rm = TRUE) %>% round(3),
                         ")")
    
  )
  
  # Return list of formatted coefficient table and fit statistics.
  return(list(
    coefficients = results_rounded_withP,
    fit_statistics = fit_stats_means
  ))
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
    as.numeric(Uncertainty)
)

# Define your formula for stage_1 and stage_2 models
Model1_stage2_formula <- "-1 + LogBidIncome"

# Call the simulator function
Model1_simulation <- Simulator(
  data = Data_Filtered,
  formula_stage_1 = Model1_stage1_formula,
  formula_stage_2 = Model1_stage2_formula,
  R = R
)

# First stage results
Model1_simulation$coefficients %>% 
  ModelOutput_Paper()


# ***********************************************************
# Section 5A: Stage One Outputs ####
# ***********************************************************

# Combine first-stage results
Output_S1 <- rbind(
  Model1_simulation$coefficients %>%
    ModelOutput_Paper() %>%
    slice(1:(n() - 3)),
  cbind(
    "Estimate" = Model1_simulation$fit_statistics[1:3],
    "Std..Error" = 0,
    "z.value" = 0,
    "P.values" = 0,
    "Variable" = names(Model1_simulation$fit_statistics[1:3])
  )
)

# Consistent date format
date_suffix <- format(Sys.Date(), "%Y_%m_%d")

# Write results to text file in CVoutput folder
Output_S1 %>%
  data.frame() %>%
  fwrite(
    sep = ",",
    here("CVoutput/Tables", 
         "Table_Alternative1_Stage1.txt")
  )


# ***********************************************************
# Section 5B: Stage Two Outputs ####
# ***********************************************************
# Combine second-stage results
Output_S2 <- rbind(
  cbind(
    "Variable" = c("α[((Y-OP))/Y]",
                   "β (E[q])",
                   "β2 (Var[q])"),
    "Estimate" = Model1_simulation$coefficients %>%
      ModelOutput(Identifier = 1) %>%
      slice((n() - 2):n()) %>%
      dplyr::select(Estimate)),
  cbind(
    "Variable" = names(Model1_simulation$fit_statistics[4:6]),
    "Estimate" = Model1_simulation$fit_statistics[4:6]
  )
)
# Write results to text file in CVoutput folder
Output_S2 %>%
  data.frame() %>%
  fwrite(
    sep = ",",
    here("CVoutput/Tables", 
         "Table_Alternative1_Stage2.txt")
  )


# ***********************************************************
# Section 5C: Combined Outputs ####
# ***********************************************************

# Combine all results into a single output
Model_All <- Model1_simulation$coefficients %>%
  ModelOutput(Identifier = 1)

# Create diagnostic output
Model_Diagnostics <- cbind(
  "Variable" = names(Model1_simulation$fit_statistics),
  "Estimate" = Model1_simulation$fit_statistics,
  "Model" = 0
) %>% data.frame()


# Combine coefficients and fit statistics for a combined model output
Output_S3 <- rbind(
  Model_All, Model_Diagnostics
)
# Write the final combined output
Output_S3 %>%
  data.frame() %>%
  fwrite(
    sep = ",",
    here("CVoutput/Tables", 
         "Table_Alternative1_Combined.txt")
  )

# ***********************************************************
# Section 6: Saving Session Information ####
# ***********************************************************


# Create file name for the session information
session_file_name <- paste0("session_info_", 
                            format(Sys.Date(), 
                                   "%Y_%m_%d"), 
                            ".txt")


# Save the session information into a text file in the Data Folder
sessioninfo::session_info() %>%
  capture.output(file = here("Data", 
                             session_file_name))


# End Of Script # ********************************************