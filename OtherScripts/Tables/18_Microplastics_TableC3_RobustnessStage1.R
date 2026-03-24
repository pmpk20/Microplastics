#### Microplastics: IOP Paper ####
## Function: Makes Table C2
## Author: PK
## Last change: 25/02/2025
# Changes:
# - CURRENTLY NON-FUNCTIONAL DUE TO MOVING VARIABLES FROM FIRST AND SECOND STAGES



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


Robustness_B1 <- here("CVoutput/Tables", 
                      "Table_RobustnessStage1_ModelB1.txt") %>% 
  fread() %>% 
  data.frame()


Robustness_B2 <- here("CVoutput/Tables", 
                      "Table_RobustnessStage1_ModelB2.txt") %>% 
  fread() %>% 
  data.frame()


## CALLED B3 BUT ACTUALLY FOURTH TABLE COLUMN
Robustness_B3 <- here("CVoutput/Tables", 
                      "Table_RobustnessStage1_ModelB3.txt") %>% 
  fread() %>% 
  data.frame()


Robustness_B4 <- here("CVoutput/Tables", 
                      "Table_RobustnessStage1_ModelB4.txt") %>% 
  fread() %>% 
  data.frame()



# ***********************************************************
# Section 2: Table combining function ####
# ***********************************************************


create_robustness_table <- function(B1, B2, B3, B4) {
  # Define variable mapping dictionary
  var_mapping <- c(
    "X.Intercept." = "Intercept",
    "AgeDummy" = "Aged older than sample median (0/1)",
    "EthnicityDummy" = "Ethnicity: any white group (0/1)",
    "Gender_Dummy" = "Gender: female (0/1)",
    "Charity" = "Charity: involved (0/1)",
    "Education_HigherEd" = "Higher education experience (0/1)",
    "Q16_ClimateCurrentEnvironment" = "Climate change: current risk to environment (1-10)",
    "Q16_ClimateCurrentSelf" = "Climate change: current risk to self (1-10)",
    "Q16_MicroplasticsCurrentEnvironment" = "MPs: current risk to environment (1-10)",
    "Q16_MicroplasticsCurrentSelf" = "MPs: current risk to self (1-10)",
    "X.Intercept..1" = "Intercept_Disp",
    "as.factor.Uncertainty.1" = "Uncertainty: low (+/- 1 point)",
    "as.factor.Uncertainty.3" = "Uncertainty: medium (+/- 3 points)",
    "as.factor.Uncertainty.5" = "Uncertainty: high (+/- 5 points)",
    "Speeders_Survey_TestDummy" = "Survey speeder dummy (0/1)",
    "Order" = "Survey order (0/1)",
    "PaymentVehicle_Dummy" = "Payment vehicle (0/1)",
    "Consequentiality" = "Consequentiality (0/1)",
    "Coronavirus" = "Coronavirus (0/1)",
    "Understanding" = "Understanding (1-10)",
    "S1_AIC" = "AIC",
    "S1_LogLik" = "Log-likelihood",
    "S1_PseudoR2" = "Pseudo R2",
    "LogBidIncome" = "Impact of change in bid level on income",
    "I..predict.stage_1..type....response...." = "Predicted impact of changes in expected harm",
    "I.predict.stage_1..type....variance..." = "Predicted impact of variance in expected harm",
    "S2_AIC" = "AIC_S2",
    "S2_LogLik" = "Log-likelihood_S2",
    "S2_PseudoR2" = "Pseudo R2_S2",
    "S2_EOP_Mean" = "EOP (SD)"
  )
  
  # Define the final variable order
  final_vars <- c(
    # Mean model variables
    "Intercept",
    "Aged older than sample median (0/1)",
    "Ethnicity: any white group (0/1)",
    "Gender: female (0/1)",
    "Charity: involved (0/1)",
    "Higher education experience (0/1)",
    "Climate change: current risk to environment (1-10)",
    "Climate change: current risk to self (1-10)",
    "MPs: current risk to environment (1-10)",
    "MPs: current risk to self (1-10)",
    
    # Dispersion model variables
    "Intercept_Disp",
    "Uncertainty: low (+/- 1 point)",
    "Uncertainty: medium (+/- 3 points)",
    "Uncertainty: high (+/- 5 points)",
    "Aged older than sample median (0/1)_Disp",
    "Ethnicity: any white group (0/1)_Disp",
    "Gender: female (0/1)_Disp",
    "Charity: involved (0/1)_Disp",
    "Higher education experience (0/1)_Disp",
    "Survey speeder dummy (0/1)",
    "Survey order (0/1)",
    "Payment vehicle (0/1)",
    "Consequentiality (0/1)",
    "Coronavirus (0/1)",
    "Understanding (1-10)",
    
    # First stage diagnostics
    "AIC",
    "Log-likelihood",
    "Pseudo R2",
    
    # Second stage
    "Impact of change in bid level on income",
    "Predicted impact of changes in expected harm",
    "Predicted impact of variance in expected harm",
    
    # Second stage diagnostics
    "AIC_S2",
    "Log-likelihood_S2",
    "Pseudo R2_S2",
    "EOP (SD)"
  )
  
  # Map variables to their nice names in each dataset
  models <- list(B1, B2, B3, B4)
  for (i in 1:length(models)) {
    model <- models[[i]]
    model$NiceName <- sapply(model$Variable, function(x) {
      if (x %in% names(var_mapping)) {
        return(var_mapping[x])
      } else {
        return(NA)
      }
    })
    models[[i]] <- model
  }
  
  # Create empty dataframe with desired structure
  result <- data.frame(
    Variable = final_vars,
    B1 = rep("", length(final_vars)),
    B2 = rep("", length(final_vars)),
    B3 = rep("", length(final_vars)),
    B4 = rep("", length(final_vars)),
    stringsAsFactors = FALSE
  )
  
  # Fill in values from each model
  for (i in 1:length(models)) {
    model_name <- paste0("B", i)
    model <- models[[i]]
    
    for (j in 1:nrow(model)) {
      if (!is.na(model$NiceName[j])) {
        # Handle dispersion model variables that appear in both sections
        if (model$NiceName[j] %in% c("Aged older than sample median (0/1)", 
                                     "Ethnicity: any white group (0/1)",
                                     "Gender: female (0/1)",
                                     "Charity: involved (0/1)",
                                     "Higher education experience (0/1)") &&
            model$Variable[j] != "X.Intercept." && 
            model$Variable[j] != "X.Intercept..1") {
          
          # Check if this appears after the dispersion intercept in the original data
          intercept_disp_row <- which(model$Variable == "X.Intercept..1")
          if (length(intercept_disp_row) > 0 && j > intercept_disp_row[1]) {
            search_var <- paste0(model$NiceName[j], "_Disp")
          } else {
            search_var <- model$NiceName[j]
          }
        } else {
          search_var <- model$NiceName[j]
        }
        
        var_idx <- which(result$Variable == search_var)
        if (length(var_idx) > 0) {
          result[var_idx, model_name] <- model$Estimate[j]
        }
      }
    }
  }
  
  # Add section headers
  sections <- list(
    "**First stage: Mean model (λ̂)**" = 1,
    "**First stage: Dispersion model (ψ̂)**" = 11,
    "**First stage: Diagnostics**" = 26,
    "**Second stage: WTP model (α̂,β̂,γ̂)**" = 29,
    "**Second stage: Diagnostics**" = 32
  )
  
  final_result <- data.frame(
    Variable = character(),
    B1 = character(),
    B2 = character(),
    B3 = character(),
    B4 = character(),
    stringsAsFactors = FALSE
  )
  
  section_positions <- c(1, 11, 26, 29, 32, nrow(result) + 1)
  
  for (s in 1:length(sections)) {
    section_name <- names(sections)[s]
    section_start <- section_positions[s]
    section_end <- section_positions[s + 1] - 1
    
    # Add section header
    final_result <- rbind(
      final_result,
      data.frame(
        Variable = section_name,
        B1 = "",
        B2 = "",
        B3 = "",
        B4 = "",
        stringsAsFactors = FALSE
      )
    )
    
    # Add rows for this section, cleaning up variable names
    section_rows <- result[section_start:section_end, ]
    section_rows$Variable <- gsub("_Disp$", "", section_rows$Variable)  # Remove _Disp suffix
    section_rows$Variable <- gsub("Intercept_Disp", "Intercept", section_rows$Variable)  # Fix intercept
    section_rows$Variable <- gsub("AIC_S2", "AIC", section_rows$Variable)  # Fix S2 diagnostics
    section_rows$Variable <- gsub("Log-likelihood_S2", "Log-likelihood", section_rows$Variable)
    section_rows$Variable <- gsub("Pseudo R2_S2", "Pseudo R2", section_rows$Variable)
    
    final_result <- rbind(final_result, section_rows)
  }
  
  # Add significance note
  final_result <- rbind(
    final_result,
    data.frame(
      Variable = "*** = P value < 0.01, ** = P value < 0.05, * = P value < 0.10, . = P value < 0.15",
      B1 = "",
      B2 = "",
      B3 = "",
      B4 = "",
      stringsAsFactors = FALSE
    )
  )
  
  # Rename columns
  colnames(final_result) <- c("Variable", "Model B1", "Model B2", "Model B3", "Model B4")
  
  return(final_result)
}
# ***********************************************************
# Section 3: Export Data ####
# ***********************************************************



# Create the table
TableC3 <- create_robustness_table(Robustness_B1, Robustness_B2, Robustness_B3, Robustness_B4)


colnames(TableC3) <- c("Variable", 
  "Mean model: risks only Dispersion: selected covariates",	
  "Mean model: risks only Dispersion: all covariates",
  "Mean model: all covariates Dispersion: selected covariates",
  "Mean model: selected covariates Dispersion: original model"
)


# ***********************************************************
# Section 4: Export Data ####
# ***********************************************************


TableC3 %>% 
  data.frame() %>% 
  fwrite(sep = ",",
         here("CVoutput/Tables", 
              "TableC3_RobustnessStage1Models.txt"))


