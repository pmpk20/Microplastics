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
    "Q16_ClimateCurrentEnvironment" = "Climate change: current risk to environment (1-10)",
    "Q16_ClimateCurrentSelf" = "Climate change: current risk to self (1-10)",
    "Q16_MicroplasticsCurrentEnvironment" = "MPs: current risk to humanity (1-10)",
    "Q16_MicroplasticsCurrentSelf" = "Microplastics: current risk to self (1-10)",
    "PaymentVehicle_Dummy" = "Payment vehicle (0/1)",
    "Consequentiality" = "Consequentiality (0/1)",
    "Coronavirus" = "Coronavirus (0/1)",
    "Understanding" = "Understanding (1-10)",
    "AgeDummy" = "Aged older than sample median (0/1)",
    "EthnicityDummy" = "Ethnicity any white group (0/1)",
    "Gender_Dummy" = "Gender: female (0/1)",
    "Charity" = "Charity: involved (0/1)",
    "Education_HigherEd" = "Higher education experience (0/1)",
    "Speeders_Survey_TestDummy" = "Survey speeder dummy (0/1)",
    "Order" = "Survey order (0/1)",
    "X.Intercept..1" = "Intercept",
    "as.factor.Uncertainty.1" = "Uncertainty: low (+/- 1 point)",
    "as.factor.Uncertainty.3" = "Uncertainty: medium (+/- 3 points)",
    "as.factor.Uncertainty.5" = "Uncertainty: high (+/- 5 points)",
    "S1_AIC" = "AIC",
    "S1_LogLik" = "Log-likelihood",
    "S1_PseudoR2" = "Pseudo-R2",
    "LogBidIncome" = "α (i.e., LogBidIncome)",
    "I..predict.stage_1..type....response...." = "β (i.e., Means)",
    "I.predict.stage_1..type....variance..." = "γ (i.e., Variance)",
    "S2_AIC" = "AIC",
    "S2_LogLik" = "Log-likelihood",
    "S2_PseudoR2" = "Pseudo-R2",
    "S2_EOP_Mean" = "EOP (SD)"
  )
  
  # Define the final variable order
  final_vars <- c(
    # Mean model variables
    "Intercept",
    "Climate change: current risk to environment (1-10)",
    "Climate change: current risk to self (1-10)",
    "MPs: current risk to humanity (1-10)",
    "Microplastics: current risk to self (1-10)",
    "Payment vehicle (0/1)",
    "Consequentiality (0/1)",
    "Coronavirus (0/1)",
    "Understanding (1-10)",
    "Aged older than sample median (0/1)",
    "Ethnicity any white group (0/1)",
    "Gender: female (0/1)",
    "Charity: involved (0/1)",
    "Higher education experience (0/1)",
    "Survey speeder dummy (0/1)",
    "Survey order (0/1)",
    
    # Dispersion model variables
    "Intercept",
    "Uncertainty: low (+/- 1 point)",
    "Uncertainty: medium (+/- 3 points)",
    "Uncertainty: high (+/- 5 points)",
    "Aged older than sample median (0/1)",
    "Ethnicity any white group (0/1)",
    "Gender: female (0/1)",
    "Charity: involved (0/1)",
    "Higher education experience (0/1)",
    "Survey speeder dummy (0/1)",
    "Survey order (0/1)",
    "Payment vehicle (0/1)",
    "Consequentiality (0/1)",
    "Coronavirus (0/1)",
    "Understanding (1-10)",
    
    # First stage diagnostics
    "AIC",
    "Log-likelihood",
    "Pseudo-R2",
    
    # Second stage
    "α (i.e., LogBidIncome)",
    "β (i.e., Means)",
    "γ (i.e., Variance)",
    
    # Second stage diagnostics
    "AIC",
    "Log-likelihood",
    "Pseudo-R2",
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
        var_idx <- which(result$Variable == model$NiceName[j])
        if (length(var_idx) > 0) {
          # For dispersion model intercept, need to handle duplicate name
          if (model$NiceName[j] == "Intercept" && model$Variable[j] == "X.Intercept..1") {
            var_idx <- var_idx[2]  # Use the second "Intercept" in the list
          }
          result[var_idx, model_name] <- model$Estimate[j]
        }
      }
    }
  }
  
  # Add section headers
  sections <- list(
    "Mean model" = 1,
    "Dispersion model" = 20,
    "First stage diagnostics" = 35,
    "Second stage" = 38,
    "Second stage diagnostics" = 41
  )
  
  final_result <- data.frame(
    Variable = character(),
    B1 = character(),
    B2 = character(),
    B3 = character(),
    B4 = character(),
    stringsAsFactors = FALSE
  )
  
  current_row <- 1
  for (section in names(sections)) {
    section_start <- sections[[section]]
    
    # Add section header
    final_result <- rbind(
      final_result,
      data.frame(
        Variable = paste0("**", section, "**"),
        B1 = "",
        B2 = "",
        B3 = "",
        B4 = "",
        stringsAsFactors = FALSE
      )
    )
    
    # Determine section end
    if (section == tail(names(sections), 1)) {
      section_end <- nrow(result)
    } else {
      next_section <- which(names(sections) == section) + 1
      section_end <- sections[[names(sections)[next_section]]] - 1
    }
    
    # Add rows for this section
    final_result <- rbind(
      final_result,
      result[section_start:section_end, ]
    )
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
# Section X: New code to verify ####
# ***********************************************************




create_robustness_table <- function(B1, B2, B3, B4) {
  
  format_coefficient <- function(estimate_string) {
    if (estimate_string == "" || is.na(estimate_string)) {
      return("")
    }
    
    if (grepl("£", estimate_string)) {
      return(estimate_string)
    }
    
    # Extract coefficient 
    coef <- gsub("^([0-9.-]+).*", "\\1", estimate_string)
    
    # Extract ALL significance markers (*, .)
    stars <- gsub(".*?([*.]+).*", "\\1", estimate_string)
    if (!grepl("[*.]", estimate_string)) stars <- ""
    
    # Extract standard error
    se <- gsub(".*\\(([0-9.-]+)\\).*", "\\1", estimate_string)
    if (!grepl("\\(", estimate_string)) se <- ""
    
    coef_formatted <- sprintf("%.3f", as.numeric(coef))
    
    if (se != "") {
      se_formatted <- sprintf("%.3f", as.numeric(se))
      result <- paste0(coef_formatted, stars, " (", se_formatted, ")")
    } else {
      result <- paste0(coef_formatted, stars)
    }
    
    return(result)
  }
  
  get_estimate <- function(table, var_name) {
    if (var_name %in% table$Variable) {
      return(format_coefficient(table$Estimate[table$Variable == var_name]))
    } else {
      return("")
    }
  }
  
  # Separate dispersion variables from mean variables
  sections <- list(
    "First stage: Mean model" = c(
      "X.Intercept.", "AgeDummy", "EthnicityDummy", "Gender_Dummy", "Charity",
      "Education_HigherEd", "Speeders_Survey_TestDummy", "Order", 
      "PaymentVehicle_Dummy", "Consequentiality", "Coronavirus", "Understanding",
      "Q16_ClimateCurrentEnvironment", "Q16_ClimateCurrentSelf",
      "Q16_MicroplasticsCurrentEnvironment", "Q16_MicroplasticsCurrentSelf"
    ),
    "First stage: Dispersion model" = c(
      "X.Intercept..1", "as.factor.Uncertainty.1", "as.factor.Uncertainty.3", 
      "as.factor.Uncertainty.5"
      # Note: removed duplicate demographics - they're handled in mean model section
    ),
    "First stage: Diagnostics" = c("S1_AIC", "S1_LogLik", "S1_PseudoR2"),
    "Second stage: WTP model" = c(
      "LogBidIncome", "I..predict.stage_1..type....response....", 
      "I.predict.stage_1..type....variance..."
    ),
    "Second stage: Diagnostics" = c("S2_AIC", "S2_LogLik", "S2_PseudoR2", "S2_EOP_Mean")
  )
  
  # Variable name mapping
  var_names <- c(
    "X.Intercept." = "Intercept",
    "AgeDummy" = "Aged older than sample median (0/1)",
    "EthnicityDummy" = "Ethnicity: any white group (0/1)", 
    "Gender_Dummy" = "Gender: female (0/1)",
    "Charity" = "Charity: involved (0/1)",
    "Education_HigherEd" = "Higher education experience (0/1)",
    "Speeders_Survey_TestDummy" = "Survey speeder dummy (0/1)",
    "Order" = "Survey order (0/1)",
    "PaymentVehicle_Dummy" = "Payment vehicle (0/1)",
    "Consequentiality" = "Consequentiality (0/1)", 
    "Coronavirus" = "Coronavirus (0/1)",
    "Understanding" = "Understanding (1-10)",
    "Q16_ClimateCurrentEnvironment" = "Climate change current risk to environment (1-10)",
    "Q16_ClimateCurrentSelf" = "Climate change current risk to self (1-10)",
    "Q16_MicroplasticsCurrentEnvironment" = "Microplastics current risk to environment (1-10)",
    "Q16_MicroplasticsCurrentSelf" = "Microplastics current risk to self (1-10)",
    "X.Intercept..1" = "Intercept",
    "as.factor.Uncertainty.1" = "Uncertainty: low (+/- 1 point)",
    "as.factor.Uncertainty.3" = "Uncertainty: medium (+/- 3 points)",
    "as.factor.Uncertainty.5" = "Uncertainty: high (+/- 5 points)",
    "LogBidIncome" = "Impact of change in bid level on income",
    "I..predict.stage_1..type....response...." = "Predicted impact of changes in expected harm",
    "I.predict.stage_1..type....variance..." = "Predicted impact of variance in expected harm",
    "S1_AIC" = "AIC", 
    "S1_LogLik" = "Log-likelihood",
    "S1_PseudoR2" = "Pseudo R2",
    "S2_AIC" = "AIC",
    "S2_LogLik" = "Log-likelihood", 
    "S2_PseudoR2" = "Pseudo R2",
    "S2_EOP_Mean" = "EOP (SD)"
  )
  
  # Initialize results
  result <- data.frame(
    Variable = character(),
    Model1 = character(),
    Model2 = character(), 
    Model3 = character(),
    Model4 = character(),
    stringsAsFactors = FALSE
  )
  
  # Build table
  for (section_name in names(sections)) {
    result <- rbind(result, data.frame(
      Variable = paste0("**", section_name, "**"),
      Model1 = "", Model2 = "", Model3 = "", Model4 = "",
      stringsAsFactors = FALSE
    ))
    
    for (var in sections[[section_name]]) {
      result <- rbind(result, data.frame(
        Variable = var_names[var],
        Model1 = get_estimate(B1, var),
        Model2 = get_estimate(B2, var),
        Model3 = get_estimate(B4, var), # B4 is the simple model
        Model4 = get_estimate(B3, var),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Add significance note
  result <- rbind(result, data.frame(
    Variable = "*** = P value < 0.01, ** = P value < 0.05, * = P value < 0.10",
    Model1 = "", Model2 = "", Model3 = "", Model4 = "",
    stringsAsFactors = FALSE
  ))
  
  # Set column names
  colnames(result) <- c("Variable", 
                        "Same mean model, selected covariates in dispersion",
                        "Same mean model, all covariates in dispersion", 
                        "Only long-term risks in mean model",
                        "All covariates in mean model")
  
  return(result)
}

# Create the table
robustness_table <- create_robustness_table(Robustness_B1, Robustness_B2, Robustness_B3, Robustness_B4)


# ***********************************************************
# Section 3: Export Data ####
# ***********************************************************



# Create the table
TableC3 <- create_robustness_table(Robustness_B1, Robustness_B2, Robustness_B3, Robustness_B4)


# ***********************************************************
# Section 4: Export Data ####
# ***********************************************************


TableC3 %>% 
  data.frame() %>% 
  fwrite(sep = ",",
         here("CVoutput/Tables", 
              "TableC3_RobustnessStage1Models.txt"))


