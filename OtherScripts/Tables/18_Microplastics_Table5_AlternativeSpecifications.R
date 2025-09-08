#### Microplastics: IOP Paper ####
## Function: Makes Table 4
## Author: PK
## Last change: 25/02/2025
# Changes:
# - Claude made most of this to save me time manipulating in Word



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


Table_Alternative1_Combined <- here("CVoutput/Tables", 
         "Table_Alternative1_Combined.txt") %>% fread() %>% 
  data.frame()


Table_Alternative2_Combined <- here("CVoutput/Tables", 
                                    "Table_Alternative2_Combined.txt") %>% fread() %>% 
  data.frame()


# ***********************************************************
# Section 2: Define function ####
# ***********************************************************


# Function to create the table
create_Table5 <- function(table1, table2) {
  
  # Function to extract and format coefficient
  format_coefficient <- function(estimate_string) {
    if (estimate_string == "" || is.na(estimate_string)) {
      return("")
    }
    
    # Special handling for EOP (contains £ symbols)
    if (grepl("£", estimate_string)) {
      return(estimate_string)  # Return as-is for currency values
    }
    
    # Extract coefficient (everything before the first space or parenthesis)
    coef <- gsub("^([0-9.-]+).*", "\\1", estimate_string)
    
    # Extract significance stars
    stars <- gsub(".*?([*]+).*", "\\1", estimate_string)
    if (!grepl("\\*", estimate_string)) stars <- ""
    
    # Extract standard error (everything between parentheses)
    se <- gsub(".*\\(([0-9.-]+)\\).*", "\\1", estimate_string)
    if (!grepl("\\(", estimate_string)) se <- ""
    
    # Format coefficient to 3 decimal places
    coef_formatted <- sprintf("%.3f", as.numeric(coef))
    
    # Reconstruct the estimate string
    if (se != "") {
      se_formatted <- sprintf("%.3f", as.numeric(se))
      result <- paste0(coef_formatted, stars, " (", se_formatted, ")")
    } else {
      result <- paste0(coef_formatted, stars)
    }
    
    return(result)
  }
  
  # Define the variable order and section headings
  sections <- list(
    "Mean model" = c(
      "X.Intercept.", 
      "AgeDummy", 
      "Charity", 
      "EthnicityDummy", 
      "Gender_Dummy", 
      "Education_HigherEd", 
      "Q16_ClimateCurrentEnvironment", 
      "Q16_ClimateCurrentSelf",
      "Q16_MicroplasticsCurrentEnvironment", 
      "Q16_MicroplasticsCurrentSelf"
    ),
    "Dispersion model" = c(
      "X.Intercept..1", 
      "as.numeric.Uncertainty.", 
      "VarianceChange"
    ),
    "Second stage" = c(
      "LogBidIncome", 
      "I..predict.stage_1..type....response....", 
      "I.predict.stage_1..type....variance..."
    ),
    "First stage diagnostics" = c(
      "S1_AIC", "S1_LogLik", "S1_PseudoR2"
    ),
    "Second stage diagnostics" = c(
      "S2_AIC", "S2_LogLik", "S2_PseudoR2", "S2_EOP_Mean"
    )
  )
  
  # Create variable name mapping
  var_names <- c(
    "X.Intercept." = "Intercept",
    "AgeDummy" = "Aged older than sample median (0/1)",
    "Charity" = "Charity: involved (0/1)",
    "EthnicityDummy" = "Ethnicity any white group (0/1)",
    "Gender_Dummy" = "Gender: female (0/1)",
    "Education_HigherEd" = "Higher education experience (0/1)",
    "Q16_ClimateCurrentEnvironment" = "Climate change current risk to the environment (1-10)",
    "Q16_ClimateCurrentSelf" = "Climate change current risk to the self (1-10)",
    "Q16_MicroplasticsCurrentEnvironment" = "Microplastics current risk to the environment (1-10)",
    "Q16_MicroplasticsCurrentSelf" = "Microplastics current risk to the self (1-10)",
    "X.Intercept..1" = "Intercept",
    "as.numeric.Uncertainty." = "Uncertainty (0-5)",
    "VarianceChange" = "VarianceChange: upper bound (-10/+10)",
    "LogBidIncome" = "Log-bid income relationship",
    "I..predict.stage_1..type....response...." = "Mean prediction",
    "I.predict.stage_1..type....variance..." = "Dispersion prediction",
    "S1_AIC" = "AIC",
    "S1_LogLik" = "Log-likelihood",
    "S1_PseudoR2" = "Pseudo R2",
    "S2_AIC" = "AIC",
    "S2_LogLik" = "Log-likelihood",
    "S2_PseudoR2" = "Residual Deviance",
    "S2_EOP_Mean" = "EOP (SD)"
  )
  
  # Initialize the results table
  result <- data.frame(
    Variable = character(),
    Alt1 = character(),
    Alt2 = character(),
    stringsAsFactors = FALSE
  )
  
  # Build the table section by section
  for (section_name in names(sections)) {
    # Add section header
    result <- rbind(
      result,
      data.frame(
        Variable = paste0("**", section_name, "**"),
        Alt1 = "",
        Alt2 = "",
        stringsAsFactors = FALSE
      )
    )
    
    # Add variables for this section
    for (var in sections[[section_name]]) {
      if (var %in% table1$Variable) {
        est1_raw <- table1$Estimate[table1$Variable == var]
        est1 <- format_coefficient(est1_raw)
      } else {
        est1 <- ""
      }
      
      if (var %in% table2$Variable) {
        est2_raw <- table2$Estimate[table2$Variable == var]
        est2 <- format_coefficient(est2_raw)
      } else {
        est2 <- ""
      }
      
      result <- rbind(
        result,
        data.frame(
          Variable = var_names[var],
          Alt1 = est1,
          Alt2 = est2,
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  # Add significance note
  result <- rbind(
    result,
    data.frame(
      Variable = "*** = P value < 0.01, ** = P value < 0.05, * = P value < 0.10",
      Alt1 = "",
      Alt2 = "",
      stringsAsFactors = FALSE
    )
  )
  
  # Rename columns
  colnames(result) <- c("Variable", "Alternative Model I", "Alternative Model II")
  
  return(result)
}


# ***********************************************************
# Section 3: Make table ####
# ***********************************************************



# Create the table
Table5 <- create_Table5(Table_Alternative1_Combined, Table_Alternative2_Combined)


# ***********************************************************
# Section 4: Export table ####
# ***********************************************************

# Write the final combined output
Table5 %>%
  data.frame() %>%
  fwrite(
    sep = ",",
    here("CVoutput/Tables", 
         "Table5_AlternativeSpecifications.txt")
  )


# END OF SCRIPT  ***********************************************************