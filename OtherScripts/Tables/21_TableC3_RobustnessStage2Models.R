#### Microplastics: IOP Paper ####
## Function: Makes Table C3
## Author: PK
## Last change: 14/07/2025
# Changes:
# - 


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


# Read the data
M1 <- here("CVoutput/Tables", 
           "Table_RobustnessStage2_ModelC1.txt") %>% 
  fread() %>% data.frame()

M2 <- here("CVoutput/Tables", 
           "Table_RobustnessStage2_ModelC2.txt") %>% 
  fread() %>% data.frame()

M3 <- here("CVoutput/Tables", 
           "Table_RobustnessStage2_ModelC3.txt") %>% 
  fread() %>% data.frame()

M4 <- here("CVoutput/Tables", 
           "Table_RobustnessStage2_ModelC4.txt") %>% 
  fread() %>% data.frame()


# ***********************************************************
# Section 2: Capture variable names ####
# ***********************************************************


# Create comparison table
# Get all unique variables across models
all_vars <- unique(c(M1$Variable, M2$Variable, M3$Variable))



# Define the desired order
## BE CAREFUL IF YOU CHANGE CVSCRIPTS/09*
desired_order <- c(
  "X.Intercept.",
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
  "X.Intercept..1",
  "as.factor.Uncertainty.1",
  "as.factor.Uncertainty.3",
  "as.factor.Uncertainty.5",
  "S1_AIC",
  "S1_LogLik",
  "S1_PseudoR2",
  "LogBidIncome",
  "I..predict.stage_1..type....response....",
  "I.predict.stage_1..type....variance...",
  "AgeDummy.1",
  "EthnicityDummy.1",
  "Gender_Dummy.1",
  "Charity.1",
  "Education_HigherEd.1",
  "Q16_ClimateCurrentEnvironment.1",
  "Q16_ClimateCurrentSelf.1",
  "Q16_MicroplasticsCurrentEnvironment.1",
  "Q16_MicroplasticsCurrentSelf.1",
  "Q16_MicroplasticsTen.1",
  "Q16_MicroplasticsTwentyFive.1",
  "Q16_MicroplasticsFifty.1",
  "Speeders_Survey_TestDummy",
  "Order",
  "PaymentVehicle_Dummy",
  "Consequentiality",
  "Coronavirus",
  "Understanding",
  "S2_AIC",
  "S2_LogLik",
  "S2_PseudoR2",
  "S2_EOP_Mean"
)


# ***********************************************************
# Section 3: Construct table ####
# ***********************************************************


# Create base data frame with all variables
comparison_table <- data.frame(Variable = all_vars)


# Merge estimates from each model
comparison_table <- comparison_table %>%
  left_join(M1 %>% dplyr::select(Variable, M1 = Estimate), by = "Variable") %>%
  left_join(M2 %>% dplyr::select(Variable, M2 = Estimate), by = "Variable") %>%
  left_join(M3 %>% dplyr::select(Variable, M3 = Estimate), by = "Variable") %>% 
  left_join(M4 %>% dplyr::select(Variable, M4 = Estimate), by = "Variable")


# Clean up - replace NA with empty string or dash
comparison_table[is.na(comparison_table)] <- ""


# Reorder the table
comparison_table <- comparison_table %>%
  mutate(Variable = factor(Variable, levels = desired_order)) %>%
  arrange(Variable) %>%
  mutate(Variable = as.character(Variable))


# ***********************************************************
# Section 4: Export table ####
# ***********************************************************


comparison_table %>% write.csv(quote = FALSE, row.names = FALSE)


comparison_table %>% 
  data.frame() %>% 
  fwrite(sep = ",",
         here("CVoutput/Tables", 
              "Table_RobustnessStage2_AllModels.txt"))
