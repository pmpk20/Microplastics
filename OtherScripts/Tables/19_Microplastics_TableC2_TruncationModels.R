#### Microplastics: IOP Paper ####
## Function: Makes Table C2
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


Truncation_T1 <- here("CVoutput/Tables", 
                      "Table_Truncation_ModelT1_Speed.txt") %>% 
  fread() %>% 
  data.frame()


Truncation_T2 <- here("CVoutput/Tables", 
                      "Table_Truncation_ModelT2_A_PV.txt") %>% 
  fread() %>% 
  data.frame()


Truncation_T3 <- here("CVoutput/Tables", 
                      "Table_Truncation_ModelT3_B_Order.txt") %>% 
  fread() %>% 
  data.frame()


Truncation_T4 <- here("CVoutput/Tables", 
     "Table_Truncation_ModelT4_Cons.txt") %>% 
  fread() %>% 
  data.frame()



# ***********************************************************
# Section 2: Combine tables ####
# ***********************************************************


TableC2 <- cbind(
  Truncation_T1,
  Truncation_T2,
  Truncation_T3,
  Truncation_T4
)




# ***********************************************************
# Section 3: Export Data ####
# ***********************************************************


TableC2 %>% 
  data.frame() %>% 
  fwrite(sep = ",",
         here("CVoutput/Tables", 
              "TableC2_TruncationModels.txt"))


