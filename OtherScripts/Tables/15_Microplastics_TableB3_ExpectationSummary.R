#### Microplastics: IOP Paper ####
## Function: Table B3 
## Author: PK
## Last change: 31/01/25
# Comment:
## - Thank you claude for tidying this code


# *****************************
# Replication Information: ####
# Selected output of 'sessionInfo()'
# *****************************
# R version 4.3.0 (2023-04-21)
# Platform: x86_64-apple-darwin20 (64-bit)
# Running under: macOS Ventura 13.5
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: Europe/Helsinki
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] AER_1.2-10     survival_3.5-7 sandwich_3.0-2 lmtest_0.9-40  zoo_1.8-12     car_3.1-2      carData_3.0-5 
# [8] boot_1.3-28.1  betareg_3.1-4 
# 
# loaded via a namespace (and not attached):
#   [1] gridExtra_2.3        inline_0.3.19        rlang_1.1.2          magrittr_2.0.3       multcomp_1.4-25     
# [6] matrixStats_1.1.0    compiler_4.3.0       flexmix_2.3-19       loo_2.6.0            callr_3.7.3         
# [11] vctrs_0.6.4          reshape2_1.4.4       stringr_1.5.1        pkgconfig_2.0.3      crayon_1.5.2        
# [16] fastmap_1.1.1        backports_1.4.1      ellipsis_0.3.2       utf8_1.2.4           threejs_0.3.3       
# [21] cmdstanr_0.6.1.9000  promises_1.2.1       markdown_1.11        ps_1.7.5             xfun_0.41           
# [26] modeltools_0.2-23    jsonlite_1.8.7       later_1.3.1          parallel_4.3.0       prettyunits_1.2.0   
# [31] R6_2.5.1             dygraphs_1.1.1.6     stringi_1.8.2        StanHeaders_2.26.28  estimability_1.4.1  
# [36] Rcpp_1.0.11          rstan_2.32.3         knitr_1.45           base64enc_0.1-3      bayesplot_1.10.0    
# [41] httpuv_1.6.12        Matrix_1.6-3         splines_4.3.0        nnet_7.3-19          igraph_1.5.1        
# [46] tidyselect_1.2.0     rstudioapi_0.15.0    abind_1.4-5          codetools_0.2-19     miniUI_0.1.1.1      
# [51] curl_5.1.0           processx_3.8.2       pkgbuild_1.4.2       lattice_0.22-5       tibble_3.2.1        
# [56] plyr_1.8.9           shiny_1.8.0          bridgesampling_1.1-2 posterior_1.5.0      coda_0.19-4         
# [61] RcppParallel_5.1.7   xts_0.13.1           pillar_1.9.0         tensorA_0.36.2       checkmate_2.3.0     
# [66] DT_0.30              stats4_4.3.0         shinyjs_2.1.0        distributional_0.3.2 generics_0.1.3      
# [71] ggplot2_3.4.4        rstantools_2.3.1.1   munsell_0.5.0        scales_1.2.1         gtools_3.9.5        
# [76] xtable_1.8-4         glue_1.6.2           emmeans_1.8.9        tools_4.3.0          shinystan_2.6.0     
# [81] colourpicker_1.3.0   mvtnorm_1.2-3        grid_4.3.0           QuickJSR_1.0.7       crosstalk_1.2.1     
# [86] colorspace_2.1-0     nlme_3.1-163         Formula_1.2-5        cli_3.6.1            fansi_1.0.5         
# [91] Brobdingnag_1.2-9    dplyr_1.1.4          V8_4.4.0             gtable_0.3.4         digest_0.6.33       
# [96] TH.data_1.1-2        brms_2.20.4          htmlwidgets_1.6.3    farver_2.1.1         htmltools_0.5.7     
# [101] lifecycle_1.0.4      mime_0.12            shinythemes_1.2.0    MASS_7.3-60   

rm(list=ls())

## Useful for all scripts:
library(data.table)
library(magrittr)
library(dplyr)
library(tidyverse)
library(here)
library(DCchoice)
library(janitor)



# ***********************************************************
# Section 1: Import Data ####
# ***********************************************************


Data <- here("Data",
       "Microplastics_AllData_Wide_Anonymised.csv") %>% 
  fread() %>% 
  data.frame()



# ***********************************************************
# Section 2: Rearrange Data ####
# ***********************************************************


TableB3 <- bind_rows(
  Data %>%
    tabyl(MeanExpectedCurrent, Uncertainty) %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns(position = "front") %>%
    mutate(Time = "Current") %>%
    rename(Expectations = MeanExpectedCurrent),
  Data %>%
    tabyl(MeanExpectedFuture, Uncertainty) %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns(position = "front") %>%
    mutate(Time = "Future") %>%
    rename(Expectations = MeanExpectedFuture)
) %>%
  dplyr::select(Time, Expectations, `0`, `1`, `3`, `5`)


# ***********************************************************
# Section 3: Export Table ####
# ***********************************************************


TableB3 %>% 
  data.frame() %>% 
  fwrite(sep = ",",
         here("CVoutput/Tables", 
              "TableB3_V2.csv"))


# End Of Script ***********************************************************