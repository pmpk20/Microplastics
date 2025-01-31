#### Microplastics: IOP Paper ####
## Function: Just report summary stats for use in the manuscript
## Author: PK
## Last change: 26/04/2024
# Change: create file



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
library(rstatix)


# ***********************************************************
# Section 1: Import Data ####
# ***********************************************************

Data <-
  here("Data",
       "Microplastics_AllData_Long_Anonymised_2022_06_19.csv") %>%
  fread() %>%
  data.frame()


Data <- here("Data",
             "Microplastics_AllData_Wide_Anonymised.csv") %>% 
  fread() %>% 
  data.frame()



# ***********************************************************
# Section 2: Trim Data ####
# ***********************************************************

Data_Step1 <- Data[Data$Gender < 2, ]
# Data_Step1 <- Data

Data_Step1$IncomeMonthly <- (Data_Step1$Income * 12)

Data_Step1 <- Data_Step1 %>%
  mutate(Income_Bracket = case_when(
    Income == 250 ~ "£0 – £20,500",
    Income == 750 ~ "£0 – £20,500",
    Income == 1250 ~ "£0 – £20,500",
    Income == 1750 ~ "£20,501 – £26,800",
    Income == 2250 ~ "£26,801 – £54,000",
    Income == 2500 ~ "£26,801 – £54,000",
    Income == 2750 ~ "£26,801 – £54,000",
    Income == 3500 ~ "£26,801 – £54,000",
    Income == 4500 ~ "£54,000+",
    Income == 5000 ~ "£54,000+",
    TRUE ~ NA_character_ # To handle any potential unexpected values
  ))

# ***********************************************************
# Section 3A: Gender ####
# ***********************************************************


# Data_Step1$Gender <-Data_Step1$Gender %>% ifelse(. > 1, 2, .)


# Your observed sample counts
observed_Gender <-
  Data_Step1 %>% rstatix::freq_table(Gender) 

# Population proportions you are testing against
population_Gender <- c(0.49, 0.51)

# Perform the chi-square goodness-of-fit test
result_Gender <- cbind(
  Variable = "Gender" ,
  chisq_test(
    x = observed_Gender %>% dplyr::select(n),
    p = population_Gender,
    simulate.p.value = TRUE,
    rescale.p = TRUE
  ) %>% dplyr::select(statistic, p)
)



## Stitch columns here
Descriptive_Gender <- cbind(
  observed_Gender,
  population_Gender %>% multiply_by(100) %>% round(2) %>% paste0(., "%"),
  result_Gender$Variable,
  result_Gender$statistic %>% round(2),
  result_Gender$p %>% round(2))


colnames(Descriptive_Gender) <- 
  c("Category",
    "N",
    "sample",
    "population",
    "Variable",
    "statistic",
    "pvalue")



# ***********************************************************
# Section 3B: Age ####
## More involved as we have to rescale the census to sum to 1
## and exclude <18
# ***********************************************************

observed_AgeBracket <-
  Data_Step1 %>% rstatix::freq_table(AgeBracket) 




# Original proportions for age 18 and above
original_proportions <- c(16.2, 13.3, 14.6, 12.1, 10.8, 11.7)

# Sum of the original proportions
total_proportion <- sum(original_proportions) # 78.7

# Rescaled proportions
rescaled_proportions <- original_proportions / total_proportion * 100

# Display the rescaled proportions
rescaled_proportions



# Population proportions you are testing against
population_AgeBracket <- rescaled_proportions %>% 
  magrittr::divide_by(100)





# Perform the chi-square goodness-of-fit test
result_AgeBracket <- cbind(
  Variable = "AgeBracket" ,
  chisq_test(
    x = observed_AgeBracket %>% dplyr::select(n),
    p = population_AgeBracket,
    simulate.p.value = TRUE,
    rescale.p = TRUE
  ) %>% dplyr::select(statistic, p)
)



## Stitch columns here
Descriptive_AgeBracket <- cbind(
  observed_AgeBracket,
  population_AgeBracket %>% multiply_by(100) %>% round(2) %>% paste0(., "%"),
  result_AgeBracket$Variable,
  result_AgeBracket$statistic %>% round(2),
  result_AgeBracket$p %>% round(2))


colnames(Descriptive_AgeBracket) <- 
  c("Category",
    "N",
    "sample",
    "population",
    "Variable",
    "statistic",
    "pvalue")



# ***********************************************************
# Section 3C: Ethnicity ####
# ***********************************************************

observed_EthnicityDummy <-
  Data_Step1 %>% rstatix::freq_table(EthnicityDummy)

# Population proportions you are testing against
population_EthnicityDummy <- c(87,
                               13)/100

# Perform the chi-square goodness-of-fit test
result_EthnicityDummy <- cbind(
  Variable = "EthnicityDummy" ,
  chisq_test(
    x = observed_EthnicityDummy %>% dplyr::select(n),
    p = population_EthnicityDummy,
    simulate.p.value = TRUE,
    rescale.p = TRUE
  ) %>% dplyr::select(statistic, p)
)


## Stitch together here
Descriptive_EthnicityDummy <- cbind(
  observed_EthnicityDummy,
  population_EthnicityDummy %>% multiply_by(100) %>% round(2) %>% paste0(., "%"),
  result_EthnicityDummy$Variable,
  result_EthnicityDummy$statistic %>% round(2),
  result_EthnicityDummy$p %>% round(2))



colnames(Descriptive_EthnicityDummy) <- 
  c("Category",
    "N",
    "sample",
    "population",
    "Variable",
    "statistic",
    "pvalue")

# ***********************************************************
# Section 3D: Income ####
# ***********************************************************


observed_Income_Bracket <-
  Data_Step1 %>% rstatix::freq_table(Income_Bracket) 

# Population proportions you are testing against
population_Income_Bracket <- c(26.0,
                               19.0,
                               25.0,
                               30.0) / 100

# Perform the chi-square goodness-of-fit test
result_Income_Bracket <- cbind(
  Variable = "Income_Bracket" ,
  chisq_test(
    x = observed_Income_Bracket %>% dplyr::select(n),
    p = population_Income_Bracket,
    simulate.p.value = TRUE,
    rescale.p = TRUE
  ) %>% dplyr::select(statistic, p)
)


Descriptive_Income_Bracket <- cbind(
  observed_Income_Bracket,
  population_Income_Bracket %>% multiply_by(100) %>% round(2) %>% paste0(., "%"),
  result_Income_Bracket$Variable,
  result_Income_Bracket$statistic %>% round(2),
  result_Income_Bracket$p %>% round(2))

colnames(Descriptive_Income_Bracket) <- 
  c("Category",
    "N",
    "sample",
    "population",
    "Variable",
    "statistic",
    "pvalue")


# ***********************************************************
# Section 4: Combining ####
# ***********************************************************



TestOutputs <- rbind(
  Descriptive_AgeBracket,
  Descriptive_EthnicityDummy,
  Descriptive_Gender,
  Descriptive_Income_Bracket
)

## Rearrange columns
TableX <- cbind(
  TestOutputs$Variable,
  TestOutputs$Category,
  TestOutputs$N,
  TestOutputs$sample,
  TestOutputs$population,
  TestOutputs$statistic,
  TestOutputs$pvalue
) %>% data.frame()

colnames(TableX) <-   c("Variable",
                        "Category",
                        "N",
                        "sample",
                        "population",
                        "statistic",
                        "pvalue")


# ***********************************************************
# Section 5: Export ####
# ***********************************************************


## Output to screen in a nice format for making tables in word
TableX %>% write.csv(quote = FALSE, row.names = FALSE)

## Output to a discrete file if that's helpful
TableX %>% fwrite(
  sep = ",",
  here("CVoutput/Tables", "TableB1_SampleTest.txt"),
  row.names = TRUE,
  quote = FALSE
)


# *************************************************************************
#### END OF SCRIPT ####
# *************************************************************************