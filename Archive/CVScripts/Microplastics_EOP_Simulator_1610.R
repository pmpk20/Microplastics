#### Microplastics: IOP Paper ####
## Function: Simulate EOP at different levels
## Author: PK
## Last change: 14/02/24
# Changes:



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


## Key for this script:
library(betareg)
library(boot)
library(AER)
library(snow)


# ***********************************************************
# Section 1: Import Data ####
# ***********************************************************


Data <-
  here("Data",
       "Data_PlusTestEOP.csv") %>%
  fread() %>%
  data.frame()



# ***********************************************************
# Section 2: Create  additional variables ####
# ***********************************************************


# ## Rescale to check
# Data$Income[Data$Income == 5000] <- 7500


## Income and income weighted bid
Data$Income_Annual <- Data$Income %>% multiply_by(12)
Data$LogBidIncome <-
  log((Data$Income_Annual - Data$Bid) / (Data$Income_Annual))


## Transform Mean expected future
Data$MEF <- (Data$MeanExpectedFuture + 5.001) / 10.002
summary(Data$MEF)


## Transform mean expected current
Data$MEC <-
  (Data$MeanExpectedCurrent + 5.001) / 10.002
summary(Data$MEC)


## Verify that transforms don't change relationships
plot((Data$MeanExpectedFuture + Data$MeanExpectedCurrent),
     (Data$MEF + Data$MEC))


# For consistency with boot function
Data$NewMean <- ((Data$MeanExpectedFuture + Data$MeanExpectedCurrent) / 2)


## Half differences between variance bounds
Data$Uncertainty <-
  ((Data$VarianceLowerBound - Data$VarianceUpperBound) / 2)

## Transform to Cameron (2005) measure
Data$var.cameron <- (0.5 * Data$Uncertainty) ^ 2


## Negative correlation so Uncertainty up means CV down
##
cor.test(Data$var.cameron, Data$CV)

# ****************************************
# Misc variable transformations


Data$Education_HigherEd <- ifelse(Data$Education == 5,
                                  1, ## 1 = higher education
                                  0) ## 0 = all other

## Drop "other" due to small sample
Data_Trim <- Data[Data$Gender < 2, ]
Data$Gender_Female <- ifelse(Data$Gender == 0,
                             "Male",
                             "Female")

Data$Gender_Dummy <- ifelse(Data$Gender_Female == "Female",
                            0,
                            1)

# ********************************************
# Section 2: Define functions ####
# ********************************************



AddStars <- function(ZValues) {
  
  ## 2(1 - pnorm(Z))
  Converted <-  (1 - pnorm(abs(ZValues))) %>% multiply_by(2)
  Converted_trimmed <- Converted %>% round(3) %>% sprintf("%.3f", .)
  ifelse(
    Converted < 0.01,
    paste0(Converted_trimmed, " ***"),
    ifelse(
      Converted < 0.05,
      paste0(Converted_trimmed, " **"),
      ifelse(
        Converted < 0.1,
        paste0(Converted_trimmed, " *"),
        paste0(Converted_trimmed, " "))))
}

# ********************************************
# Section 3: Simple models ####
# ********************************************



stage_1_Simple <- betareg(
  MEF ~
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
    as.factor(Uncertainty),
  Data,
  type = "BC"
)


stage_2_Simple <- glm(CV ~ -1 +
                        LogBidIncome + 
                        I((predict(stage_1_Simple, type = 'response') + MEC) /
                            2) +
                        I(0 - predict(stage_1_Simple, type = 'variance')), 
                      family = binomial(link = 'probit'), 
                      Data)



# ***********************************************************
# Section 2: Define EOP formula ####
# ***********************************************************

# summary_function <- function(data, column) {
#   data %>%
#     summarise(across({{column}}, 
#                      list(
#                        "2.5%" = ~round(quantile(.x, c(0.025)), 3),
#                        Median = ~round(median(.x, na.rm = TRUE), 3), 
#                        Mean = ~round(mean(.x, na.rm = TRUE), 3), 
#                        SD = ~round(sd(.x, na.rm = TRUE), 3),
#                        "97.5%" = ~round(quantile(.x, c(0.025)), 3)),
#                      .names = "{.fn}"), 
#               .groups = "drop")
# }






summary_function <- function(EOP) {
  cbind(
    "2.5%" = round(quantile(EOP, c(0.025)), 3),
    Median = round(median(EOP, na.rm = TRUE), 3), 
    Mean = round(mean(EOP, na.rm = TRUE), 3), 
    SD = round(sd(EOP, na.rm = TRUE), 3),
    "97.5%" = round(quantile(EOP, c(0.025)), 3))
}


EOP_Maker <- function(Data, Means, Variances, Y) {
  
  ## Beta_0 being the parameter on scaled income
  # B0 <- stage_2_Simple["LogBidIncome", "Estimate"] %>% as.numeric()
  B0 <- stage_2_Simple$coefficients["LogBidIncome"] %>% as.numeric()
  # B0 <- 85.78322
  
  
  ## Delta parameters are the mean and var parameters recovered from stage_2
  # Delta_0 <- stage_2_Simple['I((predict(stage_1, type = "response") + MEC)/2)', "Estimate"] %>% as.numeric()
  Delta_0 <- stage_2_Simple$coefficients[3] %>% as.numeric()
  # Delta_0 <- 1.850919
  # Delta_1 <- stage_2_Simple['I(0 - predict(stage_1, type = "variance"))', "Estimate"] %>% as.numeric() 
  Delta_1 <- stage_2_Simple$coefficients[4] %>% as.numeric()
  # Delta_1 <- 15.58868
  
  
  A <-
    ((Delta_0 * Means +
        (Delta_1 * (0 - Variances))
    )) %>% as.numeric()
  
  
  ## Formula here: Y - Y exp(-A/B0)exp(1/2*B0^2)
  EOP <- (Y - (Y*exp(- A / B0))) *
    exp(1 %>% divide_by(B0 %>% raise_to_power(2) %>% multiply_by(2)))
  
  
  return(EOP)
  ## Uncomment this is you want the summary of EOP rather than vector
  # summary_function(EOP) %>% data.frame()
  
  # (100 / (Data$Income_Annual/12) * (EOP/12)) %>% 
  #   summary()
  
}


Means <- c(I((predict(stage_1_Simple, type = "response") + Data$MEC)/2))
Variances <- (betareg::predict(stage_1_Simple, type = "variance"))
## Define Y == gross monthly income * 12
Y <- Data$Income_Annual


# ***********************************************************
# Section 3: EOP summary by different levels ####
# ***********************************************************

Output <-  EOP_Maker(Data,
          Means,
          Variances,
          Y)


Data$EOP <- Output


Data %>% 
  data.frame() %>% 
  fwrite(sep = ",",
         here("Data", "Data_WithEOP_24_10_17.csv"))




# 
# SummaryTable <- 
#   rbind(
#     EOP_Maker(Data,
#               Means,
#               Variances,
#               Y),
#     
#     EOP_Maker(Data,
#               Means,
#               Variances = (betareg::predict(stage_1_Simple,type = "variance")) %>% mean(),
#               Y),
#     EOP_Maker(Data,
#               Means = Means*1.1,
#               Variances = (betareg::predict(stage_1_Simple,type = "variance")) %>% mean(),
#               Y),
#     EOP_Maker(Data,
#               Means = Means*0.9,
#               Variances = (betareg::predict(stage_1_Simple,type = "variance")) %>% mean(),
#               Y),
#     
#     EOP_Maker(Data,
#               Means = c(I((predict(stage_1_Simple, type = "response") + Data$MEC)/2)) %>% mean(),
#               Variances,
#               Y),
#     EOP_Maker(Data,
#               Means = c(I((predict(stage_1_Simple, type = "response") + Data$MEC)/2)) %>% mean(),
#               Variances = Variances*1.1,
#               Y),
#     EOP_Maker(Data,
#               Means = c(I((predict(stage_1_Simple, type = "response") + Data$MEC)/2)) %>% mean(),
#               Variances = Variances*0.9,
#               Y),
#     
#     EOP_Maker(Data,
#               Means,
#               Variances,
#               Y = Data$Income_Annual*1.1),
#     EOP_Maker(Data,
#               Means,
#               Variances,
#               Y = Data$Income_Annual*0.9)
#   )


# ***********************************************************
# Section 4: Export summary table ####
# ***********************************************************
# 
# SummaryTable$Variable <-  c(
#   "All variables at normal levels",
#   "Variance held at mean value",
#   "Variance held at mean value, mean increased by 10%",
#   "Variance held at mean value, mean reduced by 10%",
#   "Mean at mean level, all others at normal level",
#   "Mean of means and 10% increase in variance",
#   "Mean of means and 10% decrease in variance",
#   "Mean and variance vary, income increased by 10%",
#   "Mean and variance vary, income decreased by 10%")
# 
# 
# 
# SummaryTable %>% 
#   data.frame() %>% 
#   fwrite(sep = ",",
#          here("Data","SummaryTable.txt"))


