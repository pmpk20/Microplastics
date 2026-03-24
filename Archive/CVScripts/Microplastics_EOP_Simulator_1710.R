#### Microplastics: IOP Paper ####
## Function: 10/10 specifications
## Author: PK
## Last change: 17/10/2024
# Changes:
# - so this is EOP within the boot function

# *****************************
# Replication Information: ####
# Selected output of 'sessionInfo()'
# *****************************
# R version 4.4.1 (2024-06-14 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 22631)
# Matrix products: default
# locale:
#   [1] LC_COLLATE=English_United Kingdom.utf8 
# [2] LC_CTYPE=English_United Kingdom.utf8   
# [3] LC_MONETARY=English_United Kingdom.utf8
# [4] LC_NUMERIC=C                           
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


rm(list=ls())

## Useful for all scripts:
library(data.table)
library(magrittr)
library(dplyr)
library(tidyverse)
library(here)
library(DCchoice)
library(janitor)

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
       "Microplastics_AllData_Long_Anonymised_2022_06_19.csv") %>%
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


## Half differences between variance bounds
Data$Uncertainty <-
  ((Data$VarianceLowerBound - Data$VarianceUpperBound) / 2)

## Transform to Cameron (2005) measure
Data$var.cameron <- (0.5 * Data$Uncertainty) ^ 2


## Negative correlation so Uncertainty up means CV down
##
# cor.test(Data$var.cameron, Data$CV)



Data$PaymentVehicle_Dummy <- ifelse(Data$WaterBills == 0, 0, 1)



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



PvalueConverter <- function(ZValues) {
  ## 2(1 - pnorm(Z))
  Converted <-  (1 - pnorm(abs(ZValues))) %>% multiply_by(2)
  
  ## Commenting out this so I can manipulate P value stars later
  # Converted_trimmed <- Converted %>% round(3) %>% sprintf("%.3f", .)
  # ifelse(
  #   Converted < 0.01,
  #   paste0(Converted_trimmed, " ***"),
  #   ifelse(
  #     Converted < 0.05,
  #     paste0(Converted_trimmed, " **"),
  #     ifelse(
  #       Converted < 0.1,
  #       paste0(Converted_trimmed, " *"),
  #       paste0(Converted_trimmed, " ")
  #     )
  #   )
  # )
}



PvalueLabeller <- function(estimate, p_values) {
  ifelse(
    p_values < 0.01, 
    estimate %>% round(3) %>% paste0("***"),
    ifelse(
      p_values < 0.05, 
      estimate %>% round(3) %>% paste0("**"),
      ifelse(
        p_values < 0.1, 
        estimate %>% round(3) %>% paste0("*"),
        round(estimate, 3)
      )
    )
  )
}



# Optimized ModelOutput function
ModelOutput <- function(Estimates, Identifier) {
  Estimates %>%
    data.frame() %>%
    mutate(
      Variable = rownames(Estimates),
      Estimate = PvalueLabeller(Estimate, P.values) %>% 
        paste0(" (", 
               round(Std..Error, 3), 
               ")"),
      Model = Identifier
    ) %>%
    select(Variable, Estimate, Model)
}



summary_function <- function(EOP) {
  cbind(
    "2.5%" = round(quantile(EOP, c(0.025)), 3),
    Median = round(median(EOP, na.rm = TRUE), 3),
    Mean = round(mean(EOP, na.rm = TRUE), 3),
    SD = round(sd(EOP, na.rm = TRUE), 3),
    "97.5%" = round(quantile(EOP, c(0.975)), 3)
  )
}


# ********************************************
# Section 3: Proper 2stage, consistent SE ####
## Writing it all as a function
## so we can quickly return estimates for different
## specifications
# ********************************************


Simulator <- function(data, 
                      formula_stage_1,
                      formula_stage_2,
                      R = 100) {
  
  # Start bootstrap. 
  # Note that lower case data =/= Data in the environment
  boot.function <- function(data, indices) {
    
    # Segmentt data
    d <- data[indices, ]
    
    # Stage 1: Betareg model using the passed formula
    stage_1 <- betareg(formula_stage_1, ## specify in the function
                       d, 
                       type = "BC") ## bias correction

    stage_2 <- glm(paste0(
      "CV ~ ", 
      formula_stage_2, 
      " + I((predict(stage_1, type = 'response') + MEC) / 2) + I(0 - predict(stage_1, type = 'variance'))"
    ) %>% as.formula(),
    family = binomial(link = "probit"),
    data = d
    )
    
    
    
    B0 <- stage_2$coefficients["LogBidIncome"] %>% as.numeric()
    Delta_0 <- stage_2$coefficients['I((predict(stage_1, type = "response") + MEC)/2)'] %>% as.numeric()
    Delta_1 <- stage_2$coefficients['I(0 - predict(stage_1, type = "variance"))'] %>% as.numeric()
    
    
    Means <- c(I((predict(stage_1, type = "response") + Data$MEC)/2))
    Variances <- (betareg::predict(stage_1, type = "variance"))
    ## Define Y == gross monthly income * 12
    Y <- Data$Income_Annual
    A <-
      ((Delta_0 * Means +
          (Delta_1 * (0 - Variances))
      )) %>% as.numeric()
    ## Formula here: Y - Y exp(-A/B0)exp(1/2*B0^2)
    EOP <- (Y - (Y*exp(- A / B0))) *
      exp(1 %>% divide_by(B0 %>% raise_to_power(2) %>% multiply_by(2)))
    
    
    return(EOP)
    
    # Return model coefficients (mean, precision, and standard errors for both stages)
    # return(c(summary(stage_1)$coefficients$mean[, 1],
    #          summary(stage_1)$coefficients$precision[, 1],
    #          summary(stage_2)$coefficients[, 1],
    #          summary(stage_1)$coefficients$mean[, 2],
    #          summary(stage_1)$coefficients$precision[, 2],
    #          summary(stage_2)$coefficients[, 2]))
  }
  
  # Run the bootstrap
  boot.results <- boot(data = data,
                       statistic = boot.function,
                       R = R,
                       parallel = "snow")
  
  # Extracting the results
  # l <- length(boot.results$t0)
  results <- boot.results$t0
  

  results %>% summary_function() %>% return()  
    
  # # Compile the final results with estimates, std. errors, and z-values
  # results <- cbind(
  #   Estimate = boot.results$t0[1:(l / 2)],
  #   # Recover estimates
  #   `Std. Error` = boot.results$t[, (l / 2 + 1):l] %>% colMeans(),
  #   # standard errors
  #   `z value` = boot.results$t0[1:(l / 2)] / (boot.results$t[, (l / 2 + 1):l] %>% colMeans())  # z-values
  # )
  # 
  # # Rounding results for easier presentation
  # results_rounded <- results %>% round(3)
  # 
  # # Adding P-values
  # results_rounded_withP <- cbind(
  #   results_rounded,
  #   "P values" = results[, 3] %>% PvalueConverter() ## note Ps calculated without rounding
  # ) %>% data.frame()
  # 
  # # Return the final rounded results with P-values
  # return(results_rounded_withP)
}



# ********************************************
# Model1: Preferred ####
# ********************************************



# Define your formula for stage_1 and stage_2 models
Model1_stage1_formula <- as.formula(
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
    as.factor(Uncertainty)
)


# Define your formula for stage_1 and stage_2 models
Model1_stage2_formula <- "-1 + LogBidIncome"

# Call the simulator function
Model1_simulation <- Simulator(data = Data,
                               formula_stage_1 = Model1_stage1_formula,
                               formula_stage_2 = Model1_stage2_formula,
                               R = 10
)  

  
# *****************************
# Section x: Export plot ####
# *****************************

# Data$EOP <- Model1_simulation
# 
# Data %>% 
#   data.frame() %>% 
#   fwrite(sep = ",",
#          here("Data", "Data_WithEOP_24_10_17.csv"))



# *****************************
# End of script
# *****************************