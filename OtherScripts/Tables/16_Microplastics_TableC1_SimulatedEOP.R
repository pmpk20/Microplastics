#### Microplastics IOP: Create Table C1 ###############
# Function: To simulate EOP with different parameters
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Last Edited: 18/02/2025

# **********************************************************************************
#### Section 0: Replication Information ####
## Here is the output of sessionfo.
# **********************************************************************************

# ─ Session info ─────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United Kingdom.utf8
# ctype    English_United Kingdom.utf8
# tz       Europe/London
# date     2025-01-30
# rstudio  2023.06.2+561 Mountain Hydrangea (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────
# package      * version date (UTC) lib source
# abind          1.4-5   2016-07-21 [1] CRAN (R 4.4.0)
# AER          * 1.2-12  2024-02-03 [1] CRAN (R 4.4.0)
# backports      1.5.0   2024-05-23 [1] CRAN (R 4.4.0)
# betareg      * 3.2-0   2024-07-07 [1] CRAN (R 4.4.1)
# boot         * 1.3-30  2024-02-26 [1] CRAN (R 4.4.1)
# broom          1.0.6   2024-05-17 [1] CRAN (R 4.4.0)
# car          * 3.1-2   2023-03-30 [1] CRAN (R 4.4.0)
# carData      * 3.0-5   2022-01-06 [1] CRAN (R 4.4.0)
# cellranger     1.1.0   2016-07-27 [1] CRAN (R 4.4.0)
# cli            3.6.3   2024-06-21 [1] CRAN (R 4.4.1)
# colorspace     2.1-0   2023-01-23 [1] CRAN (R 4.4.0)
# data.table   * 1.15.4  2024-03-30 [1] CRAN (R 4.4.0)
# DCchoice     * 0.2.0   2023-07-10 [1] CRAN (R 4.4.0)
# dplyr        * 1.1.4   2023-11-17 [1] CRAN (R 4.4.0)
# fansi          1.0.6   2023-12-08 [1] CRAN (R 4.4.0)
# flexmix        2.3-19  2023-03-16 [1] CRAN (R 4.4.0)
# forcats      * 1.0.0   2023-01-29 [1] CRAN (R 4.4.0)
# Formula        1.2-5   2023-02-24 [1] CRAN (R 4.4.0)
# generics       0.1.3   2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2      * 3.5.1   2024-04-23 [1] CRAN (R 4.4.0)
# ggpubr       * 0.6.0   2023-02-10 [1] CRAN (R 4.4.0)
# ggsignif       0.6.4   2022-10-13 [1] CRAN (R 4.4.0)
# glue           1.7.0   2024-01-09 [1] CRAN (R 4.4.0)
# gridExtra      2.3     2017-09-09 [1] CRAN (R 4.4.0)
# gtable         0.3.5   2024-04-22 [1] CRAN (R 4.4.0)
# here         * 1.0.1   2020-12-13 [1] CRAN (R 4.4.0)
# hms            1.1.3   2023-03-21 [1] CRAN (R 4.4.0)
# httr           1.4.7   2023-08-15 [1] CRAN (R 4.4.0)
# Icens          1.72.0  2023-04-25 [1] Bioconductor
# interval       1.1-1.0 2023-08-24 [1] CRAN (R 4.4.0)
# janitor      * 2.2.0   2023-02-02 [1] CRAN (R 4.4.1)
# km.ci          0.5-6   2022-04-06 [1] CRAN (R 4.4.0)
# KMsurv         0.1-5   2012-12-03 [1] CRAN (R 4.4.0)
# knitr          1.47    2024-05-29 [1] CRAN (R 4.4.0)
# lattice        0.22-6  2024-03-20 [1] CRAN (R 4.4.1)
# lifecycle      1.0.4   2023-11-07 [1] CRAN (R 4.4.0)
# lmtest       * 0.9-40  2022-03-21 [1] CRAN (R 4.4.0)
# lubridate    * 1.9.3   2023-09-27 [1] CRAN (R 4.4.0)
# magrittr     * 2.0.3   2022-03-30 [1] CRAN (R 4.4.0)
# MASS           7.3-61  2024-06-13 [1] CRAN (R 4.4.1)
# Matrix         1.7-0   2024-04-26 [1] CRAN (R 4.4.1)
# MLEcens        0.1-7   2022-10-18 [1] CRAN (R 4.4.0)
# modeltools     0.2-23  2020-03-05 [1] CRAN (R 4.4.0)
# munsell        0.5.1   2024-04-01 [1] CRAN (R 4.4.0)
# nnet           7.3-19  2023-05-03 [1] CRAN (R 4.4.1)
# perm           1.0-0.4 2023-08-24 [1] CRAN (R 4.4.0)
# pillar         1.9.0   2023-03-22 [1] CRAN (R 4.4.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.4.0)
# pkgload        1.3.4   2024-01-16 [1] CRAN (R 4.4.0)
# PostcodesioR * 0.3.1   2021-12-01 [1] CRAN (R 4.4.0)
# purrr        * 1.0.2   2023-08-10 [1] CRAN (R 4.4.0)
# R6             2.5.1   2021-08-19 [1] CRAN (R 4.4.0)
# RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.4.0)
# readr        * 2.1.5   2024-01-10 [1] CRAN (R 4.4.0)
# readxl       * 1.4.3   2023-07-06 [1] CRAN (R 4.4.0)
# rlang          1.1.4   2024-06-04 [1] CRAN (R 4.4.0)
# rprojroot      2.0.4   2023-11-05 [1] CRAN (R 4.4.0)
# rstatix        0.7.2   2023-02-01 [1] CRAN (R 4.4.0)
# rstudioapi     0.16.0  2024-03-24 [1] CRAN (R 4.4.0)
# sandwich     * 3.1-0   2023-12-11 [1] CRAN (R 4.4.0)
# scales       * 1.3.0   2023-11-28 [1] CRAN (R 4.4.0)
# sessioninfo  * 1.2.2   2021-12-06 [1] CRAN (R 4.4.2)
# snakecase      0.11.1  2023-08-27 [1] CRAN (R 4.4.1)
# snow         * 0.4-4   2021-10-27 [1] CRAN (R 4.4.0)
# stringi        1.8.4   2024-05-06 [1] CRAN (R 4.4.0)
# stringr      * 1.5.1   2023-11-14 [1] CRAN (R 4.4.0)
# survival     * 3.7-0   2024-06-05 [1] CRAN (R 4.4.1)
# survminer    * 0.4.9   2021-03-09 [1] CRAN (R 4.4.0)
# survMisc       0.5.6   2022-04-07 [1] CRAN (R 4.4.0)
# tibble       * 3.2.1   2023-03-20 [1] CRAN (R 4.4.0)
# tidygeocoder * 1.0.5   2021-11-02 [1] CRAN (R 4.4.1)
# tidyr        * 1.3.1   2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect     1.2.1   2024-03-11 [1] CRAN (R 4.4.0)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.4.0)
# timechange     0.3.0   2024-01-18 [1] CRAN (R 4.4.0)
# tzdb           0.4.0   2023-05-12 [1] CRAN (R 4.4.0)
# utf8           1.2.4   2023-10-22 [1] CRAN (R 4.4.0)
# vctrs          0.6.5   2023-12-01 [1] CRAN (R 4.4.0)
# withr          3.0.0   2024-01-16 [1] CRAN (R 4.4.0)
# xfun           0.45    2024-06-16 [1] CRAN (R 4.4.1)
# xtable         1.8-4   2019-04-21 [1] CRAN (R 4.4.0)
# zoo          * 1.8-12  2023-04-13 [1] CRAN (R 4.4.0)
# 
# [1] C:/Users/earpkin/AppData/Local/Programs/R/R-4.4.1/library



## Libraries here: -----------------------------------------------------------------
## Setting up all relevant libraries
library(data.table)
library(magrittr)
library(dplyr)
library(tidyverse)
library(here)
library(DCchoice)
library(janitor)
library(betareg)
library(boot)
library(AER)
library(snow)
library(scales)
library(survminer)
library(ggplot2)
library(ggtext)
library(rstatix)
library(Rfast)


# ***********************************************************
# Section 1: Import Data ####
# ***********************************************************

# Load data from the specified path using data.table::fread
Data <- here("Data", "Microplastics_AllData_Wide_Anonymised.csv") %>%
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


## Restate variance bounds
Data$VarianceUpperBound <- ifelse(Data$Variance == 4, 5,
                                  ifelse(Data$Variance == 3, 3,
                                         ifelse(Data$Variance == 2, 1, 0)))
Data$VarianceLowerBound <- ifelse(Data$Variance == 4, 5,
                                  ifelse(Data$Variance == 3, 3,
                                         ifelse(Data$Variance == 2, 1, 0)))


Data$VarianceLowerBound = Data$MeanExpectedFuture + Data$VarianceLowerBound
Data$VarianceUpperBound = Data$MeanExpectedFuture - Data$VarianceUpperBound


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



# summary_function <- function(EOP) {
#   cbind(
#     "2.5%" = round(quantile(EOP, c(0.025)), 3),
#     Median = round(median(EOP, na.rm = TRUE), 3),
#     Mean = round(mean(EOP, na.rm = TRUE), 3),
#     SD = round(sd(EOP, na.rm = TRUE), 3),
#     "97.5%" = round(quantile(EOP, c(0.975)), 3)
#   )
# }


summary_function <- function(EOP) {
  cbind(
    "2.5%" = EOP %>% quantile(c(0.025)) %>% round(2) %>% sprintf("%.2f", .) %>%  paste0("£", .),
    Median = EOP %>% median(na.rm = TRUE) %>% round(2) %>%  sprintf("%.2f", .) %>% paste0("£", .), 
    Mean = EOP %>% mean(na.rm = TRUE) %>% round(2) %>%  sprintf("%.2f", .) %>% paste0("£", .), 
    SD = EOP %>% sd(na.rm = TRUE) %>% round(2) %>% sprintf("%.2f", .) %>%  paste0("£", .),
    "97.5%" = EOP %>% quantile(c(0.975)) %>% round(2) %>% sprintf("%.2f", .) %>%  paste0("£", .),
    "Percent" = ((EOP / Data$Income_Annual) * 100) %>% 
      mean(na.rm = TRUE) %>% 
      round(2) %>% 
      sprintf("%.2f", .) %>% 
      paste0(., "%")
  )
}



summary_function_new <- function(EOP) {
  cbind(
    "2.5%" = EOP %>% matrixStats::colQuantiles(probs = c(0.025)),
    Median = EOP %>% Rfast::colMedians(na.rm = TRUE) %>% round(2), 
    Mean = EOP %>% Rfast::colmeans(), 
    SD = EOP %>% Rfast::colVars(std = TRUE),
    "97.5%" = EOP %>% matrixStats::colQuantiles(probs = c(0.975)),
    "Percent" = ((EOP / Data$Income_Annual) * 100) %>% 
      Rfast::colmeans()
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
                      Mean_multiplier, 
                      Variance_multiplier, 
                      Income_multiplier, 
                      R = R) {
  
  # Start bootstrap. 
  # Note that lower case data =/= Data in the environment
  boot.function <- function(data, 
                            Mean_multiplier,
                            Variance_multiplier,
                            Income_multiplier,
                            indices) {
    
    # Segment data
    d <- data[indices, ]
    
    
    
    # Stage 1: Betareg model using the passed formula
    stage_1 <- betareg(formula_stage_1, ## specify in the function
                       d, 
                       type = "BC") ## bias correction
    
    
    # Compute Stage 2 predictors
    d$S2_Mean <- (predict(stage_1, type = "response") + d$MEC) / 2 
    d$S2_Variance <- (I(0 - predict(stage_1, type = 'variance')))
    d$S2_Income <- d$LogBidIncome
    
    stage_2_formula <- as.formula("CV ~ -1 + S2_Income + S2_Mean + S2_Variance")
    
    # Fit Stage 2 GLM
    stage_2 <- glm(
      formula = stage_2_formula,
      family = binomial(link = "probit"),
      data = d
    )
    
    
    
    B0 <- stage_2$coefficients["S2_Income"] %>% as.numeric()
    Delta_0 <- stage_2$coefficients['S2_Mean'] %>% as.numeric()
    Delta_1 <- stage_2$coefficients['S2_Variance'] %>% as.numeric()
    
    ## Scale or fix MEAN EXPECTATIONS ***************************
    S2_Mean <- if (is.numeric(Mean_multiplier)) {
      # Multiply as usual if it's numeric
      c(I((predict(stage_1, type = "response") + d$MEC)/2)) * Mean_multiplier
    } else if (is.character(Mean_multiplier) && Mean_multiplier == "mean") {
      # Perform the preplanned operation if "mean" is specified
      c(I((predict(stage_1, type = "response") + d$MEC)/2))  %>% mean(na.rm = TRUE)
    } else {
      stop("Invalid Mean_multiplier: must be numeric or 'mean'")
    }
    
    ## Scale or fix VARIANCE of EXPECTATIONS ***************************
    S2_Variance <- if (is.numeric(Variance_multiplier)) {
      # Multiply as usual if it's numeric
      (betareg::predict(stage_1, type = "variance")) * Variance_multiplier
    } else if (is.character(Variance_multiplier) && Variance_multiplier == "mean") {
      # Perform the preplanned operation if "mean" is specified
      (betareg::predict(stage_1, type = "variance")) %>% mean(na.rm = TRUE)
    } else {
      stop("Invalid Variance_multiplier: must be numeric or 'mean'")
    }
    
    
    ## Scale or fix INCOME ***************************
    S2_Income <- if (is.numeric(Income_multiplier)) {
      # Multiply as usual if it's numeric
      d$Income_Annual * Income_multiplier
    } else if (is.character(Income_multiplier) && Income_multiplier == "mean") {
      # Perform the preplanned operation if "mean" is specified
      d$Income_Annual %>% mean(na.rm = TRUE)
    } else {
      stop("Invalid Income_multiplier: must be numeric or 'mean'")
    }
    
    
    Means <- S2_Mean
    Variances <- S2_Variance
    # Means <- c(I((predict(stage_1, type = "response") + Data$MEC)/2))
    # Variances <- (betareg::predict(stage_1, type = "variance"))
    ## Define Y == gross monthly income * 12
    Y <- S2_Income
    A <-
      ((Delta_0 * Means +
          (Delta_1 * (0 - Variances))
      )) %>% as.numeric()
    ## Formula here: Y - Y exp(-A/B0)exp(1/2*B0^2)
    EOP <- (Y - (Y*exp(- A / B0))) *
      exp(1 %>% divide_by(B0 %>% raise_to_power(2) %>% multiply_by(2)))
    
    
    ## REMOVE WHEN DONE
    # EOP %>% summary_function()
    return(EOP)
    
  }
  
  # Run the bootstrap
  boot.results <- boot(
    data = data,
    statistic = function(data, indices) {
      boot.function(
    data = data,
    Mean_multiplier = Mean_multiplier,
    Variance_multiplier = Variance_multiplier,
    Income_multiplier = Income_multiplier,
    indices = indices
      )
    },
    R = R,
    parallel = "snow" # Use "snow" if you have parallel setup
  )
  
  # Extracting the results
  # results <- boot.results$t0
  # results <- boot.results$t %>% as.matrix() %>% summary_function_new()
  results <- boot.results$t %>% as.matrix() %>% 
    summary_function_new() %>% 
    Rfast::colmeans() %>% 
    round(2) %>% 
    sprintf("%.2f", .) %>%
    paste0("£", .)
  names(results) <- c("2.5%", "Median", "Mean", "SD", "97.5%", "Percent")
  ## Here the summary of the data
  # results %>% return()
  results %>% return()
  ## Here just the raw data
  # results %>% return()
  
}



# ********************************************
# S4: Export Plot Data ####
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
# Model1_stage2_formula <- "-1 + LogBidIncome"

# Call the simulator function
Model1_simulation <- Simulator(data = Data,
                               formula_stage_1 = Model1_stage1_formula,
                               Mean_multiplier = 1,
                               Variance_multiplier = 1,
                               Income_multiplier = 1,
                               R = 100
)
# 
# # Model1_simulation %>% summary_function()
# 
# 
# Data$EOP <- Model1_simulation
# 
# Data %>%
#   data.frame() %>%
#   fwrite(sep = ",",
#          here("Data", "Data_WithEOP_24_11_25.csv"))


# ***********************************************************
# S5: EOP Variation ####
# ***********************************************************


R <- 1000


SummaryTable <- 
  rbind(
    ## Here everything at default levels
    Simulator(data = Data,
              formula_stage_1 = Model1_stage1_formula,
              Mean_multiplier = 1,
              Variance_multiplier = 1,
              Income_multiplier = 1,
              R = R
    ),
    ## Sample median variance
    Simulator(data = Data,
              formula_stage_1 = Model1_stage1_formula,
              Mean_multiplier = 1,
              Variance_multiplier = "mean",
              Income_multiplier = 1,
              R = R
    ),
    ## Fix variance, 10% INcrease in means
    Simulator(data = Data,
              formula_stage_1 = Model1_stage1_formula,
              Mean_multiplier = 1.1,
              Variance_multiplier = "mean",
              Income_multiplier = 1,
              R = R
    ),
    ## Fix variance, 10% DEcrease in means
    Simulator(data = Data,
              formula_stage_1 = Model1_stage1_formula,
              Mean_multiplier = 0.9,
              Variance_multiplier = "mean",
              Income_multiplier = 1,
              R = R
    ),
    ## Sample median mean
    Simulator(data = Data,
              formula_stage_1 = Model1_stage1_formula,
              Mean_multiplier = "mean",
              Variance_multiplier = 1,
              Income_multiplier = 1,
              R = R
    ),
    ## Fix variance, 10% INcrease in means
    Simulator(data = Data,
              formula_stage_1 = Model1_stage1_formula,
              Mean_multiplier = "mean",
              Variance_multiplier = 1.1,
              Income_multiplier = 1,
              R = R
    ),
    ## Fix variance, 10% DEcrease in means
    Simulator(data = Data,
              formula_stage_1 = Model1_stage1_formula,
              Mean_multiplier = "mean",
              Variance_multiplier = 0.9,
              Income_multiplier = 1,
              R = R
    ),
    ## Let mean and variance change but 10% increase in income
    Simulator(data = Data,
              formula_stage_1 = Model1_stage1_formula,
              Mean_multiplier = 1,
              Variance_multiplier = 1,
              Income_multiplier = 1.1,
              R = R
    ),
    Simulator(data = Data,
              formula_stage_1 = Model1_stage1_formula,
              Mean_multiplier = 1,
              Variance_multiplier = 1,
              Income_multiplier = 0.9,
              R = R
    )
  )


# ***********************************************************
# Section 4: Export summary table ####
# ***********************************************************

Backup <- SummaryTable

SummaryTable <- SummaryTable %>% data.frame()
SummaryTable$Variable <-  c(
  "All variables at normal levels",
  "Variance held at mean value",
  "Variance held at mean value, mean increased by 10%",
  "Variance held at mean value, mean reduced by 10%",
  "Mean at mean level, all others at normal level",
  "Mean of means and 10% increase in variance",
  "Mean of means and 10% decrease in variance",
  "Mean and variance vary, income increased by 10%",
  "Mean and variance vary, income decreased by 10%")



SummaryTable %>% 
  data.frame() %>% 
  fwrite(sep = ",",
         here("Data", "Microplastics_TableC1_SimulatedEOP.txt"))
## formally _1710 version which didn't have % income



# *****************************
# End of script
# *****************************