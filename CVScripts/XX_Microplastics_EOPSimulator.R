#### Microplastics: IOP Paper ####
## Function: In-text specification
## Author: PK
## Last change: 30/01/25
# Changes:
# - using factor uncertainty instead
# - new code to output the combined table
# - new code to report median (SD) of EOP



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

# Load data from the specified path using data.table::fread
Data <- here("Data", "Microplastics_AllData_Wide_Anonymised.csv") %>%
  fread() %>%
  data.frame()

# Select the specified columns
Data_Filtered <- Data %>%
  dplyr::select(
    CV,
    MEC,
    MEF,
    AgeDummy,
    EthnicityDummy,
    Gender_Dummy,
    Charity,
    Education_HigherEd,
    Q16_ClimateCurrentEnvironment,
    Q16_ClimateCurrentSelf,
    Q16_MicroplasticsCurrentEnvironment,
    Q16_MicroplasticsCurrentSelf,
    Q16_MicroplasticsTen,
    Q16_MicroplasticsTwentyFive,
    Q16_MicroplasticsFifty,
    Uncertainty,
    LogBidIncome,
    Income_Annual
  )

# ***********************************************************
# Section 2: Define Functions ####
# ***********************************************************

# Function to convert z-values to p-values
PvalueConverter <- function(ZValues) {
  2 * (1 - pnorm(abs(ZValues)))
}

# Function to add significance stars to an estimate based on p-value
PvalueLabeller <- function(estimate, p_values) {
  estimate_rounded <- round(estimate, 3)
  case_when(
    p_values < 0.001 ~ paste0(estimate_rounded, "***"),
    p_values < 0.01  ~ paste0(estimate_rounded, "**"),
    p_values < 0.05 ~ paste0(estimate_rounded, "*"),
    p_values < 0.1 ~ paste0(estimate_rounded, "."),
    TRUE ~ as.character(estimate_rounded)
  )
}

# Function to format p-values for paper
PvalueLabeller_Paper <- function(p_values) {
  case_when(
    p_values < 0.001 ~ "<0.001***",
    p_values < 0.01 ~ sprintf("%.3f***", p_values),
    p_values < 0.05 ~ sprintf("%.3f**", p_values),
    p_values < 0.1 ~ sprintf("%.3f*", p_values),
    TRUE ~ sprintf("%.3f", p_values)
  )
}

# Function to format model output for printing
ModelOutput <- function(Estimates, Identifier) {
  Estimates %>%
    data.frame() %>%
    dplyr::mutate(
      Variable = rownames(Estimates),
      Estimate = paste0(PvalueLabeller(Estimate, P.values), " (", round(Std..Error, 3), ")"),
      Model = Identifier
    ) %>%
    dplyr::select(Variable, Estimate, Model)
}


## Reports moments of distribution of EOP values
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
    EOP %>% summary_function()
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
  results <- boot.results$t0
  
  ## Here the summary of the data
  # results %>% return()
  results %>% summary_function() %>% return()
  ## Here just the raw data
  # results %>% return()
  
}


# ***********************************************************
# Section 4: Model Output ####
# ***********************************************************

# Define number of bootstrap iterations
# R <- 100
R <- 1000

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
                               Mean_multiplier = 1,
                               Variance_multiplier = 1,
                               Income_multiplier = 1,
                               R = 1000
)

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
         here("CVoutput/Tables", 
              "TableC1_EOPSummaryStats.txt"))

## formally _1710 version which didn't have % income



# ***********************************************************
# Section 6: Saving Session Information ####
# ***********************************************************

# Create file name for the session information
session_file_name <- paste0("session_info_", format(Sys.Date(), "%Y_%m_%d"), ".txt")

# Save the session information into a text file in the Data Folder
sessioninfo::session_info() %>%
  capture.output(file = here("Data", session_file_name))

# End Of Script # ********************************************