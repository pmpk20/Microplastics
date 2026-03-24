#### Microplastics: IOP Paper ####
## Function: Repeat Cameron 2005 table 2
## Author: Dr Peter King (p.king1@Leeds.ac.uk)
## Last change: 17/11/23


# *****************************
# Replication Information: ####
# Selected output of 'sessionInfo()'
# *****************************


# R version 4.2.2 (2022-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19045)
# Matrix products: default
#   [1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8   
# [3] LC_MONETARY=English_United Kingdom.utf8 LC_NUMERIC=C                           
# [5] LC_TIME=English_United Kingdom.utf8    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] here_1.0.1        forcats_0.5.2     stringr_1.5.0     purrr_1.0.1      
# [5] readr_2.1.3       tidyr_1.2.1       tibble_3.1.8      tidyverse_1.3.2  
# [9] ggridges_0.5.4    ggplot2_3.4.0     reshape2_1.4.4    apollo_0.2.8     
# [13] dplyr_1.0.10      magrittr_2.0.3    data.table_1.14.6
# 
# loaded via a namespace (and not attached):
#   [1] mcmc_0.9-7          matrixStats_0.63.0  fs_1.6.0            lubridate_1.9.1    
# [5] RColorBrewer_1.1-3  httr_1.4.4          rprojroot_2.0.3     numDeriv_2016.8-1.1
# [9] tools_4.2.2         backports_1.4.1     utf8_1.2.2          R6_2.5.1           
# [13] DBI_1.1.3           colorspace_2.0-3    withr_2.5.0         tidyselect_1.2.0   
# [17] mnormt_2.1.1        compiler_4.2.2      cli_3.6.0           rvest_1.0.3        
# [21] quantreg_5.94       SparseM_1.81        xml2_1.3.3          sandwich_3.0-2     
# [25] scales_1.2.1        mvtnorm_1.1-3       digest_0.6.31       rmarkdown_2.20     
# [29] RSGHB_1.2.2         MCMCpack_1.6-3      pkgconfig_2.0.3     htmltools_0.5.4    
# [33] dbplyr_2.3.0        fastmap_1.1.0       rlang_1.0.6         readxl_1.4.1       
# [37] rstudioapi_0.14     generics_0.1.3      zoo_1.8-11          jsonlite_1.8.4     
# [41] googlesheets4_1.0.1 Matrix_1.5-1        Rcpp_1.0.9          munsell_0.5.0      
# [45] fansi_1.0.3         lifecycle_1.0.3     stringi_1.7.12      yaml_2.3.7         
# [49] MASS_7.3-58.1       plyr_1.8.8          grid_4.2.2          parallel_4.2.2     
# [53] crayon_1.5.2        lattice_0.20-45     haven_2.5.1         splines_4.2.2      
# [57] hms_1.1.2           knitr_1.42          pillar_1.8.1        randtoolbox_2.0.3  
# [61] reprex_2.0.2        glue_1.6.2          evaluate_0.20       modelr_0.1.10      
# [65] vctrs_0.5.1         tzdb_0.3.0          miscTools_0.6-26    MatrixModels_0.5-1 
# [69] cellranger_1.1.0    gtable_0.3.1        assertthat_0.2.1    xfun_0.36          
# [73] broom_1.0.3         rngWELL_0.10-9      coda_0.19-4         survival_3.4-0     
# [77] googledrive_2.0.0   gargle_1.2.1        maxLik_1.5-2        timechange_0.2.0   
# [81] ellipsis_0.3.2  


# install.packages("DCchoice",
#                  repos = c("http://www.bioconductor.org/packages/release/bioc",
#                            "https://cran.rstudio.com/"),
#                  dep = TRUE)

# 
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("Icens")
# 

# renv::snapshot()
rm(list=ls())
library(data.table)
library(magrittr)
library(dplyr)
library(apollo)
library(reshape2)
library(ggplot2)
library(ggridges)
library(tidyverse)
library(here)
library(DCchoice)
library(survminer)
library(survival)


# *****************************
# Section 1: Import Data ####
# *****************************



## Start with the latest anonymised data in one-row per one-respondent format
Data <-
  here("Data",
       "Microplastics_AllData_Long_Anonymised_2022_06_19.csv") %>%
  fread() %>%
  data.frame()


# ***************************************************
# Section 2: Defining variables ####
# ***************************************************


# ***************************************************
# MEAN VARIABLES

## VERY IMPORTANT STEP DO NOT MISS
## Noting that zero is defined as "as harmful as we currently believe"
## We recast zeros
# Data$MeanExpectedFuture_Adjusted <- ifelse(Data$MeanExpectedFuture == 0, 
#                            Data$MeanExpectedCurrent, 
#                            Data$MeanExpectedFuture)
# 
Data$Mean_Change <- Data$MeanExpectedCurrent + Data$MeanExpectedFuture

Data$Mean_Change_Log <- (Data$MeanExpectedCurrent + Data$MeanExpectedFuture) %>% magrittr::add(10) %>% log()
Data$Mean_Log <- Data$MeanExpectedFuture %>% magrittr::add(5) %>% log()
Data$Mean_Change_Square <- Data$Mean_Change %>% magrittr::raise_to_power(2) 
Data$Mean_Int_Var <- Data$MeanExpectedFuture %>% multiply_by(Data$Variance) 


## Recentering on 5 rather than 0
Data$MeanExpectedCurrent_Scaled <- Data$MeanExpectedCurrent + 5
Data$MeanExpectedFuture_Scaled <- Data$MeanExpectedFuture + 5

## Not used yet but calculated for later
Data$Mean_MinusLowerBound <- Data$MeanExpectedFuture - Data$Variance
Data$Mean_PlusUpperBound <- Data$MeanExpectedFuture + Data$Variance


## Dealing with reported mean:
Data$MeanExpectedFuture_SampleMean <- Data$MeanExpectedFuture %>% mean()
# Data$Mean_Change <- c(Data$MeanExpectedFuture - Data$MeanExpectedCurrent) 


# ***************************************************
# VARIANCE VARIABLES

## Dealing with reported variance
Data$Variance_StatedConfidenceLevel <- Data$Variance

## Divide by four as Cameron does
Data$Variance_ConfidenceAsSD <- ifelse(Data$Variance == 1, 0, 
                                       ifelse(Data$Variance == 2, 2, 
                                              ifelse(Data$Variance == 3, 6, 10))) %>% 
  divide_by(4)

## Square SD
Data$Variance_ConfidenceAsVariance <- Data$Variance_ConfidenceAsSD %>% raise_to_power(2)


## Sum of squared individual differences from reported mean
Data$Variance_IndividualLevel <- 
  (((Data$MeanExpectedBest - Data$MeanExpectedFuture) ^ 2) +
     ((Data$MeanExpectedWorst - Data$MeanExpectedFuture) ^ 2)) %>%
  divide_by(2)

## More transforms but for variance
Data$Variance_Square <- Data$Variance_ConfidenceAsVariance %>% magrittr::raise_to_power(2)
Data$Variance_Log <- Data$Variance_ConfidenceAsVariance %>% add(1) %>% log()

Data$Variance_InverseBy1 <- 1 %>% divide_by(Data$Variance_ConfidenceAsVariance %>% add(1))
Data$Variance_InverseByMean <- Data$Mean_Change %>% divide_by(Data$Variance_ConfidenceAsVariance %>% add(1)) 

Data$Var_Neg <- (Data$Variance_ConfidenceAsVariance * -1)


## Old code just in case useful again one day
# Data$EstimatedVariance_Square <- Data$EstimatedVariance %>% magrittr::raise_to_power(2)
# Data$SE <- ((Data$MeanExpectedBest - Data$MeanExpectedWorst) %>% divide_by(qnorm(p = 0.95) %>% multiply_by(2))) 



# ***************************************************
# OTHER VARIABLES


## Define an annual measure of the monthtly gross income
Data$Income_Annual <- Data$Income %>% multiply_by(12)


## Log of change in net income 
Data$LogBidIncome <- ((Data$Income_Annual - Data$Bid) %>% divide_by(Data$Income_Annual)) %>% log()


## Easy way to operationalise Q16 bank of questions
Data$Q16_Comparison <- (Data$Q16_ClimateCurrentSelf - Data$Q16_MicroplasticsCurrentSelf) 



# ***************************************************
# Section 3: Setting up functions ####
# ***************************************************

AddStars <- function(Input) {
  ifelse(
    Input < 0.01,
    paste0(Input %>% round(3), "***"),
    ifelse(
      Input < 0.05,
      paste0(Input %>% round(3), "**"),
      ifelse(
        Input < 0.1,
        paste0(Input %>% round(3), "*"),
        paste0(Input %>% round(3), " "))))
}


## So this code outputs a table of estimate,  p.v stars and s.e in brackets ##
### To make it easy,  just change the model name here and the code will output the table for your model:
ModelOutputs <- function(Model) {
  Input <- Model$coefficients %>% data.frame()
  Estimates <- summary(Model)$coefficients
  
  data.frame("Variable" =  Estimates %>% rownames(),
             "Estimate" =  paste(
               ifelse(
                 Estimates[, 4] < 0.01,
                 paste0(Estimates[, 1] %>% round(3) %>% sprintf("%.3f", .),  "***"),
                 ifelse(
                   Estimates[, 4] < 0.05,
                   paste0(Estimates[, 1] %>% round(3) %>% sprintf("%.3f", .),  "**"),
                   ifelse(
                     Estimates[, 4] < 0.1,
                     paste0(Estimates[, 1] %>% round(3) %>% sprintf("%.3f", .),  "*"),
                     Estimates[, 1] %>% round(3) %>% sprintf("%.3f", .)
                   )
                 )
               ),
               paste0("(", Estimates[, 2] %>% round(3) %>% sprintf("%.3f", .), ")")
             ))
}




Diagnostics <- function(Model) {
  
  cbind(
    "Variable" = c("AIC", "LogLik"),
    
    "Estimate" = c(
      Model %>% AIC() %>% round(3) %>% sprintf("%.3f", .),
      Model %>% logLik() %>% as.numeric() %>% round(3) %>% sprintf("%.3f", .)
    )) %>% data.frame()
  
}



# **************************************
# Section 3B: Recover Cameron WTP = EOP  ####
# **************************************


EOP_maker <- function(Model) {
  B0 <- Model$coefficients["LogBidIncome"] %>% as.numeric()
  Y <- Data$Income_Annual 
  A <- ((data.frame(Model$coefficients)[grepl("Mean_Change", rownames(data.frame(Model$coefficients))), ] * c(Data$Mean_Change)) +
          (data.frame(Model$coefficients)[grepl("Variance_ConfidenceAsVariance", rownames(data.frame(Model$coefficients))), ] * Model$model$`c(-(Variance_ConfidenceAsVariance))`)) %>% as.numeric()
  
  # EOP <- Y - Y exp (- A / B0) exp (1 / 2 B0 ^2)
  
  EOP <- Y - (Y*exp(- A / B0)) *
    exp(1 / B0 %>% raise_to_power(2) %>% multiply_by(2))
  
  return(EOP %>% as.data.frame())
}


# ***************************************************
# Model_0: Comparions  ####
# ***************************************************

## Comparison models
Model_0_sbchoice <- sbchoice(
  CV ~  c(Mean_Change) +
    c(-(Variance_ConfidenceAsVariance))| Bid,
  dist = "normal",
  data = Data
)


Model_0_glm <- glm(
  CV ~  Bid +
    c(Mean_Change) +
    c(-(Variance_ConfidenceAsVariance)),
  family = binomial(link = "probit"),
  data = Data
)



# ***************************************************
# Model_1: logbid, mean, var only  ####
# ***************************************************


## Basic income, bid, mean, var only
Model_1 <- glm(
  CV ~ -1 + 
    LogBidIncome +
    Mean_Change +
    Var_Neg,
  family = binomial(link = "probit"),
  data = Data
)


Model_1 %>% summary()

Model_1_Outputted <- Model_1 %>% ModelOutputs()



# ***************************************************
# Model_2: Model1 + quadratic + interaction  ####
# ***************************************************


## Basic income, bid, mean, var only
Model_2 <- glm(
  CV ~ -1 + 
    LogBidIncome +
    Mean_Change +
    Var_Neg +
    Mean_Change_Square +
    Variance_Square +
    Mean_Int_Var,
  family = binomial(link = "probit"),
  data = Data
)


Model_2 %>% summary()

Model_2_Outputted <- Model_2 %>% ModelOutputs()



# ***************************************************
# Model_3: Model2 + controls  ####
# ***************************************************


## Model 2 with income interactions
Model_3 <- glm(
  CV ~ -1 + 
    Mean_Change +
    Var_Neg +
    Mean_Change_Square +
    Variance_Square +
    (LogBidIncome * AgeDummy)  +
    (LogBidIncome * Gender)  +
    (LogBidIncome * EthnicityDummy)  +
    (LogBidIncome * Charity)  +
    (LogBidIncome * Consequentiality)  +
    (LogBidIncome * Understanding) , 
  family = binomial(link = "probit"),
  data = Data
)


Model_3 %>% summary()

Model_3_Outputted <- Model_3 %>% ModelOutputs()



# ***************************************************
# Model_4: Model3 with mean ints  ####
# ***************************************************


## Model 3 but with mean interactions
Model_4 <- glm(
  CV ~ -1 + 
    LogBidIncome +
    Var_Neg +
    Mean_Change_Square +
    Variance_Square +
    (Mean_Change * AgeDummy)  +
    (Mean_Change * Gender)  +
    (Mean_Change * EthnicityDummy)  +
    (Mean_Change * Charity)  +
    (Mean_Change * Consequentiality)  +
    (Mean_Change * Understanding),
  family = binomial(link = "probit"),
  data = Data
)


Model_4 %>% summary()

Model_4_Outputted <- Model_4 %>% ModelOutputs()


# ***************************************************
# Model_5: Model4 new interaction  ####
# ***************************************************


## Model 4 but var ints
Model_5 <- glm(
  CV ~ -1 + 
    LogBidIncome +
    Mean_Change +
    Mean_Change_Square +
    Variance_Square +
    (Var_Neg * AgeDummy)  +
    (Var_Neg * Gender)  +
    (Var_Neg * EthnicityDummy)  +
    (Var_Neg * Charity)  +
    (Var_Neg * Consequentiality)  +
    (Var_Neg * Understanding),
  family = binomial(link = "probit"),
  data = Data
)


Model_5 %>% summary()

Model_5_Outputted <- Model_5 %>% ModelOutputs()


# ***************************************************
# Model_6: Model with all interactions  ####
# ***************************************************



## Basic income, bid, mean, var only
Model_6 <- glm(CV ~ -1 +
                 Mean_Change_Square +
                 Variance_Square +
                 (Mean_Change * AgeDummy)  +
                 (Mean_Change * Gender)  +
                 (Mean_Change * EthnicityDummy)  +
                 (Mean_Change * Charity)  +
                 (Mean_Change * Consequentiality)  +
                 (Mean_Change * Understanding) +
                 (Mean_Change * Order) +
                 (Mean_Change * Q16_ClimateCurrentEnvironment) +
                 (Mean_Change * Q16_ClimateCurrentSelf) +
                 (Mean_Change * Q16_MicroplasticsCurrentEnvironment) +
                 (Mean_Change * Q16_MicroplasticsCurrentSelf) +
                 (Mean_Change * Q16_MicroplasticsTen) +
                 (Mean_Change * Q16_MicroplasticsTwentyFive) +
                 (Mean_Change * Q16_MicroplasticsFifty) +
                 (Mean_Change * Q17_PandemicEnvironment) +
                 (Mean_Change * Q17_PandemicMicroplastics) +
                 (Var_Neg * AgeDummy)  +
                 (Var_Neg * Gender)  +
                 (Var_Neg * EthnicityDummy)  +
                 (Var_Neg * Charity)  +
                 (Var_Neg * Consequentiality)  +
                 (Var_Neg * Understanding) +
                 (Var_Neg * Order) +
                 (Var_Neg * Q16_ClimateCurrentEnvironment) +
                 (Var_Neg * Q16_ClimateCurrentSelf) +
                 (Var_Neg * Q16_MicroplasticsCurrentEnvironment) +
                 (Var_Neg * Q16_MicroplasticsCurrentSelf) +
                 (Var_Neg * Q16_MicroplasticsTen) +
                 (Var_Neg * Q16_MicroplasticsTwentyFive) +
                 (Var_Neg * Q16_MicroplasticsFifty) +
                 (Var_Neg * Q17_PandemicEnvironment) +
                 (Var_Neg * Q17_PandemicMicroplastics) +
                 (LogBidIncome * AgeDummy)  +
                 (LogBidIncome * Gender)  +
                 (LogBidIncome * EthnicityDummy)  +
                 (LogBidIncome * Charity)  +
                 (LogBidIncome * Consequentiality)  +
                 (LogBidIncome * Understanding) +
                 (LogBidIncome * Order) +
                 (LogBidIncome * Q16_ClimateCurrentEnvironment) +
                 (LogBidIncome * Q16_ClimateCurrentSelf) +
                 (LogBidIncome * Q16_MicroplasticsCurrentEnvironment) +
                 (LogBidIncome * Q16_MicroplasticsCurrentSelf) +
                 (LogBidIncome * Q16_MicroplasticsTen) +
                 (LogBidIncome * Q16_MicroplasticsTwentyFive) +
                 (LogBidIncome * Q16_MicroplasticsFifty) +
                 (LogBidIncome * Q17_PandemicEnvironment) +
                 (LogBidIncome * Q17_PandemicMicroplastics) ,
               family = binomial(link = "probit"),
               data = Data
  )
  


Model_6 %>% summary()

Model_6_Outputted <- Model_6 %>% ModelOutputs()


# *****************************
# Section 5: Export Model ####
# *****************************


## Using zeros to pad rows
cbind(
  rbind(Model_1_Outputted, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  rbind(Model_2_Outputted, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  rbind(Model_3_Outputted, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  rbind(Model_4_Outputted, 0, 0),
  rbind(Model_5_Outputted, 0, 0),
  Model_6_Outputted
) %>% data.frame() %>% fwrite(
  sep = ",",
  here("Tables", "TableX_ModelOutputs_Updated.txt")
)




## Append to table
cbind(Diagnostics(Model_1),
      Diagnostics(Model_2),
      Diagnostics(Model_3),
      Diagnostics(Model_4),
      Diagnostics(Model_5),
      Diagnostics(Model_6)) %>% 
  data.frame() %>% 
  fwrite(
        sep = ",")



# ***************************************************
# Model_X: Trying out different interactions  ####
# ***************************************************

Model_Basic <- glm(CV ~ -1 + Mean_Change +
      Mean_Change_Square +
      Variance_Square +
      LogBidIncome + 
      Var_Neg ,
    family = binomial(link = "probit"),
    data = Data
)

Model_Means <- glm(CV ~ -1 +
                     Mean_Change_Square +
                     Variance_Square +
                     LogBidIncome + 
                     Var_Neg +
                     (Mean_Change * AgeDummy)  +
                     (Mean_Change * Gender)  +
                     (Mean_Change * EthnicityDummy)  +
                     (Mean_Change * Charity)  +
                     (Mean_Change * Consequentiality)  +
                     (Mean_Change * Understanding) +
                     (Mean_Change * Order) +
                     (Mean_Change * Q16_Comparison),
                   family = binomial(link = "probit"),
                   data = Data
) 


Model_Vars <- glm(CV ~ -1 +
                    Mean_Change_Square +
                    Variance_Square +
                    LogBidIncome + 
                    Mean_Change +
                    (Var_Neg * AgeDummy)  +
                    (Var_Neg * Gender)  +
                    (Var_Neg * EthnicityDummy)  +
                    (Var_Neg * Charity)  +
                    (Var_Neg * Consequentiality)  +
                    (Var_Neg * Understanding) +
                    (Var_Neg * Order) +
                    (Var_Neg * Q16_Comparison) ,
                  family = binomial(link = "probit"),
                  data = Data
) 



Model_LBI <- glm(CV ~ -1 +
                   Mean_Change_Square +
                   Variance_Square +
                   Var_Neg + 
                   Mean_Change +
                   (LogBidIncome * AgeDummy)  +
                   (LogBidIncome * Gender)  +
                   (LogBidIncome * EthnicityDummy)  +
                   (LogBidIncome * Charity)  +
                   (LogBidIncome * Consequentiality)  +
                   (LogBidIncome * Understanding) +
                   (LogBidIncome * Order) +
                   (LogBidIncome * Q16_Comparison) ,
                 family = binomial(link = "probit"),
                 data = Data
)




cbind(
  rbind(Model_Basic %>% ModelOutputs(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  Model_Means %>% ModelOutputs(),
  Model_Vars %>% ModelOutputs(),
  Model_LBI %>% ModelOutputs()
) %>% data.frame() %>% fwrite(
  sep = ",",
  here("Tables", "TableX_ModelOutputs_Updated.txt")
)



# ***************************************************
# Recovering EOP  ####
# ***************************************************



## Define all parameters
B0 <- Model_Basic$coefficients["LogBidIncome"] %>% as.numeric()
Y <- Data$Income_Annual 
A <- ((Model_Basic$coefficients["Mean_Change"] %>% as.numeric() * Data$Mean_Change) +
    (Model_Basic$coefficients["Var_Neg"] %>% as.numeric() * Data$Var_Neg))
  

## Calculate expected option prices
EOP <- Y - (Y*exp(- A / B0)) *
    exp(1 / B0 %>% raise_to_power(2) %>% multiply_by(2))
  

## Column bind to existing data
Data_WithEOP <- cbind(
  Data,
  "EOP" = EOP
)


Data_WithEOP %>% data.frame() %>% fwrite(
  sep = ",",
  here("CVoutput", "Data_WithEOP_23_11_20.csv")
)


# END OF SCRIPT ************************