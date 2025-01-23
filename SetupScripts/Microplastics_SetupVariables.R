#### Microplastics: IOP Paper ####
## Function: Define correct measures
## Author: Dr Peter King (p.king1@Leeds.ac.uk)
## Last change: 13/11/2023
## Change:
## - Adding new variables 


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



# *****************************
# Section 2: Define variables ####
# *****************************


## Define an annual measure of the monthtly gross income
Data$Income_Annual <- Data$Income * 12


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




## VERY IMPORTANT STEP DO NOT MISS
## Noting that zero is defined as "as harmful as we currently believe"
## We recast zeros
# Data$MeanExpectedFuture_Adjusted <- ifelse(Data$MeanExpectedFuture == 0, 
#                            Data$MeanExpectedCurrent, 
#                            Data$MeanExpectedFuture)
# 
Data$Mean_Change <- Data$MeanExpectedCurrent + Data$MeanExpectedFuture





## Recentering on 5 rather than 0
Data$MeanExpectedCurrent_Scaled <- Data$MeanExpectedCurrent + 5
Data$MeanExpectedFuture_Scaled <- Data$MeanExpectedFuture + 5

## Not used yet but calculated for later
Data$Mean_MinusLowerBound <- Data$MeanExpectedFuture - Data$Variance
Data$Mean_PlusUpperBound <- Data$MeanExpectedFuture + Data$Variance


## Dealing with reported mean:
Data$MeanExpectedFuture_SampleMean <- Data$MeanExpectedFuture %>% mean()
# Data$Mean_Change <- c(Data$MeanExpectedFuture - Data$MeanExpectedCurrent) 

## Misc changes:
Data$Q16_Comparison <- (Data$Q16_ClimateCurrentSelf - Data$Q16_MicroplasticsCurrentSelf) 
Data$LogBidIncome <- c(log((Data$Income_Annual - Data$Bid) / Data$Income_Annual))


# **************************************
# Section 2B: Define model summary function  ####
# **************************************


## So this code outputs a table of estimate,  p.v stars and s.e in brackets ##
### To make it easy,  just change the model name here and the code will output the table for your model:
ModelOutputs <- function(Model) {
  Input <- Model$coefficients %>% data.frame()
  Estimates <- summary(Model)$coefficients
  
  data.frame("Variable" =  Estimates %>% rownames(),
             "Estimate" =  paste(
               ifelse(
                 Estimates[, 4] < 0.01,
                 paste0(round(Estimates[, 1],  3),  "***"),
                 ifelse(
                   Estimates[, 4] < 0.05,
                   paste0(round(Estimates[, 1],  3),  "**"),
                   ifelse(
                     Estimates[, 4] < 0.1,
                     paste0(round(Estimates[, 1],  3),  "*"),
                     round(Estimates[, 1],  3)
                   )
                 )
               ),
               paste0("(", round(Estimates[, 2],  3), ")")
             ))
}


# **************************************
# Section 3: Specify Cameron model  ####
# **************************************


## So this is:
# -1: no intercept
# log(income-bid/income): income bid parameter
# mean future - current: change in expectations
# variance: entered as negative 
Model_Cameron2005 <- glm(
  CV ~ -1 + 
    c(log((Income_Annual - Bid) / Income_Annual)) +
    c(Mean_Change) +
    c(-(Variance_ConfidenceAsVariance)),
  family = binomial(link = "probit"),
  data = Data
)

# **************************************
# Section 3B: Recover Cameron WTP = EOP  ####
# **************************************


EOP_maker <- function(Model) {
  B0 <- Model$coefficients[1] %>% as.numeric()
  Y <- Data$Income * 12
  A <- ((Model$coefficients[2] * c(Data$Mean_Change)) +
          (Model$coefficients[3] * c(-(Data$Variance)))) %>% as.numeric()
  
  # EOP <- Y - Y exp (- A / B0) exp (1 / 2 B0 ^2)
  
  EOP <- Y - (Y*exp(- A / B0)) *
    exp(1 / B0 %>% raise_to_power(2) %>% multiply_by(2))
  
  return(EOP %>% as.data.frame())
}






# **************************************
# Section 4: Specify complex model  ####
# **************************************


## Same as above BUT
## Adding all covariates as interactions with 
## BOTH mean and variance
## note using E[t] rather than t- E[t]
Model_Cameron_MeanInteractions <- glm(
  CV ~ -1 + 
    LogBidIncome +
    Mean_Change +
    (MeanExpectedFuture * AgeDummy) +
    (MeanExpectedFuture * Gender) +
    (MeanExpectedFuture * EthnicityDummy) +
    (MeanExpectedFuture * Q16_Comparison) +
    (MeanExpectedFuture * Coronavirus) +
    (MeanExpectedFuture * IncomeDummy) +
    (MeanExpectedFuture * Charity) +
    (MeanExpectedFuture * Consequentiality) +
    (MeanExpectedFuture * DURATION) +
    (MeanExpectedFuture * Order) +
    (MeanExpectedFuture * Understanding) +
    c(-(Variance)),
  family = binomial(link = "probit"),
  data = Data
)

  
## DROPPING THESE FOR NOW UNTIL WE HAVE A 
## MOTIVATION FOR INCLUDING ALL OR A FACTOR OR TRANSFORM
# (ET * Q16_ClimateCurrentEnvironment) +
# (ET * Q16_ClimateCurrentSelf) +
# (ET * Q16_MicroplasticsCurrentEnvironment) +
# (ET * Q16_MicroplasticsTen) +
# (ET * Q16_MicroplasticsTwentyFive) +
# (ET * Q16_MicroplasticsFifty) +
# (ET * Q17_PandemicEnvironment) +
# (ET * Q17_PandemicMicroplastics) +


## Factors influencing variance here:
Model_Cameron_VarInteractions <- glm(
  CV ~ -1 + 
    LogBidIncome +
    Mean_Change +
    c(-(Variance)) +
    (Variance * AgeDummy) +
    (Variance * Gender) +
    (Variance * EthnicityDummy) +
    (Variance * Q16_Comparison) +
    (Variance * Coronavirus) +
    (Variance * IncomeDummy) +
    (Variance * Charity) +
    (Variance * Consequentiality) +
    (Variance * DURATION) +
    (Variance * Order) +
    (Variance * Understanding),
  family = binomial(link = "probit"),
  data = Data
)


## Preserving all here:
Model_Cameron_AllInteractions <- glm(
  CV ~ -1 + 
    LogBidIncome +
    Mean_Change +
    (Mean_Change * AgeDummy) +
    (Mean_Change * Gender) +
    (Mean_Change * EthnicityDummy) +
    (Mean_Change * Q16_Comparison) +
    (Mean_Change * Coronavirus) +
    (Mean_Change * IncomeDummy) +
    (Mean_Change * Charity) +
    (Mean_Change * Consequentiality) +
    (Mean_Change * DURATION) +
    (Mean_Change * Order) +
    (Mean_Change * Understanding) +
    c(-(Variance)) +
    (Variance * AgeDummy) +
    (Variance * Gender) +
    (Variance * EthnicityDummy) +
    (Variance * Q16_Comparison) +
    (Variance * Coronavirus) +
    (Variance * IncomeDummy) +
    (Variance * Charity) +
    (Variance * Consequentiality) +
    (Variance * DURATION) +
    (Variance * Order) +
    (Variance * Understanding),
  family = binomial(link = "probit"),
  data = Data
)


# **************************************
# Section 5: Model outputs  ####
# **************************************

# %>% write.csv(row.names = FALSE, quote = FALSE)
ModelOutputs(Model_Cameron2005) %>% write.csv(row.names = FALSE, quote = FALSE)
ModelOutputs(Model_Cameron_MeanInteractions) %>% write.csv(row.names = FALSE, quote = FALSE)
ModelOutputs(Model_Cameron_VarInteractions) %>% write.csv(row.names = FALSE, quote = FALSE)
ModelOutputs(Model_Cameron_AllInteractions) %>% write.csv(row.names = FALSE, quote = FALSE)


## Loglikelihoods and AIC
rbind(
  c(
  logLik(Model_Cameron2005) %>% as.numeric() %>% round(2),
  logLik(Model_Cameron_MeanInteractions) %>% as.numeric() %>% round(2),
  logLik(Model_Cameron_VarInteractions) %>% as.numeric() %>% round(2),
  logLik(Model_Cameron_AllInteractions) %>% as.numeric() %>% round(2)), 
  c(
  AIC(Model_Cameron2005) %>% as.numeric() %>% round(2),
  AIC(Model_Cameron_MeanInteractions) %>% as.numeric() %>% round(2),
  AIC(Model_Cameron_VarInteractions) %>% as.numeric() %>% round(2),
  AIC(Model_Cameron_AllInteractions) %>% as.numeric() %>% round(2))) %>% 
  data.frame() %>% 
  write.csv(row.names = FALSE, quote = FALSE)


## END OF SCRIPT **********************************************************