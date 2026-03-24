#### Microplastics: IOP Paper ####
## Function: Just estimate complex model WTP
## Author: Dr Peter King (p.king1@Leeds.ac.uk)
## Last change: 29/08/2023


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


## Scaling the mean variable by transforms in the paper
Data$Mean_Mean <- Data$MeanExpectedFuture
Data$Mean_Change <- (Data$MeanExpectedFuture - Data$MeanExpectedCurrent)
Data$Mean_Log <- Data$MeanExpectedFuture %>% magrittr::add(5) %>% log()
Data$Mean_Square <- Data$MeanExpectedFuture %>% magrittr::raise_to_power(2) 
Data$Mean_Int_Var <- Data$MeanExpectedFuture %>% multiply_by(Data$Variance) 
Data$Mean_Int_Charity <- Data$MeanExpectedFuture %>% multiply_by(Data$Charity)


## We have four categories of gender so maybe best to drop this and avoid issues
Data$Mean_Int_Gender <- Data$MeanExpectedFuture %>% multiply_by(Data$Gender)



## More transforms but for variance
Data$Variance_Square <- Data$Variance %>% magrittr::raise_to_power(2)
Data$Variance_Log <- Data$Variance %>% log()
Data$Variance_Divided <- 1 %>% divide_by(Data$Variance + 1)
Data$Variance_MeanDivided <- Data$MeanExpectedFuture %>% divide_by(Data$Variance + 1)


## Misc variable transforms
Data$Q16_Comparison <- (Data$Q16_ClimateCurrentSelf - Data$Q16_MicroplasticsCurrentSelf) 
Data$LogBidIncome <- ((Data$Income - Data$Bid) %>% divide_by(Data$Income)) %>% log()
Data$LogBidIncome_Gender <- (Data$LogBidIncome * Data$Gender)


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
  Input <- Model %>% summary()
  Estimates <- Input$glm.summary$coefficients %>% data.frame()
  
  data.frame("Variable" =  Estimates %>% rownames(),
             "Estimate" =  paste(
               ifelse(
                 Estimates$Pr...z.. < 0.01,
                 paste0(round(Estimates$Estimate,  3),  "***"),
                 ifelse(
                   Estimates$Pr...z.. < 0.05,
                   paste0(round(Estimates$Estimate,  3),  "**"),
                   ifelse(
                     Estimates$Pr...z.. < 0.1,
                     paste0(round(Estimates$Estimate,  3),  "*"),
                     round(Estimates$Estimate,  3)
                   )
                 )
               ),
               paste0("(", round(Estimates$Std..Error,  3), ")")
             ))
}



Diagnostics <- function(Model) {
  
  cbind(
    "Variable" = c("AIC", "LogLik", "R2", "Mean WTP"),
    
    "Estimate" = c(
      Model %>% AIC() %>% round(3),
      Model %>% logLik() %>% as.numeric() %>% round(3),
      summary(Model)$adjpsdR2 %>% as.numeric() %>% round(3),
      summary(Model)$meanWTP %>% as.numeric() %>% round(3)
    )) %>% data.frame()
  
}


# *****************************
# Section 4A: Model 1  ####
# *****************************


## Testing mean current or mean future
SBDC_Model_1 <- sbchoice(
  CV ~ 
    LogBidIncome +
    MeanExpectedFuture +
    Variance  |
    Bid,
  data = Data,
  dist = "normal"
) 

SBDC_Model_1 %>% summary()


## Trying current mean just in case
SBDC_Model_1b <- sbchoice(
  CV ~ 
    LogBidIncome +
    MeanExpectedCurrent +
    Variance  |
    Bid,
  data = Data,
  dist = "normal"
) 

SBDC_Model_1b %>% summary()


## Trying mean change instead
SBDC_Model_1c <- sbchoice(
  CV ~ 
    LogBidIncome +
    Mean_Change +
    Variance  |
    Bid,
  data = Data,
  dist = "normal"
) 

SBDC_Model_1c %>% summary()


# *****************************
# Section 4B: Model 2  ####
# *****************************


## Testing mean current or mean future
SBDC_Model_2 <- sbchoice(
  CV ~ 
    LogBidIncome +
    Mean_Change +
    Mean_Square +
    Variance_Square +
    Mean_Int_Var + 
    Variance  |
    Bid,
  data = Data,
  dist = "normal"
) 

SBDC_Model_2 %>% summary()


# *****************************
# Section 4C: Model 3  ####
# *****************************


## Model 2 but with covariates
SBDC_Model_3 <- sbchoice(
  CV ~ 
    LogBidIncome +
    Mean_Change +
    Mean_Square +
    Variance_Square +
    Mean_Int_Var + 
    Variance  +
    Gender + 
    Charity + 
    EthnicityDummy +
    AgeDummy +
    Consequentiality +
    Order |
    Bid,
  data = Data,
  dist = "normal"
) 

SBDC_Model_3 %>% summary()



# *****************************
# Section 4D: Model 4  ####
# *****************************


## Testing variance divided instead
SBDC_Model_4 <- sbchoice(
  CV ~ 
    LogBidIncome +
    Mean_Change +
    Mean_Square +
    Variance_Square +
    Variance_MeanDivided + 
    Gender + 
    Charity + 
    EthnicityDummy +
    AgeDummy +
    Consequentiality +
    Order |
    Bid,
  data = Data,
  dist = "normal"
) 

SBDC_Model_4 %>% summary()


# *****************************
# Section 4E: Model 5  ####
# *****************************


## Testing log instead of square
SBDC_Model_5 <- sbchoice(
  CV ~ 
    Mean_Change +
    Mean_Log +
    LogBidIncome +
    Variance  +
    Variance_Log +
    Gender + 
    Charity + 
    EthnicityDummy +
    AgeDummy +
    Consequentiality +
    Order |
    Bid,
  data = Data,
  dist = "normal"
) 

SBDC_Model_5 %>% summary()



# *****************************
# Section 4F: Model 6  ####
# *****************************


## 5 but with different income
SBDC_Model_6 <- sbchoice(
  CV ~ 
    Mean_Change +
    Mean_Log +
    Income +
    Variance  +
    Variance_Log +
    Gender + 
    Charity + 
    EthnicityDummy +
    AgeDummy +
    Consequentiality +
    Order |
    Bid,
  data = Data,
  dist = "normal"
) 


SBDC_Model_6 %>% summary()


# *****************************
# Section 4F: Model 6  ####
# *****************************

# 
# ## Testing mean current or mean future
# SBDC_Model_7 <- sbchoice(
#   CV ~ 
#     Mean_Change + 
#     Variance + 
#     LogBidIncome +
#     Gender + 
#     Charity + 
#     Mean_Square + 
#     Variance_Square + 
#     EthnicityDummy +
#     AgeDummy +
#     Consequentiality +
#     Order |
#     Bid,
#   data = Data,
#   dist = "normal"
# ) 
# 
# SBDC_Model_7 %>% summary()


# *****************************
# Section 5A: Reformat outputs ####
# *****************************

Output_Model_1 <- SBDC_Model_1 %>% ModelOutputs()
Output_Model_1b <- SBDC_Model_1b %>% ModelOutputs()
Output_Model_1c <- SBDC_Model_1c %>% ModelOutputs()
Output_Model_2 <- SBDC_Model_2 %>% ModelOutputs()
Output_Model_3 <- SBDC_Model_3 %>% ModelOutputs()
Output_Model_4 <- SBDC_Model_4 %>% ModelOutputs()
Output_Model_5 <- SBDC_Model_5 %>% ModelOutputs()
Output_Model_6 <- SBDC_Model_6 %>% ModelOutputs()


# *****************************
# Section 5B: Reorder models ####
# *****************************

Output_Model_1_Reordered <- rbind(Output_Model_1[c(1, 5, 3, 4, 2),],
                                  0, 0, 0, 0, 0, 0, 0, 0, 0)

Output_Model_1b_Reordered <- rbind(Output_Model_1b[c(1, 5, 3, 4, 2),],
                                   0, 0, 0, 0, 0, 0, 0, 0, 0)

Output_Model_1c_Reordered <- rbind(Output_Model_1c[c(1, 5, 3, 4, 2),],
                                   0, 0, 0, 0, 0, 0, 0, 0, 0)

Output_Model_2_Reordered <-
  rbind(Output_Model_2[c(1, 8, 3, 7, 2),], 
        Output_Model_2[c(4, 5, 6),],
        0, 0, 0, 0, 0, 0)

Output_Model_3_Reordered <-
  rbind(Output_Model_3[c(1, 14, 3, 7, 2), ], 
        Output_Model_3[c(4, 5, 6, 8:13), ]
  )


Output_Model_4_Reordered <-
  rbind(Output_Model_4[c(1, 13, 3, 6),],
        Output_Model_4[c(2, 4, 5), ],
        0, 
        Output_Model_4[c(7:12), ])



Output_Model_5_Reordered <-
  rbind(Output_Model_5[c(1, 13, 2, 5, 4, 3, 6),], 
        0, 
        Output_Model_5[c(7:12),])



Output_Model_6_Reordered <-
  rbind(Output_Model_6[c(1, 13, 2, 5, 4, 3, 6), ],
        0, 
        Output_Model_6[c(7:12), ])


# *****************************
# Section 5C: Tidy uo outputs ####
# *****************************


## Stitch together
Output <- cbind(
  Output_Model_1_Reordered,
  Output_Model_1b_Reordered,
  Output_Model_1c_Reordered,
  Output_Model_2_Reordered,
  Output_Model_3_Reordered,
  Output_Model_4_Reordered,
  Output_Model_5_Reordered,
  Output_Model_6_Reordered
)


Output %>% View()




# *****************************
# Section 5B: Construct Diagnostics ####
# *****************************

Output_AddedRows <-  rbind(
  Output,
  "Diagnostics",
  cbind(
    SBDC_Model_1 %>% Diagnostics(),
    SBDC_Model_1b %>% Diagnostics(),
    SBDC_Model_1c %>% Diagnostics(),
    SBDC_Model_2 %>% Diagnostics(),
    SBDC_Model_3 %>% Diagnostics(),
    SBDC_Model_4 %>% Diagnostics(),
    SBDC_Model_5 %>% Diagnostics(),
    SBDC_Model_6 %>% Diagnostics()
  )
)

# *****************************
# Section 5: Export Model ####
# *****************************


Output_AddedRows %>% fwrite(
  sep = ",",
  here("Tables", "TableX_ModelOutputs.txt")
)



# END OF SCRIPT ************************