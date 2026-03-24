#### Microplastics: IOP Paper ####
## Function: Output model 
## Author: Dr Peter King (p.king1@Leeds.ac.uk)
## Last change: 24/08/2023


#------------------------------
# Replication Information: ####
# Selected output of 'sessionInfo()'
#------------------------------


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


#------------------------------
# Section 1: Import Data ####
#------------------------------



## Start with the latest anonymised data in one-row per one-respondent format
Data <-
  here("Data",
       "Microplastics_AllData_Long_Anonymised_2022_06_19.csv") %>%
  fread() %>%
  data.frame()


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

#------------------------------
# Section 2: Recoding variables ####
#------------------------------


Data$IncomeAnnual <- Data$Income %>% multiply_by(12)
Data$IncomeBracket <- ifelse(Data$IncomeAnnual <= 21000,
                             0,
                             ifelse(
                               Data$IncomeAnnual <= 30000,
                               1,
                               ifelse(
                                 Data$IncomeAnnual <= 50000,
                                 2,
                                 ifelse(Data$IncomeAnnual > 50000, 3, 4)
                               )
                             )) 



## More concise to define a mean (SD) function here
MeanSD <- function(Input) {
  paste0(mean(Input) %>% round(2),
         " (",
         sd(Input) %>% round(2),
         ")")
}


Variables <- c("Gender", 
               "AgeBracket",
               "EthnicityDummy",
               "IncomeBracket",
               "Order",
               "Charity")


## Set here for consistency
ColumnNames <- c("Category", "N", "Sample",
                 "Mean",
                 "Variance",
                 "WTP")



#------------------------------
# Section 3: Create table by looping through variables ####
#------------------------------


FunctionOutput = vector("list",  
                        length = Data[, Variables] %>% colnames() %>% length())


for (i in Data[, Variables] %>% colnames()){ 
  
  
  ## for each variable we report these details
  TableSlice <- Data %>% 
    select(MeanExpectedCurrent,
           Variance,
           ModelWTP,
           matches(i)) %>% 
    dplyr::group_by(across(i)) %>% 
    summarise("N" = n(), 
              "Sample" = paste0((100/nrow(Data)*n()) %>% round(2), "%"),
              "Mean" = MeanSD(MeanExpectedCurrent), 
              "Variance" = MeanSD(Variance),
              "WTP" = paste0("£", MeanSD(ModelWTP)))


  ## rename first column for ease of merge
  colnames(TableSlice) <- ColumnNames
  
  
  ## add identifying column
  TableSlice <- cbind("Variable" = i, TableSlice) 
  
  
  ## Save output
  FunctionOutput[[i]] <- TableSlice
  
  
  }


## Stitch all together here
TableData = do.call(bind_rows,  FunctionOutput)



#------------------------------
# Section 4: Mean and variance tests ####
#------------------------------


## Gender ****************************************************
Length <- Data$Gender %>% unique() %>% length()
TestOutput_Means = matrix(0, nrow = Length, ncol = 1)
TestOutput_Vars = matrix(0, nrow = Length, ncol = 1)

for (i in 0:(Length-1)){
  
  Test_Means <- wilcox.test(
    Data$MeanExpectedCurrent[Data$Gender == i],
    Data$MeanExpectedCurrent[Data$Gender != i])

  
  Test_Vars <- wilcox.test(
    Data$Variance[Data$Gender == i],
    Data$Variance[Data$Gender != i])
  
  
  ## Save output
  TestOutput_Means[[i+1]] <- paste0(Test_Means$statistic %>% round(2), " (", Test_Means$p.value %>% AddStars(), ")")
  TestOutput_Vars[[i+1]] <- paste0(Test_Vars$statistic %>% round(2), " (", Test_Vars$p.value %>% AddStars(), ")")
}

Rows_Means_Gender <- TestOutput_Means %>% data.frame()
Rows_Vars_Gender <- TestOutput_Vars %>% data.frame()




## AgeBracket ****************************************************
Length <- Data$AgeBracket %>% unique() %>% length()
TestOutput_Means = matrix(0, nrow = Length, ncol = 1)
TestOutput_Vars = matrix(0, nrow = Length, ncol = 1)


for (i in 1:6){
  
  Test_Means <- wilcox.test(
    Data$MeanExpectedCurrent[Data$AgeBracket == i],
    Data$MeanExpectedCurrent[Data$AgeBracket != i])
  
  
  Test_Vars <- wilcox.test(
    Data$Variance[Data$AgeBracket == i],
    Data$Variance[Data$AgeBracket != i])
  
  
  ## Save output
  TestOutput_Means[[i]] <- paste0(Test_Means$statistic %>% round(2), " (", Test_Means$p.value %>% AddStars(), ")")
  TestOutput_Vars[[i]] <- paste0(Test_Vars$statistic %>% round(2), " (", Test_Vars$p.value %>% AddStars(), ")")
}

Rows_Means_AgeBracket <- TestOutput_Means %>% data.frame()
Rows_Vars_AgeBracket <- TestOutput_Vars %>% data.frame()




## EthnicityDummy ****************************************************
Length <- Data$EthnicityDummy %>% unique() %>% length()
TestOutput_Means = matrix(0, nrow = Length, ncol = 1)
TestOutput_Vars = matrix(0, nrow = Length, ncol = 1)

for (i in 0:(Length-1)){
  
  Test_Means <- wilcox.test(
    Data$MeanExpectedCurrent[Data$EthnicityDummy == i],
    Data$MeanExpectedCurrent[Data$EthnicityDummy != i])
  
  
  Test_Vars <- wilcox.test(
    Data$Variance[Data$EthnicityDummy == i],
    Data$Variance[Data$EthnicityDummy != i])
  
  
  ## Save output
  TestOutput_Means[[i+1]] <- paste0(Test_Means$statistic %>% round(2), " (", Test_Means$p.value %>% AddStars(), ")")
  TestOutput_Vars[[i+1]] <- paste0(Test_Vars$statistic %>% round(2), " (", Test_Vars$p.value %>% AddStars(), ")")
}

Rows_Means_EthnicityDummy <- TestOutput_Means %>% data.frame()
Rows_Vars_EthnicityDummy <- TestOutput_Vars %>% data.frame()






## IncomeBracket ****************************************************


## IncomeBracket ****************************************************
Length <- Data$IncomeBracket %>% unique() %>% length()
TestOutput_Means = matrix(0, nrow = Length, ncol = 1)
TestOutput_Vars = matrix(0, nrow = Length, ncol = 1)

for (i in 0:(Length-1)){
  
  Test_Means <- wilcox.test(
    Data$MeanExpectedCurrent[Data$IncomeBracket == i],
    Data$MeanExpectedCurrent[Data$IncomeBracket != i])
  
  
  Test_Vars <- wilcox.test(
    Data$Variance[Data$IncomeBracket == i],
    Data$Variance[Data$IncomeBracket != i])
  
  
  ## Save output
  TestOutput_Means[[i+1]] <- paste0(Test_Means$statistic %>% round(2), " (", Test_Means$p.value %>% AddStars(), ")")
  TestOutput_Vars[[i+1]] <- paste0(Test_Vars$statistic %>% round(2), " (", Test_Vars$p.value %>% AddStars(), ")")
}

Rows_Means_IncomeBracket <- TestOutput_Means %>% data.frame()
Rows_Vars_IncomeBracket <- TestOutput_Vars %>% data.frame()



## Order ****************************************************
Length <- Data$Order %>% unique() %>% length()
TestOutput_Means = matrix(0, nrow = Length, ncol = 1)
TestOutput_Vars = matrix(0, nrow = Length, ncol = 1)

for (i in 0:(Length-1)){
  
  Test_Means <- wilcox.test(
    Data$MeanExpectedCurrent[Data$Order == i],
    Data$MeanExpectedCurrent[Data$Order != i])
  
  
  Test_Vars <- wilcox.test(
    Data$Variance[Data$Order == i],
    Data$Variance[Data$Order != i])
  
  
  ## Save output
  TestOutput_Means[[i+1]] <- paste0(Test_Means$statistic %>% round(2), " (", Test_Means$p.value %>% AddStars(), ")")
  TestOutput_Vars[[i+1]] <- paste0(Test_Vars$statistic %>% round(2), " (", Test_Vars$p.value %>% AddStars(), ")")
}

Rows_Means_Order <- TestOutput_Means %>% data.frame()
Rows_Vars_Order <- TestOutput_Vars %>% data.frame()



## Charity ****************************************************
Length <- Data$Charity %>% unique() %>% length()
TestOutput_Means = matrix(0, nrow = Length, ncol = 1)
TestOutput_Vars = matrix(0, nrow = Length, ncol = 1)

for (i in 0:(Length-1)){
  
  Test_Means <- wilcox.test(
    Data$MeanExpectedCurrent[Data$Charity == i],
    Data$MeanExpectedCurrent[Data$Charity != i])
  
  
  Test_Vars <- wilcox.test(
    Data$Variance[Data$Charity == i],
    Data$Variance[Data$Charity != i])
  
  
  ## Save output
  TestOutput_Means[[i+1]] <- paste0(Test_Means$statistic %>% round(2), " (", Test_Means$p.value %>% AddStars(), ")")
  TestOutput_Vars[[i+1]] <- paste0(Test_Vars$statistic %>% round(2), " (", Test_Vars$p.value %>% AddStars(), ")")
}

Rows_Means_Charity <- TestOutput_Means %>% data.frame()
Rows_Vars_Charity <- TestOutput_Vars %>% data.frame()


## Combine all ****************************************************


Tests_Means <- rbind(
Rows_Means_Gender,
Rows_Means_AgeBracket,
Rows_Means_EthnicityDummy,
Rows_Means_IncomeBracket,
Rows_Means_Order,
Rows_Means_Charity
  )




Tests_Vars <- rbind(
  Rows_Vars_Gender,
  Rows_Vars_AgeBracket,
  Rows_Vars_EthnicityDummy,
  Rows_Vars_IncomeBracket,
  Rows_Vars_Order,
  Rows_Vars_Charity
)

#------------------------------
# Section 3: Export table ####
#------------------------------

Output <- bind_cols(
  TableData[, 1:5],
  "MeanTests" = Tests_Means$.,
  "Variance" = TableData$Variance,
  "VarianceTests" = Tests_Vars$.
)

Output %>% fwrite(sep = ",",
                     here("Tables","TableX_Summaries.txt"))

