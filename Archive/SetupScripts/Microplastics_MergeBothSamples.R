#### DEFRA: Microplastics ####
## Function: Merges all responses in long and wide
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 18/06/2022
## TODO: setup RENV


#------------------------------
# Replication Information: ####
# Selected output of 'sessionInfo()'
#------------------------------


# R version 4.1.3 (2022-03-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)
# [1] LC_COLLATE=English_United Kingdom.1252 
#   [1] magrittr_2.0.3     lubridate_1.8.0    tidygeocoder_1.0.5 PostcodesioR_0.3.1
# [5] DCchoice_0.1.0     here_1.0.1         forcats_0.5.1      stringr_1.4.0     
# [9] dplyr_1.0.8        purrr_0.3.4        readr_2.1.2        tidyr_1.2.0       
# [13] tibble_3.1.6       ggplot2_3.3.6      tidyverse_1.3.1   
#   [1] tidyselect_1.1.2 splines_4.1.3    haven_2.5.0      lattice_0.20-45 
# [5] colorspace_2.0-3 vctrs_0.4.1      generics_0.1.2   utf8_1.2.2      
# [9] survival_3.3-1   rlang_1.0.2      pillar_1.7.0     glue_1.6.2      
# [13] withr_2.5.0      DBI_1.1.2        MLEcens_0.1-5    dbplyr_2.1.1    
# [17] modelr_0.1.8     readxl_1.4.0     lifecycle_1.0.1  munsell_0.5.0   
# [21] gtable_0.3.0     cellranger_1.1.0 rvest_1.0.2      tzdb_0.3.0      
# [25] fansi_1.0.3      broom_0.8.0      scales_1.2.0     backports_1.4.1 
# [29] jsonlite_1.8.0   fs_1.5.2         Icens_1.66.0     interval_1.1-0.8
# [33] hms_1.1.1        stringi_1.7.6    grid_4.1.3       rprojroot_2.0.3 
# [37] cli_3.3.0        tools_4.1.3      perm_1.0-0.2     Formula_1.2-4   
# [41] crayon_1.5.1     pkgconfig_2.0.3  ellipsis_0.3.2   MASS_7.3-56     
# [45] Matrix_1.4-1     xml2_1.3.3       reprex_2.0.1     assertthat_0.2.1
# [49] httr_1.4.2       rstudioapi_0.13  R6_2.5.1         compiler_4.1.3 

## Any issues installing packages try:
# Sys.setenv(RENV_DOWNLOAD_METHOD="libcurl")
# Sys.setenv(RENV_DOWNLOAD_FILE_METHOD=getOption("download.file.method"))

# renv::snapshot()
rm(list=ls())
library(tidyverse)
library(here)
library(DCchoice)
library(PostcodesioR)
library(tidygeocoder)
library(lubridate)
library(tidyr)
library(magrittr)
library(readxl)


#------------------------------
# Section 1: Import Data ####
# Selected output of 'sessionInfo()'
#------------------------------


here() ## This is the preferred approach to Setwd()
Order0_Wide <- data.frame(read.csv("Microplastics_Order0_WideFormat_Anonymised_2022_06_18.csv"))
Order0_Long <- data.frame(read.csv("Microplastics_Order0_LongFormat_Anonymised_2022_06_18.csv"))

Order1_Wide <- data.frame(read.csv("Microplastics_Order1_WideFormat_Anonymised_2022_06_18.csv"))
Order1_Long <- data.frame(read.csv("Microplastics_Order1_LongFormat_Anonymised_2022_06_18.csv"))


#------------------------------
# Section 2: Merge Wide ####
# Selected output of 'sessionInfo()'
#------------------------------

## Dropping columns that aren't in both datasets:
Order0_Wide$X.2 <- NULL
Order0_Wide$X.1 <- NULL
Order0_Wide$RID <- NULL
Order1_Wide$IP <- NULL 

## Adding Columns:
Order1_Wide <- add_column(Order1_Wide,"Task"=rep((1:4),times=nrow(Order1_Wide)/4),.after = "X")
Order1_Wide <- add_column(Order1_Wide,"AgeBracket"=rep(Order1_Long$AgeBracket,each=4),.after = "Gender")
Order1_Wide <- add_column(Order1_Wide,"CouncilTax"=rep(Order1_Long$CouncilTax,each=4),.after = "WaterBills.1")
Order1_Wide <- add_column(Order1_Wide,"Q16_ClimateCurrentEnvironment"=rep(Order1_Long$Q16_ClimateCurrentEnvironment,each=4),.after = "CouncilTax")
Order1_Wide <- add_column(Order1_Wide,"Income"=rep(Order1_Long$Income,each=4),.after = "Education")
Order1_Wide <- add_column(Order1_Wide,"av_A"=rep(1,times=nrow(Order1_Wide)),.after = "CV")


## Row bind together so all in one.
AllData_Wide <- rbind(Order0_Wide,Order1_Wide)


#------------------------------
# Section 3: Merge Long ####
# Selected output of 'sessionInfo()'
#------------------------------

Order0_Long$X.1 <- NULL

## Row bind together so all in one.
AllData_Long <- rbind(Order0_Long,Order1_Long)


#------------------------------
# Section 4: Export all ####
# Selected output of 'sessionInfo()'
#------------------------------


## Adding fitted WTP to wide data for now
AllData_Wide <- cbind(AllData_Wide,rep(AllData_Long[,81:96],each=4))


## Save all data:
Date <- gsub(pattern = "-",replacement = "_",Sys.Date())
write.csv(AllData_Long,file = paste0("Microplastics_AllData_Long_Anonymised_",Date,".csv"))
write.csv(AllData_Wide,file = paste0("Microplastics_AllData_Wide_Anonymised_",Date,".csv"))




