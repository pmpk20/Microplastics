#### DEFRA: Microplastics ####
## Function: Imports and transforms the 1101 from prolific
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 15/06/2022
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


Covariates <- data.frame(read_excel("kent_eapmicro_live_covariates_2022_06_15.xlsx"))
DCE <- data.frame(read_excel("kent_eapmicro_live_dce_2022_06_15.xlsx"))
Timers <- data.frame(read_excel("KENT_EAPMICRO_Live_pages_2022_06_15.xlsx"))


#------------------------------
# Section 2: Rename columns ####
# Selected output of 'sessionInfo()'
#------------------------------


# colnames(Covariates) <- c("RID","Version",)

colnames(DCE) <- c("RID","DESIGN_ROW","SCENARIO","SEQ","Choice","A1X1","A1X2","A1X3",
  "A2X1","A2X2","A2X3",
  "A3X1","A3X2","A3X3")

#------------------------------
# Section 3: Reshape DCE and append ####
# Selected output of 'sessionInfo()'
#------------------------------


## Reshape DCE:
DCE_Wider <- data.frame(DCE %>% pivot_wider(id_cols = RID,names_from = SCENARIO,values_from = c("Choice","A1X1","A1X2","A1X3","A2X1","A2X2","A2X3","A3X1","A3X2","A3X3")))

## Drop Missing RIDS
Covariates_Trimmed <- Covariates[Covariates$STATUS==7,]
DCE_Wider_Trimmed <- DCE_Wider[DCE_Wider$RID %in% Covariates_Trimmed$RID,]


## Combine data
Combined_Trimmed <- cbind(Covariates_Trimmed,DCE_Wider_Trimmed)


## Drop unnecesary columns
Combined_Trimmed <- Combined_Trimmed[,-which(names(Combined_Trimmed) %in%c("STATUS",
                                               "HTTP_URL",
                                               "SESSION","START",
                                               "SRC","EXTID","consent"))]

## Recoding Gender:
Combined_Trimmed$q2 <- recode(Combined_Trimmed$q2,'1'=0,'2'=1,"3"=2,"4"=3)
## Note: Male == 0, Female ==1
## Rename timing of survey length to more easily understood name
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q2")] <- "Gender"

## Categorising respondent age:
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q3")] <- "AgeBracket"

## Categorising respondent Postcode:
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q4")] <- "Postcode"

## Categorising respondent Ethnicity:
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q5")] <- "Ethnicity"

## Categorising Variance Estimates:
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q89_1")] <- "MeanExpectedCurrent"
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q89_2")] <- "MeanExpectedFuture"
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q89_3")] <- "MeanExpectedBest"
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q89_4")] <- "MeanExpectedWorst"
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q7")] <- "Variance"

## Water bills question:
Combined_Trimmed$q8 <- recode(Combined_Trimmed$q8,
                              '1'=0,
                              '2'=100,
                              "3"=200,
                              "4"=300,
                              "5"=400,
                              "6"=500,
                              "7"=600,
                              "8"=700,
                              "9"=800,
                              "10"=900,
                              "11"=1000,
                              "12"=0)
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q8")] <- "WaterBills"


## Fixing Bid Levels:
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="bid_random")] <- "BidLevelFrequency"

## Renaming bid levels
Combined_Trimmed$bid <- as.numeric(gsub(Combined_Trimmed$bid,pattern = "£",replacement = ""))
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="bid")] <- "Bid"

## Merging and recoding CV question
Combined_Trimmed$CV <- coalesce(Combined_Trimmed$q9,Combined_Trimmed$q10)
Combined_Trimmed$CV <- recode(Combined_Trimmed$CV,'1'=1,'2'=0)


## Renaming protest responses
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q11")] <- "CVProtests"
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q11_other")] <- "CVProtestTexts"


## Renaming attitudinal scales:
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q16_1")] <- "Q16_ClimateCurrentEnvironment"
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q16_2")] <- "Q16_ClimateCurrentSelf"
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q16_3")] <- "Q16_MicroplasticsCurrentEnvironment"
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q16_4")] <- "Q16_MicroplasticsCurrentSelf"
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q16_5")] <- "Q16_MicroplasticsTen"
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q16_6")] <- "Q16_MicroplasticsTwentyFive"
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q16_7")] <- "Q16_MicroplasticsFifty"

## Same for Q17
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q17_1")] <- "Q17_PandemicEnvironment"
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q17_2")] <- "Q17_PandemicMicroplastics"


## Renaming Sociodemographics:
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q18")] <- "Coronavirus"
Combined_Trimmed$Coronavirus %<>% recode('1'=1,'2'=0)
## Recoding so had it ==1, no ==0

colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q19")] <- "Charity"
Combined_Trimmed$Charity %<>% recode('1'=1,'2'=0)
## Recoding so involved/donated ==1, no ==0

colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q20")] <- "Consequentiality"
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q20_other")] <- "ConsequentialityText"
Combined_Trimmed$Consequentiality %<>% recode('1'=1,'2'=0)
## Recoding so yes ==1, no ==0


colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q21")] <- "Education"
Combined_Trimmed$Education[Combined_Trimmed$Education==6]<- 4
## Recoding "unknown level, foreign, vocational"etc as "A level" equivalent.


colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q22")] <- "Income"

Combined_Trimmed$Income <- recode(Combined_Trimmed$Income,
                              "1"=250,
                              "2" = 750,
                              "3"=1250,
                              "4"=1750,
                              "5"=2250,
                              "6"=2750,
                              "7"=3500,
                              "8"=4500,
                              "9"=5000,
                              "10"=2500)
## Note value==6 coded as median income


colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q23")] <- "Understanding"


# ----------------------------------------------------------------------------------------------------------
## Renaming misc columns:

colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q9")] <- "WaterBills"
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q10")] <- "CouncilTax"
colnames(Combined_Trimmed)[which(names(Combined_Trimmed)=="q24")] <- "FreeText"


# ----------------------------------------------------------------------------------------------------------
## Putting Choices And Attributes Together

# ## Take all the respondent choices and change into a single vector
Choices <- data.frame(Choice = c(t(
  data.frame(rep(Combined_Trimmed[,41:44], times=1)))[,]))


Combined_NoCE <- Combined_Trimmed[,-c(41:80)]
## Combine the choices above with a `Task` variable and all the remaining data reshaped from long to wide
Combined_Reshaped <- (data.frame(rep((1:4),times=nrow(Combined_Trimmed)),
                        Choices,
                        data.frame(apply(X = Combined_NoCE,2,function(x) rep(x,each=4)))))
colnames(Combined_Reshaped)[1:2] <- c("Task","Choice") ## Note: The `Task` variable refers to which number of choice tasks the respondent did.



## APOLLO always needs these availability indicators in case any alternative is not available.
Combined_Reshaped$av_A <- rep(1,nrow(Combined_Reshaped)) ## Alternative A: Alt1
Combined_Reshaped$av_B <- rep(1,nrow(Combined_Reshaped)) ## Alternative B: Alt2
Combined_Reshaped$av_C <- rep(1,nrow(Combined_Reshaped)) ## Alternative C: Status Quo


## Add useful variables:
Combined_Reshaped$ID <- seq.int(nrow(Combined_Reshaped)) # Unique identifier for each respondent and choice (Uniques: 30,063)
Combined_Reshaped$Respondent <-rep(1:nrow(Combined_Trimmed),each=length(unique(Combined_Reshaped$Task))) # unique identifier for each respondent (Uniques: 3407)


## Remove missing or incomplete RIDs
DCE_Trimmed <- DCE[DCE$RID %in% Combined_Trimmed$RID,]
DCE_Trimmed$CEBlock <- ifelse(DCE_Trimmed$DESIGN_ROW<5,1,2)

## Renaming attribute columns:
Attributes <- DCE_Trimmed[,6:14]
colnames(Attributes) <- c("Performance_A","Emission_A","Price_A",
  "Performance_B","Emission_B","Price_B",
  "Performance_C","Emission_C","Price_C")


## Rename merged wide data to "database" for Apollo:
database <- cbind(Combined_Reshaped,Attributes)

## Drop unnecesary columns
database <- database[,-which(names(database) %in%c("CVProtests", "CVProtestTexts",
                                                   "ConsequentialityText","START",
                                                   "FreeText"))]


## Recode NA or missing as ZERO
database %<>%  mutate(across(where(is.numeric), ~replace_na(.x, 0)))


# ----------------------------------------------------------------------------------------------------------
## Recoding attributes:


## Recode Attribute Levels:
database$Price_A <- recode(database$Price_A,'1'=0.00)
database$Price_B <- recode(database$Price_B,'1'=0.50,'2'=1.00,"3"=2.50,"4"=5.00)
database$Price_C <- recode(database$Price_C,'1'=0.50,'2'=1.00,"3"=2.50,"4"=5.00)

database$Performance_A <- recode(database$Performance_A,'1'=0.00)
database$Performance_B <- recode(database$Performance_B,'1'=5,'2'=10,"3"=50)
database$Performance_C <- recode(database$Performance_C,'1'=5,'2'=10,"3"=50)


database$Emission_A <- recode(database$Emission_A,'1'=0.00)
database$Emission_B <- recode(database$Emission_B,'1'=10,'2'=40,"3"=90)
database$Emission_C <- recode(database$Emission_C,'1'=10,'2'=40,"3"=90)


# ----------------------------------------------------------------------------------------------------------
## Exporting data:



## Save all data:
Date <- gsub(pattern = "-",replacement = "_",Sys.Date())
write.csv(Combined_Trimmed,file = paste0("Microplastics_Order0_Long_",Date,".csv"))
write.csv(database,file = paste0("Microplastics_Order0_Wide_",Date,".csv"))



# ----------------------------------------------------------------------------------------------------------
## Exporting anonymised data:


AllWTP <- data.frame(read.csv("Microplastics_DataWithAllWTP_2022_06_16.csv"))
WTP <- data.frame(read.csv("Microplastics_DataWithWTP_2022_06_16.csv"))
Long <- data.frame(read.csv("Microplastics_Anonymised_LongFormat_2022_06_16.csv"))
Wide <- data.frame(read.csv("Microplastics_Anonymised_2022_06_16.csv"))
database <- data.frame(read.csv("Microplastics_database_2022_06_15.csv"))


AllWTP$Order <- 0
WTP$Order <- 0
Long$Order <- 0
Wide$Order <- 0
database$Order <- 0


## Order 0: CV then CE questions 
Data$Order <- 0

Data_Anonymised <- Data[, -which(
  names(Data) %in% c("VERSION","IP",
    "X","X.2","Postcode",
    "X.1",
    "X.3",
    "CVProtests",
    "CVProtestTexts",
    "ConsequentialityText",
    "START",
    "FreeText"
  )
)]

write.csv(Data_Anonymised,file = paste0("Microplastics_Anonymised_",Date,".csv"))


write.csv(AllWTP,file = paste0("Microplastics_Order0AllWTP_",Date,".csv"))
write.csv(WTP,file = paste0("Microplastics_Order0CVWTP_",Date,".csv"))
write.csv(Long,file = paste0("Microplastics_Order0_WideFormat_Anonymised_",Date,".csv"))
write.csv(Wide,file = paste0("Microplastics_Order0_LongFormat_Anonymised_",Date,".csv"))
write.csv(database,file = paste0("Microplastics_Order0_WideFormat_",Date,".csv"))


## Next steps:
### Estimate CE:
#  source("Microplastics_FinalPilot25_MNL_2022_05_03.R")
### Estimate CV:
# source("Microplastics_FinalPilot25_CV_2022_05_03.R")


# End Of Script ----------------------------------------------------------------------------------------------------------