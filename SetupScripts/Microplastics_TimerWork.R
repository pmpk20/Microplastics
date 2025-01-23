#### Micropalstics Timing Paper ####
## Function: Reports time length complete and per page
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 22/07/2022
## TODO: count WTP

## Notes:
### Is there a function to calculate page differences? 


#------------------------------
# Replication Information: ####
#------------------------------

# R version 4.1.3 (2022-03-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)
# Matrix products: default
# locale: [1] LC_COLLATE=English_United Kingdom.1252 
# other attached packages:
#   [1] forcats_0.5.1   stringr_1.4.0   purrr_0.3.4     readr_2.1.2    
# [5] tidyr_1.2.0     tibble_3.1.6    tidyverse_1.3.1 reshape2_1.4.4 
# [9] ggridges_0.5.3  ggplot2_3.3.6   magrittr_2.0.3  dplyr_1.0.8    
# [13] apollo_0.2.7   


#------------------------------
# Setup Environment: ####
#------------------------------

rm(list = ls())
# setwd("Z:/Parakeets")


library(apollo)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggridges)
library(reshape2)
library(readxl)
library(mded)
library(tidyverse)

#------------------------------
# Import Data: ####
#------------------------------


## This is the latest per person data
Data <- data.frame(read.csv("Microplastics_AllData_Long_PlusSpatial_2022_07_12.csv"))

Timers_Order0 <- data.frame(read_excel("KENT_EAPMICRO_Live_pages_2022_06_15.xlsx"))
Timers_Order1 <- data.frame(read_excel("KENT_EAPMICRO_LiveOrderCECV_pages.xlsx"))

#-----------------------------------------------------------------------------
# Manipulate Timer Order 0: CV-CE Data: ####
#-----------------------------------------------------------------------------


## Exclude incompletes:
Timers_Order0_Trimmed <- Timers_Order0[(Timers_Order0$RID %in% Data$RID[Data$Order==0])==TRUE,]


## Calculate read time for each page:
## NOTE: Is there a function way to do this?
Timers_Order0_Trimmed$PAGE_READ_1 <- Timers_Order0_Trimmed$PAGE_SUBMIT_1 - Timers_Order0_Trimmed$PAGE_DISPLAY_1
Timers_Order0_Trimmed$PAGE_READ_2 <- Timers_Order0_Trimmed$PAGE_SUBMIT_2 - Timers_Order0_Trimmed$PAGE_DISPLAY_2
Timers_Order0_Trimmed$PAGE_READ_3 <- Timers_Order0_Trimmed$PAGE_SUBMIT_3 - Timers_Order0_Trimmed$PAGE_DISPLAY_3
Timers_Order0_Trimmed$PAGE_READ_4 <- Timers_Order0_Trimmed$PAGE_SUBMIT_4 - Timers_Order0_Trimmed$PAGE_DISPLAY_4
Timers_Order0_Trimmed$PAGE_READ_5 <- Timers_Order0_Trimmed$PAGE_SUBMIT_5 - Timers_Order0_Trimmed$PAGE_DISPLAY_5
Timers_Order0_Trimmed$PAGE_READ_6 <- Timers_Order0_Trimmed$PAGE_SUBMIT_6 - Timers_Order0_Trimmed$PAGE_DISPLAY_6
Timers_Order0_Trimmed$PAGE_READ_7 <- Timers_Order0_Trimmed$PAGE_SUBMIT_7 - Timers_Order0_Trimmed$PAGE_DISPLAY_7
Timers_Order0_Trimmed$PAGE_READ_8 <- Timers_Order0_Trimmed$PAGE_SUBMIT_8 - Timers_Order0_Trimmed$PAGE_DISPLAY_8
Timers_Order0_Trimmed$PAGE_READ_9 <- Timers_Order0_Trimmed$PAGE_SUBMIT_9 - Timers_Order0_Trimmed$PAGE_DISPLAY_9
Timers_Order0_Trimmed$PAGE_READ_10 <- Timers_Order0_Trimmed$PAGE_SUBMIT_10 - Timers_Order0_Trimmed$PAGE_DISPLAY_10
Timers_Order0_Trimmed$PAGE_READ_11 <- Timers_Order0_Trimmed$PAGE_SUBMIT_11 - Timers_Order0_Trimmed$PAGE_DISPLAY_11
Timers_Order0_Trimmed$PAGE_READ_12 <- Timers_Order0_Trimmed$PAGE_SUBMIT_12 - Timers_Order0_Trimmed$PAGE_DISPLAY_12
Timers_Order0_Trimmed$PAGE_READ_13 <- Timers_Order0_Trimmed$PAGE_SUBMIT_13 - Timers_Order0_Trimmed$PAGE_DISPLAY_13
Timers_Order0_Trimmed$PAGE_READ_14 <- Timers_Order0_Trimmed$PAGE_SUBMIT_14 - Timers_Order0_Trimmed$PAGE_DISPLAY_14
Timers_Order0_Trimmed$PAGE_READ_15 <- Timers_Order0_Trimmed$PAGE_SUBMIT_15 - Timers_Order0_Trimmed$PAGE_DISPLAY_15
Timers_Order0_Trimmed$PAGE_READ_16 <- Timers_Order0_Trimmed$PAGE_SUBMIT_16 - Timers_Order0_Trimmed$PAGE_DISPLAY_16
Timers_Order0_Trimmed$PAGE_READ_17 <- Timers_Order0_Trimmed$PAGE_SUBMIT_17 - Timers_Order0_Trimmed$PAGE_DISPLAY_17
Timers_Order0_Trimmed$PAGE_READ_18 <- Timers_Order0_Trimmed$PAGE_SUBMIT_18 - Timers_Order0_Trimmed$PAGE_DISPLAY_18
Timers_Order0_Trimmed$PAGE_READ_19 <- Timers_Order0_Trimmed$PAGE_SUBMIT_19 - Timers_Order0_Trimmed$PAGE_DISPLAY_19
Timers_Order0_Trimmed$PAGE_READ_20 <- Timers_Order0_Trimmed$PAGE_SUBMIT_20 - Timers_Order0_Trimmed$PAGE_DISPLAY_20
Timers_Order0_Trimmed$PAGE_READ_21 <- Timers_Order0_Trimmed$PAGE_SUBMIT_21 - Timers_Order0_Trimmed$PAGE_DISPLAY_21
Timers_Order0_Trimmed$PAGE_READ_22 <- Timers_Order0_Trimmed$PAGE_SUBMIT_22 - Timers_Order0_Trimmed$PAGE_DISPLAY_22
Timers_Order0_Trimmed$PAGE_READ_23 <- Timers_Order0_Trimmed$PAGE_SUBMIT_23 - Timers_Order0_Trimmed$PAGE_DISPLAY_23
Timers_Order0_Trimmed$PAGE_READ_24 <- Timers_Order0_Trimmed$PAGE_SUBMIT_24 - Timers_Order0_Trimmed$PAGE_DISPLAY_24

## Label Pages:
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_1")] <- "PAGE_READ_Consent" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_2")] <- "PAGE_READ_AgeGender" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_3")] <- "PAGE_READ_PostEth" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_4")] <- "PAGE_READ_Info" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_5")] <- "PAGE_READ_MicroScaleNow" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_6")] <- "PAGE_READ_MicroScaleFuture" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_7")] <- "PAGE_READ_MicroVarBest" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_8")] <- "PAGE_READ_MicroVarWorst" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_9")] <- "PAGE_READ_CVWaterBills" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_10")] <- "PAGE_READ_CV_Water" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_11")] <- "PAGE_READ_CV_Council" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_12")] <- "PAGE_READ_CE_Detergent" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_13")] <- "PAGE_READ_CE_Info" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_14")] <- "PAGE_READ_CE_Example" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_15")] <- "PAGE_READ_CE_Choice1" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_16")] <- "PAGE_READ_CE_Choice2" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_17")] <- "PAGE_READ_CE_Choice3" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_18")] <- "PAGE_READ_CE_Choice4" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_19")] <- "PAGE_READ_Attitudes_1" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_20")] <- "PAGE_READ_Attitudes_2" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_21")] <- "PAGE_READ_Debrief_1" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_22")] <- "PAGE_READ_Debrief_2" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_23")] <- "PAGE_READ_End" 
colnames(Timers_Order0_Trimmed)[which(names(Timers_Order0_Trimmed)=="PAGE_READ_24")] <- "PAGE_READ_Screenout" 


#-----------------------------------------------------------------------------
# Manipulate Timer Order 1: CE-CV Data: ####
#-----------------------------------------------------------------------------


## Exclude Incompletes
Timers_Order1_Trimmed <- Timers_Order1[(Timers_Order1$RID %in% Data$RID[Data$Order==1])==TRUE,]


## Calculate read time for each page:
## NOTE: Is there a function way to do this?
Timers_Order1_Trimmed$PAGE_READ_1 <- Timers_Order1_Trimmed$PAGE_SUBMIT_1 - Timers_Order1_Trimmed$PAGE_DISPLAY_1
Timers_Order1_Trimmed$PAGE_READ_2 <- Timers_Order1_Trimmed$PAGE_SUBMIT_2 - Timers_Order1_Trimmed$PAGE_DISPLAY_2
Timers_Order1_Trimmed$PAGE_READ_3 <- Timers_Order1_Trimmed$PAGE_SUBMIT_3 - Timers_Order1_Trimmed$PAGE_DISPLAY_3
Timers_Order1_Trimmed$PAGE_READ_4 <- Timers_Order1_Trimmed$PAGE_SUBMIT_4 - Timers_Order1_Trimmed$PAGE_DISPLAY_4
Timers_Order1_Trimmed$PAGE_READ_5 <- Timers_Order1_Trimmed$PAGE_SUBMIT_5 - Timers_Order1_Trimmed$PAGE_DISPLAY_5
Timers_Order1_Trimmed$PAGE_READ_6 <- Timers_Order1_Trimmed$PAGE_SUBMIT_6 - Timers_Order1_Trimmed$PAGE_DISPLAY_6
Timers_Order1_Trimmed$PAGE_READ_7 <- Timers_Order1_Trimmed$PAGE_SUBMIT_7 - Timers_Order1_Trimmed$PAGE_DISPLAY_7
Timers_Order1_Trimmed$PAGE_READ_8 <- Timers_Order1_Trimmed$PAGE_SUBMIT_8 - Timers_Order1_Trimmed$PAGE_DISPLAY_8
Timers_Order1_Trimmed$PAGE_READ_9 <- Timers_Order1_Trimmed$PAGE_SUBMIT_9 - Timers_Order1_Trimmed$PAGE_DISPLAY_9
Timers_Order1_Trimmed$PAGE_READ_10 <- Timers_Order1_Trimmed$PAGE_SUBMIT_10 - Timers_Order1_Trimmed$PAGE_DISPLAY_10
Timers_Order1_Trimmed$PAGE_READ_11 <- Timers_Order1_Trimmed$PAGE_SUBMIT_11 - Timers_Order1_Trimmed$PAGE_DISPLAY_11
Timers_Order1_Trimmed$PAGE_READ_12 <- Timers_Order1_Trimmed$PAGE_SUBMIT_12 - Timers_Order1_Trimmed$PAGE_DISPLAY_12
Timers_Order1_Trimmed$PAGE_READ_13 <- Timers_Order1_Trimmed$PAGE_SUBMIT_13 - Timers_Order1_Trimmed$PAGE_DISPLAY_13
Timers_Order1_Trimmed$PAGE_READ_14 <- Timers_Order1_Trimmed$PAGE_SUBMIT_14 - Timers_Order1_Trimmed$PAGE_DISPLAY_14
Timers_Order1_Trimmed$PAGE_READ_15 <- Timers_Order1_Trimmed$PAGE_SUBMIT_15 - Timers_Order1_Trimmed$PAGE_DISPLAY_15
Timers_Order1_Trimmed$PAGE_READ_16 <- Timers_Order1_Trimmed$PAGE_SUBMIT_16 - Timers_Order1_Trimmed$PAGE_DISPLAY_16
Timers_Order1_Trimmed$PAGE_READ_17 <- Timers_Order1_Trimmed$PAGE_SUBMIT_17 - Timers_Order1_Trimmed$PAGE_DISPLAY_17
Timers_Order1_Trimmed$PAGE_READ_18 <- Timers_Order1_Trimmed$PAGE_SUBMIT_18 - Timers_Order1_Trimmed$PAGE_DISPLAY_18
Timers_Order1_Trimmed$PAGE_READ_19 <- Timers_Order1_Trimmed$PAGE_SUBMIT_19 - Timers_Order1_Trimmed$PAGE_DISPLAY_19
Timers_Order1_Trimmed$PAGE_READ_20 <- Timers_Order1_Trimmed$PAGE_SUBMIT_20 - Timers_Order1_Trimmed$PAGE_DISPLAY_20
Timers_Order1_Trimmed$PAGE_READ_21 <- Timers_Order1_Trimmed$PAGE_SUBMIT_21 - Timers_Order1_Trimmed$PAGE_DISPLAY_21
Timers_Order1_Trimmed$PAGE_READ_22 <- Timers_Order1_Trimmed$PAGE_SUBMIT_22 - Timers_Order1_Trimmed$PAGE_DISPLAY_22
Timers_Order1_Trimmed$PAGE_READ_23 <- Timers_Order1_Trimmed$PAGE_SUBMIT_23 - Timers_Order1_Trimmed$PAGE_DISPLAY_23
Timers_Order1_Trimmed$PAGE_READ_24 <- Timers_Order1_Trimmed$PAGE_SUBMIT_24 - Timers_Order1_Trimmed$PAGE_DISPLAY_24

colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_1")] <- "PAGE_READ_Consent" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_2")] <- "PAGE_READ_AgeGender" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_3")] <- "PAGE_READ_PostEth" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_4")] <- "PAGE_READ_Info" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_5")] <- "PAGE_READ_MicroScaleNow" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_6")] <- "PAGE_READ_MicroScaleFuture" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_7")] <- "PAGE_READ_MicroVarBest" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_8")] <- "PAGE_READ_MicroVarWorst" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_9")] <- "PAGE_READ_CE_Detergent" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_10")] <- "PAGE_READ_CE_Info" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_11")] <- "PAGE_READ_CE_Example" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_12")] <- "PAGE_READ_CE_Choice1" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_13")] <- "PAGE_READ_CE_Choice2" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_14")] <- "PAGE_READ_CE_Choice3" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_15")] <- "PAGE_READ_CE_Choice4" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_16")] <- "PAGE_READ_CVWaterBills" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_17")] <- "PAGE_READ_CV_Water" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_18")] <- "PAGE_READ_CV_Council" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_19")] <- "PAGE_READ_Attitudes_1" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_20")] <- "PAGE_READ_Attitudes_2" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_21")] <- "PAGE_READ_Debrief_1" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_22")] <- "PAGE_READ_Debrief_2" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_23")] <- "PAGE_READ_End" 
colnames(Timers_Order1_Trimmed)[which(names(Timers_Order1_Trimmed)=="PAGE_READ_24")] <- "PAGE_READ_Screenout" 


#-----------------------------------------------------------------------------
# Combine All Timer Data: ####
#-----------------------------------------------------------------------------


AllTimers <- rbind(
  cbind(Timers_Order0_Trimmed,"Order"=0),
  cbind(Timers_Order1_Trimmed,"Order"=1))


AllTimers$PAGE_READ_CV_Coalesced <- coalesce(AllTimers$PAGE_READ_CV_Water,AllTimers$PAGE_READ_CV_Council)

AllTimers_Reading <- cbind(AllTimers[,50:58],
      "PAGE_READ_CV_Coalesced"=AllTimers$PAGE_READ_CV_Coalesced,
      AllTimers[,61:71])


#------------------------------
# Word Count Data: ####
#------------------------------



## I calculate words per page of the survey:
WordCounts <- c(237, 80, 56, 202,
  30, 75, 39,39,
  44,101,
  84,167,171,
  38,38,38,38,
  125,54,
  191,56)


WordCounts_T <- data.frame(t(WordCounts)) %>% slice(rep(1:n(),each=nrow(AllTimers_Reading)))
WordCounts_DF <- data.frame(t(WordCounts)) %>% melt()
WordCounts_DF$variable <- data.frame("variable"=AllTimers_Reading %>% colnames())[,1]


colnames(WordCounts_T) = colnames(AllTimers_Reading)
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_Consent")] <-  "PAGE_Count_Consent"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_AgeGender")] <-  "PAGE_Count_AgeGender"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_PostEth")] <-  "PAGE_Count_PostEth"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_Info")] <-  "PAGE_Count_Info"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_MicroScaleNow")] <-  "PAGE_Count_MicroScaleNow"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_MicroScaleFuture")] <-  "PAGE_Count_MicroScaleFuture"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_MicroVarBest")] <-  "PAGE_Count_MicroVarBest"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_MicroVarWorst")] <-  "PAGE_Count_MicroVarWorst"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_CE_Detergent")] <-  "PAGE_Count_CE_Detergent"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_CE_Info")] <-  "PAGE_Count_CE_Info"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_CE_Example")] <-  "PAGE_Count_CE_Example"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_CE_Choice1")] <-  "PAGE_Count_CE_Choice1"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_CE_Choice2")] <-  "PAGE_Count_CE_Choice2"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_CE_Choice3")] <-  "PAGE_Count_CE_Choice3"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_CE_Choice4")] <-  "PAGE_Count_CE_Choice4"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_CVWaterBills")] <-  "PAGE_Count_CVWaterBills"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_CV_Water")] <-  "PAGE_Count_CV_Water"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_CV_Council")] <-  "PAGE_Count_CV_Council"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_Attitudes_1")] <-  "PAGE_Count_Attitudes_1"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_Attitudes_2")] <-  "PAGE_Count_Attitudes_2"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_Debrief_1")] <-  "PAGE_Count_Debrief_1"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_Debrief_2")] <-  "PAGE_Count_Debrief_2"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_End")] <-  "PAGE_Count_End"
colnames(WordCounts_T)[which(names(WordCounts_T)=="PAGE_READ_Screenout")] <-  "PAGE_Count_Screenout"

#------------------------------
# Expected Reading Time Data: ####
#------------------------------


## So expected word counts is page word count/average WPM (=250) * 60 to be in seconds
WPM <- 250
ExpectedTime <- (WordCounts/WPM)*60
ExpectedTime_T <- data.frame(t(ExpectedTime)) %>% slice(rep(1:n(),each=nrow(AllTimers_Reading)))
ExpectedTime_DF <- data.frame(t(ExpectedTime)) %>% melt()
ExpectedTime_DF$variable <- data.frame("variable"=AllTimers_Reading %>% colnames())[,1]


## Updating rownames:
colnames(ExpectedTime_T) = colnames(AllTimers_Reading)
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_Consent")] <-  "PAGE_Exp_Consent"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_AgeGender")] <-  "PAGE_Exp_AgeGender"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_PostEth")] <-  "PAGE_Exp_PostEth"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_Info")] <-  "PAGE_Exp_Info"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_MicroScaleNow")] <-  "PAGE_Exp_MicroScaleNow"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_MicroScaleFuture")] <-  "PAGE_Exp_MicroScaleFuture"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_MicroVarBest")] <-  "PAGE_Exp_MicroVarBest"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_MicroVarWorst")] <-  "PAGE_Exp_MicroVarWorst"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_CE_Detergent")] <-  "PAGE_Exp_CE_Detergent"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_CE_Info")] <-  "PAGE_Exp_CE_Info"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_CE_Example")] <-  "PAGE_Exp_CE_Example"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_CE_Choice1")] <-  "PAGE_Exp_CE_Choice1"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_CE_Choice2")] <-  "PAGE_Exp_CE_Choice2"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_CE_Choice3")] <-  "PAGE_Exp_CE_Choice3"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_CE_Choice4")] <-  "PAGE_Exp_CE_Choice4"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_CVWaterBills")] <-  "PAGE_Exp_CVWaterBills"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_CV_Water")] <-  "PAGE_Exp_CV_Water"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_CV_Council")] <-  "PAGE_Exp_CV_Council"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_Attitudes_1")] <-  "PAGE_Exp_Attitudes_1"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_Attitudes_2")] <-  "PAGE_Exp_Attitudes_2"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_Debrief_1")] <-  "PAGE_Exp_Debrief_1"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_Debrief_2")] <-  "PAGE_Exp_Debrief_2"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_End")] <-  "PAGE_Exp_End"
colnames(ExpectedTime_T)[which(names(ExpectedTime_T)=="PAGE_READ_Screenout")] <-  "PAGE_Exp_Screenout"


#------------------------------
# Plotting: ####
#------------------------------


## Plotting median completion length in seconds per page
AllTimers_Reading %>% 
  melt() %>% 
  ggplot(aes(x = variable, y = value, group =1)) + 
  stat_summary(fun = "median", geom = "smooth")


## Plot Actual and Expected Time
cbind(ExpectedTime_DF,
      "Mean"=data.frame("Mean"=AllTimers_Reading %>% 
                          colMeans())[,1]) %>% 
  ggplot(aes(x = variable, y = value, group =1)) + 
  stat_summary(aes(colour="Expected Time"),fun = "median", geom = "smooth")+
  stat_summary(aes(colour="Actual Time",x = variable, y = Mean, group =1),
               fun = "median", geom = "smooth")

TimerPlot_ActualVExp <- cbind(ExpectedTime_DF,
      "Mean"=data.frame("Mean"=AllTimers_Reading %>% 
                          colMeans())[,1]) %>% 
  ggplot(aes(x = variable, y = value, group =1)) + 
  stat_summary(aes(colour="Expected Time"),fun = "median", geom = "smooth")+
  stat_summary(aes(colour="Actual Time",x = variable, y = Mean, group =1),
               fun = "median", geom = "smooth")+
  scale_x_discrete(name = "Page Number",labels = seq(1,21,1) )+
  scale_y_continuous(name = "Time In Seconds",limits=c(0,60),breaks=seq(0,60,10))+    scale_colour_manual(name="Timing:", values=RColorBrewer::brewer.pal(9,"Blues")[c(5,8)])+
  coord_cartesian() +theme_bw()+
  theme(legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank())


## Export plot here:
ggsave(TimerPlot_ActualVExp,device = "jpeg",
       filename = "TimerPlot_ActualVExp_2022_07_27.jpeg",
       width=20,height=15,units = "cm",dpi=1000)

#------------------------------
# Correlation Tests: ####
#------------------------------


## Initialise vector
CorTestStats <- data.frame("Statistic"=rep(0,each=nrow(AllTimers_Reading)),
                           "P.Value"=rep(0,each=nrow(AllTimers_Reading)))


## Estimate correlation test and P.value per person
for (i in 1:nrow(AllTimers_Reading)) {

  CorTestStats[i,] <- data.frame(cor.test(ExpectedTime,
                                          as.numeric(AllTimers_Reading[i,]))[c("estimate","p.value")])
  
}
  
## Estimate R squared using squared correlation
CorTestStats$RSq <- CorTestStats$Statistic^2


## Combine all data together in one place 
AllData_TimesTests <- cbind(Data,
      AllTimers_Reading,
      WordCounts_T,ExpectedTime_T,
      CorTestStats)

## Example of tests we can run between significant and non-significant
wilcox.test(AllData_TimesTests$ModelWTP[AllData_TimesTests$P.Value<0.05],
            AllData_TimesTests$ModelWTP[AllData_TimesTests$P.Value>=0.05])$p.value
mded(AllData_TimesTests$ModelWTP[AllData_TimesTests$P.Value<0.05],
            AllData_TimesTests$ModelWTP[AllData_TimesTests$P.Value>=0.05])$stat


## This reproduces the table of R2 by frequency
Categories <- table(cut(AllData_TimesTests$RSq,breaks=c(0,0.2,0.4,0.6,0.8,1)))
OutputTable <- rbind(round(prop.table(Categories)*100,2),Categories)

## Export Data:
write.csv(AllData_TimesTests,"AllData_TimesTests_2022_07_27.csv")



colnames(AllData_TimesTests)[which(names(AllData_TimesTests)=="PAGE_READ_CV_Coalesced.1")] <-  "PAGE_Count_CV_Coalesced"
colnames(AllData_TimesTests)[which(names(AllData_TimesTests)=="PAGE_READ_CV_Coalesced.2")] <-  "PAGE_Exp_CV_Coalesced"



# End Of Script ------------------------