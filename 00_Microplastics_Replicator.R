#### Microplastics IOP: Full run through of how to replicate the paper  ###############
# Function: To list all files in one go
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Last Edited: 04/03/2025


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

# **********************************************************************************
#### Section One: Prepare Data For Analysis ####
# **********************************************************************************


## Don't need to run these every time
here("SetupScripts", "01_Microplastics_SetupData_Order0.R") %>% source()
here("SetupScripts", "02_Microplastics_SetupData_Order1.R") %>% source()
here("SetupScripts", "03_Microplastics_MergeSamples.R") %>% source()





# **********************************************************************************
#### Section Twp: Modelling ####
# **********************************************************************************


## Simulate first then estimate all kinds of specifications
here("CVScripts", "04_Microplastics_Models_SimulateEOP.R") %>% source()

## Find table2 and table3 here 
here("CVScripts", "05_Microplastics_Models_BothStages_InText.R") %>% source()

## These currently have the wrong suffixes!
here("CVScripts", "06_Microplastics_Models_Alternative1_Asymmetric.R") %>% source()
here("CVScripts", "07_Microplastics_Models_Alternative2_NumericUncertainty.R") %>% source()


here("CVScripts", "08_Microplastics_Models_RobustnessStage1.R") %>% source()

## Removing this as challenges with conceptual validity
# here("CVScripts", "09_Microplastics_Models_RobustnessStage2.R") %>% source()
here("CVScripts", "10_Microplastics_Models_Truncation.R") %>% source()


## If simulating EOP at mean income, mean mean, and mean variance
here("CVScripts", "XX_Microplastics_EOPAtMeans_Bootstrap.R") %>% source()


## Was tableC1 but now labelled Table4
here("OtherScripts/Tables", "16_Microplastics_Table4_SimulatedEOP.R") %>% source()


# **********************************************************************************
#### Section Three: Tables/Figures ####
# **********************************************************************************


## Pre-estimation results
here("OtherScripts/Tables", "11_Microplastics_Table1.R") %>% source()
here("OtherScripts/Tables", "12_Microplastics_TableB1_Sample.R") %>% source()
here("OtherScripts/Tables", "13_Microplastics_TableB3.R") %>% source()
here("OtherScripts/Figures", "15_Microplastics_FigureB1_Turnbull.R") %>% source()
here("OtherScripts/Figures", "22_Microplastics_FigureB2_Attitudes.R") %>% 


## To execute 14_ you need to have run 04_ first
## Note that Fig2 may no longer be in the MS
here("OtherScripts/Figures", "14_Microplastics_Figure2_EOP.R") %>% source()


## Just need this to plot now, no separate file
here("OtherScripts/Figures", "17_Microplastics_FigureB4_MeanVariance_PlotSetup.R") %>% source()


## Tables combing model results for convenience
here("OtherScripts/Tables", "18_Microplastics_Table5_AlternativeSpecifications.R") %>% source()
here("OtherScripts/Tables", "19_Microplastics_TableC2_TruncationModels.R") %>% source()
here("OtherScripts/Tables", "20_Microplastics_TableC3_RobustnessStage1Models.R") %>% source()
here("OtherScripts/Tables", "21_TableC4_RobustnessStage2Models.R") %>% source()



# End Of Script ##############################################################