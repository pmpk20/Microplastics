#### Microplastics IOP: Plots Figure B2  ###############
# Function: To visualise the distribution of responses to the attitudinal Qs
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
library(ggdist)


# ***************************************************
# Section 1: Import Data ####
# ***************************************************




## Start with the latest anonymised data in one-row per one-respondent format
Data <-
  here("Data",
       "Microplastics_AllData_Wide_Anonymised.csv") %>%
  fread() %>%
  data.frame()


# ***************************************************
# Section 2: Prepare plot data ####
# ***************************************************



PlotData <- Data[,
                 c(
                   "Q16_ClimateCurrentEnvironment",
                   "Q16_ClimateCurrentSelf",
                   "Q16_MicroplasticsCurrentEnvironment",
                   "Q16_MicroplasticsCurrentSelf",
                   "Q16_MicroplasticsTen",
                   "Q16_MicroplasticsTwentyFive",
                   "Q16_MicroplasticsFifty"
                 )] %>% 
  magrittr::subtract(6) %>% ## -6 to center correctly 
  pivot_longer(cols = 1:7,
               names_to = "variable") 



## Correct plotting order
PlotData$variable <-
  factor(PlotData$variable, levels = unique(PlotData$variable))


# ***************************************************
# Section 3: Prepare plotting ####
# ***************************************************


## Easier to define all plot text sizes here
TextSize <- 14

TextSetup <- element_text(size = TextSize,
                          colour = "black",
                          family = "serif")


ColourSet <- c(RColorBrewer::brewer.pal(n = 9,
                                        name = "Blues")[c(2, 3, 4, 6, 7, 8, 9)])

## Define y axis labels here
## Add mean (SD) data here too for illustration
## Using rev() to set the correct order
Labels_Y <-
  c(
    paste0("Climate\nCurrent effect on the environment\n", 
           Data$Q16_ClimateCurrentEnvironment %>% mean() %>% round(2) %>% subtract(6), 
           " (", 
           Data$Q16_ClimateCurrentEnvironment %>% sd() %>% round(2), ")"),
    
    paste0("Climate\nCurrent effect on self\n", 
           Data$Q16_ClimateCurrentSelf %>% mean() %>% round(2) %>% subtract(6), 
           " (", 
           Data$Q16_ClimateCurrentSelf %>% sd() %>% round(2), ")"),
    
    paste0("Microplastics\nCurrent effect on the environment\n", 
           Data$Q16_MicroplasticsCurrentEnvironment %>% mean() %>% round(2) %>% subtract(6), 
           " (", 
           Data$Q16_MicroplasticsCurrentEnvironment %>% sd() %>% round(2), ")"),
    
    paste0("Microplastics\nCurrent effect on self\n", 
           Data$Q16_MicroplasticsCurrentSelf %>% mean() %>% round(2) %>% subtract(6), 
           " (", 
           Data$Q16_MicroplasticsCurrentSelf %>% sd() %>% round(2), ")"),
    
    paste0("Microplastics\nEffect on humanity in ten years\n", 
           Data$Q16_MicroplasticsTen %>% mean() %>% round(2) %>% subtract(6), 
           " (", 
           Data$Q16_MicroplasticsTen %>% sd() %>% round(2), ")"),
    
    paste0("Microplastics\nEffect on humanity in twenty five years\n", 
           Data$Q16_MicroplasticsTwentyFive %>% mean() %>% round(2) %>% subtract(6), 
           " (", 
           Data$Q16_MicroplasticsTwentyFive %>% sd() %>% round(2), ")"),
    
    paste0("Microplastics\nEffect on humanity in fifty years\n", 
           Data$Q16_MicroplasticsFifty %>% mean() %>% round(2) %>% subtract(6), 
           " (", 
           Data$Q16_MicroplasticsFifty %>% sd() %>% round(2), ")")
    
  ) %>% rev()


Labels_Y_New <-
  c(
    "Climate\nCurrent effect on the environment",
    "Climate\nCurrent effect on self",
    "Microplastics\nCurrent effect on the environment",
    "Microplastics\nCurrent effect on self",
    "Microplastics\nEffect on humanity in ten years",
    "Microplastics\nEffect on humanity in twenty five years",
    "Microplastics\nEffect on humanity in fifty years") %>% rev()




## This is convoluted but it adds nice formatting for x axis
Labels_X <- c(
  "-5\n(No threat)",
  seq.int(from = -4, to = -1, by = 1),
  "0\n",
  paste0("+", seq.int(
    from = 1, to = 4, by = 1
  )),
  "+5\n(Significant\nthreat)"
)


# ***************************************************
# Section 4: Plot ####
# ***************************************************


FigureB2_Attitudes <- 
  
  PlotData %>%  
  
  ggplot(aes(
    x = value,
    y = variable %>% rev(),
    group = variable %>% rev(),
    fill = variable %>% rev()
  )) +
  
  ggdist::stat_histinterval(breaks = breaks_fixed(weights = 1),
                            position = "dodge",
                            point_interval = "mean_qi",
                            outline_bars = TRUE,
                            slab_colour = "black", 
                            slab_linewidth = 0.5
  ) + 
  
  geom_vline(xintercept = 0, linetype = 'dashed') +
  
  scale_x_continuous(
    name = "Perceived threat",
    limits = c(-5, 5),
    breaks = seq(-5, 5, 1),
    labels = Labels_X  
  ) +
  
  scale_y_discrete(name = "Question 16: Attitudes", 
                   label = Labels_Y_New) +
  
  coord_cartesian() +
  
  theme_bw() +
  
  scale_fill_manual(name = "",
                    values = ColourSet)+
  theme(
    legend.position = "none",
    legend.text = TextSetup,
    axis.text.x = TextSetup,
    axis.title.x  = TextSetup,
    axis.text.y  = TextSetup,
    axis.title.y = TextSetup,
    legend.title = TextSetup,
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  )




# ***************************************************
# Section 5: Export plot ####
# ***************************************************



## Export and save in the right location
ggsave(
  FigureB2_Attitudes,
  device = "png",
  filename = here("CVOutput", "FigureB2_Attitudes.png"),
  width = 25,
  height = 15,
  units = "cm",
  dpi = 500
)

## END OF SCRIPT ##########