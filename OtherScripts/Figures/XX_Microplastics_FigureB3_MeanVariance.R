#### Microplastics: IOP Paper ####
## Function: Plot Figure B3
## Author: Dr Peter King (p.king1@Leeds.ac.uk)
## Last change: 13/02/2025
## Changes:
### - changing to histintervals

# Figure B3. 
# Histograms show the distribution of mean expectations by level of stated uncertainty. 
# Black points indicate the median of each distribution. 
# Vertical dashed line at zero indicates that a respondent expects future harm to 
# be the same as today. 
# Note that the scale for mean expectations was scored [-5, +5], 
# so the values here are the mean expectations about the current harmfulness 
# plus the future harmfulness i.e., how much will things change from now, 
# this is why the presented scale is technically [-10, 10].


# *****************************
# Replication Information: ####
# Selected output of 'sessionInfo()'
# *****************************


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
# date     2025-02-13
# rstudio  2023.06.2+561 Mountain Hydrangea (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────
# package        * version date (UTC) lib source
# abind            1.4-5   2016-07-21 [1] CRAN (R 4.4.0)
# AER            * 1.2-12  2024-02-03 [1] CRAN (R 4.4.0)
# betareg        * 3.2-0   2024-07-07 [1] CRAN (R 4.4.1)
# biglm          * 0.9-3   2024-06-12 [1] CRAN (R 4.4.2)
# boot           * 1.3-30  2024-02-26 [1] CRAN (R 4.4.1)
# car            * 3.1-2   2023-03-30 [1] CRAN (R 4.4.0)
# carData        * 3.0-5   2022-01-06 [1] CRAN (R 4.4.0)
# cli              3.6.3   2024-06-21 [1] CRAN (R 4.4.1)
# colorspace       2.1-0   2023-01-23 [1] CRAN (R 4.4.0)
# commonmark       1.9.1   2024-01-30 [1] CRAN (R 4.4.0)
# data.table     * 1.15.4  2024-03-30 [1] CRAN (R 4.4.0)
# DBI            * 1.2.3   2024-06-02 [1] CRAN (R 4.4.0)
# DCchoice       * 0.2.0   2023-07-10 [1] CRAN (R 4.4.0)
# distributional   0.4.0   2024-02-07 [1] CRAN (R 4.4.0)
# dplyr          * 1.1.4   2023-11-17 [1] CRAN (R 4.4.0)
# fansi            1.0.6   2023-12-08 [1] CRAN (R 4.4.0)
# farver           2.1.2   2024-05-13 [1] CRAN (R 4.4.0)
# flexmix          2.3-19  2023-03-16 [1] CRAN (R 4.4.0)
# forcats        * 1.0.0   2023-01-29 [1] CRAN (R 4.4.0)
# Formula          1.2-5   2023-02-24 [1] CRAN (R 4.4.0)
# generics         0.1.3   2022-07-05 [1] CRAN (R 4.4.0)
# ggdist         * 3.3.2   2024-03-05 [1] CRAN (R 4.4.0)
# ggplot2        * 3.5.1   2024-04-23 [1] CRAN (R 4.4.0)
# ggtext         * 0.1.2   2022-09-16 [1] CRAN (R 4.4.0)
# glue             1.7.0   2024-01-09 [1] CRAN (R 4.4.0)
# gridtext         0.1.5   2022-09-16 [1] CRAN (R 4.4.0)
# gtable           0.3.5   2024-04-22 [1] CRAN (R 4.4.0)
# here           * 1.0.1   2020-12-13 [1] CRAN (R 4.4.0)
# hms              1.1.3   2023-03-21 [1] CRAN (R 4.4.0)
# Icens            1.72.0  2023-04-25 [1] Bioconductor
# interval         1.1-1.0 2023-08-24 [1] CRAN (R 4.4.0)
# janitor        * 2.2.0   2023-02-02 [1] CRAN (R 4.4.1)
# labeling         0.4.3   2023-08-29 [1] CRAN (R 4.4.0)
# lattice          0.22-6  2024-03-20 [1] CRAN (R 4.4.1)
# lifecycle        1.0.4   2023-11-07 [1] CRAN (R 4.4.0)
# lmtest         * 0.9-40  2022-03-21 [1] CRAN (R 4.4.0)
# lubridate      * 1.9.3   2023-09-27 [1] CRAN (R 4.4.0)
# magrittr       * 2.0.3   2022-03-30 [1] CRAN (R 4.4.0)
# markdown         1.13    2024-06-04 [1] CRAN (R 4.4.0)
# MASS           * 7.3-61  2024-06-13 [1] CRAN (R 4.4.1)
# Matrix         * 1.7-0   2024-04-26 [1] CRAN (R 4.4.1)
# mgcv             1.9-1   2023-12-21 [1] CRAN (R 4.4.1)
# MLEcens          0.1-7   2022-10-18 [1] CRAN (R 4.4.0)
# modeltools       0.2-23  2020-03-05 [1] CRAN (R 4.4.0)
# munsell          0.5.1   2024-04-01 [1] CRAN (R 4.4.0)
# nlme             3.1-165 2024-06-06 [1] CRAN (R 4.4.1)
# nnet             7.3-19  2023-05-03 [1] CRAN (R 4.4.1)
# perm             1.0-0.4 2023-08-24 [1] CRAN (R 4.4.0)
# pillar           1.9.0   2023-03-22 [1] CRAN (R 4.4.0)
# pkgconfig        2.0.3   2019-09-22 [1] CRAN (R 4.4.0)
# purrr          * 1.0.2   2023-08-10 [1] CRAN (R 4.4.0)
# R6               2.5.1   2021-08-19 [1] CRAN (R 4.4.0)
# ragg             1.3.2   2024-05-15 [1] CRAN (R 4.4.0)
# RColorBrewer     1.1-3   2022-04-03 [1] CRAN (R 4.4.0)
# Rcpp             1.0.12  2024-01-09 [1] CRAN (R 4.4.0)
# readr          * 2.1.5   2024-01-10 [1] CRAN (R 4.4.0)
# rlang            1.1.4   2024-06-04 [1] CRAN (R 4.4.0)
# rprojroot        2.0.4   2023-11-05 [1] CRAN (R 4.4.0)
# rstudioapi       0.16.0  2024-03-24 [1] CRAN (R 4.4.0)
# sandwich       * 3.1-0   2023-12-11 [1] CRAN (R 4.4.0)
# scales           1.3.0   2023-11-28 [1] CRAN (R 4.4.0)
# sessioninfo    * 1.2.2   2021-12-06 [1] CRAN (R 4.4.2)
# snakecase        0.11.1  2023-08-27 [1] CRAN (R 4.4.1)
# snow           * 0.4-4   2021-10-27 [1] CRAN (R 4.4.0)
# speedglm       * 0.3-5   2023-05-06 [1] CRAN (R 4.4.2)
# stringi          1.8.4   2024-05-06 [1] CRAN (R 4.4.0)
# stringr        * 1.5.1   2023-11-14 [1] CRAN (R 4.4.0)
# survival       * 3.7-0   2024-06-05 [1] CRAN (R 4.4.1)
# systemfonts      1.1.0   2024-05-15 [1] CRAN (R 4.4.0)
# textshaping      0.4.0   2024-05-24 [1] CRAN (R 4.4.0)
# tibble         * 3.2.1   2023-03-20 [1] CRAN (R 4.4.0)
# tidyr          * 1.3.1   2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect       1.2.1   2024-03-11 [1] CRAN (R 4.4.0)
# tidyverse      * 2.0.0   2023-02-22 [1] CRAN (R 4.4.0)
# timechange       0.3.0   2024-01-18 [1] CRAN (R 4.4.0)
# tzdb             0.4.0   2023-05-12 [1] CRAN (R 4.4.0)
# utf8             1.2.4   2023-10-22 [1] CRAN (R 4.4.0)
# vctrs            0.6.5   2023-12-01 [1] CRAN (R 4.4.0)
# withr            3.0.0   2024-01-16 [1] CRAN (R 4.4.0)
# xfun             0.45    2024-06-16 [1] CRAN (R 4.4.1)
# xml2             1.3.6   2023-12-04 [1] CRAN (R 4.4.0)
# zoo            * 1.8-12  2023-04-13 [1] CRAN (R 4.4.0)
# 
# [1] C:/Users/earpkin/AppData/Local/Programs/R/R-4.4.1/library


# renv::snapshot()
rm(list = ls())

library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(here)
library(DCchoice)
library(ggdist)

# *****************************
# Section 1: Import Data ####
# *****************************


## Start with the latest anonymised data in one-row per one-respondent format
Data <-
  here("Data",
       "Data_WithEOP_24_11_25.csv") %>%
  fread() %>%
  data.frame()


# *****************************
# Section 2: Data Setup ####
# *****************************


Data$Mean_Change <- Data$MeanExpectedCurrent + Data$MeanExpectedFuture
# Data$Variance_ConfidenceAsSD <- ifelse(Data$Variance == 1, 0, 
#                                        ifelse(Data$Variance == 2, 2, 
#                                               ifelse(Data$Variance == 3, 6, 10))) %>% 
#   divide_by(4)
# 
# 
# Data$Variance_ConfidenceAsVariance <- Data$Variance_ConfidenceAsSD %>% raise_to_power(2)


## This is convoluted but it adds nice formatting for x axis
Labels_X <- c(
  "-10\n(Better)",
  seq.int(from = -9, to = -1, by = 1),
  "0\n(No change)",
  paste0("+", seq.int(
    from = 1, to = 9, by = 1
  )),
  "+10\n(Worse)"
)


Labels_X_2 <- c(
  "-10\n(Better)",
  -5,
  0,
  paste0("+5"),
  "+10\n(Worse)"
)




## Set here for ease
TextSetup <- element_text(
  size = 14,
  colour = "black",
  family = "serif"
)


Quartile_breaks <- quantile(
  Data$Income_Annual,
  probs = seq(0, 1, by = 1/4),
  na.rm = TRUE
)

Data$Income_Quartile <-
  cut(
    Data$Income_Annual,
    breaks = Quartile_breaks,
    labels = 1:4,
    include.lowest = TRUE
  ) %>%
  as.numeric()


# *****************************
# Section 3: Construct plot ####
# *****************************

FigureB3 <- 
  Data %>%
  
  mutate(Mean_Bins = cut(Mean_Change, breaks = seq(-10, 10, by = 1))) %>%
  
  ggplot(aes(
    x = Mean_Bins,
    y = Variance %>% as.factor(),
    group = Variance %>% as.factor(),
    fill = Variance %>% as.factor()
  )) +
  
  ggdist::stat_histinterval(outline_bars = TRUE,
                            slab_colour = "black",
                            slab_linewidth = 0.45) +
  theme_bw() +
  
  geom_vline(xintercept = 7, ## actually plots at 0 on the scale 
             linetype = 'dotted', 
             col = 'black') +
  
  scale_x_discrete(name = "Mean expectations about the future harmfulness",
                   labels = seq(-6, 10, by = 1) ) +
  
  scale_y_discrete(name = "Uncertainty",
                   breaks = seq(1, 4, 1), 
                   labels = 
                     c(
                       "Highly certain\n(+/- zero points)\nN = 151",
                       "Mostly certain\n(+/- one point)\nN = 995",
                       "Mostly uncertain\n(+/- three points)\nN = 307",
                       "Highly uncertain\n(+/- five points)\nN = 111"
                     )) +
  
  scale_colour_manual(name = "") +
  
  scale_fill_manual(name = "", 
                    values = RColorBrewer::brewer.pal(n = 9, name = "Blues")[c(3, 5, 7, 9)]) +
  
  guides(colour = "none", 
         fill = "none") +
  
  theme(
    strip.background = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    ## Change text to be clearer for reader
    legend.text = TextSetup,
    axis.text.x = TextSetup,
    axis.title.x = TextSetup,
    axis.text.y = TextSetup,
    axis.title.y = TextSetup,
    legend.title = TextSetup
  ) 



# *****************************
# Section x: Export plot ####
# *****************************


## Export and save in the right location
ggsave(
  FigureB3,
  device = "png",
  filename = here("CVOutput", "FigureB3_MeanByVariance.png"),
  width = 25,
  height = 15,
  units = "cm",
  dpi = 500
)

