#### Microplastics: IOP Paper ####
## Function: In-text specification
## Author: PK
## Last change: 27/11/2024
# Changes:
# - using factor uncertainty instead
# - new code to output the combined table
# - new code to report median (SD) of EOP


# *****************************
# Section 0: Replication Information: ####
# Selected output of 'sessionInfo()'
# *****************************


# > session_info()
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
# package     * version date (UTC) lib source
# abind         1.4-5   2016-07-21 [1] CRAN (R 4.4.0)
# AER         * 1.2-12  2024-02-03 [1] CRAN (R 4.4.0)
# betareg     * 3.2-0   2024-07-07 [1] CRAN (R 4.4.1)
# biglm       * 0.9-3   2024-06-12 [1] CRAN (R 4.4.2)
# boot        * 1.3-30  2024-02-26 [1] CRAN (R 4.4.1)
# car         * 3.1-2   2023-03-30 [1] CRAN (R 4.4.0)
# carData     * 3.0-5   2022-01-06 [1] CRAN (R 4.4.0)
# cli           3.6.3   2024-06-21 [1] CRAN (R 4.4.1)
# colorspace    2.1-0   2023-01-23 [1] CRAN (R 4.4.0)
# data.table  * 1.15.4  2024-03-30 [1] CRAN (R 4.4.0)
# DBI         * 1.2.3   2024-06-02 [1] CRAN (R 4.4.0)
# DCchoice    * 0.2.0   2023-07-10 [1] CRAN (R 4.4.0)
# dplyr       * 1.1.4   2023-11-17 [1] CRAN (R 4.4.0)
# fansi         1.0.6   2023-12-08 [1] CRAN (R 4.4.0)
# flexmix       2.3-19  2023-03-16 [1] CRAN (R 4.4.0)
# forcats     * 1.0.0   2023-01-29 [1] CRAN (R 4.4.0)
# Formula       1.2-5   2023-02-24 [1] CRAN (R 4.4.0)
# generics      0.1.3   2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2     * 3.5.1   2024-04-23 [1] CRAN (R 4.4.0)
# ggtext      * 0.1.2   2022-09-16 [1] CRAN (R 4.4.0)
# glue          1.7.0   2024-01-09 [1] CRAN (R 4.4.0)
# gridtext      0.1.5   2022-09-16 [1] CRAN (R 4.4.0)
# gtable        0.3.5   2024-04-22 [1] CRAN (R 4.4.0)
# here        * 1.0.1   2020-12-13 [1] CRAN (R 4.4.0)
# hms           1.1.3   2023-03-21 [1] CRAN (R 4.4.0)
# Icens         1.72.0  2023-04-25 [1] Bioconductor
# interval      1.1-1.0 2023-08-24 [1] CRAN (R 4.4.0)
# janitor     * 2.2.0   2023-02-02 [1] CRAN (R 4.4.1)
# lattice       0.22-6  2024-03-20 [1] CRAN (R 4.4.1)
# lifecycle     1.0.4   2023-11-07 [1] CRAN (R 4.4.0)
# lmtest      * 0.9-40  2022-03-21 [1] CRAN (R 4.4.0)
# lubridate   * 1.9.3   2023-09-27 [1] CRAN (R 4.4.0)
# magrittr    * 2.0.3   2022-03-30 [1] CRAN (R 4.4.0)
# MASS        * 7.3-61  2024-06-13 [1] CRAN (R 4.4.1)
# Matrix      * 1.7-0   2024-04-26 [1] CRAN (R 4.4.1)
# MLEcens       0.1-7   2022-10-18 [1] CRAN (R 4.4.0)
# modeltools    0.2-23  2020-03-05 [1] CRAN (R 4.4.0)
# munsell       0.5.1   2024-04-01 [1] CRAN (R 4.4.0)
# nnet          7.3-19  2023-05-03 [1] CRAN (R 4.4.1)
# perm          1.0-0.4 2023-08-24 [1] CRAN (R 4.4.0)
# pillar        1.9.0   2023-03-22 [1] CRAN (R 4.4.0)
# pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.4.0)
# purrr       * 1.0.2   2023-08-10 [1] CRAN (R 4.4.0)
# R6            2.5.1   2021-08-19 [1] CRAN (R 4.4.0)
# Rcpp          1.0.12  2024-01-09 [1] CRAN (R 4.4.0)
# readr       * 2.1.5   2024-01-10 [1] CRAN (R 4.4.0)
# rlang         1.1.4   2024-06-04 [1] CRAN (R 4.4.0)
# rprojroot     2.0.4   2023-11-05 [1] CRAN (R 4.4.0)
# rstudioapi    0.16.0  2024-03-24 [1] CRAN (R 4.4.0)
# sandwich    * 3.1-0   2023-12-11 [1] CRAN (R 4.4.0)
# scales        1.3.0   2023-11-28 [1] CRAN (R 4.4.0)
# sessioninfo * 1.2.2   2021-12-06 [1] CRAN (R 4.4.2)
# snakecase     0.11.1  2023-08-27 [1] CRAN (R 4.4.1)
# snow        * 0.4-4   2021-10-27 [1] CRAN (R 4.4.0)
# speedglm    * 0.3-5   2023-05-06 [1] CRAN (R 4.4.2)
# stringi       1.8.4   2024-05-06 [1] CRAN (R 4.4.0)
# stringr     * 1.5.1   2023-11-14 [1] CRAN (R 4.4.0)
# survival    * 3.7-0   2024-06-05 [1] CRAN (R 4.4.1)
# tibble      * 3.2.1   2023-03-20 [1] CRAN (R 4.4.0)
# tidyr       * 1.3.1   2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect    1.2.1   2024-03-11 [1] CRAN (R 4.4.0)
# tidyverse   * 2.0.0   2023-02-22 [1] CRAN (R 4.4.0)
# timechange    0.3.0   2024-01-18 [1] CRAN (R 4.4.0)
# tzdb          0.4.0   2023-05-12 [1] CRAN (R 4.4.0)
# utf8          1.2.4   2023-10-22 [1] CRAN (R 4.4.0)
# vctrs         0.6.5   2023-12-01 [1] CRAN (R 4.4.0)
# withr         3.0.0   2024-01-16 [1] CRAN (R 4.4.0)
# xml2          1.3.6   2023-12-04 [1] CRAN (R 4.4.0)
# zoo         * 1.8-12  2023-04-13 [1] CRAN (R 4.4.0)
# 
# [1] C:/Users/earpkin/AppData/Local/Programs/R/R-4.4.1/library

# rm(list=ls())

# rm(list=ls())

## Useful for all scripts:
library(data.table)
library(magrittr)
library(dplyr)
library(tidyverse)
library(here)
library(DCchoice)
library(janitor)


## Key for this script:
library(betareg)
library(boot)
library(AER)
library(snow)
library(speedglm)
library(ggtext)


# ***********************************************************
# Section 1: Import Data ####
# ***********************************************************


# This version fixes some coding errors
# Data <-
#   here("Data",
#        "Data_WithEOP_24_12_01.csv") %>%
#   fread() %>%
#   data.frame()


Data <-
  here("Data",
       "Microplastics_AllData_Wide_Anonymised_WithEOP_UpdatedA.csv") %>%
  fread() %>%
  data.frame()

# ***********************************************************
# Section 2: Create  additional variables ####
# ***********************************************************


# ## Rescale to check
# Data$Income[Data$Income == 5000] <- 7500


## Income and income weighted bid
Data$Income_Annual <- Data$Income %>% multiply_by(12)
Data$LogBidIncome <-
  log((Data$Income_Annual - Data$Bid) / (Data$Income_Annual))


## Transform Mean expected future
Data$MEF <- (Data$MeanExpectedFuture + 5.001) / 10.002
summary(Data$MEF)


## Transform mean expected current
Data$MEC <-
  (Data$MeanExpectedCurrent + 5.001) / 10.002
summary(Data$MEC)


## Half differences between variance bounds
Data$Uncertainty <-
  ((Data$VarianceLowerBound - Data$VarianceUpperBound) / 2)

## Transform to Cameron (2005) measure
Data$var.cameron <- (0.5 * Data$Uncertainty) ^ 2


## Negative correlation so Uncertainty up means CV down
##
# cor.test(Data$var.cameron, Data$CV)



Data$PaymentVehicle_Dummy <- ifelse(Data$WaterBills == 0, 0, 1)



# ****************************************
# Misc variable transformations


Data$Education_HigherEd <- ifelse(Data$Education == 5,
                                  1, ## 1 = higher education
                                  0) ## 0 = all other

## Drop "other" due to small sample
Data_Trim <- Data[Data$Gender < 2, ]
Data$Gender_Female <- ifelse(Data$Gender == 0,
                             "Male",
                             "Female")

Data$Gender_Dummy <- ifelse(Data$Gender_Female == "Female",
                            0,
                            1)


# ********************************************
# Section 3: New simulator ####
# ********************************************




Simulator <- function(data, 
                      R = R) {
  
  boot.function <- function(data, indices) {
    d <- data[indices, ]
    
    
    # Define your formula for stage_1 and stage_2 models
    formula_stage_1 <- as.formula(
      MEF ~
        -1 + ## Changing to no intercept here
        AgeDummy + 
        EthnicityDummy +
        Gender_Dummy  + 
        Charity +
        Education_HigherEd +
        Q16_ClimateCurrentEnvironment +
        Q16_ClimateCurrentSelf +
        Q16_MicroplasticsCurrentEnvironment + 
        Q16_MicroplasticsCurrentSelf +
        Q16_MicroplasticsTen + 
        Q16_MicroplasticsTwentyFive + 
        Q16_MicroplasticsFifty |
        1 +  # intercept here
        as.factor(Uncertainty)
    )
    
    
    stage_1 <- betareg(formula_stage_1, d, type = "BC")
   
    Means <- c(I((predict(stage_1, type = "response") )))
    Variances <- c(I((betareg::predict(stage_1, type = "variance"))))
    ## Define Y == gross monthly income * 12
    Y <- d$Income_Annual
   
    return(c(Means = Means, Variances = Variances, Y = Y))

  }
  boot.results <- boot(data = data,
                       statistic = boot.function,
                       R = R,
                       parallel = "snow")
  
  l <- length(boot.results$t0) / 3
  means <- boot.results$t[, 1:l] %>% colMeans()
  variances <- boot.results$t[, (l + 1):(2 * l)] %>% colMeans()
  Y <- boot.results$t[, (2 * l + 1):(3 * l)] %>% colMeans()
 
  return(list(
    "Means" = means,
    "Variances" = variances,
    "Y" = Y
  ))
}



# ********************************************
# Section 4: Model output ####
# ********************************************




## Define here just once
R <- 1000
# R <- 100


Data_Filtered <- Data %>% dplyr::select(c(
  "CV",
  "MEC",
  "MEF", 
  "AgeDummy",
  "EthnicityDummy",
  "Gender_Dummy",  
  "Charity",
  "Education_HigherEd",
  "Q16_ClimateCurrentEnvironment",
  "Q16_ClimateCurrentSelf",
  "Q16_MicroplasticsCurrentEnvironment", 
  "Q16_MicroplasticsCurrentSelf",
  "Q16_MicroplasticsTen", 
  "Q16_MicroplasticsTwentyFive", 
  "Q16_MicroplasticsFifty", 
  "Uncertainty",
  "LogBidIncome",
  "Income_Annual"
))


# Call the simulator function
Model1_simulation <- Simulator(data = Data_Filtered,
                               R = R
)  

Data$Predicted_Means <- Model1_simulation$Means
Data$Predicted_Variances <- Model1_simulation$Variances
Data$Predicted_Y <- Model1_simulation$Y


# Data %>% 
#   data.frame() %>% 
#   fwrite(sep = ",",
#          here("Data", "FigureZ_PlotData_1302.csv"))


## Assign here
PlotData <- Data

# Data_1 <-
#   here("Data", "FigureZ_PlotData_1212.csv") %>% 
#   fread() %>% 
#   data.frame()
# Data_2 <-
#   here("Data", "Data_WithEOP_24_12_01.csv") %>% 
#   fread() %>% 
#   data.frame()
# 
# Data_2$Predicted_Means <- Data_1$Predicted_Means
# Data_2$Predicted_Variances <- Data_1$Predicted_Variances
# Data_2$Predicted_Y <- Data_1$Predicted_Y

# *****************************
# Section 5: Plot Setup ####
# *****************************


# # OLD custom labeller function
# custom_labeller <- function(variable, value) {
#   if (variable == "Variance") {
#     labels <- c(
#       "0" = "**A**: Highly certain: +/- zero points (N = 151)",
#       "1" = "**B**: Mostly certain: +/- one point (N = 995)",
#       "3" = "**C**: Mostly uncertain: (+/- three points (N = 307)",
#       "5" = "**D**: Highly uncertain: (+/- five points (N = 111)"
#     )
#     return(labels[value])
#   }
# }


## updated from above to follow labeller deprecation issue
custom_labeller <- as_labeller(
  c(
    "0" = "**A**: Highly certain: +/- zero points (N = 151)",
    "1" = "**B**: Mostly certain: +/- one point (N = 995)",
    "3" = "**C**: Mostly uncertain: (+/- three points (N = 307)",
    "5" = "**D**: Highly uncertain: (+/- five points (N = 111)"
  )
)


## Set here for ease
TextSetup <- element_text(
  size = 14,
  colour = "black",
  family = "serif"
)

# ********************************************
# Section 6: Figure B4 ####
# *******************************************



Fig_Test_1 <- 
  PlotData[, c("Predicted_Means",
           "Predicted_Variances",
           "Predicted_Y",
           "Uncertainty")] %>%
  arrange(Uncertainty) %>%
  mutate(Variance = factor(Uncertainty, levels = unique(Uncertainty))) %>% 
  ggplot(aes(x = Predicted_Means %>% as.numeric(),
             y = Predicted_Variances %>% as.numeric(),
             group = Uncertainty %>% as.factor()),
         colour = Variance,
         fill = Variance) +
  
  stat_smooth(aes(colour = Uncertainty %>% as.factor()),
              linewidth = 1.25) + 
  geom_point(aes(colour = Uncertainty %>% as.factor()),
             alpha = 0.25, size = 0.75) +
  theme_bw() +
  facet_wrap(~ Variance,
             labeller = custom_labeller) +
  
    # Axes and Labels
  ylab("Predicted variance from the first stage") +
  
  scale_x_continuous(
    name = "Predicted mean expected harmfulness from the first stage") +
  # scale_x_continuous(name =
  #                      "Predicted mean expected harmfulness from the first stage",
  #                    breaks = seq.int(
  #                      from = 0.74,
  #                      to = 0.77,
  #                      by = ((0.77 - 0.74) / 5)
  #                    )) +
  # scale_y_continuous(breaks = seq.int(
  #   from = -0.0329,
  #   to = -0.03034932,
  #   by = ((-0.03034932--0.0329) / 5) %>% abs()
  # ) %>% round(4)) +
  
    # scale_color_brewer(palette = "Reds") +

  # Colours and fills for Income Quintile
  scale_colour_manual(
    name = "Variance",
    # Legend title for clarity
    values = c("black", "blue", "#008080", "skyblue"),
    # Custom colours
    labels = c(
      "Highly certain: +/- zero points (N = 151)",
      "Mostly certain: +/- one point (N = 995)",
      "Mostly uncertain: +/- three points (N = 307)",
      "Highly uncertain: +/- five points (N = 111)"
    )
  ) +
  scale_fill_manual(
    name = "Variance",
    # Legend title for clarity
    values = c("black", "blue", "#008080", "skyblue"),
    # Custom colours
    labels = c(
      "Highly certain: +/- zero points (N = 151)",
      "Mostly certain: +/- one point (N = 995)",
      "Mostly uncertain: +/- three points (N = 307)",
      "Highly uncertain: +/- five points (N = 111)"
    )
  ) +
  
  guides(
    colour = guide_legend(title = "Variance", nrow = 2),
    fill = guide_legend(title = "Variance", nrow = 2)
  ) +
  
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = ggtext::element_markdown(
      size = TextSetup$size,
      colour = TextSetup$colour,
      family = TextSetup$family
    ),
    legend.position = "bottom",
    legend.text = TextSetup,
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = TextSetup,
    axis.title.x = TextSetup,
    axis.text.y = TextSetup,
    axis.title.y = TextSetup,
    legend.title = TextSetup
  ) 


Fig_Test_1




## Export and save in the right location
ggsave(
  Fig_Test_1,
  device = "png",
  filename = here("CVOutput", 
                  "Microplastics_FigureB4_MeanVariance_Nointercept.png"),
  width = 25,
  height = 15,
  units = "cm",
  dpi = 500
)




# ********************************************
# Section 6B: Plot not grouped ####
# *******************************************



# Fig_Test_2 <- 
#   PlotData[, c("Predicted_Means",
#            "Predicted_Variances",
#            "Predicted_Y",
#            "Uncertainty")] %>%
#   arrange(Uncertainty) %>%
#   mutate(Variance = factor(Uncertainty, levels = unique(Uncertainty))) %>%
#   ggplot(aes(
#     y = Predicted_Variances,
#     x = Predicted_Means    )) +
#   
#   # Smooth curves with alpha for SE shading
#   stat_smooth(alpha = 0.25,
#               linewidth = 1.25) +
#   
#   theme_bw() +
#   
#   # Axes and Labels
#   ylab("Predicted variance from the first stage") +
#   
#   
#   scale_x_continuous(name =
#                        "Predicted mean expected harmfulness from the first stage") +
#   
#   theme(
#     strip.background = element_rect(fill = "white"),
#     strip.text = ggtext::element_markdown(
#       size = TextSetup$size,
#       colour = TextSetup$colour,
#       family = TextSetup$family
#     ),
#     legend.position = "bottom",
#     legend.text = TextSetup,
#     legend.background = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.text.x = TextSetup,
#     axis.title.x = TextSetup,
#     axis.text.y = TextSetup,
#     axis.title.y = TextSetup,
#     legend.title = TextSetup
#   ) 
# 
# 
# 
# 
# ## Export and save in the right location
# ggsave(
#   Fig_Test_2,
#   device = "png",
#   filename = here("CVOutput", "Microplastics_FigureZ_PredictedMeanVariance_2_Ungrouped.png"),
#   width = 25,
#   height = 15,
#   units = "cm",
#   dpi = 500
# )






# ********************************************
# Section 6C: Plot kinda grouped ####
# *******************************************



# Fig_Test_3 <- 
#   PlotData[, c("Predicted_Means",
#            "Predicted_Variances",
#            "Predicted_Y",
#            "Uncertainty")] %>%
#   arrange(Uncertainty) %>%
#   mutate(Variance = factor(Uncertainty, levels = unique(Uncertainty))) %>%
#   ggplot(aes(
#     y = Predicted_Variances,
#     x = Predicted_Means,
#     colour = Variance,
#     fill = Variance
#   )) +
#   
#   # Smooth curves with alpha for SE shading
#   stat_smooth(aes(fill = Variance,
#                   colour = Variance),
#               alpha = 0.25,
#               linewidth = 1.25) +
#   
#   theme_bw() +
#   
#   
#   # Axes and Labels
#   ylab("Predicted variance from the first stage") +
#   
#   scale_x_continuous(
#     name = "Predicted mean expected harmfulness from the first stage") +
#   # scale_x_continuous(name =
#   #                      "Predicted mean expected harmfulness from the first stage",
#   #                    breaks = seq.int(
#   #                      from = 0.74,
#   #                      to = 0.77,
#   #                      by = ((0.77 - 0.74) / 5)
#   #                    )) +
#   # scale_y_continuous(breaks = seq.int(
#   #   from = -0.0329,
#   #   to = -0.03034932,
#   #   by = ((-0.03034932--0.0329) / 5) %>% abs()
#   # ) %>% round(4)) +
#   
#   # Colours and fills for Income Quintile
#   scale_colour_manual(
#     name = "Variance",
#     # Legend title for clarity
#     values = c("black", "blue", "#008080", "skyblue"),
#     # Custom colours
#     labels = c(
#       "Highly certain: +/- zero points (N = 151)",
#       "Mostly certain: +/- one point (N = 995)",
#       "Mostly uncertain: +/- three points (N = 307)",
#       "Highly uncertain: +/- five points (N = 111)"
#     )
#   ) +
#   scale_fill_manual(
#     name = "Variance",
#     # Legend title for clarity
#     values = c("black", "blue", "#008080", "skyblue"),
#     # Custom colours
#     labels = c(
#       "Highly certain: +/- zero points (N = 151)",
#       "Mostly certain: +/- one point (N = 995)",
#       "Mostly uncertain: +/- three points (N = 307)",
#       "Highly uncertain: +/- five points (N = 111)"
#     )
#   ) +
#   
#   guides(
#     colour = guide_legend(title = "Variance", nrow = 2),
#     fill = guide_legend(title = "Variance", nrow = 2)
#   ) +
#   
#   theme(
#     strip.background = element_rect(fill = "white"),
#     strip.text = ggtext::element_markdown(
#       size = TextSetup$size,
#       colour = TextSetup$colour,
#       family = TextSetup$family
#     ),
#     legend.position = "bottom",
#     legend.text = TextSetup,
#     legend.background = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.text.x = TextSetup,
#     axis.title.x = TextSetup,
#     axis.text.y = TextSetup,
#     axis.title.y = TextSetup,
#     legend.title = TextSetup
#   ) 
# 
# 
# 
# ## Export and save in the right location
# ggsave(
#   Fig_Test_3,
#   device = "png",
#   filename = here("CVOutput", 
#                   "Microplastics_FigureZ_PredictedMeanVariance_3_Group.png"),
#   width = 25,
#   height = 15,
#   units = "cm",
#   dpi = 500
# )



