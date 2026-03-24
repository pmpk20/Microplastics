#### Microplastics: IOP Paper ####
## Function: Smooth WTP by mean/variance
## Author: Dr Peter King (p.king1@Leeds.ac.uk)
## Last change: 02/12/2024
## Changes:
### - changing to new EOP figures
### - Figure_X_2 is the one in-text


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


# renv::snapshot()
rm(list = ls())

library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(here)
library(DCchoice)
library(ggtext)  # Make sure to load this package


# *****************************
# Section 1: Import Data ####
# *****************************



## Start with the latest anonymised data in one-row per one-respondent format
# Data <-
#   here("Data",
#        "Data_newWTP.csv") %>%
#   fread() %>%
#   data.frame()

# Data <- here("CVoutput", "Data_WithEOP_23_11_20.csv") %>% fread() %>% data.frame()


## Start with the latest anonymised data in one-row per one-respondent format
Data <-
  here("Data",
       "Data_WithEOP_24_12_01.csv") %>%
  fread() %>%
  data.frame()


## Start with the latest anonymised data in one-row per one-respondent format
Data <-
  here("Data",
       "Microplastics_AllData_Wide_Anonymised_WithEOP.csv") %>%
  fread() %>%
  data.frame()


# *****************************
# Section 2: Data Setup ####
# *****************************


Data$Mean_Change <- Data$MeanExpectedCurrent + Data$MeanExpectedFuture
Data$Variance_ConfidenceAsSD <- ifelse(Data$Variance == 1, 0, 
                                       ifelse(Data$Variance == 2, 2, 
                                              ifelse(Data$Variance == 3, 6, 10))) %>% 
  divide_by(4)


Data$Variance_ConfidenceAsVariance <- Data$Variance_ConfidenceAsSD %>% raise_to_power(2)



## Isolate variables we want only
PlotData <- Data[, c("MeanExpectedFuture", 
                     "Uncertainty",
                     "EOP")]

## Force to order we want
PlotData <- PlotData %>% arrange(Uncertainty) 

PlotData$Variance <- factor(PlotData$Uncertainty,
                            levels = unique(PlotData$Uncertainty))




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


# ## Isolate variables we want only
# PlotData <- Data[, c("Mean_Change", 
#                      "Variance_ConfidenceAsVariance",
#                      "EOP")]
# 
# ## Force to order we want
# PlotData <- PlotData %>% arrange(Variance_ConfidenceAsVariance) 
# 
# PlotData$Variance <- factor(PlotData$Variance_ConfidenceAsVariance,
#                             levels = unique(PlotData$Variance_ConfidenceAsVariance))


# 
# FigureX_Smooth <-
#   PlotData %>%
#   ggplot(aes(y = EOP, 
#              x = Mean_Change, 
#              colour = Variance)) +
#   
#   stat_smooth(aes(fill = Variance), 
#               alpha = 0.35, 
#               linewidth = 1.5) +
#   
#   scale_colour_manual(
#     name = "",
#     values = c("black", "navy", "blue","skyblue") %>% rev(),
#     label = c(
#       "Exactly\n(N = 151)",
#       "Plus or minus\none point on the scale\n(N = 995)",
#       "Plus or minus\nthree point on the scale\n(N = 307)",
#       "Not at all\n(N = 111)"
#     )) +
#   
#   scale_fill_manual(
#     name = "",
#     values = c("black", "navy", "blue","skyblue") %>% rev(),
#     label = c(
#       "Exactly\n(N = 151)",
#       "Plus or minus\none point on the scale\n(N = 995)",
#       "Plus or minus\nthree point on the scale\n(N = 307)",
#       "Not at all\n(N = 111)"
#     )) +
#   
#   theme_bw() +
#   
#   facet_wrap( ~ Variance, labeller = as_labeller(c(
#     "0" = "Exactly\n(N = 151)",
#     "0.25" = "Plus or minus one point on the scale\n(N = 995)",
#     "2.25" = "Plus or minus three point on the scale\n(N = 307)",
#     "6.25" = "Not at all\n(N = 111)"
#   )), scales = "free_y") +
#   
#   ylab("Expected option prices") +
#   
#   geom_vline(xintercept = 0,
#              linetype = 'dotted',
#              col = 'red') +
#   
#   geom_hline(yintercept = 0,
#              linetype = 'dotted',
#              col = 'red') +
#   
#   scale_x_continuous(name = "Mean expected harm",
#                      breaks = seq(-10, 10, 1),
#                      limits = c(-10, 10),
#                      labels = Labels_X) + 
#   
#   scale_y_continuous(breaks = seq(-200, 700, 100)) +
#   
#   
#   theme(
#     strip.background = element_rect(fill = "white"),
#     legend.position = "bottom",
#     legend.text = TextSetup,
#     legend.background = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.text.x = TextSetup,
#     axis.title.x = TextSetup,
#     ## Change text to be clearer for reader
#     axis.text.y = TextSetup,
#     axis.title.y = TextSetup,
#     legend.title = TextSetup
#   ) 


# FigureX_Smooth <-
#   Data[, c("Mean_Change", 
#            "Uncertainty",
#            "EOP")] %>% 
#   arrange(Uncertainty) %>% 
#   mutate(Variance = factor(PlotData$Uncertainty,
#                            levels = unique(PlotData$Uncertainty))) %>% 
#   ggplot(aes(y = EOP, 
#              x = Mean_Change, 
#              colour = Variance)) +
#   
#   stat_smooth(aes(fill = Variance), 
#               linewidth = 1.5) +
#   # geom_point(aes(fill = Variance), 
#   #            size = 0.5, alpha = 0.75) +
#   
#   theme_bw() +
#   
#   facet_grid( ~ Variance, labeller = as_labeller(c(
#     "0" = "Highly certain\n(+/- zero points)\nN = 151",
#     "1" = "Mostly certain\n(+/- one point)\nN = 995",
#     "3" = "Mostly uncertain\n(+/- three points)\nN = 307",
#     "5" = "Highly uncertain\n(+/- five points)\nN = 111"
#   ))) +
#   
#   ylab("Expected option prices") +
#   
#   geom_vline(xintercept = 0,
#              linetype = 'dotted',
#              col = 'red') +
#   
#   geom_hline(yintercept = 0,
#              linetype = 'dotted',
#              col = 'red') +
#   
#   scale_x_continuous(name = "Mean expected harm\n[Better (-10), Worse (+10)]") + 
#   
#   scale_y_continuous(breaks = seq(-300, 800, 100)) +
#   
#   scale_colour_manual(
#     name = "",
#     values = c("black", "navy", "blue","skyblue") %>% rev(),
#     label = c(
#       "Highly certain\n(+/- zero points)\nN = 151",
#       "Mostly certain\n(+/- one point)\nN = 995",
#       "Mostly uncertain\n(+/- three points)\nN = 307",
#       "Highly uncertain\n(+/- five points)\nN = 111"
#     )) +
#   
#   scale_fill_manual(
#     name = "",
#     values = c("black", "navy", "blue","skyblue") %>% rev(),
#     label = c(
#       "Highly certain\n(+/- zero points)\nN = 151",
#       "Mostly certain\n(+/- one point)\nN = 995",
#       "Mostly uncertain\n(+/- three points)\nN = 307",
#       "Highly uncertain\n(+/- five points)\nN = 111"
#     )) +
#   guides(fill = "none",
#          colour = "none")+
#   theme(
#     strip.background = element_rect(fill = "white"),
#     legend.position = "bottom",
#     legend.text = TextSetup,
#     legend.background = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.text.x = TextSetup,
#     axis.title.x = TextSetup,
#     ## Change text to be clearer for reader
#     axis.text.y = TextSetup,
#     axis.title.y = TextSetup,
#     legend.title = TextSetup
#   ) +
#   coord_cartesian(ylim = c(-100, 600))

# *****************************
# Section x: Export plot ####
# *****************************

# 
# ## Export and save in the right location
# ggsave(
#   FigureX_Smooth,
#   device = "png",
#   filename = here("CVOutput", "FigureX_Smooth_EOP.png"),
#   width = 25,
#   height = 15,
#   units = "cm",
#   dpi = 500
# )

## END OF SCRIPT ##########







# *****************************
# Section 2: Data Setup ####
# *****************************


# Create a custom labeller function
custom_labeller <- function(variable, value) {
  if (variable == "Variance") {
    labels <- c(
      "0" = "**A**: Highly certain: +/- zero points (N = 151)",
      "1" = "**B**: Mostly certain: +/- one point (N = 995)",
      "3" = "**C**: Mostly uncertain: (+/- three points (N = 307)",
      "5" = "**D**: Highly uncertain: (+/- five points (N = 111)"
    )
    return(labels[value])
  }
}



Figure_X_2 <- Data[, c("Mean_Change", 
                       "Uncertainty",
                       "EOP",
                       "Income_Quartile")] %>%
  arrange(Uncertainty) %>% 
  mutate(Variance = factor(Uncertainty, levels = unique(Uncertainty)),
         Income_Quartile = as.factor(Income_Quartile)) %>% 
  ggplot(aes(y = EOP, 
             x = Mean_Change, 
             colour = Income_Quartile, 
             fill = Income_Quartile)) +  
  
  # Smooth curves with alpha for SE shading
  stat_smooth(aes(fill = Income_Quartile, colour = Income_Quartile), alpha = 0.25,  linewidth = 1.25) +
  
  theme_bw() +
  
  facet_wrap( ~ Variance,
              nrow = 2,
              ncol = 2,
              labeller = custom_labeller
  ) + 
  
  # Axes and Labels
  ylab("Expected option prices") +
  
  # Add red reference lines
  geom_vline(xintercept = 0, linetype = 'dotted', col = 'red') +
  geom_hline(yintercept = 0, linetype = 'dotted', col = 'red') +
  
  scale_x_continuous(name = "Mean expected harmfulness\n[Better (-10), Worse (+10)]", limits = c(-10, 10)) + 
  scale_y_continuous(breaks = seq(-500, 1500, 250)) +
  
  # Colours and fills for Income Quintile
  scale_colour_manual(
    name = "Income Quartile",  # Legend title for clarity
    values = c("black", "blue", "#008080", "skyblue"),  # Custom colours
    labels = c(
      "Q1 (Lowest Income)",
      "Q2",
      "Q3",
      "Q4 (Highest Income)"
    )) +
  scale_fill_manual(
    name = "Income Quartile",  # Ensure legend aligns with colour scale
    values = c("black", "blue", "#008080",  "skyblue"),
    labels = c(
      "Q1 (Lowest Income)",
      "Q2",
      "Q3",
      "Q4 (Highest Income)"
    )) +
  
  guides(colour = guide_legend(title = "Income Quartile")) +
  
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_markdown(size = TextSetup$size, 
                                  colour = TextSetup$colour, 
                                  family = TextSetup$family),
    legend.position = "bottom",
    legend.text = TextSetup,
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = TextSetup,
    axis.title.x = TextSetup,
    ## Change text to be clearer for reader
    axis.text.y = TextSetup,
    axis.title.y = TextSetup,
    legend.title = TextSetup
  ) +
  coord_cartesian(ylim = c(-500, 1000))


## Export and save in the right location
ggsave(
  Figure_X_2,
  device = "png",
  filename = here("CVOutput", "FigureX_Smooth_Income_EOP.png"),
  width = 25,
  height = 15,
  units = "cm",
  dpi = 500
)

# 
# 
# 
# 
# Mean_breaks <- quantile(
#   Data$Mean_Change,
#   probs = seq(0, 1, by = 1/4),
#   na.rm = TRUE
# )
# 
# Data$Mean_Change_Quartile <-
#   cut(
#     Data$Mean_Change,
#     breaks = Mean_breaks,
#     labels = 1:4,
#     include.lowest = TRUE
#   ) %>%
#   as.numeric()
# 
# Data$Uncertainty_Recode <- dplyr::recode(Data$Uncertainty,
#               '0' = 0,
#               '1' = 1,
#               '3' = 2,
#               '5' = 3)
# 
# Data[, c("Mean_Change_Quartile", 
#          "Uncertainty_Recode",
#          "EOP")] %>%
#   ggplot(aes(y = EOP, 
#              x = Uncertainty_Recode)) +  
#   
#   stat_smooth(aes(fill = Uncertainty_Recode), 
#               linewidth = 1.5) +
#   geom_point(aes(fill = Uncertainty_Recode), 
#              size = 0.5, alpha = 0.75) +
#   theme_bw() +
#   
#   # Faceting by Variance
#   facet_grid( ~ Mean_Change_Quartile)
# 
# 
# 
# 
# VarianceUpperBound_breaks <- quantile(
#   Data$VarianceUpperBound,
#   probs = seq(0, 1, by = 1/4),
#   na.rm = TRUE
# )
# 
# Data$VarianceUpperBound_Quartile <-
#   cut(
#     Data$VarianceUpperBound,
#     breaks = VarianceUpperBound_breaks,
#     labels = 1:4,
#     include.lowest = TRUE
#   ) %>%
#   as.numeric()
# 
# 
# 
# 
# VarianceLowerBound_breaks <- quantile(
#   Data$VarianceLowerBound,
#   probs = seq(0, 1, by = 1/4),
#   na.rm = TRUE
# )
# 
# Data$VarianceLowerBound_Quartile <-
#   cut(
#     Data$VarianceLowerBound,
#     breaks = VarianceLowerBound_breaks,
#     labels = 1:4,
#     include.lowest = TRUE
#   ) %>%
#   as.numeric()
# 
