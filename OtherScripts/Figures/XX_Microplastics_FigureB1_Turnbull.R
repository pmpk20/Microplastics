#### Microplastics: IOP Paper ####
## Function: Turnbull estimators of CV ~ Bid | Uncertainty
## Author: PK
## Last change: 19/01/25
# Comment:
## - Outputs figure B1


# *****************************
# Replication Information: ####
# Selected output of 'sessionInfo()'
# *****************************
# R version 4.3.0 (2023-04-21)
# Platform: x86_64-apple-darwin20 (64-bit)
# Running under: macOS Ventura 13.5
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: Europe/Helsinki
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] AER_1.2-10     survival_3.5-7 sandwich_3.0-2 lmtest_0.9-40  zoo_1.8-12     car_3.1-2      carData_3.0-5 
# [8] boot_1.3-28.1  betareg_3.1-4 
# 
# loaded via a namespace (and not attached):
#   [1] gridExtra_2.3        inline_0.3.19        rlang_1.1.2          magrittr_2.0.3       multcomp_1.4-25     
# [6] matrixStats_1.1.0    compiler_4.3.0       flexmix_2.3-19       loo_2.6.0            callr_3.7.3         
# [11] vctrs_0.6.4          reshape2_1.4.4       stringr_1.5.1        pkgconfig_2.0.3      crayon_1.5.2        
# [16] fastmap_1.1.1        backports_1.4.1      ellipsis_0.3.2       utf8_1.2.4           threejs_0.3.3       
# [21] cmdstanr_0.6.1.9000  promises_1.2.1       markdown_1.11        ps_1.7.5             xfun_0.41           
# [26] modeltools_0.2-23    jsonlite_1.8.7       later_1.3.1          parallel_4.3.0       prettyunits_1.2.0   
# [31] R6_2.5.1             dygraphs_1.1.1.6     stringi_1.8.2        StanHeaders_2.26.28  estimability_1.4.1  
# [36] Rcpp_1.0.11          rstan_2.32.3         knitr_1.45           base64enc_0.1-3      bayesplot_1.10.0    
# [41] httpuv_1.6.12        Matrix_1.6-3         splines_4.3.0        nnet_7.3-19          igraph_1.5.1        
# [46] tidyselect_1.2.0     rstudioapi_0.15.0    abind_1.4-5          codetools_0.2-19     miniUI_0.1.1.1      
# [51] curl_5.1.0           processx_3.8.2       pkgbuild_1.4.2       lattice_0.22-5       tibble_3.2.1        
# [56] plyr_1.8.9           shiny_1.8.0          bridgesampling_1.1-2 posterior_1.5.0      coda_0.19-4         
# [61] RcppParallel_5.1.7   xts_0.13.1           pillar_1.9.0         tensorA_0.36.2       checkmate_2.3.0     
# [66] DT_0.30              stats4_4.3.0         shinyjs_2.1.0        distributional_0.3.2 generics_0.1.3      
# [71] ggplot2_3.4.4        rstantools_2.3.1.1   munsell_0.5.0        scales_1.2.1         gtools_3.9.5        
# [76] xtable_1.8-4         glue_1.6.2           emmeans_1.8.9        tools_4.3.0          shinystan_2.6.0     
# [81] colourpicker_1.3.0   mvtnorm_1.2-3        grid_4.3.0           QuickJSR_1.0.7       crosstalk_1.2.1     
# [86] colorspace_2.1-0     nlme_3.1-163         Formula_1.2-5        cli_3.6.1            fansi_1.0.5         
# [91] Brobdingnag_1.2-9    dplyr_1.1.4          V8_4.4.0             gtable_0.3.4         digest_0.6.33       
# [96] TH.data_1.1-2        brms_2.20.4          htmlwidgets_1.6.3    farver_2.1.1         htmltools_0.5.7     
# [101] lifecycle_1.0.4      mime_0.12            shinythemes_1.2.0    MASS_7.3-60   

rm(list=ls())

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
library(scales)
library(survminer)


# ***********************************************************
# Section 1: Import Data ####
# ***********************************************************


Data <- here("Data",
             "Microplastics_AllData_Wide_Anonymised.csv") %>% 
  fread() %>% 
  data.frame()



# ***********************************************************
# Section 2: Create  additional variables ####
# ***********************************************************


## Split Data into groups by uncertainty
Data_Uncertainty_0 <- Data[Data$Uncertainty == 0, ]
Data_Uncertainty_1 <- Data[Data$Uncertainty == 1, ]
Data_Uncertainty_3 <- Data[Data$Uncertainty == 3, ]
Data_Uncertainty_5 <- Data[Data$Uncertainty == 5, ]

# ***********************************************************
# Section 3: Single-Bound Turnbull By Group ####
# ***********************************************************


## Simple response ~ bid formula here
Formula_SB <- CV ~ Bid


## store results
TB_Uncertainty_0 <- turnbull.sb(Formula_SB, data = Data_Uncertainty_0)
TB_Uncertainty_1 <- turnbull.sb(Formula_SB, data = Data_Uncertainty_1)
TB_Uncertainty_3 <- turnbull.sb(Formula_SB, data = Data_Uncertainty_3)
TB_Uncertainty_5 <- turnbull.sb(Formula_SB, data = Data_Uncertainty_5)


## Produce summary object
TB_Uncertainty_0_Summary <- TB_Uncertainty_0 %>% summary()
TB_Uncertainty_1_Summary <- TB_Uncertainty_1 %>% summary()
TB_Uncertainty_3_Summary <- TB_Uncertainty_3 %>% summary()
TB_Uncertainty_5_Summary <- TB_Uncertainty_5 %>% summary()


## Save output from each model
## then reshape dataframe to make plotting easier
PlotData <- cbind(
  "Bid" = TB_Uncertainty_0_Summary$estimates[, 1],
  "Uncertainty_0" = TB_Uncertainty_0_Summary$estimates[, 2],
  "Uncertainty_1" = TB_Uncertainty_1_Summary$estimates[, 2],
  "Uncertainty_3" = TB_Uncertainty_3_Summary$estimates[, 2],
  "Uncertainty_5" = TB_Uncertainty_5_Summary$estimates[, 2]
) %>% data.frame() %>% pivot_longer(cols = 2:5)


# ***********************************************************
# Section 4A: Setup the plot variables ####
# ***********************************************************


## Calculate, format, and store all medians here
Medians <- c(
  paste0("(", 
         TB_Uncertainty_0_Summary$medianWTP[1] %>% round(2) %>% sprintf("£%.2f", .), 
         ") - (", 
         TB_Uncertainty_0_Summary$medianWTP[2] %>% round(2) %>% sprintf("%.2f", .), 
         ")"),
  paste0("(", 
         TB_Uncertainty_1_Summary$medianWTP[1] %>% round(2) %>% sprintf("£%.2f", .), 
         ") - (", 
         TB_Uncertainty_1_Summary$medianWTP[2] %>% round(2) %>% sprintf("%.2f", .), 
         ")"),
  paste0("(", 
         TB_Uncertainty_3_Summary$medianWTP[1] %>% round(2) %>% sprintf("£%.2f", .), 
         ") - (", 
         TB_Uncertainty_3_Summary$medianWTP[2] %>% round(2) %>% sprintf("£%.2f", .), 
         ")"),
  paste0("(", 
         TB_Uncertainty_5_Summary$medianWTP[1] %>% round(2) %>% sprintf("£%.2f", .), 
         ") - (", 
         TB_Uncertainty_5_Summary$medianWTP[2] %>% round(2) %>% sprintf("£%.2f", .), 
         ")")
)

## All means here
Means <- c(
  TB_Uncertainty_0_Summary$meanWTP %>% round(2) %>% sprintf("£%.2f", .),
  TB_Uncertainty_1_Summary$meanWTP %>% round(2) %>% sprintf("£%.2f", .),
  TB_Uncertainty_3_Summary$meanWTP %>% round(2) %>% sprintf("£%.2f", .),
  TB_Uncertainty_5_Summary$meanWTP %>% round(2) %>% sprintf("£%.2f", .)
)


## stitch labels and values together
LegendLabels <- c(
  paste0(
    "Highly certain\n(+/- 0 points)",
    "\nMean: ",
    Means[1],
    "\nMedian: ",
    Medians[1] %>% str_extract("£\\d+")
  ),
  paste0(
    "Mostly certain\n(+/- 1 point)",
    "\nMean: ",
    Means[2],
    "\nMedian: ",
    Medians[2] %>% str_extract("£\\d+")
  ),
  paste0(
    "Mostly uncertain\n(+/- 3 points)",
    "\nMean: ",
    Means[3],
    "\nMedian: ",
    Medians[3] %>% str_extract("£\\d+")
  ),
  paste0(
    "Highly uncertain\n(+/- 5 points)",
    "\nMean: ",
    Means[4],
    "\nMedian: ",
    Medians[4] %>% str_extract("£\\d+")
  )
)



## Plot colours defined here
Plot_Colours <- c(RColorBrewer::brewer.pal(name = "Blues", 
                                           n = 9)[c(2)])

## Set all text sizes in one place
TextSize <- 14


TextSetup <- element_text(
  size = TextSize,
  colour = "black",
  family = "serif"
)



Bids <- c("£0",
          "£1",
          "£5",
          "£10",
          "£20",
          "£30",
          "£40",
          "£50",
          "£60",
          "£70",
          "£80",
          "Inf")

# ***********************************************************
# Section 4A: Plot geom_point and line####
# ***********************************************************




## Using ggplot where we + different graph elements together
## Using ggplot where we + different graph elements together
Plot_Turnbull <- PlotData %>% 
  ggplot(aes(y = value, 
             x = Bid, 
             colour = name)) +
  
  geom_point(aes(fill = name), size = 3) +
  geom_line(aes(colour = name), linewidth = 1.5) +
  
  scale_fill_manual(guide = "none",
                    values = Plot_Colours %>% rep(each = 4)) +  
  theme_bw()  +
  
  guides(colour = guide_legend(title = "How confident are you in this prediction?",
                               title.position = "top",
                               override.aes =
                                 list(fill = Plot_Colours))) +
  
  ylab("Likelihood of voting `yes` to CV question ") +
  
  scale_colour_manual(
    name = "",
    values = c("skyblue", "purple", "darkblue","black") %>% rev(),
    label = LegendLabels) +
  
  geom_hline(yintercept = 0.5,
             linetype = 'dotted',
             col = 'black') +
  
  scale_x_continuous(name = "Bid level in additional GBP per HH per year", 
                     labels = c("£0", "£1", "£5", "£10", "£20", "£30", "£40", "£50", "£60", "£70", "£80"),
                     breaks = seq.int(from = 0, to = 10, by = 1)) +
  
  ## So here I'm printing all bid levels
  ## but jittering 0, 1 to be slightly further apart for clarity
  scale_x_continuous(name = "Bid level in additional GBP per HH per year", 
                     labels = c(0, 10, 20, 30, 40, 50, 60, 70, 80) %>% round(2) %>% sprintf("£%.2f", .),
                      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
  
  scale_y_continuous(breaks = seq.int(from = 0, to = 1, by = 0.1),
                     limits = c(0, 1)) +
  
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.text = TextSetup,
    axis.text.x = TextSetup,
    axis.title.x = TextSetup,
    axis.text.y = TextSetup,
    axis.title.y = TextSetup,
    legend.title = TextSetup
  ) 


# ***********************************************************
# Section 4B: Export ####
# ***********************************************************


## Export and save in the right location
ggsave(
  Plot_Turnbull,
  device = "png",
  filename = here("CVOutput", "Figure_Plot_Turnbull.png"),
  width = 25,
  height = 15,
  units = "cm",
  dpi = 500
)

# End of script ***********************************************************