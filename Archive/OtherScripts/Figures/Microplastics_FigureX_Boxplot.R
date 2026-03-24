#### Microplastics: IOP Paper ####
## Function: Boxplot WTP by mean/variance
## Author: Dr Peter King (p.king1@Leeds.ac.uk)
## Last change: 25/08/2023


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


# *****************************
# Section 1: Import Data ####
# *****************************



## Start with the latest anonymised data in one-row per one-respondent format
Data <-
  here("Data",
       "Data_newWTP.csv") %>%
  fread() %>%
  data.frame()



# *****************************
# Section 2: Data Setup ####
# *****************************


## Isolate variables we want only
PlotData <- Data[, c("MeanExpectedFuture", "Variance", "ModelWTP")]

## Force to order we want
PlotData <- PlotData %>% arrange(Variance) 

PlotData$Variance <- factor(PlotData$Variance,
                            levels = unique(PlotData$Variance))

## Set here for ease
TextSize <- 14

# *****************************
# Section 3: Boxplots ####
# *****************************


FigureX_Boxplot <- 
  PlotData %>% ggplot(aes(
    x = ModelWTP,
    y = MeanExpectedFuture,
    fill = as.factor(Variance)
  )) +
  
  stat_boxplot(geom = "errorbar",
               width = 0.1) +
  
  geom_boxplot(outlier.shape = NA) +
  
  theme_bw() +
  
  xlab("Marginal WTP (GBP)\nin local council tax,\nper household, per annum") +
  
  guides(fill = 
           guide_legend(title = "How confident are you in this prediction?",
                        title.position = "top"))+
  
  scale_fill_manual(
    name = "",
    values = RColorBrewer::brewer.pal(9, "Blues")[c(9, 7, 4, 1)],
    label = c(
      "Exactly\n(N = 151)",
      "Plus or minus\none point on the scale\n(N = 995)",
      "Plus or minus\nthree point on the scale\n(N = 307)",
      "Not at all\n(N = 111)"
    )) +
  
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
   
  scale_y_continuous(name = "Mean expected harm",
                     breaks = seq(-5, 5, 1),
                     limits = c(-5, 5)) + 
  
  scale_x_continuous(breaks = seq(20, 200, 20),
                     limits = c(20, 200)) +
  
  theme(
    legend.position = "bottom",
    legend.text = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    ),
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    ),
    axis.title.x = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    ),
    ## Change text to be clearer for reader
    axis.text.y = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    ),
    axis.title.y = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    ),
    legend.title = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    )
  ) +
  coord_flip()


# *****************************
# Section x: Estimate data ####
# *****************************


## Export and save in the right location
ggsave(
  FigureX_Boxplot,
  device = "png",
  filename = here("CVOutput", "FigureX_Boxplot.png"),
  width = 25,
  height = 15,
  units = "cm",
  dpi = 500
)

## END OF SCRIPT ##########



FigureX_Boxplot_Inverted <- 
  PlotData %>% ggplot(aes(
    y = ModelWTP,
    x = MeanExpectedFuture,
    fill = as.factor(MeanExpectedFuture)
  )) +
  
  stat_boxplot(geom = "errorbar",
               width = 0.1) +
  
  geom_boxplot(outlier.shape = NA) +
  
  theme_bw() +
  
  ylab("Marginal WTP (GBP)\nin local council tax,\nper household, per annum") +
  
  guides(fill = 
           guide_legend(title = "How confident are you in this prediction?",
                        title.position = "top"))+
  
  scale_fill_brewer(
    name = "") + 
  
  geom_vline(xintercept = 0, linetype='dotted', col = 'red') +
  
  scale_x_continuous(name = "Mean expected harm",
                     breaks = seq(-5, 5, 1),
                     limits = c(-5, 5)) + 
  
  scale_y_continuous(breaks = seq(20, 200, 20),
                     limits = c(20, 200)) +
  
  theme(
    legend.position = "bottom",
    legend.text = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    ),
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    ),
    axis.title.x = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    ),
    ## Change text to be clearer for reader
    axis.text.y = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    ),
    axis.title.y = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    ),
    legend.title = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    )
  ) 
