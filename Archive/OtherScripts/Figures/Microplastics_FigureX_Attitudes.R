#### Microplastics: IOP Paper ####
## Function: Plot and describe all Attitudes
## Author: Dr Peter King (p.king1@Leeds.ac.uk)
## Last change: 29/08/2023


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


# install.packages("DCchoice",
#                  repos = c("http://www.bioconductor.org/packages/release/bioc",
#                            "https://cran.rstudio.com/"),
#                  dep = TRUE)

# 
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("Icens")
# 

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


# *****************************
# Section 1: Import Data ####
# *****************************



## Start with the latest anonymised data in one-row per one-respondent format
Data <-
  here("Data",
       "Microplastics_AllData_Long_Anonymised_2022_06_19.csv") %>%
  fread() %>%
  data.frame()


# ***************************************************
# Section 2: Defining variables ####
# ***************************************************



# ***************************************************
# Section 3: Plot data here ####
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
                 )] %>% magrittr::subtract(6) %>% pivot_longer(cols = 1:7,
                                                               names_to = "variable") 


# ***************************************************
# Section 4: Add details for plotting here ####
# ***************************************************


## Easier to define all plot text sizes here
TextSize <- 10


## Correct plotting order
PlotData$variable <-
  factor(PlotData$variable, levels = unique(PlotData$variable))


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


## Can add this line if plotting changes too
# paste0(
#   "Expected \nchange\n",
#   Data$Change %>% mean() %>% round(2),
#   " (",
#   Data$Change %>% sd() %>% round(2),
#   ")"
# ), 



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
# Section 4: Plotting data ####
# ***************************************************


FigureX_Attitudes <- 
  PlotData %>%  ggplot(aes(
    x = value,
    y = variable %>% rev(),
    group = variable %>% rev(),
    fill = variable %>% rev()
  )) +
  ggdist::stat_histinterval(breaks = breaks_fixed(weights = 1),
                            position = "dodge",
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
                   label = Labels_Y) +
  coord_cartesian() +
  theme_bw() +
  scale_fill_manual(name = "", 
                    values = c(RColorBrewer::brewer.pal(n = 9, 
                                                        name = "Blues")[c(2, 3, 4, 6, 7, 8, 9)])) +
  theme(
    legend.position = "none",
    legend.text = element_text(
      size = TextSize,
      colour = "black",
      family = "serif"
    ),
    
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
    ),
    
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  )





# *****************************
# Section x: Estimate data ####
# *****************************


## Export and save in the right location
ggsave(
  FigureX_Attitudes,
  device = "png",
  filename = here("CVOutput", "FigureX_Attitudes.png"),
  width = 25,
  height = 15,
  units = "cm",
  dpi = 500
)

## END OF SCRIPT ##########