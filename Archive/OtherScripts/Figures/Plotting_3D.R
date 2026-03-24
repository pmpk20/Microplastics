#### Microplastics: IOP Paper ####
## Function: Just estimate complex model WTP
## Author: Dr Peter King (p.king1@Leeds.ac.uk)
## Last change: 24/08/2023


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

# install.packages("scatterplot3d") # Install
library("scatterplot3d")
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
library(plotly)
library(plot3D)

# *****************************
# Section 1: Import Data ####
# *****************************



## Start with the latest anonymised data in one-row per one-respondent format
# Data <-
#   here("Data",
#        "Microplastics_AllData_Long_Anonymised_2022_06_19.csv") %>%
#   fread() %>%
#   data.frame()

Data <-
  here("Data",
       "Data_WithEOP_24_11_05.csv") %>%
  fread() %>%
  data.frame()



# *****************************
# Section 2: Attempt plotting ####
# https://plotly.com/r/3d-surface-plots/
# *****************************

# scatterplot3d(Data$ModelWTP ~ Data$MeanExpectedCurrent + Data$Variance)


PlotData <- Data[, c("MEF", "Variance", "EOP")] 


plot_ly(PlotData, x = ~Variance, y = ~MEF, z = ~EOP) %>%
  plotly::add_mesh()


plot_ly(z = ~ PlotData %>% as.matrix()) %>% add_surface()

PlotData_Matrix <- xtabs(EOP ~ Variance + MEF, data = PlotData)
plot_ly(z = ~PlotData_Matrix) %>% add_surface()

# *****************************
# Section 3: Boxplots ####
# *****************************


ggplot(PlotData, aes(
  y = ModelWTP,
  x = MeanExpectedCurrent,
  fill = as.factor(Variance)
)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(outlier.shape = NA) +
  theme_bw()


FigureX_Boxplot <- ggplot(PlotData, aes(
  x = ModelWTP,
  y = MeanExpectedCurrent,
  fill = as.factor(Variance)
)) +
  stat_boxplot(geom = "errorbar",
               width = 0.1) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  xlab("Marginal WTP (GBP)\nin local council tax,\nper household, per annum") +
  
  scale_fill_manual(
    name = "Variance",
    values = RColorBrewer::brewer.pal(9, "Blues")[c(1, 3, 6, 9)],
    
    label = c(
      "1",
      "2",
      "3",
      "3"),
    
    guide = guide_legend(reverse = FALSE)) +
  # scale_fill_brewer(
  #   name = "Variance",
  #   type = "seq"
  # ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10,
                               colour = "black",
                               family = "serif"),
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 10,
                               colour = "black",
                               family = "serif"), ## Change text to be clearer for reader
    axis.text.y = element_text(size = 10,
                               colour = "black",
                               family = "serif")
  ) +
  coord_flip()


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

# *****************************
# Section x: Estimate data ####
# *****************************


Simz <- 10
Length <- (Data$MeanExpectedCurrent %>% unique() %>% length())
MeanLoop = vector("list", length = (Data$MeanExpectedCurrent %>% unique() %>% length()))
MeanData = vector("list", length = (Data$MeanExpectedCurrent %>% unique() %>% length()))
MeanLabeller <- MeanLoop

VarianceLoop = vector("list", length = (Data$Variance %>% unique() %>% length()))
VarianceLabeller <- VarianceLoop


for (k in 1:(Data$MeanExpectedCurrent %>% unique() %>% length())){
  for (i in 1:(Data$Variance %>% unique() %>% length())){
    
    Output <- krCI(SBDC_Model_Newer_Future, 
                   nsim = Simz,
                   individual = data.frame(AgeDummy = mean(Data$AgeDummy),
                                           Gender  = mean(Data$Gender),
                                           EthnicityDummy  = mean(Data$EthnicityDummy),
                                           MeanExpectedFuture  = k,
                                           Variance  = i,
                                           IncomeDummy  = mean(Data$IncomeDummy),
                                           Charity  = mean(Data$Charity),
                                           Consequentiality  = mean(Data$Consequentiality),
                                           Order  = mean(Data$Order),
                                           Understanding = mean(Data$Understanding)))$mWTP
    
    
    VarianceLoop[[i]] <- Output
    VarianceLabeller[[i]] <- i
  }
  TableData = do.call(cbind,  VarianceLoop)
  MeanData[[k]] <- TableData
  
  
  MeanLabeller[[k]] <- rep(x = k, times = Simz)
}

MeanOutput = do.call(rbind,  MeanData) %>% data.frame()
colnames(MeanOutput) <- VarianceLabeller  

MeanColumn = rep(1:Simz, each = Simz+1)
# MeanOutput$MeanExpectedCurrent <- rep(1:(Simz+1), times = 1)
MeanOutput$MeanExpectedCurrent <- MeanColumn
MeanOutput_Pivot <-
  MeanOutput %>% pivot_longer(cols = 1:4,
                              names_to = "Variance",
                              values_to = "WTP") 


MeanOutput_NoInf <- MeanOutput[is.finite(rowSums(MeanOutput)), ]
MeanOutput_Pivot_NoInf <-
  MeanOutput_NoInf %>% pivot_longer(cols = 1:4,
                              names_to = "Variance",
                              values_to = "WTP") 



## Probably best version here
# plot3D::persp3D(
#   z = MeanOutput %>% data.matrix(), 
#   phi = 10, theta = 600, expand = 0.5,
#   border = "black"
# )

# *****************************
# Section x: Interpolate ####
# *****************************


Interpolation <-
  interp(Data$MeanExpectedCurrent,
         Data$Variance,
         Data$ModelWTP,
         duplicate = "strip")
Interpolation_Matrix <- Interpolation$z

plot3D::persp3D(
  z = Interpolation_Matrix %>% data.matrix(), 
  phi = 10, theta = 600, expand = 0.5,
  border = "black"
)
plot_ly(z = ~Interpolation_Matrix, border = "black") %>% add_surface()


MeanOutput_Pivot_DF <- data.frame(MeanOutput_Pivot)

Interpolation2 <-
  akima::interp(
    x = MeanOutput_Pivot$MeanExpectedCurrent,
    y = MeanOutput_Pivot$Variance,
    z = MeanOutput_Pivot$WTP,
    linear = FALSE,
    duplicate = "strip"
  )

Interpolation_Matrix2 <- Interpolation2$z

plot3D::persp3D(
  x = Interpolation2$x,
  y = Interpolation2$y,
  z = Interpolation2$z,
  phi = 7.5,
  theta = -37.5,
  expand = 0.5,
  border = "black"
)


# *****************************
# Section x: Old ####
# *****************************


## Same info here
plot_ly(z = ~MeanOutput %>% data.matrix()) %>% add_surface()



## Stitch all together here
MeanOutput_Pivot

Test <- xtabs(WTP ~ MeanExpectedCurrent + Variance, MeanOutput_Pivot)

plot_ly(z = ~Test) %>% add_surface()




plot3D::persp3D(z = MeanOutput_Pivot %>% data.matrix() , border = "black")

plot3D::persp3D(
  z = MeanOutput_Pivot %>% data.matrix(),
  scale = FALSE,
  expand = 0.01,
  bty = "g",
  phi = 20,
  col = "#0072B2",
  border = "black",
  shade = 0.2,
  ltheta = 90,
  space = 0.3,
  ticktype = "detailed",
  d = 2,
  animated= TRUE
)
