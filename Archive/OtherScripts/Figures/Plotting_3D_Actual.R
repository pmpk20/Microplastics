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
Data <-
  here("Data",
       "Microplastics_AllData_Long_Anonymised_2022_06_19.csv") %>%
  fread() %>%
  data.frame()



# *****************************
# Section 20: CV models ####
# *****************************



## Testing mean current or mean future
SBDC_Model_Newer_Future <- sbchoice(
  CV ~ AgeDummy +
    Gender +
    EthnicityDummy +
    MeanExpectedFuture + 
    Variance + 
    IncomeDummy +
    Charity +
    Consequentiality +
    Order +
    Understanding |
    Bid,
  data = Data,
  dist = "normal"
) 



# *****************************
# Section 2A: Initialise variables ####
# *****************************


Simz <- 1000
Length <- (Data$MeanExpectedCurrent %>% unique() %>% length())
MeanLoop = vector("list", length = (Data$MeanExpectedCurrent %>% unique() %>% length()))
MeanData = vector("list", length = (Data$MeanExpectedCurrent %>% unique() %>% length()))
MeanLabeller <- MeanLoop

VarianceLoop = vector("list", length = (Data$Variance %>% unique() %>% length()))
VarianceLabeller <- VarianceLoop




# *****************************
# Section 2B: Loop ####
# *****************************


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




# *****************************
# Section 2C: Tidy loop output ####
# *****************************

MeanOutput = do.call(rbind,  MeanData) %>% data.frame()
colnames(MeanOutput) <- VarianceLabeller  

MeanColumn = rep(1:Simz, each = 11)
MeanOutput$MeanExpectedCurrent <- MeanColumn
MeanOutput_Pivot <-
  MeanOutput %>% pivot_longer(cols = 1:4,
                              names_to = "Variance",
                              values_to = "WTP") 


## This was causing a lot of problems! 
MeanOutput_Pivot$Variance %<>% as.numeric()


# *****************************
# Section 2D: Interpolate ####
# *****************************


Interpolation2 <-
  akima::interp(
    x = MeanOutput_Pivot$MeanExpectedCurrent,
    y = MeanOutput_Pivot$Variance,
    z = MeanOutput_Pivot$WTP,
    linear = FALSE,
    duplicate = "strip"
  )

Interpolation_Matrix2 <- Interpolation2$z



# *****************************
# Section 3: Plot with PLOT3D ####
# *****************************


par(mar=c(1,1,1,1))
png(filename =  here("CVoutput", "CameronPlot_1.png"))



plot3D::persp3D(
  x = Interpolation2$x,
  y = Interpolation2$y,
  z = Interpolation2$z,
  phi = 7.5,
  theta = -37.5,
  expand = 0.5,
  border = "black",
  xlab = "Means",
  ylab = "Variances",
  zlab = "WTP"
)
dev.off()


# ggsave(
#   plot = plot3D::persp3D(
#     x = Interpolation2$x,
#     y = Interpolation2$y,
#     z = Interpolation2$z,
#     phi = 7.5,
#     theta = -37.5,
#     expand = 0.5,
#     border = "black",
#     xlab = "Means",
#     ylab = "Variances",
#     zlab = "WTP"
#   ),
#   filename =
#     here("CVoutput", "CameronPlot_1.png"),
#   device = "png", dpi = 250
#   
# )
# 

# *****************************
# Section 3: Plot with RGL ####
# *****************************



rgl.snapshot(filename = here("CVoutput", "CameronPlot_1.png"))

rgl::persp3d(x = Interpolation2$x,
             y = Interpolation2$y,
             z = Interpolation2$z,
             xlab = "Means",
             ylab = "Variances",
             zlab = "WTP", 
             col = RColorBrewer::brewer.pal(name = "Blues",n = 8), 
             shade = TRUE
)


Plot1 <- rgl::persp3d(x = Interpolation2$x,
             y = Interpolation2$y,
             z = Interpolation2$z,
             xlab = "Means",
             ylab = "Variances",
             zlab = "WTP", 
             col = RColorBrewer::brewer.pal(name = "Blues",n = 8), 
             shade = TRUE,
             theta = 45
)

rgl.snapshot(filename = here("CVoutput", "CameronPlot_1.png"))















## Start with the latest anonymised data in one-row per one-respondent format
Data <-
  here("Data",
       "Data_WithEOP_24_11_05.csv") %>%
  fread() %>%
  data.frame()



## Isolate variables we want only
PlotData <- 
  Data %>% 
  mutate(Income_Quintile = ntile(Income, 5) %>% as.factor()) %>% 
  dplyr::filter(Income_Quintile == 1 &
                  EthnicityDummy == 0 &
                  Charity == 1) %>% 
  dplyr::select(c("MeanExpectedFuture", 
                  "Uncertainty",
                  "EOP",
                  "Income"))


# Filter dataset for a specific respondent profile
# filtered_data <- plot_data %>%
#   filter(Income == 40000 & Gender == "male" & Conservatism == "neutral" & Informedness == "neutral")

# Interpolate based on filtered data
interp_data <- with(PlotData, 
                    interp(
                      x = MeanExpectedFuture, 
                      y = Uncertainty, 
                      z = EOP, 
                      duplicate = "mean", 
                      nx = 50,  # Interpolated grid points along X
                      ny = 50   # Interpolated grid points along Y
                    ))

# Prepare interpolated surface data for plotly
interp_surface <- list(
  z = interp_data$z,
  x = interp_data$x,
  y = interp_data$y
)

# Plot interpolated surface
plot_ly(
  x = ~interp_surface$x, 
  y = ~interp_surface$y, 
  z = ~interp_surface$z, 
  type = "surface",
  colorscale = list(
    c(0, 0.25, 0.5, 0.75, 1),
    c("blue", "cyan", "green", "yellow", "red")
  )
) %>%
  layout(
    title = "3D Surface Plot: Benchmarking WTP for Specific Respondent Type",
    scene = list(
      xaxis = list(title = "E[t] (Mean Expected Harm)"),
      yaxis = list(title = "Var[t] (Variance)"),
      zaxis = list(title = "$WTP/mo (EOP)", range = c(0, 500))  # Limit Z-axis
    )
  )
