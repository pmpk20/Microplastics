#### DEFRA: Microplastics ####
## Function: Just plots the pandemic questions
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 28/07/2022
## TODO: setup RENV


#------------------------------
# Replication Information: ####
# Selected output of 'sessionInfo()'
#------------------------------

# R version 4.1.3 (2022-03-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)
# [1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252   
# other attached packages:
#   [1] lubridate_1.8.0    tidygeocoder_1.0.5 PostcodesioR_0.3.1 DCchoice_0.1.0    
# [5] here_1.0.1         forcats_0.5.1      stringr_1.4.0      dplyr_1.0.8       
# [9] purrr_0.3.4        readr_2.1.2        tidyr_1.2.0        tibble_3.1.6      
# [13] ggplot2_3.3.5      tidyverse_1.3.1  

## Any issues installing packages try:
# Sys.setenv(RENV_DOWNLOAD_METHOD="libcurl")
# Sys.setenv(RENV_DOWNLOAD_FILE_METHOD=getOption("download.file.method"))

# renv::snapshot()
rm(list=ls())
library(here)
library(DCchoice)
library(lubridate)
library(tidyr)
library(apollo)
library(ggridges)
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(ggdist)
library(RColorBrewer)


#------------------------------
# Section 1: Import Data ####
# Selected output of 'sessionInfo()'
#------------------------------


here() ## This is the preferred approach to Setwd()


## Start with the latest anonymised data in one-row per one-respondent format
Data <-
  here("Data",
       "Microplastics_AllData_Long_Anonymised_2022_06_19.csv") %>%
  fread() %>%
  data.frame()






#---------------------------------------------
# Section 2: Setting Up Plot ####
#---------------------------------------------



## Define text for Y axis to make more sense:
YAxisLabels_Pandemic = c(
  "Pandemic:\n Changed Thoughts On\n Conserving The Environment",
  "Pandemic:\n Changed Thoughts On\n Microplastics"
)


## Probably a better way but adding summary stats to the legend.
LegendLabels_Pandemic = c(
  paste0(
    "Pandemic: Environment\n (Mean: ",
    round(mean(Data$Q17_PandemicEnvironment), 2),
    ",\n SD: ",
    round(sd(Data$Q17_PandemicEnvironment), 2),
    ") \n"
  ),
  paste0(
    "Pandemic: Microplastics\n (Mean: ",
    round(mean(Data$Q17_PandemicMicroplastics), 2),
    ",\n SD: ",
    round(sd(Data$Q17_PandemicMicroplastics), 2),
    ") \n"
  )
)


## Some nice explanatory text for X axis to make more sense:
XAxisLabels_Pandemic = c("1\n (Less Concerned)",
                         2,
                         "3\n (Unchanged)",
                         4,
                         "5\n (More Concerned)")



#---------------------------------------------
# Section 3: Creating Plot ####
### So what's going on here:
### melt() for nice format of data 
### stat_halfeye() to display nice ridges not histograms
### theme_bw() and all the theme() arguments are visual only
### yes I'm using fill_manual but the colours are from ColorBrewer
#---------------------------------------------


PlotOfAttitudes_Pandemic <-
  reshape2::melt(Data[, 28:29]) %>% ggplot(aes(
    x = value,
    y = variable,
    group = variable,
    fill = variable
  )) +
  stat_halfeye() +
  coord_cartesian() + theme_bw() +
  ggtitle(paste0(
    "How Strongly Do You Agree Or Disagree \nWith The Following Statements:"
  )) +
  scale_x_continuous(
    name = "Beliefs.",
    labels = XAxisLabels_Pandemic,
    limits = c(1, 5),
    breaks = seq(1, 5, 1)
  ) +
  scale_y_discrete(name = "Statement",
                   labels = YAxisLabels_Pandemic) +
  scale_fill_manual(
    name = "Item:",
    values = RColorBrewer::brewer.pal(9, "Blues")[c(5, 9)],
    labels = LegendLabels_Pandemic,
    guide = guide_legend(reverse = TRUE)
  ) +
  theme(
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

#---------------------------------------------
# Section 4: Exporting Plot ####
#---------------------------------------------


ggsave(PlotOfAttitudes_Pandemic,device = "jpeg",
       filename = "PlotOfAttitudes_Pandemic_2022_07_28.jpeg",
       width=20,height=15,units = "cm",dpi=1000)



# End Of Script -------------------------------------------