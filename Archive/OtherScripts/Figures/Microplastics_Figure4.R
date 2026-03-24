#### DEFRA: Microplastics ####
## Function: Plots conditional distributions of WTP
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 26/07/2022
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

#------------------------------
# Section 1: Import Data ####
# Selected output of 'sessionInfo()'
#------------------------------


here() ## This is the preferred approach to Setwd()


## Import Data:
Microplastics_MXL_PooledSample_AllCovariates_WTP <- data.frame(read.csv("Microplastics_MXL_PooledSample_AllCovariates_WTP.csv"))


# Trim the stupidly large price estimates
Microplastics_MXL_PooledSample_AllCovariates_WTP <- Microplastics_MXL_PooledSample_AllCovariates_WTP[Microplastics_MXL_PooledSample_AllCovariates_WTP$b_Price.post.mean> -5,]
WTP <- Microplastics_MXL_PooledSample_AllCovariates_WTP %>% select(ends_with(".post.mean")) %>% select(!starts_with("b_Price"))
WTP <- WTP*-1

WTP <-
  cbind(    "b_Price.post.mean"=Microplastics_MXL_PooledSample_AllCovariates_WTP$b_Price.post.mean,
            Microplastics_MXL_PooledSample_AllCovariates_WTP %>% select(ends_with(".post.mean")) %>% select(!starts_with("b_Price")) *
              -1
  )

Labels = c("Price","Performance:10%","Performance:50%","Emission:40%","Emission:90%")

#----------------------------------------------
# Section 3: Make Plots ####
#----------------------------------------------

ReportPlot <- WTP %>% select(ends_with(".post.mean")) %>% reshape2::melt() %>% ggplot(aes(x=value,y=variable,group=variable,fill=variable))+
  geom_density_ridges()+geom_vline(xintercept=0,linetype='dashed')+
  scale_x_continuous(name="mWTP in pounds.",
                     limits=c(-2.5,2.5),
                     breaks= seq(-2.5,2.5,0.5))+
  ggtitle("Posterior Conditional Distribution Of Attribute mWTPs.")+  
  scale_y_discrete(name="Attribute",
                   label=Labels)+
  coord_cartesian()+theme_bw()+
  scale_fill_brewer(name="Attributes",
                    label=Labels,
                    guide=guide_legend(reverse = TRUE))+
  theme(legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank())


#----------------------------------------------
# Section 4: Export plots ####
#----------------------------------------------


ggsave(ReportPlot,device = "jpeg",
       filename = "Microplastics_MXL_PooledSample_AllCovariates_ReportPlot.jpeg",
       width=20,height=15,units = "cm",dpi=1000)


# End Of Script -------------------------------------------------------