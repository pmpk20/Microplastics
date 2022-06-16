#### DEFRA: Microplastics ####
## Function: Tests all Survey Engine CV responses
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 16/06/2022
## TODO: setup RENV


#----------------------------------------------------
# Replication Information: ####
# Selected output of 'sessionInfo()'
#----------------------------------------------------


# R version 4.1.3 (2022-03-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)
# [1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252   
# [1] MASS_7.3-56    compiler_4.1.3 tools_4.1.3    renv_0.15.4  

## Any issues installing packages try:
# Sys.setenv(RENV_DOWNLOAD_METHOD="libcurl")
# Sys.setenv(RENV_DOWNLOAD_FILE_METHOD=getOption("download.file.method"))
# install.packages("tidyverse",repos = "http://cran.us.r-project.org")


# renv::snapshot()
rm(list=ls())
# library(tidyverse)
library(here)
library(DCchoice)
library(PostcodesioR)
# library(tidygeocoder)
library(lubridate)
library(tidyr)
library(reshape2)
# library(plotly)



#------------------------------
# Section 1: Import Data ####
# Selected output of 'sessionInfo()'
#------------------------------


here() ## This is the preferred approach to Setwd()

## Import Data:
Data <- data.frame(read.csv(here("Microplastics_LongData_2022_06_15.csv")))


#----------------------------------------------------
# Section 2: Estimate and Output Bid Only Models ####
#----------------------------------------------------


##
SBDC_Model_BidOnly <- sbchoice(CV ~ 1 | Bid, data = Data,dist="logistic")
SBDC_Model_BidOnly_WTP <- bootCI(SBDC_Model_BidOnly)
summary(SBDC_Model_BidOnly)

## WTP (Median):
SBDC_Model_BidOnly_WTP$out[4,1]

## Range:
cbind(SBDC_Model_BidOnly_WTP$out[4,2],SBDC_Model_BidOnly_WTP$out[4,3])

## N:
nrow(SBDC_Model_BidOnly$data)

## AIC:
AIC(SBDC_Model_BidOnly)

## R2:
summary(SBDC_Model_BidOnly)$adjpsdR2

## LogLik:
logLik(SBDC_Model_BidOnly)


#### Individual-Specific Fitted WTP ####
SBDC_Model_BidOnly_WTP_Fitted <- data.frame(apply(Data, 
                                                  1, 
                                                  function(i) c(krCI(SBDC_Model_BidOnly,individual = data.frame(Bid=Data$Bid[i]))$out[1,1])))
saveRDS(SBDC_Model_BidOnly_WTP_Fitted,"SBDC_Model_BidOnly_WTP_Fitted.rds")





#----------------------------------------------------
#### Section 3: Replicating Cameron 2005  ####

# Paper here: https://doi.org/10.1016/j.jpubeco.2004.01.005
# The paper says we need:

## The certain level of environmental quality with mitigation t*, 
## individual characteristics x, 
### Which can be allowed to shift each of the three basic utility parameters. 


# The key explanatory variables capturing ‘‘scope’’
# are constructed from the mean 
# and the variance of the individual’s subjective distribution
# concerning future environmental quality in the absence of mitigation: E[t] and Var[t].
# Approximations for each of these variables are elicited from each respondent in our
# survey

#----------------------------------------------------


## Dependent Variable:
Data$CV


## Explanatory Variables:
### Bid Level:
Data$Bid


### Income Level:
Data$Income


### Age:
# Data$AgeLevels <- recode(Data$AgeBracket,"18 - 29"=25,"30 - 39"=35,"40 - 49"=45)


## Gender:
Data$Gender


## Transformations From The Paper:
Data$Transform1 <- log((Data$Income-Data$Bid)/Data$Income)

### Expectations:
Data$WhatWillScienceSay <- ifelse(is.na(Data$MeanExpectedFuture)==TRUE,0,Data$MeanExpectedFuture)



#----------------------------------------------------
## Variances:
### So there are two approaches: converting std.devs, or calculating difference from means


### The paper says:
# "For dispersion
# measures, we have elected to ask for ‘‘plus’’ and ‘‘minus’’ amounts 
# relative to their expected value (and described as a 95% range), 
# and then to interpret this as four standard deviations."
# Bounds<-Data$ConfidenceInterval %>%
#   as.data.frame() %>%
#   separate(1,
#            into = c("Upper", "Lower"), sep = "_") %>%
#   mutate_all(as.numeric)
# 
# Data$EstimatedVariance <- (Bounds$Upper/4)^2




Data$VarianceUpperBound <- ifelse(Data$Variance==4,5,
                                  ifelse(Data$Variance==3,3,
                                         ifelse(Data$Variance==2,1,0)))
Data$VarianceLowerBound <- ifelse(Data$Variance==4,5,
                                  ifelse(Data$Variance==3,3,
                                         ifelse(Data$Variance==2,1,0)))

Data$EstimatedVariance <- (Data$VarianceUpperBound/4)^2


#----------------------------------------------------
#### Estimating Option Price Model  ####

## Here using a constant of log of (income-Bid)/income
Model1 <- sbchoice(CV~Transform1+MeanExpectedFuture+EstimatedVariance|Bid,data=Data,dist="normal")
summary(Model1)

#### Individual-Specific Fitted WTP ####
for (i in 1:nrow(Data)){
  Data$ModelWTP[i] <- krCI(Model1, 
                                individual = data.frame(Transform1=as.numeric(Data$Transform1[i]),
                                                        MeanExpectedFuture=as.numeric(Data$MeanExpectedFuture[i]),
                                                        WhatWillScienceSay=as.numeric(Data$WhatWillScienceSay[i]),
                                                        EstimatedVariance=as.numeric(Data$EstimatedVariance[i]),
                                                        Bid=as.numeric(Data$Bid[i])))$out[1,1]
}       


write.csv(Data,"Microplastics_DataWithWTP_2022_06_16.csv")
# saveRDS(Model1_WTP_Fitted,"Model1_WTP_Fitted.rds")


#----------------------------------------------------
# Section 3: Plotting the 3D surface from Cameron 2005 ####
#----------------------------------------------------

# Data = data.frame(Expectations=as.numeric(Data$WhatWillScienceSay),
#                   Variance=as.numeric(Data$EstimatedVariance),
#                   WTP=as.numeric(Data$ModelWTP))
# 
# Data_matrix <- t(acast(Data, Expectations~Variance, value.var="WTP"))
# 
# plot_ly(
#   x = as.numeric(colnames(Data_matrix)), 
#   y = as.numeric(rownames(Data_matrix)), 
#   z = Data_matrix 
# ) %>% 
#   add_surface() %>%  layout(
#     title = "",
#     scene = list(
#       xaxis = list(title = "E[t]"),
#       yaxis = list(title = "var[t]"),
#       zaxis = list(title = "WTP"),
#       camera = list(eye = list(x = 1.95, y = -1.25, z = 1.25))
#     ))

