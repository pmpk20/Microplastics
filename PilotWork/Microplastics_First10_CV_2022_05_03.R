#### DEFRA: Microplastics ####
## Function: Tests the CV responses
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 03/05/2022
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

# renv::snapshot()
rm(list=ls())
library(tidyverse)
library(here)
library(DCchoice)
library(PostcodesioR)
library(tidygeocoder)
library(lubridate)
library(tidyr)



#------------------------------
# Section 1: Import Data ####
# Selected output of 'sessionInfo()'
#------------------------------


here() ## This is the preferred approach to Setwd()

## Import Data:
First10 <- data.frame(read.csv(here("First10_Transformed_2022-05-03.csv")))


#----------------------------------------------------
# Section 2: Estimate and Output Bid Only Models ####
#----------------------------------------------------


##
SBDC_Model_BidOnly <- sbchoice(CV_Responses ~ 1 | BID, data = First10,dist="logistic")
SBDC_Model_BidOnly_WTP <- bootCI(SBDC_Model_BidOnly)


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
SBDC_Model_BidOnly_WTP_Fitted <- data.frame(apply(First10, 
                                     1, 
                                     function(i) c(krCI(SBDC_Model_BidOnly,individual = data.frame(BID=First10$BID[i]))$out[1,1])))
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
First10$CV_Responses


## Explanatory Variables:
### BID Level:
First10$BID


### Income Level:
First10$Income


### Age:
First10$AgeLevels <- recode(First10$AgeBracket,"18 - 29"=25,"30 - 39"=35,"40 - 49"=45)


## Gender:
First10$Gender


## Transformations From The Paper:
First10$Transform1 <- log((First10$Income-First10$BID)/First10$Income)

### Expectations:
First10$WhatWillScienceSay <- ifelse(is.na(First10$WhatWillScienceSay)==TRUE,0,First10$WhatWillScienceSay)



#----------------------------------------------------
## Variances:
### So there are two approaches: converting std.devs, or calculating difference from means


### The paper says:
# "For dispersion
# measures, we have elected to ask for ‘‘plus’’ and ‘‘minus’’ amounts 
# relative to their expected value (and described as a 95% range), 
# and then to interpret this as four standard deviations."
Bounds<-First10$ConfidenceInterval %>%
  as.data.frame() %>%
  separate(1,
           into = c("Upper", "Lower"), sep = "_") %>%
  mutate_all(as.numeric)

First10$EstimatedVariance <- (Bounds$Upper/4)^2



#----------------------------------------------------
#### Estimating Option Price Model  ####

## Here using a constant of log of (income-bid)/income
Model1 <- sbchoice(CV_Responses~Transform1+AgeLevels+Gender+WhatWillScienceSay+EstimatedVariance|BID,data=First10,dist="normal")



