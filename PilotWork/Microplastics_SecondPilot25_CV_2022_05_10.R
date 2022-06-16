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
library(reshape2)
library(plotly)


#------------------------------
# Section 1: Import Data ####
# Selected output of 'sessionInfo()'
#------------------------------


here() ## This is the preferred approach to Setwd()

## Import Data:
SecondPilot25 <- data.frame(read.csv(here("SecondPilot25_Transformed_2022_05_10.csv")))


#----------------------------------------------------
# Section 2: Estimate and Output Bid Only Models ####
#----------------------------------------------------


##
SBDC_Model_BidOnly <- sbchoice(CV_Responses ~ 1 | BID, data = SecondPilot25,dist="logistic")
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
SBDC_Model_BidOnly_WTP_Fitted <- data.frame(apply(SecondPilot25, 
                                                  1, 
                                                  function(i) c(krCI(SBDC_Model_BidOnly,individual = data.frame(BID=SecondPilot25$BID[i]))$out[1,1])))
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
SecondPilot25$CV_Responses


## Explanatory Variables:
### BID Level:
SecondPilot25$BID


### Income Level:
SecondPilot25$Income


### Age:
# SecondPilot25$AgeLevels <- recode(SecondPilot25$AgeBracket,"18 - 29"=25,"30 - 39"=35,"40 - 49"=45)


## Gender:
SecondPilot25$Gender


## Transformations From The Paper:
SecondPilot25$Transform1 <- log((SecondPilot25$Income-SecondPilot25$BID)/SecondPilot25$Income)

### Expectations:
SecondPilot25$WhatWillScienceSay <- ifelse(is.na(SecondPilot25$WhatWillScienceSay)==TRUE,0,SecondPilot25$WhatWillScienceSay)



#----------------------------------------------------
## Variances:
### So there are two approaches: converting std.devs, or calculating difference from means


### The paper says:
# "For dispersion
# measures, we have elected to ask for ‘‘plus’’ and ‘‘minus’’ amounts 
# relative to their expected value (and described as a 95% range), 
# and then to interpret this as four standard deviations."
Bounds<-SecondPilot25$ConfidenceInterval %>%
  as.data.frame() %>%
  separate(1,
           into = c("Upper", "Lower"), sep = "_") %>%
  mutate_all(as.numeric)

SecondPilot25$EstimatedVariance <- (Bounds$Upper/4)^2



#----------------------------------------------------
#### Estimating Option Price Model  ####

## Here using a constant of log of (income-bid)/income
Model1 <- sbchoice(CV_Responses~Transform1+WhatWillScienceSay+EstimatedVariance|BID,data=SecondPilot25,dist="normal")


#### Individual-Specific Fitted WTP ####
for (i in 1:nrow(SecondPilot25)){
  SecondPilot25$ModelWTP[i] <- krCI(Model1, 
                                    individual = data.frame(Transform1=as.numeric(SecondPilot25$Transform1[i]),
                                                            WhatWillScienceSay=as.numeric(SecondPilot25$WhatWillScienceSay[i]),
                                                            EstimatedVariance=as.numeric(SecondPilot25$EstimatedVariance[i]),
                                                            BID=as.numeric(SecondPilot25$BID[i])))$out[1,1]
}                                      

# saveRDS(Model1_WTP_Fitted,"Model1_WTP_Fitted.rds")


#----------------------------------------------------
# Section 3: Plotting the 3D surface from Cameron 2005 ####
#----------------------------------------------------

Data = data.frame(Expectations=as.numeric(SecondPilot25$WhatWillScienceSay),
                  Variance=as.numeric(SecondPilot25$EstimatedVariance),
                  WTP=as.numeric(SecondPilot25$ModelWTP))

Data_matrix <- t(acast(Data, Expectations~Variance, value.var="WTP"))

plot_ly(
  x = as.numeric(colnames(Data_matrix)), 
  y = as.numeric(rownames(Data_matrix)), 
  z = Data_matrix 
) %>% 
  add_surface() %>%  layout(
    title = "",
    scene = list(
      xaxis = list(title = "E[t]"),
      yaxis = list(title = "var[t]"),
      zaxis = list(title = "WTP"),
      camera = list(eye = list(x = 1.95, y = -1.25, z = 1.25))
    ))

