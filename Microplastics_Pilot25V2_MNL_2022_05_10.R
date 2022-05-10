#### DEFRA: Microplastics ####
## Function: Estimates a basic MNL on the 25 V2 respondents.
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 10/05/2022
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
library(tidyverse)
library(here)
library(DCchoice)
library(PostcodesioR)
library(tidygeocoder)
library(lubridate)
library(tidyr)
library(apollo)


#------------------------------
# Section 1: Import Data ####
# Selected output of 'sessionInfo()'
#------------------------------


here() ## This is the preferred approach to Setwd()

## Import Data:
database3 <- data.frame(read.csv(here("database_Transformed_2022_05_10.csv")))
database2 <- data.frame(read.csv(here("database_Transformed_2022_05_08.csv")))

database <- rbind(database2,database3)
database$Respondent<- rep(1:75,each=4)
database <- database[which(!is.na(database$Choice)),]

#### Estimate MNL ####


apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "Microplastics_MNL_2022_05_10",
  modelDescr      = "Microplastics_MNL_2022_05_10",
  indivID         = "Respondent"
)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_A      = 0,asc_B=0,asc_C=0,
              b_Performance=0,
              b_Emission=0,
              b_Tax=0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_B","asc_C")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["A"]]  =asc_A
  
  V[["B"]]  =asc_B+b_Tax *(TaxB+
                             b_Performance*(PerformanceB)+
                             b_Emission*(EmissionB))
  
  V[["C"]]  =asc_C+b_Tax *(TaxC+
                             b_Performance*(PerformanceC)+
                             b_Emission*(EmissionC))
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(A=1, B=2, C=3), 
    avail         = list(A=1, B=1, C=1), 
    choiceVar     = Choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

Microplastics_MNL_2022_05_10 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(Microplastics_MNL_2022_05_10,modelOutput_settings = list(printPVal=TRUE))
Microplastics_MNL_2022_05_10$estimate



# -1*(Microplastics_MNL_2022_05_10$estimate/Microplastics_MNL_2022_05_10$estimate["b_Tax"])


# apollo_saveOutput(Microplastics_MNL_2022_05_10,saveOutput_settings = list(printPVal=TRUE))
# 
# 
# apollo_deltaMethod(Microplastics_MNL_2022_05_10,
#                    deltaMethod_settings = list(operation="ratio",
#                                                parName1="b_PopSmallDecrease",
#                                                parName2="b_Tax"))




#### MNL but linear in parameters ####


# apollo_initialise()
# 
# ### Set core controls
# apollo_control = list(
#   modelName       = "Microplastics_MNL_2022_05_10",
#   modelDescr      = "Microplastics_MNL_2022_05_10",
#   indivID         = "Respondent"
# )
# 
# 
# # ################################################################# #
# #### DEFINE MODEL PARAMETERS                                     ####
# # ################################################################# #
# 
# ### Vector of parameters, including any that are kept fixed in estimation
# apollo_beta=c(asc_A      = 0,asc_B=0,asc_C=0,
#               b_Performance_5=0,b_Performance_50=0,
#               b_Emission_40=0,b_Emission_90=0,
#               b_Tax=0)
# 
# ### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
# apollo_fixed = c("asc_B","asc_C")
# 
# # ################################################################# #
# #### GROUP AND VALIDATE INPUTS                                   ####
# # ################################################################# #
# 
# apollo_inputs = apollo_validateInputs()
# 
# # ################################################################# #
# #### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# # ################################################################# #
# 
# apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
#   
#   ### Attach inputs and detach after function exit
#   apollo_attach(apollo_beta, apollo_inputs)
#   on.exit(apollo_detach(apollo_beta, apollo_inputs))
#   
#   ### Create list of probabilities P
#   P = list()
#   
#   
#   ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
#   V = list()
#   V[["A"]]  =asc_A
#   
#   V[["B"]]  =asc_B+b_Tax *(TaxB+
#                              b_Performance_5*(PerformanceB==5)+
#                              b_Performance_50*(PerformanceB==50)+
#                              b_Emission_40*(EmissionB==40)+
#                              b_Emission_90*(EmissionB==90))
#   
#   V[["C"]]  =asc_C+b_Tax *(TaxC+
#                              b_Performance_5*(PerformanceC==5)+
#                              b_Performance_50*(PerformanceC==50)+
#                              b_Emission_40*(EmissionC==40)+
#                              b_Emission_90*(EmissionC==90))
#   
#   ### Define settings for MNL model component
#   mnl_settings = list(
#     alternatives  = c(A=1, B=2, C=3), 
#     avail         = list(A=1, B=1, C=1), 
#     choiceVar     = Choice,
#     utilities     = V
#   )
#   
#   ### Compute probabilities using MNL model
#   P[["model"]] = apollo_mnl(mnl_settings, functionality)
#   
#   ### Take product across observation for same individual
#   P = apollo_panelProd(P, apollo_inputs, functionality)
#   
#   ### Prepare and return outputs of function
#   P = apollo_prepareProb(P, apollo_inputs, functionality)
#   return(P)
# }
