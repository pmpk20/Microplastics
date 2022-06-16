#### DEFRA: Microplastics ####
## Function: Estimates a basic MNL on the survey engine respondents
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 16/06/2022
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
# database4 <- data.frame(read.csv(here("database_Transformed_2022_05_13.csv")))
# database3 <- data.frame(read.csv(here("database_Transformed_2022_05_10.csv")))
# database2 <- data.frame(read.csv(here("database_Transformed_2022_05_08.csv")))
# 
# database4$ClimateEnvironment <- NULL
# database4$ClimateSelf <- NULL
# database4$Q30_1 <- NULL
# database4$Q32 <- NULL
# colnames(database4)[which(names(database4)=="MicroplasticsEnvironment")] <- "ThreatToEnvironment"
# colnames(database4)[which(names(database4)=="MicroplasticsSelf")] <- "ThreatToSelf"
# 
# database2$Q29.1 <- NULL
# database3$Q29.1 <- NULL
# database4$Q35_2_TEXT <- NULL
# 
# 
# database <- rbind(database2,database3,database4)
# database$Respondent<- rep(1:(nrow(database)/4),each=4)
# database <- database[which(!is.na(database$Choice)),]
# write.csv(database,"database_AllPilots_2022_05_13.csv")



#### Estimate MNL ####

database <- data.frame(read.csv(here("Microplastics_database_2022_06_15.csv")))


#------------------------------
# Setup Apollo: ####
#------------------------------

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "Microplastics_SurveyEngine_MXL_2022_06_16",
  modelDescr      = "Microplastics_SurveyEngine_MXL_2022_06_16",
  indivID         = "Respondent",
  mixing=TRUE,
  nCores=10
)


#------------------------------
# Define Parameters: ####
#------------------------------

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_A      = 0,
              asc_B      = 0,
              asc_C      = 0,
              mu_Performance_10=1,
              mu_Performance_50=1,
              mu_Emissions_40=1,
              mu_Emissions_90=1,
              mu_Price=-3,
              sig_Performance_10=0,
              sig_Performance_50=0,
              sig_Emissions_40=0,
              sig_Emissions_90=0,
              sig_Price=-0.01)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_B","asc_C")



### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "pmc",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("Draws_Performance_10" , "Draws_Performance_50","Draws_Emissions_40",
                     "Draws_Emissions_90","draws_Price" ),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)
### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Price"]] = -exp(mu_Price + sig_Price * draws_Price )
  randcoeff[["b_Performance_10"]] =  (mu_Performance_10+    sig_Performance_10*Draws_Performance_10)
  randcoeff[["b_Performance_50"]] =  (mu_Performance_50+    sig_Performance_50*Draws_Performance_50)
  randcoeff[["b_Emissions_40"]] =  ( mu_Emissions_40+    sig_Emissions_40*Draws_Emissions_40)
  randcoeff[["b_Emissions_90"]] =  (mu_Emissions_90+    sig_Emissions_90*Draws_Emissions_90)
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()


#------------------------------
# Establish Likelihood Function: ####
#------------------------------


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["A"]]  = asc_A 
  
  
  V[["B"]]  = asc_B  +  b_Price *((Price_B)+
                                   (b_Performance_10*(Performance_B==10))+
                                   (b_Performance_50*(Performance_B==50))+
                                   (b_Emissions_40*(Emission_B==40))+
                                    (b_Emissions_90*(Emission_B==90)))
  
  V[["C"]]  = asc_C  + b_Price *((Price_C)+
                                  (b_Performance_10*(Performance_B==10))+
                                  (b_Performance_50*(Performance_B==50))+
                                  (b_Emissions_40*(Emission_C==40))+
                                  (b_Emissions_90*(Emission_C==90)))
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(A=1, B=2, C=3), 
    avail         = list(A=1, B=1, C=1), 
    choiceVar     = Choice,
    utilities     = V
  )
  
  ## Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ## Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ## Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ## Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#------------------------------
# Estimation: ####
#------------------------------

## Actually estimates the model
Microplastics_SurveyEngine_MXL_2022_06_16 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

## Model output and results here alongside saving information
apollo_modelOutput(Microplastics_SurveyEngine_MXL_2022_06_16,modelOutput_settings = list(printPVal=TRUE))
apollo_saveOutput(Microplastics_SurveyEngine_MXL_2022_06_16,saveOutput_settings = list(printPVal=TRUE))
saveRDS(Microplastics_SurveyEngine_MXL_2022_06_16, file="Microplastics_SurveyEngine_MXL_2022_06_16.rds")


#------------------------------
# Summarise WTP: ####
#------------------------------

Model <- readRDS("Microplastics_SurveyEngine_MXL_2022_06_16.rds") ## Enter model of interest RDS here
Microplastics_SurveyEngine_MXL_2022_06_16_WTP <- apollo_conditionals(Model,apollo_probabilities,apollo_inputs )
write.csv(Microplastics_SurveyEngine_MXL_2022_06_16_WTP,"Microplastics_SurveyEngine_MXL_2022_06_16_WTP.csv")

# 
# Microplastics_SurveyEngine_MXL_2022_06_16_WTPSummary <-data.frame(cbind("b_Performance_10"=Microplastics_SurveyEngine_MXL_2022_06_16_WTP$b_Performance_10$post.mean,
#                                                                                 "b_Performance_50"=Microplastics_SurveyEngine_MXL_2022_06_16_WTP$b_Performance_50$post.mean,
#                                                                                 "b_Emissions_40"=Microplastics_SurveyEngine_MXL_2022_06_16_WTP$b_Emissions_40$post.mean,
#                                                                                 "b_Emissions_90"=Microplastics_SurveyEngine_MXL_2022_06_16_WTP$b_Emissions_90$post.mean,
#                                                                                 "b_ManagementDeterrent"=Microplastics_SurveyEngine_MXL_2022_06_16_WTP$b_ManagementDeterrent$post.mean,
#                                                                                 "b_ManagementNothing"=Microplastics_SurveyEngine_MXL_2022_06_16_WTP$b_ManagementNothing$post.mean))
# apollo_sink()
# Microplastics_SurveyEngine_MXL_2022_06_16_WTP %>% select(ends_with(".post.mean")) %>% summarise(across(everything(),list(mean)))
# apollo_sink()
Microplastics_SurveyEngine_MXL_2022_06_16_WTP <- data.frame(read.csv("Microplastics_SurveyEngine_MXL_2022_06_16_WTP.csv"))


# Trim the stupidly large price estimates
Microplastics_SurveyEngine_MXL_2022_06_16_WTP <- Microplastics_SurveyEngine_MXL_2022_06_16_WTP[Microplastics_SurveyEngine_MXL_2022_06_16_WTP$b_Price.post.mean> -5,]
WTP <- Microplastics_SurveyEngine_MXL_2022_06_16_WTP %>% select(ends_with(".post.mean")) %>% select(!starts_with("b_Price"))
WTP <- WTP*-1

ggsave(Microplastics_SurveyEngine_MXL_2022_06_16_WTP %>% select(ends_with(".post.mean")) %>% reshape2::melt() %>% ggplot(aes(x=value,y=variable,group=variable,fill=variable))+
         geom_density_ridges()+geom_vline(xintercept=0,linetype='dashed')+
         scale_x_continuous(name="mWTP in pounds."), 
       device = "jpeg",
       filename = "Microplastics_SurveyEngine_MXL_2022_06_16_WTP_DensityPlot.jpeg",
       width=20,height=15,units = "cm",dpi=1000)

## Plot Without Costs:
ggsave(WTP %>% reshape2::melt() %>% ggplot(aes(x=value,y=variable,group=variable,fill=variable))+
         geom_density_ridges()+geom_vline(xintercept=0,linetype='dashed')+
         scale_x_continuous(name="mWTP in pounds."), 
       device = "jpeg",
       filename = "Microplastics_SurveyEngine_MXL_2022_06_16_WTP_NoCost_DensityPlot.jpeg",
       width=20,height=15,units = "cm",dpi=1000)