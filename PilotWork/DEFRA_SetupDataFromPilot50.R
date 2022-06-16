#### DEFRA: Microplastics ####
## Function: Imports and transforms Second 10 respondents
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 03/05/2022
## TODO: setup RENV


#------------------------------
# Replication Information: ####
# Selected output of 'sessionInfo()'
#------------------------------

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
library(magrittr)


#------------------------------
# Section 1: Import Data ####
# Selected output of 'sessionInfo()'
#------------------------------


here() ## This is the preferred approach to Setwd()

## Import Data:
Pilot50 <- data.frame(read.csv(here("DEFRA_Microplastics_50Pilot_2022_05_08.csv")))

## Drop Weird Header Rows From Qualtrics
Pilot50 <- Pilot50[c(3:nrow(Pilot50)),]


# ----------------------------------------------------------------------------------------------------------
#### Section 2: Coding Variables ####
# ----------------------------------------------------------------------------------------------------------


# Dropping columns:
# ----------------------------------------------------------------------------------------------------------

## Drop columns unnecessary for analysis:
Pilot50 <- Pilot50[,-which(names(Pilot50) %in%c("StartDate",
                                                   "EndDate",
                                                   "Status",
                                                   "IPAddress",
                                                   "Progress",
                                                   "ResponseId",
                                                   "RecipientLastName",
                                                   "RecipientSecondName",
                                                   "RecipientEmail",
                                                   "ExternalReference",
                                                   "DistributionChannel",
                                                   "UserLanguage"))]


## Remove unnecessary randomisation columns
Pilot50 <- Pilot50[,-c(45:55)]


# Renaming and Recoding:
# ----------------------------------------------------------------------------------------------------------


## Recoding Gender:
Pilot50$Q2 <- recode(Pilot50$Q2,'Female'=0,'Male'=1)
## Rename timing of survey length to more easily understood name
colnames(Pilot50)[which(names(Pilot50)=="Q2")] <- "Gender"


## Categorising respondent age:
Pilot50$Q3 <- recode(Pilot50$Q3,"18 - 29"=1,"30 - 39"=2,"40 - 49"=3,"50 - 59"=4,"60 - 69"=5,"70+"=6)
colnames(Pilot50)[which(names(Pilot50)=="Q3")] <- "AgeBracket"


## CV then CE equals Zero, CE Second gets 1
Pilot50$CVCEGroup <- recode(Pilot50$CVCEGroup,'CVCE'=0,'CECV'=1)

## Rename timing of survey length to more easily understood name
colnames(Pilot50)[which(names(Pilot50)=="Duration..in.seconds.")] <- "Timing"


## Convert Date to Usable Format 
Pilot50$RecordedDate %<>% lubridate::as_date()



# Geocoding:
# ----------------------------------------------------------------------------------------------------------

## Rename respondents postcode to more specific name
colnames(Pilot50)[which(names(Pilot50)=="Q4")] <- "EnteredPostcode"


## Rename respondents locations
colnames(Pilot50)[which(names(Pilot50)=="LocationLatitude")] <- "Latitude"
colnames(Pilot50)[which(names(Pilot50)=="LocationLongitude")] <- "Longitude"

## Postcode lookup to check:
postcode_lookup(Pilot50$EnteredPostcode[1])

## Geolocate specific respondents:
str(reverse_geo(lat = Pilot50$Latitude[1],long = Pilot50$Longitude[1]))

# ----------------------------------------------------------------------------------------------------------
#### Section 3: CV Section ####
# ----------------------------------------------------------------------------------------------------------


Pilot50$BID <- as.numeric(Pilot50$BID)
Pilot50$Q14 <- recode(Pilot50$Q14,'No'=0,'Yes'=1)
colnames(Pilot50)[which(names(Pilot50)=="Q14")] <- "CV_Responses"



## Organising the scales:
# ----------------------------------------------------------------------------------------------------------

## Rename the scales:
Pilot50$Q7_1 %<>% as.numeric()
Pilot50$Q7_1 <- ifelse(is.na(Pilot50$Q7_1)==TRUE,0,Pilot50$Q7_1)
colnames(Pilot50)[which(names(Pilot50)=="Q7_1")] <- "WhatWillScienceSay"

## Rename the variance question
colnames(Pilot50)[which(names(Pilot50)=="Q7B")] <- "ConfidenceInterval"

## Recoding intervals to enable splitting the column %>% 
Pilot50$ConfidenceInterval <- recode(Pilot50$ConfidenceInterval,
                                      "Exactly"="0_0",
                                      "Plus or minus 1 point on the scale."="1_1",
                                      "Plus or minus 3 points on the scale."="3_3",
                                      "Not at all."="5_5")

Pilot50 <- cbind(Pilot50,Pilot50$ConfidenceInterval %>% as.data.frame() %>% separate(1,
                                                                                        into = c("Upper","Lower"),
                                                                                        sep="_") %>% mutate_all(as.numeric))

Pilot50$Upper <- Pilot50$WhatWillScienceSay+Pilot50$Upper
Pilot50$Lower <- Pilot50$WhatWillScienceSay-Pilot50$Lower

# ----------------------------------------------------------------------------------------------------------
#### Section 4: CE Section ####
# ----------------------------------------------------------------------------------------------------------


Zeroes <-  data.frame("PerformanceA"=0,"PerformanceB"=0,"PerformanceC"=0,
                      "EmissionA"=0,"EmissionB"=0,     "EmissionC"=0,
                      "TaxA"=0,     "TaxB"=0,           "TaxC"=0)


# ----------------------------------------------------------------------------------------------------------

# recode(Pilot50$Q9) to 0,1
Q9_A_Values <-  data.frame("PerformanceA"=0,"PerformanceB"=5,"PerformanceC"=50,
                           "EmissionA"=0,"EmissionB"=10,"EmissionC"=90,
                           "TaxA"=0,"TaxB"=1,"TaxC"=1)


Q9_B_Values <-  data.frame("PerformanceA"=0,"PerformanceB"=10,"PerformanceC"=50,
                           "EmissionA"=0,"EmissionB"=40,     "EmissionC"=10,
                           "TaxA"=0,     "TaxB"=1,           "TaxC"=0.5)

# ----------------------------------------------------------------------------------------------------------


Q10_A_Values <-  data.frame("PerformanceA"=0,"PerformanceB"=50,"PerformanceC"=5,
                            "EmissionA"=0,"EmissionB"=40,"EmissionC"=10,
                            "TaxA"=0,"TaxB"=1,"TaxC"=0.5)


Q10_B_Values <-  data.frame("PerformanceA"=0,"PerformanceB"=10,"PerformanceC"=50,
                            "EmissionA"=0,"EmissionB"=10,     "EmissionC"=40,
                            "TaxA"=0,     "TaxB"=0.5,           "TaxC"=1)


# ----------------------------------------------------------------------------------------------------------


Q11_A_Values <-  data.frame("PerformanceA"=0,"PerformanceB"=50,"PerformanceC"=5,
                            "EmissionA"=0,"EmissionB"=10,"EmissionC"=40,
                            "TaxA"=0,"TaxB"=1,"TaxC"=5)


Q11_B_Values <-  data.frame("PerformanceA"=0,"PerformanceB"=5,"PerformanceC"=10,
                            "EmissionA"=0,"EmissionB"=90,     "EmissionC"=10,
                            "TaxA"=0,     "TaxB"=2.5,           "TaxC"=0.5)


# ----------------------------------------------------------------------------------------------------------


Q12_A_Values <-  data.frame("PerformanceA"=0,"PerformanceB"=50,"PerformanceC"=5,
                            "EmissionA"=0,"EmissionB"=90,"EmissionC"=40,
                            "TaxA"=0,"TaxB"=1,"TaxC"=1)


Q12_B_Values <-  data.frame("PerformanceA"=0,"PerformanceB"=50,"PerformanceC"=10,
                            "EmissionA"=0,"EmissionB"=40,     "EmissionC"=90,
                            "TaxA"=0,     "TaxB"=2.5,           "TaxC"=5)


# ----------------------------------------------------------------------------------------------------------


## Here recoding the CE block respondents got
Pilot50$Q9 <- ifelse(grepl("IM_06", Pilot50$Q9, fixed = TRUE)==TRUE,0,1)
Pilot50$Q10 <- ifelse(grepl("IM_eW", Pilot50$Q10, fixed = TRUE)==TRUE,0,1)
Pilot50$Q11 <- ifelse(grepl("IM_9R", Pilot50$Q11, fixed = TRUE)==TRUE,0,1)
Pilot50$Q12 <- ifelse(grepl("IM_4P", Pilot50$Q12, fixed = TRUE)==TRUE,0,1)



# ----------------------------------------------------------------------------------------------------------
#### Section 5: Attitudinal Section ####
# ----------------------------------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------------------------------
## Rename attitude scales to easily identify questions
colnames(Pilot50)[which(names(Pilot50)=="Q31_1")] <- "ThreatToEnvironment"
colnames(Pilot50)[which(names(Pilot50)=="Q31_2")] <- "ThreatToSelf"
colnames(Pilot50)[which(names(Pilot50)=="Q31_3")] <- "ThreatTo10"
colnames(Pilot50)[which(names(Pilot50)=="Q31_4")] <- "ThreatTo25"
colnames(Pilot50)[which(names(Pilot50)=="Q31_5")] <- "ThreatTo50"


# ----------------------------------------------------------------------------------------------------------
## Rename the pandemic-displacement attitudinal scales
colnames(Pilot50)[which(names(Pilot50)=="Q32_1")] <- "PandemicEnvironment"
colnames(Pilot50)[which(names(Pilot50)=="Q32_2")] <- "PandemicMicroplastics"



## Make sure they are actual numbers:
## And code NA as zero
Pilot50[,c(24:30)] %<>% mutate_all(as.numeric)
Pilot50[,c(24:30)][is.na(Pilot50[,c(24:30)])]<- 0


# ----------------------------------------------------------------------------------------------------------
## Code coronavirus question to 0,1
colnames(Pilot50)[which(names(Pilot50)=="Q33")] <- "Coronavirus"
Pilot50$Coronavirus <- recode(Pilot50$Coronavirus,"No"=0,"Yes"=1)



# ----------------------------------------------------------------------------------------------------------
## Code charity question to 0,1
colnames(Pilot50)[which(names(Pilot50)=="Q34")] <- "Charity"
Pilot50$Charity <- recode(Pilot50$Charity,"No"=0,"Yes"=1)


# ----------------------------------------------------------------------------------------------------------
## Code consequentiality question to 0,1
colnames(Pilot50)[which(names(Pilot50)=="Q35")] <- "Consequential"
Pilot50$Consequential <- recode(Pilot50$Consequential,"No"=0,"Yes"=1)


# ----------------------------------------------------------------------------------------------------------
## Categorise education levels
colnames(Pilot50)[which(names(Pilot50)=="Q36")] <- "Education"
Pilot50$Education <- recode(Pilot50$Education,
                             "1 - 4 Levels / CSEs / GCSEs, NVQ Level 1."=1,
                             "5 + O Levels / CSEs / GCSEs, NVQ Level 2, AS Levels, Higher Diploma, Diploma Apprenticeship."=2,
                             "2 + A Levels, NVQ Level 3, BTEC National Diploma."=3,
                             "Degree, Higher Degree, NVQ level 4-5, BTEC Higher Level, professional qualifications (e.g. teaching, nursing, accountancy)."=4)


# ----------------------------------------------------------------------------------------------------------
## Rename and code income as levels
colnames(Pilot50)[which(names(Pilot50)=="Q37")] <- "Income"
## Remove weird characters:
Pilot50$Income <- gsub(pattern = "Â£",replacement = "",Pilot50$Income)
Pilot50$Income <- recode(Pilot50$Income,
                         " 501 - 1000" = 750,
                          " 1001- 1500 "=1250,
                          " 1501 - 2000 "=1750,
                          " 2001 - 2500 "=2250,
                          " 2501 - 3000 "=2750,
                          " 3001 - 4000 "=3500,
                          " 4001 - 5000 "=4500,
                          " 5001+ "=5000,
                          " Prefer not to say "=2500)

# ----------------------------------------------------------------------------------------------------------
## Rename survey understanding (1-10) and change to numbers
colnames(Pilot50)[which(names(Pilot50)=="Q38_1")] <- "Understanding"
Pilot50$Understanding %<>% as.numeric()


# ----------------------------------------------------------------------------------------------------------
## Putting Choices And Attributes Together

# ## Take all the respondent choices and change into a single vector
Choices <- data.frame(Choice = c(t(
  data.frame(rep(Pilot50[,19:22], times=1)))[,]))


Pilot50_NoCE <- Pilot50[,-c(19:22)]
## Combine the choices above with a `Task` variable and all the remaining data reshaped from long to wide
Reshaped <- (data.frame(rep((1:4),times=nrow(Pilot50)),
                        Choices,
                        data.frame(apply(X = Pilot50_NoCE,2,function(x) rep(x,each=4)))))
colnames(Reshaped)[1:2] <- c("Task","Choice") ## Note: The `Task` variable refers to which number of choice tasks the respondent did.


## Add the right levels for each respondents choice 
Reshaped<- rbind(
  Reshaped %>% filter(Task==1 & Q9==0) %>% mutate(Q9_A_Values),
  Reshaped %>% filter(Task==1 & Q9==1) %>% mutate(Q9_B_Values),
  
  Reshaped %>% filter(Task==2 & Q10==0) %>% mutate(Q10_A_Values),
  Reshaped %>% filter(Task==2 & Q10==1) %>% mutate(Q10_A_Values),
  
  Reshaped %>% filter(Task==3 & Q11==0) %>% mutate(Q11_A_Values),
  Reshaped %>% filter(Task==3 & Q11==1) %>% mutate(Q11_A_Values),
  
  Reshaped %>% filter(Task==4 & Q12==0) %>% mutate(Q12_A_Values),
  Reshaped %>% filter(Task==4 & Q12==1) %>% mutate(Q12_A_Values))


# The text has to be numerical but need to consider the correct coding.
## NOTE: Alt1 (Status Quo) ==1, Alt2 (B) ==2, Alt3 (C) ==3
Reshaped$Choice[Reshaped$Choice=="Option A"] <- 1
Reshaped$Choice[Reshaped$Choice=="Option B"] <- 2
Reshaped$Choice[Reshaped$Choice=="Option C"] <- 3


## APOLLO always needs these availability indicators in case any alternative is not available.
Reshaped$av_A <- rep(1,nrow(Reshaped)) ## Alternative A: Alt1
Reshaped$av_B <- rep(1,nrow(Reshaped)) ## Alternative B: Alt2
Reshaped$av_C <- rep(1,nrow(Reshaped)) ## Alternative C: Status Quo

## Add useful variables:
Reshaped$ID <- seq.int(nrow(Reshaped)) # Unique identifier for each respondent and choice (Uniques: 30,063)
Reshaped$Respondent <-rep(1:nrow(Pilot50),each=length(unique(Reshaped$Task))) # unique identifier for each respondent (Uniques: 3407)

# Reshaped$Zeroes<- NULL



# ----------------------------------------------------------------------------------------------------------
## Exporting Data:

Date <- gsub(pattern = "-",replacement = "_",Sys.Date())
write.csv(Pilot50,file = paste0("Pilot50_Transformed_",Date,".csv"))
write.csv(Reshaped,file = paste0("database_Transformed_",Date,".csv"))


## Next steps:
### Estimate CE:
#  source("Microplastics_Pilot50_MNL_2022_05_03.R")
### Estimate CV:
# source("Microplastics_Pilot50_CV_2022_05_03.R")


# End Of Script ----------------------------------------------------------------------------------------------------------
