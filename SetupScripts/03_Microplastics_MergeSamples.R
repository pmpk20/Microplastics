#### DEFRA: Microplastics ####
## Function: Imports and transforms the 1101 from prolific
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 30/01/25
## Notes:
## - Thanks gemini for adding comments


# ************************************************
# Replication Information: ####
# ************************************************
# Session information is saved to a separate file 'session_info.txt'

# Clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(here)
library(data.table)
library(sessioninfo)


# ************************************************
# Section 1: Import Data ####
# ************************************************

# Set the working directory using the 'here' package - ensures portability
here()



# Import data files, specifying the date object for consistency
order0_wide <-
  here("Data", "Microplastics_Order0_Wide_anonymised_V2.csv") %>%
  fread() %>%
  data.frame()

order0_long <-
  here("Data", "Microplastics_Order0_Long_anonymised_V2.csv") %>%
  fread() %>%
  data.frame()

order1_wide <-
  here("Data", "Microplastics_Order1_Wide_anonymised_V2.csv") %>%
  fread() %>%
  data.frame()

order1_long <-
  here("Data", "Microplastics_Order1_Long_anonymised_V2.csv") %>%
  fread() %>%
  data.frame()


# ************************************************
# Section 2: Merge Wide Data ####
# ************************************************

# Combine both dataframes by rows
all_data_wide <- dplyr::bind_rows(order0_wide, order1_wide)


all_data_wide$AgeDummy <- ifelse(all_data_wide$AgeBracket <= 
                                           median(all_data_wide$AgeBracket), 
                                         0,
                                         1)

## Rename
all_data_wide$SurveyCompletionTime <- all_data_wide$DURATION %>% as.numeric()


## Add survey speeding data
all_data_wide <-
  all_data_wide %>%
  mutate("Speeders_Survey_Threshold" =
           median(SurveyCompletionTime) %>% multiply_by(0.48),
         "Slowers_Survey_Threshold" =
           median(SurveyCompletionTime) %>% multiply_by(1.48),
         
         
         "Speeders_Survey_TestDummy" = ifelse(
           SurveyCompletionTime <= Speeders_Survey_Threshold, 0, 1),
         
         "Slowers_Survey_TestDummy" = ifelse(
           SurveyCompletionTime >= Slowers_Survey_Threshold, 0, 1)
  )


## NewMEC
all_data_wide$AdjustedMEC <- ((all_data_wide$MeanExpectedFuture + all_data_wide$MeanExpectedCurrent) + 10.001)/20.002 
all_data_wide$VarianceChange <- (all_data_wide$VarianceLowerBound - all_data_wide$VarianceUpperBound)



# ************************************************
# Section 3: Merge Long Data ####
# ************************************************

# Combine both dataframes by rows
all_data_long <- dplyr::bind_rows(order0_long, order1_long)


## Adding to long by reshaping from wide
all_data_long$AgeDummy <- rep(x = all_data_wide$AgeDummy, each = 4)
all_data_long$AdjustedMEC <- rep(x = all_data_wide$AdjustedMEC, each = 4)
all_data_long$VarianceChange <- rep(x = all_data_wide$VarianceChange, each = 4)


# ************************************************
# Section 4: Export all Data ####
# ************************************************


# Save all data
all_data_long %>% fwrite(
  here("Data",
       "Microplastics_AllData_Long_Anonymised.csv"),
  sep = ",",
  row.names = FALSE
)


all_data_wide %>% fwrite(
  here("Data",
       "Microplastics_AllData_Wide_Anonymised.csv"),
  sep = ",",
  row.names = FALSE
)


# ************************************************
# Section 5: Saving Session Information ####
# ************************************************

# Create file name for the session information
session_file_name <- paste0("session_info_", format(Sys.Date(), "%Y_%m_%d"), ".txt")


# Save the session information into a text file in the Data Folder
sessioninfo::session_info() %>%
  capture.output(file = here("Data", session_file_name))


# End Of Script ----------------------------------------------------------------------------------------------------------