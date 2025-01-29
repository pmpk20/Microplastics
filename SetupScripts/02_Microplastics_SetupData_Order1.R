#### DEFRA: Microplastics ####
## Function: Imports and transforms the 1101 from prolific
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 29/01/25
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
library(readxl)
library(janitor)
library(data.table)


# ************************************************
# Section 1: Import Data ####
# ************************************************

# Set the working directory using the 'here' package - ensures portability
here()

# Import participant-level covariate data from Excel
# WAS: kent_eapmicro_live_covariates_2022_06_15.xlsx
participant_covariates <-
  here("Data/Raw data",
       "kent_eapmicro_Order1_covariates_anonymised.csv") %>%
  fread() %>% 
  data.frame()

# Import discrete choice experiment (DCE) data from Excel
# Was: kent_eapmicro_live_dce_2022_06_15.xlsx
dce_data <-
  here("Data/Raw data", 
       "kent_eapmicro_Order1_dce_anonymised.csv") %>%
  fread() %>% 
  data.frame()

# Import timing data from Excel
# Was: KENT_EAPMICRO_Live_pages_2022_06_15.xlsx
timing_data <-
  here("Data/Raw data", 
       "KENT_EAPMICRO_Order1_pages_anonymised.csv") %>%
  fread() %>% 
  data.frame()


# ************************************************
# Section 2: Rename columns in DCE data ####
# ************************************************


# Rename columns of the DCE dataset for clarity
colnames(dce_data) <-
  c(
    "participant_id",      # Unique identifier for each participant
    "design_row",          # Row number of the design
    "scenario_id",         # Unique identifier for each choice scenario
    "sequence_number",     # Sequence number of the choice task
    "choice",              # Participant's choice (1, 2, or 3)
    "alt1_x1",             # Attribute X1 for alternative 1
    "alt1_x2",             # Attribute X2 for alternative 1
    "alt1_x3",             # Attribute X3 for alternative 1
    "alt2_x1",             # Attribute X1 for alternative 2
    "alt2_x2",             # Attribute X2 for alternative 2
    "alt2_x3",             # Attribute X3 for alternative 2
    "alt3_x1",             # Attribute X1 for alternative 3
    "alt3_x2",             # Attribute X2 for alternative 3
    "alt3_x3"              # Attribute X3 for alternative 3
  )


# ************************************************
# Section 3: Reshape DCE and append ####
# ************************************************

# Reshape DCE data from long to wide format
dce_data_wider <- dce_data %>%
  pivot_wider(
    id_cols = participant_id,
    names_from = scenario_id,
    values_from = c(
      choice,
      alt1_x1,
      alt1_x2,
      alt1_x3,
      alt2_x1,
      alt2_x2,
      alt2_x3,
      alt3_x1,
      alt3_x2,
      alt3_x3
    ),
    names_glue = "scenario_{scenario_id}_{.value}" #Explicitly sets the new names.
  )

# Filter covariates to keep only completed responses (STATUS == 7)
participant_covariates_trimmed <- participant_covariates %>%
  dplyr::filter(STATUS == 7)

# Filter wide-format DCE data to match completed responses
dce_data_wider_trimmed <- dce_data_wider %>%
  dplyr::filter(participant_id %in% participant_covariates_trimmed$RID)

# Combine the trimmed covariates and DCE data
combined_data_trimmed <- dplyr::bind_cols(participant_covariates_trimmed,
                                          dce_data_wider_trimmed)

# Drop unnecessary columns
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::select( -STATUS,-HTTP_URL,-SESSION,-START,-SRC,-EXTID,-consent)

# Recode and rename gender variable
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(
    Gender = dplyr::recode(
      q2,
      '1' = 0,
      '2' = 1,
      "3" = 2,
      "4" = 3
    )
  ) %>%
  dplyr::select(-q2) #Drop old column.

# Rename survey demographic variables
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::rename(
    AgeBracket = q3,
    Postcode = q4,
    Ethnicity = q5,
    MeanExpectedCurrent = q89_1,
    MeanExpectedFuture = q89_2,
    MeanExpectedBest = q89_3,
    MeanExpectedWorst = q89_4,
    Variance = q7,
    BidLevelFrequency = bid_random,
    CVProtests = q11,
    CVProtestTexts = q11_other,
    Q16_ClimateCurrentEnvironment = q16_1,
    Q16_ClimateCurrentSelf = q16_2,
    Q16_MicroplasticsCurrentEnvironment = q16_3,
    Q16_MicroplasticsCurrentSelf = q16_4,
    Q16_MicroplasticsTen = q16_5,
    Q16_MicroplasticsTwentyFive = q16_6,
    Q16_MicroplasticsFifty = q16_7,
    Q17_PandemicEnvironment = q17_1,
    Q17_PandemicMicroplastics = q17_2,
    Coronavirus = q18,
    Charity = q19,
    Consequentiality = q20,
    ConsequentialityText = q20_other,
    Education = q21,
    Income = q22,
    Understanding = q23,
    WaterBills = q9,
    CouncilTax = q10,
    FreeText = q24
  )

# Recode water bills
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(
    WaterBills = dplyr::recode(
      WaterBills,
      '1' = 0,
      '2' = 100,
      "3" = 200,
      "4" = 300,
      "5" = 400,
      "6" = 500,
      "7" = 600,
      "8" = 700,
      "9" = 800,
      "10" = 900,
      "11" = 1000,
      "12" = 0
    )
  )

# Fix Bid Levels
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(
    Bid = as.numeric(gsub(
      BidLevelFrequency,
      pattern = "£",
      replacement = ""
    ))
  )

# Merge and recode CV question
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(
    CV = dplyr::recode(
      dplyr::coalesce(WaterBills, CouncilTax),
      '1' = 1,
      '2' = 0
    )
  ) 


# Recode sociodemographic questions
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(
    Coronavirus = dplyr::recode(Coronavirus, '1' = 1, '2' = 0),
    Charity = dplyr::recode(Charity, '1' = 1, '2' = 0),
    Consequentiality =  dplyr::recode(Consequentiality, '1' = 1, '2' = 0)
  )

# Recode education levels
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(
    Education = dplyr::if_else(Education == 6, 4, Education)
  )

# Recode income levels
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(
    Income = dplyr::recode(
      Income,
      "1" = 250,
      "2" = 750,
      "3" = 1250,
      "4" = 1750,
      "5" = 2250,
      "6" = 2750,
      "7" = 3500,
      "8" = 4500,
      "9" = 5000,
      "10" = 2500
    )
  )


# ************************************************
# Section 3.1: Additional Transformations ####
# ************************************************



## Specify here
combined_data_trimmed$Order <- 1


# Calculate annual income from monthly income
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(Income_Annual = Income * 12)


# Copy reported variance to a new column for clarity
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(Variance_StatedConfidenceLevel = Variance)


# Convert reported variance to standard deviation (SD), assuming the 
# Variance variable is a categorical measure of variance.
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(
    Variance_ConfidenceAsSD = dplyr::case_match(
      Variance,
      1 ~ 0,
      2 ~ 2,
      3 ~ 6,
      .default = 10
    ) / 4
  )


# Square the standard deviation to get variance
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(Variance_ConfidenceAsVariance = Variance_ConfidenceAsSD^2)


# Calculate individual level variance
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(
    Variance_IndividualLevel = ((MeanExpectedBest - MeanExpectedFuture)^2 +
                                  (MeanExpectedWorst - MeanExpectedFuture)^2) / 2
  )


# Calculate the sum of the current and future mean expectations
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(Mean_Change = MeanExpectedCurrent + MeanExpectedFuture)


# Scale mean expected values to a midpoint of 5
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(
    MeanExpectedCurrent_Scaled = MeanExpectedCurrent + 5,
    MeanExpectedFuture_Scaled = MeanExpectedFuture + 5
  )


# Calculate mean plus and minus the reported variance for bounds
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(
    Mean_MinusLowerBound = MeanExpectedFuture - Variance,
    Mean_PlusUpperBound = MeanExpectedFuture + Variance
  )


# Calculate the sample mean of expected future
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(MeanExpectedFuture_SampleMean = mean(MeanExpectedFuture, na.rm = TRUE))


# Calculate the difference between climate and microplastics
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(Q16_Comparison = Q16_ClimateCurrentSelf - Q16_MicroplasticsCurrentSelf)


# Calculate Log bid to income ratio
combined_data_trimmed <- combined_data_trimmed %>%
  dplyr::mutate(LogBidIncome = log((Income_Annual - Bid) / Income_Annual))


# ************************************************
# Section 4: Putting Choices and Attributes Together ####
# ************************************************


# Create a long format for choices
choices_long <- combined_data_trimmed %>%
  dplyr::select(dplyr::ends_with("Choice")) %>% 
  dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "choice_number",
    values_to = "choice",
    values_drop_na = TRUE
  ) %>%
  dplyr::select(choice)



# Create a 'Task' variable
task_variable <- rep(1:4, times = nrow(combined_data_trimmed))

# Combine the choices with the task variable and the rest of the data
combined_data_no_ce <- combined_data_trimmed %>%
  dplyr::select(-dplyr::starts_with("scenario_"),
                -dplyr::ends_with("Choice"))

combined_reshaped <- combined_data_no_ce %>%
  dplyr::slice(rep(1:dplyr::n(), each = 4)) %>% #Repeat each row 4 times.
  dplyr::mutate(Task = task_variable,
                Choice = choices_long$choice,
                av_A = 1,  # Alternative A availability indicator
                av_B = 1,  # Alternative B availability indicator
                av_C = 1,  # Alternative C availability indicator
                ID = 1:dplyr::n(), # Unique identifier for each observation
                Respondent = rep(1:nrow(combined_data_trimmed), each = 4) # Unique identifier for each respondent
  )

# Create a trimmed DCE dataframe to get attributes
dce_trimmed <- dce_data %>%
  dplyr::filter(participant_id %in% combined_data_trimmed$RID) %>%
  dplyr::mutate(CEBlock = dplyr::if_else(design_row < 5, 1, 2))

# Rename attribute columns
attributes <- dce_trimmed %>%
  dplyr::select(alt1_x1:alt3_x3) %>%
  dplyr::rename(
    Performance_A = alt1_x1,
    Emission_A = alt1_x2,
    Price_A = alt1_x3,
    Performance_B = alt2_x1,
    Emission_B = alt2_x2,
    Price_B = alt2_x3,
    Performance_C = alt3_x1,
    Emission_C = alt3_x2,
    Price_C = alt3_x3
  )

# Combine reshaped data with attributes
database <- dplyr::bind_cols(combined_reshaped, attributes)


# Remove unnecessary columns from the database
database <- database %>%
  dplyr::select(
    -CVProtests,
    -CVProtestTexts,
    -ConsequentialityText,
    -FreeText
  )


# Replace NA values with zero
database <- database %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0)))


# ************************************************
# Section 5: Recoding attributes ####
# ************************************************

# Function to recode attributes
recode_attributes <- function(data) {
  data %>%
    dplyr::mutate(
      Price_A = dplyr::recode(Price_A, '1' = 0.00),
      Price_B = dplyr::recode(Price_B, '1' = 0.50, '2' = 1.00, "3" = 2.50, "4" = 5.00),
      Price_C = dplyr::recode(Price_C, '1' = 0.50, '2' = 1.00, "3" = 2.50, "4" = 5.00),
      Performance_A = dplyr::recode(Performance_A, '1' = 0.00),
      Performance_B = dplyr::recode(Performance_B, '1' = 5, '2' = 10, "3" = 50),
      Performance_C = dplyr::recode(Performance_C, '1' = 5, '2' = 10, "3" = 50),
      Emission_A = dplyr::recode(Emission_A, '1' = 0.00),
      Emission_B = dplyr::recode(Emission_B, '1' = 10, '2' = 40, "3" = 90),
      Emission_C = dplyr::recode(Emission_C, '1' = 10, '2' = 40, "3" = 90)
    )
}
# Apply recoding
database <- recode_attributes(database)

# ************************************************
# Section 6: Exporting ####
# ************************************************


## This is one respondent per row
combined_data_trimmed %>% 
  data.frame() %>%
  fwrite(sep = ",", 
         here("Data", "Microplastics_Order1_Wide_anonymised_V2.csv"))

## This is one choice per row
database %>% 
  data.frame() %>%
  fwrite(sep = ",", 
         here("Data", "Microplastics_Order1_Long_anonymised_V2.csv"))



## Next steps:
### Estimate CE:
#  source("Microplastics_FinalPilot25_MNL_2022_05_03.R")
### Estimate CV:
# source("Microplastics_FinalPilot25_CV_2022_05_03.R")

# ************************************************
# Section 7: Saving Session Information ####
# ************************************************

# Create file name for the session information
session_file_name <- paste0("session_info_", 
                            format(Sys.Date(), 
                                   "%Y_%m_%d"), 
                            ".txt")

# Save the session information into a text file in the Data Folder
sessioninfo::session_info() %>%
  capture.output(file = here("Data", session_file_name))


# End Of Script ----------------------------------------------------------------------------------------------------------