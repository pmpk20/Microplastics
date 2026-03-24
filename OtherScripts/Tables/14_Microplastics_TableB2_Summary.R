#### Microplastics: IOP Paper ####
## Function: Table B2
## Author: PK
## Last change:
## TODO: Add table content here


# *****************************
# Replication Information: ####
# Selected output of 'sessionInfo()'
# *****************************


## Libraries here: -----------------------------------------------------------------
library(data.table)
library(magrittr)
library(dplyr)
library(tidyverse)
library(here)


# *****************************
# Section 1: Import Data ####
# *****************************


Data <-
  here("Data", "Data_WithEOP_24_12_01.csv") %>%
  fread() %>%
  data.frame()


# *****************************
# Section 2: Construct Table ####
# *****************************


## TODO: Add code to construct Table B2 here


# *****************************
# Section 3: Export Table ####
# *****************************


## TODO: Export table output here
## e.g. here("CVoutput", "Tables", "TableB2.txt")
