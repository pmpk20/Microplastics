#### DEFRA: Microplastics ####
## Function: Spatial Setup
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 12/07/2022
## TODO: setup RENV


#------------------------------
# Replication Information: ####
# Selected output of 'sessionInfo()'
#------------------------------


# R version 4.1.3 (2022-03-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)
# [1] LC_COLLATE=English_United Kingdom.1252 
#   [1] magrittr_2.0.3     lubridate_1.8.0    tidygeocoder_1.0.5 PostcodesioR_0.3.1
# [5] DCchoice_0.1.0     here_1.0.1         forcats_0.5.1      stringr_1.4.0     
# [9] dplyr_1.0.8        purrr_0.3.4        readr_2.1.2        tidyr_1.2.0       
# [13] tibble_3.1.6       ggplot2_3.3.6      tidyverse_1.3.1   
#   [1] tidyselect_1.1.2 splines_4.1.3    haven_2.5.0      lattice_0.20-45 
# [5] colorspace_2.0-3 vctrs_0.4.1      generics_0.1.2   utf8_1.2.2      
# [9] survival_3.3-1   rlang_1.0.2      pillar_1.7.0     glue_1.6.2      
# [13] withr_2.5.0      DBI_1.1.2        MLEcens_0.1-5    dbplyr_2.1.1    
# [17] modelr_0.1.8     readxl_1.4.0     lifecycle_1.0.1  munsell_0.5.0   
# [21] gtable_0.3.0     cellranger_1.1.0 rvest_1.0.2      tzdb_0.3.0      
# [25] fansi_1.0.3      broom_0.8.0      scales_1.2.0     backports_1.4.1 
# [29] jsonlite_1.8.0   fs_1.5.2         Icens_1.66.0     interval_1.1-0.8
# [33] hms_1.1.1        stringi_1.7.6    grid_4.1.3       rprojroot_2.0.3 
# [37] cli_3.3.0        tools_4.1.3      perm_1.0-0.2     Formula_1.2-4   
# [41] crayon_1.5.1     pkgconfig_2.0.3  ellipsis_0.3.2   MASS_7.3-56     
# [45] Matrix_1.4-1     xml2_1.3.3       reprex_2.0.1     assertthat_0.2.1
# [49] httr_1.4.2       rstudioapi_0.13  R6_2.5.1         compiler_4.1.3 

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
library(readxl)
library(dplyr)
library(sf)
library(stringi)
library(stringr)
library(sp)
library(raster)
library(RColorBrewer)
library(tmap)
library(gstat)


#------------------------------
# Section 1: Import Data ####
# The long data and the shapefile 
#------------------------------


## If you CBA for the above, just import this and skip it:
AllData_Long_WithPostcodes <-  data.frame(read.csv("Microplastics_AllData_Long_PlusSpatial_2022_07_11.csv", encoding = "latin1")) ## Otherwise just import these two


## Start by reading in the new shapefile
### Available here: https://geoportal.statistics.gov.uk/datasets/ons::counties-and-unitary-authorities-december-2019-boundaries-uk-buc/about
GB <- st_read("Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp")
GB <- st_transform(GB,crs=4326) ## Comes in BNG so convert to LatLon



#---------------------------------------------
# Section 2: Add county data for matching #### 
#---------------------------------------------



AllData_Long_WithPostcodes$County <- rep(0,times=nrow(AllData_Long_WithPostcodes))

## Postcode county:
### Note: County and County2 are the same but the function is wrong for some reason so fix later
for (i in 1:nrow(AllData_Long_WithPostcodes)) {
  AllData_Long_WithPostcodes$County[i] <- ifelse(
    tryCatch({
      postcode_lookup(AllData_Long_WithPostcodes$PostcodesValidated[i])$admin_county
      1
    }, error = function(e)
      0) != 0,
    postcode_lookup(AllData_Long_WithPostcodes$PostcodesValidated[i])$admin_county,
    ifelse(
      tryCatch({
        postcode_lookup(AllData_Long_WithPostcodes$PostcodesValidated[i])$admin_district
        1
      }, error = function(e)
        0) != 0,
      postcode_lookup(AllData_Long_WithPostcodes$PostcodesValidated[i])$admin_district,
      0
    )
  )
}

AllData_Long_WithPostcodes$County2 <- rep(0,times=nrow(AllData_Long_WithPostcodes))



## Postcode county:
### Note: I am aggregating at county level here as too few observations at postcode level
for (i in 1:nrow(AllData_Long_WithPostcodes)) {
  AllData_Long_WithPostcodes$County2[i] <- ifelse(
    tryCatch({
      postcode_lookup(AllData_Long_WithPostcodes$PostcodesValidated[i])$admin_district
      1
    }, error = function(e)
      0) != 0,
    postcode_lookup(AllData_Long_WithPostcodes$PostcodesValidated[i])$admin_district,
    ifelse(
      tryCatch({
        postcode_lookup(AllData_Long_WithPostcodes$PostcodesValidated[i])$admin_county
        1
      }, error = function(e)
        0) != 0,
      postcode_lookup(AllData_Long_WithPostcodes$PostcodesValidated[i])$admin_county,
      0
    )
  )
}



## Short term fix for my county code mistake. This works:
AllData_Long_WithPostcodes$CountyCoalesced <- (coalesce(AllData_Long_WithPostcodes$County, 
                                                        AllData_Long_WithPostcodes$County2))







AllData_Long_WithPostcodes$Country <- rep(0,times=nrow(AllData_Long_WithPostcodes))


## Stolen from my other projects but this does a nice geolocation job:
for (i in 1:length(AllData_Long_WithPostcodes$Country)){
  AllData_Long_WithPostcodes$Country[i] <-
    ifelse(AllData_Long_WithPostcodes$PostcodesValidated[i]!=0,
           ifelse(
             postcode_validation(AllData_Long_WithPostcodes$PostcodesValidated[i]) == TRUE,
             postcode_lookup(AllData_Long_WithPostcodes$PostcodesValidated[i])$country,
             ifelse(
               is.null(postcode_query(AllData_Long_WithPostcodes$PostcodesValidated[i])[[1]]$postcode) == TRUE,
               0,
               postcode_lookup(postcode_query(AllData_Long_WithPostcodes$PostcodesValidated[i])[[1]]$postcode)$country)),
           ifelse(
             tryCatch({
               postcode_autocomplete(PCs[i])
               1
             }, 
             error=function(e) 0)==0,
             0,
             ifelse(AllData_Long_WithPostcodes$PostcodesValidated[i]!=0,AllData_Long_WithPostcodes$PostcodesValidated[i],postcode_lookup(postcode_autocomplete(AllData_Long_WithPostcodes$PostcodesValidated[i])[[1, 1]])$country)))
  
}







#------------------------------
# Section 3: Merge Data ####
#------------------------------


# 
# ## NOTE: 
# # unique(WinterTest2$County)[54] = "West Northamptonshire"
# ## Enter Large Missing County:
# Winter$County <- ifelse(
#   Winter$County == unique(Winter$County)[58],
#   "Northamptonshire",
#   Winter$County)


AllData_Long_WithPostcodes_Trimmed <- AllData_Long_WithPostcodes[AllData_Long_WithPostcodes$PostcodesValidated!=0,]


## Join county name to county name
GB_Microplastics <- left_join(x = GB,AllData_Long_WithPostcodes_Trimmed,by=c("ctyua19nm"="CountyCoalesced"))

## Remove missing country matches
GB_Microplastics_Trimmed <- GB_Microplastics[is.na(GB_Microplastics$Country)!=TRUE,]


#----------------------------------------------------------------------------------------------------------
#### Section 3B: Export GB_Microplastics shapefile which has data per polygon #### 
#----------------------------------------------------------------------------------------------------------


write.csv(AllData_Long_WithPostcodes,  file = "Microplastics_AllData_Long_PlusSpatial_2022_07_12.csv",fileEncoding = "latin1")
st_write(GB_Microplastics,"GB_Microplastics_2022_07_12.gpkg",append=FALSE) ## NOTE: UPDATE DATE ON WHICH NEW VERSION CREATED


#------------------------------
# Section 4: Plot Data ####
#------------------------------


## Plotting Woodlands and Respondents Together: 
Microplastics_Respondents <- ggplot(data = GB_Microplastics_Trimmed) +
  geom_sf() +
  geom_point(
    aes(y = PostcodesLatitudes, x = PostcodesLongitudes,colour = factor(Country)),
    size = 1,shape=4) +scale_color_manual(name="Country",values=c("red","green","blue","black"),
                                          labels=c("England\n(N: 1290)",
                                                   "Northern Ireland\n(N: 33)",
                                                   "Scotland\n(N: 139)",
                                                   "Wales\n(N: 71)"))+
  coord_sf(
    xlim = c(st_bbox(GB_Microplastics_Trimmed)["xmin"], st_bbox(GB_Microplastics_Trimmed)["xmax"]),
    ylim = c(st_bbox(GB_Microplastics_Trimmed)["ymin"], st_bbox(GB_Microplastics_Trimmed)["ymax"]),
    expand = FALSE
  )+theme(aspect.ratio = 5/5)+
  xlab(label = 'Longitude') +
  ylab(label = 'Latitude') +
  ggtitle("Location of Respondents")+
  labs( color = "Country:")



ggsave(Microplastics_Respondents, device = "jpeg",
       filename = "Microplastics_Respondents_2022_07_12.jpeg",
       width=20,height=15,units = "cm",dpi=1000)


#------------------------------
# Section 5: IDW Plot ####
#------------------------------

GB2 <- GB_Microplastics_Trimmed[!is.na(GB_Microplastics_Trimmed$ModelWTP),]


## Create Grid over map
P <- as(GB2,"Spatial")
# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)


#----------------------------------------------------------------------------------------------------------
#### Section 2: IDW Plots ####
#----------------------------------------------------------------------------------------------------------


## Estimate and prepare interpolation for each attribute:
Colour_IDW <- gstat::idw(ModelWTP ~ 1, P, newdata=grd, idp=2)
Colour_IDW_Raster       <- raster(Colour_IDW)
Colour_IDW_Raster_Mask     <- raster::mask(Colour_IDW_Raster, GB2)
Colour_IDW_Plot_Winter <- tm_shape(Colour_IDW_Raster_Mask) + 
  tm_raster(n=5, palette=brewer.pal(5, "YlOrRd"),
            title="Colour_IDW_Winter") +tm_legend(legend.outside=TRUE)
Colour_IDW_Plot_Winter


# End Of Script ----------------------------------------------------------------------------------------------------------