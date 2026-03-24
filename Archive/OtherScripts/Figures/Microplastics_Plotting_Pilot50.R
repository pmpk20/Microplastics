#### DEFRA: Microplastics ####
## Function: Plots attitudes
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 08/05/2022
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
library(magrittr)
library(dplyr)
library(apollo)
library(reshape2)
library(ggplot2)
library(ggridges)
library(tidyr)
library(sf)



#------------------------------
# Section 1: Import Data ####
#------------------------------


Pilot50 <- data.frame(read.csv("Pilot50_Transformed_2022_05_08.csv"))



#------------------------------
# Section 2: Density Plots ####
#------------------------------


Attitudes <- Pilot50[,26:29]
Attitudes_Melted <- melt(Attitudes)

## Density Plot Of Attitudes
Attitudes_Melted %>% ggplot(aes(x = value, y = variable, group=variable,fill = variable)) + 
  geom_density_ridges()


#------------------------------
# Section 3: Line Plots ####
#------------------------------


Attitudes_Melted$variable <- recode(Attitudes_Melted$variable,"ThreatToSelf"=0,"ThreatTo10"=1,"ThreatTo25"=2,"ThreatTo50"=3)

## Plot Estimated Harm By Time
Attitudes_Melted %>% ggplot() + 
  stat_smooth(aes(y = value, x = variable, colour ="Harm"), se = TRUE) +
  ggtitle("Expected Harm By Duration") +
  scale_x_continuous(
    labels = c("Now", "10 Years", "25 Years", "50 Years"),
    name = "Time Period",
    breaks = c(0, 1, 2, 3)) +
  scale_y_continuous(name = "Harm on a scale -5 to +5.",
                     limits = c(-5, 5),
                     breaks = seq(-5, 5, 1)) + 
  labs(colour = "Mean Value") + 
  scale_colour_manual(breaks =c("Harm"),values = c("red"))



#------------------------------
# Section 4: Map Of Responses ####
#------------------------------



# Import Data: #------------------------------

GB <- st_read("Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp")
GB <- st_transform(GB,crs=4326) ## Comes in BNG so convert to LatLon


# Join Data: #------------------------------
colnames(Pilot50)[which(names(Pilot50)=="X")] <- "ID"
Pilot50_Spatial <- as.data.frame(Pilot50) %>% 
  st_as_sf(coords=c("Longitude","Latitude"), crs=4326, remove=FALSE)  

GB_Pilot <- st_join(GB, left = FALSE, Pilot50_Spatial)
# GB_Pilot <- st_join(Pilot50_Spatial, left = FALSE, GB) # join points


# Plot Respondents Location: #------------------------------
ggplot() +
  geom_sf(data=GB) +
  geom_point(data=GB_Pilot,
             aes(x = Longitude, y = Latitude,colour="Response"),
             size = 5,shape=4
  ) +
  coord_sf(
    xlim = c(st_bbox(GB)["xmin"], st_bbox(GB)["xmax"]),
    ylim = c(st_bbox(GB)["ymin"], st_bbox(GB)["ymax"]),
    expand = FALSE
  )+theme(aspect.ratio = 5/5)+
  labs(colour="Respondents")+
  xlab(label="Longitude")+
  ylab(label="Latitude")+
  scale_color_manual(values = c("Response" = 'black'))+
  ggtitle("Location Of Responses.")


## Colour polygons by income:
ggplot() +
  geom_sf(data=GB)+
  geom_sf(data = GB_Pilot, aes(fill = Income))
