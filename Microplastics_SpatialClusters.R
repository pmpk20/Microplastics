#### Microplastics: Plotting Spatial Clusters  ###############
# Script author: Peter King (p.m.king@kent.ac.uk)
# Last Edited: 12/07/2022


library(scales)
library(dplyr)
library(magrittr)
library(gstat)
library(spdep)
library(gplots)
rm(list=ls())


#----------------------------------------------------------------------------------------------------------
#### Section 0: Setup ####
#----------------------------------------------------------------------------------------------------------


## New code which preserves more data
GB_Microplastics <- st_read("GB_Microplastics_2022_07_12.gpkg")
GB_Microplastics <- st_transform(GB_Microplastics, crs = 4326)
GB_Microplastics_Trimmed <- GB_Microplastics[is.na(GB_Microplastics$Country)!=TRUE,]


# GB_Microplastics_Trimmed <- GB_Microplastics[!is.na(GB_Microplastics$ColourWTPNew),]
# GB_Microplastics_Trimmed <- GB_Microplastics_Trimmed[!is.na(GB_Microplastics_Trimmed$MilesDistance), ]
GB_Microplastics_Trimmed <- GB_Microplastics_Trimmed[!is.na(GB_Microplastics_Trimmed$PostcodesLongitudes), ]
GB_Microplastics_Trimmed <- GB_Microplastics_Trimmed[!is.na(GB_Microplastics_Trimmed$PostcodesLatitudes), ]
GB_Microplastics_Trimmed <- GB_Microplastics_Trimmed[which(!duplicated(GB_Microplastics_Trimmed$PostcodesLatitudes)), ]
GB_Microplastics_Trimmed <- GB_Microplastics_Trimmed[which(!duplicated(GB_Microplastics_Trimmed$PostcodesLongitudes)), ]

Data <- GB_Microplastics_Trimmed 



Woodlands <- data.frame(cbind(Data$PostcodesLatitudes, abs(Data$PostcodesLongitudes)))
Coords <- data.matrix(Woodlands) #converting into matrix class
kw10kmW <- knearneigh(Coords, k = sqrt(nrow(Data)))
KNN <- knn2nb(kw10kmW)
Dists <- nbdists(KNN, Coords)
Dists_W <- lapply(Dists, function(x)
  1 / (x ^ 2))
KNN_Weights <- nb2listw(KNN, glist = Dists_W)




#-----------------------------------------------------#
#### Section 1: Plotting Local Moran Statistics ####
#-----------------------------------------------------#

Plotspots <- function(WTP,Data) {
  
  K = sqrt(nrow(Data))
  
  # Woodlands <- data.frame(cbind(Data$PostcodesLatitudes,abs(Data$PostcodesLongitudes)))
  Woodlands <- data.frame(cbind(Data$PostcodesLatitudes, abs(Data$PostcodesLongitudes)))
  crdsmatW <- data.matrix(Woodlands) #converting into matrix class
  kw10kmW <- knearneigh(crdsmatW, k = K)
  kw10kmnbW <- knn2nb(kw10kmW)
  distW <- nbdists(kw10kmnbW, crdsmatW)
  dist2W <- lapply(distW, function(x)
    1 / (x ^ 2))
  kw10kmdist2W <- nb2listw(kw10kmnbW, glist = dist2W)
  kw10kmdist2 <- kw10kmdist2W
  
  WTP <- c(WTP)
  locm_WTP <- localmoran(as.data.frame(Data[,c(WTP)])[,1], kw10kmdist2) 
  summary(locm_WTP)
  Data$ScaleWTP <- as.numeric(scale(as.data.frame(Data[,c(WTP)])[,1]))
  Data$lag_WTP <- as.numeric(lag.listw(kw10kmdist2, Data$ScaleWTP))
  Data[(Data$ScaleWTP >= 0 & Data$lag_WTP <= 0) & (locm_WTP[, 5] <= 0.05), "quad_sig_WTP"] <- 3
  Data[(Data$ScaleWTP >= 0 & Data$lag_WTP <= 0) & (locm_WTP[, 5] <= 0.05), "quad_sig_WTP"] <- 4
  Data[(Data$ScaleWTP <= 0 & Data$lag_WTP >= 0) & (locm_WTP[, 5] <= 0.05), "quad_sig_WTP"] <- 5
  Data[(Data$ScaleWTP >= 0 & Data$lag_WTP >= 0) & (locm_WTP[, 5] <= 0.05), "quad_sig_WTP"] <- 1
  Data[(Data$ScaleWTP <= 0 & Data$lag_WTP <= 0) & (locm_WTP[, 5] <= 0.05), "quad_sig_WTP"] <- 2
  Data$quad_sig_WTP <- ifelse(is.na(Data$quad_sig_WTP)==TRUE,5,Data$quad_sig_WTP)
  
  # Data <- cbind(Data,"ScaleWTP"=Data2$ScaleWTP,"lag_WTP"=Data2$lag_WTP,"quad_sig_WTP"=Data2$quad_sig_WTP)
  # print(table(Data$quad_sig_WTP))
  breaks <- seq(1, 5, 1)
  labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
  np <- findInterval(Data$quad_sig_WTP, breaks)
  colors <- c("red", "blue", "yellow", "skyblue2", "white")
  Data$Colors <- as.character(col2hex(colors[np]))
  
  
  Plotname <- paste0("Plot_",substr(as.character(WTP),start = 1,stop=nchar(WTP)-6))
  
  Plotname <- tm_shape(Data)+
    tm_polygons(col="Colors")+tm_layout(legend.position = c("left","top"),panel.label.size = 0.5)+
    tm_add_legend(title="Cluster Type",labels = labels,col = colors)
  
  return(Plotname)
}


Plotspots("ModelWTP", Data)
Plotspots("b_Performance_10.post.mean", Data)
Plotspots("b_Performance_50.post.mean", Data)
Plotspots("b_Emissions_40.post.mean", Data)
Plotspots("b_Emissions_90.post.mean", Data)

#----------------------------------------------------------------------------------------------------------
#### Section 1B: Estimating Moran Statistics ####
#----------------------------------------------------------------------------------------------------------








## Using lm.MoranTest to check if covariates make a difference:
CVWTPMoran <- lm.morantest(lm(ModelWTP           ~ AgeDummy+ Gender+ EthnicityDummy+
                                    MeanExpectedFuture+ IncomeDummy+
                                    Charity+ Coronavirus+ Consequentiality+
                                    DURATION+ Order+ Understanding, 
                                  data=Data),KNN_Weights)
Performance10Moran <- lm.morantest(lm(b_Performance_10.post.mean       ~ AgeDummy+ Gender+ EthnicityDummy+
                                      MeanExpectedFuture+ IncomeDummy+
                                      Charity+ Coronavirus+ Consequentiality+
                                      DURATION+ Order+ Understanding, 
                                    data=Data),KNN_Weights)
Performance50Moran <- lm.morantest(lm(b_Performance_50.post.mean       ~ AgeDummy+ Gender+ EthnicityDummy+
                                      MeanExpectedFuture+ IncomeDummy+
                                      Charity+ Coronavirus+ Consequentiality+
                                      DURATION+ Order+ Understanding, 
                                    data=Data),KNN_Weights)
Emissions40Moran <- lm.morantest(lm(b_Emissions_40.post.mean     ~ AgeDummy+ Gender+ EthnicityDummy+
                                       MeanExpectedFuture+ IncomeDummy+
                                       Charity+ Coronavirus+ Consequentiality+
                                       DURATION+ Order+ Understanding, 
                                     data=Data),KNN_Weights)
Emissions90Moran <- lm.morantest(lm(b_Emissions_90.post.mean ~ AgeDummy+ Gender+ EthnicityDummy+
                                         MeanExpectedFuture+ IncomeDummy+
                                         Charity+ Coronavirus+ Consequentiality+
                                         DURATION+ Order+ Understanding, 
                                       data=Data),KNN_Weights)

cbind(rbind("Stat"=round(CVWTPMoran$estimate[1],3),"P.V"=round(CVWTPMoran$p.value,3)),
      rbind("Stat"=round(Performance10Moran$estimate[1],3),"P.V"=round(Performance10Moran$p.value,3)),
      rbind("Stat"=round(Performance50Moran$estimate[1],3),"P.V"=round(Performance50Moran$p.value,3)),
      rbind("Stat"=round(Emissions40Moran$estimate[1],3),"P.V"=round(Emissions40Moran$p.value,3)),
      rbind("Stat"=round(Emissions90Moran$estimate[1],3),"P.V"=round(Emissions90Moran$p.value,3)))

#----------------------------------------------------------------------------------------------------------
#### Section 2: Gi* Statistics ####
#----------------------------------------------------------------------------------------------------------


Data$GiModelWTP <- as.numeric(unlist(data.frame("GiModelWTP"=as.numeric(localG(Data$ModelWTP, KNN_Weights)))))
Data$MoranModelWTP <- as.numeric(unlist(data.frame("MoranModelWTP"=as.numeric(localmoran(Data$ModelWTP, KNN_Weights)[,5]))))

tm_shape(Data) + tm_fill("GiModelWTP",palette=brewer.pal(5, "YlOrRd"),
                                    midpoint=NA,size = 0.4,shape =21,
                                    title="ModelWTP")+tm_layout(legend.outside=TRUE)

tm_shape(Data) + tm_fill("MoranModelWTP",palette=brewer.pal(5, "YlOrRd"),
                         midpoint=NA,size = 0.4,shape =21,
                         title="ModelWTP")+tm_layout(legend.outside=TRUE)





#----------------------------------------------------------------------------------------------------------
#### Section X: Is Data Clustered? ####
#----------------------------------------------------------------------------------------------------------


Clusters <- function(Data) {
  Plotname <- paste0(ifelse(
    length(unique(Data$Season)) > 1,
    paste0("AllSeasons"),
    paste0(ifelse(
      unique(Data$Season) == 0,
      "Winter",
      ifelse(unique(Data$Season) == 1, "Spring", "Summer")
    ))))
  Variable <- paste0(Plotname,"PVs")
  
  Woodlands_Plotname <- data.frame(cbind(Data$PostcodesLatitudes, abs(Data$PostcodesLongitudes)))
  crds_Plotname <- data.matrix(Woodlands_Plotname) #converting into matrix class
  kw10_Plotname <- knearneigh(crds_Plotname, k = 5)
  kw10kmnb_Plotname <- knn2nb(kw10_Plotname)
  dist_Plotname <- nbdists(kw10kmnb_Plotname, crds_Plotname)
  dist2_Plotname <- lapply(dist_Plotname, function(x)
    1 / (x ^ 2))
  kw10kmdist_Plotname <- nb2listw(kw10kmnb_Plotname, glist = dist2_Plotname)
  
  ColourMoran_Plotname <- moran.mc(Data$ColourWTPNew,kw10kmdist_Plotname,nsim = 500)
  SmellMoran_Plotname <- moran.mc(Data$SmellWTPNew,kw10kmdist_Plotname,nsim = 500)
  SoundMoran_Plotname <- moran.mc(Data$SoundWTPNew,kw10kmdist_Plotname,nsim = 500)
  DeadwoodMoran_Plotname <- moran.mc(Data$DeadwoodWTPNew,kw10kmdist_Plotname,nsim = 500)
  TaxMoran_Plotname <- moran.mc(Data$TaxWTPNew,kw10kmdist_Plotname,nsim = 500)
  
  
  ColourLocalMoran_Plotname <-   lm.morantest(lm(ColourWTPNew           ~ WoodlandsScore+MilesDistance+MostRecentVisit+ExactAge+Gender+IncomeLevels, data=Data),kw10kmdist_Plotname)
  SmellLocalMoran_Plotname <-    lm.morantest(lm(SmellWTPNew       ~ WoodlandsScore+MilesDistance+MostRecentVisit+ExactAge+Gender+IncomeLevels, data=Data),kw10kmdist_Plotname)
  SoundLocalMoran_Plotname <-    lm.morantest(lm(SoundWTPNew       ~ WoodlandsScore+MilesDistance+MostRecentVisit+ExactAge+Gender+IncomeLevels, data=Data),kw10kmdist_Plotname)
  DeadwoodLocalMoran_Plotname <- lm.morantest(lm(DeadwoodWTPNew     ~ WoodlandsScore+MilesDistance+MostRecentVisit+ExactAge+Gender+IncomeLevels, data=Data),kw10kmdist_Plotname)
  TaxLocalMoran_Plotname <-      lm.morantest(lm(TaxWTPNew ~ WoodlandsScore+MilesDistance+MostRecentVisit+ExactAge+Gender+IncomeLevels, data=Data),kw10kmdist_Plotname)
  
  
  Variable <- rbind(
    cbind(
      ColourMoran_Plotname$p.value,  
      SmellMoran_Plotname$p.value, 
      SoundMoran_Plotname$p.value,   
      DeadwoodMoran_Plotname$p.value,
      TaxMoran_Plotname$p.value
    ),
    cbind(
      ColourLocalMoran_Plotname$p.value,  
      SmellLocalMoran_Plotname$p.value, 
      SoundLocalMoran_Plotname$p.value,   
      DeadwoodLocalMoran_Plotname$p.value,
      TaxLocalMoran_Plotname$p.value
    ))
  return(Variable)
}

Clusters(Data_Winter)
Clusters(Data_Spring)
Clusters(Data_Summer)
Clusters(Data_Autumn)
Clusters(Data_AllSeasons)

