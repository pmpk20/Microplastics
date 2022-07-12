#### DEFRA: Microplastics ####
## Function: Plots attitudes of all SurveyEngine Responses
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


here() ## This is the preferred approach to Setwd()

## Import Data:
Data <- data.frame(read.csv("Microplastics_AllData_Long_Anonymised_2022_06_19.csv"))


ComparisonMelted <- reshape2::melt(cbind("Climate"=Data$Q16_ClimateCurrentSelf,"Microplastics"=Data$Q16_MicroplasticsCurrentSelf))
ComparisonMelted$Var2 <- recode(ComparisonMelted$Var2,"Climate"=0,"Microplastics"=1)



#------------------------------
# Section 2: Density Plots ####
#------------------------------

Labels <- c(paste0("Now\n (Mean: ",round(mean(Data$MeanExpectedCurrent),2),",\n SD: ",round(sd(Data$MeanExpectedCurrent),2),") \n"),
            
            paste0("10 Years\n (Mean: ",round(mean(Data$MeanExpectedFuture),2),",\n SD: ",round(sd(Data$MeanExpectedFuture),2),") \n"))
## Using GGDIST
NowAndTenYearsDist <- Data[,9:10] %>% reshape2::melt()%>% ggplot()+
  aes(y=variable,x= value,fill=factor(variable))+
  stat_histinterval(side="top",position="dodgejust",outline_bars = TRUE,slab_color="gray45")+
  scale_x_continuous(name = "Expected Harm on a scale -5 (Much less harmful) to +5 (Much more harmful).",
                     limits = c(-5, 5),
                     breaks = seq(-5, 5, 0.5)) +
  scale_y_discrete(labels = c("Now", "10 Years"),
                   name = "Time Period")+
  scale_fill_brewer(name="Attributes",
                    label=Labels,
                    guide=guide_legend(reverse = TRUE))+
  theme(legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"))+
  ggtitle("Distribution Of Beliefs About Microplastics.")

ggsave(NowAndTenYearsDist,device = "jpeg",
       filename = "NowAndTenYearsDist_2022_07_06.jpeg",
       width=20,height=15,units = "cm",dpi=1000)


## Plot Now and 10 years time expected mean:
Data[,9:10] %>% reshape2::melt() %>% ggplot(aes(x = value, y = variable, group=variable,fill = variable)) + 
  geom_density_ridges()+ggtitle("Expected Effects Of Microplastics") +
  scale_x_continuous(name = "Expected Harm on a scale -5 (Much less harmful) to +5 (Much more harmful).",
                     limits = c(-5, 5),
                     breaks = seq(-5, 5, 1)) +scale_y_discrete(
                       labels = c("Now", "10 Years"),
                       name = "Time Period",
                       breaks = c(0, 1)) +geom_vline(xintercept = 0)




Attitudes <- Data[,24:27]
Attitudes_Melted <- reshape2::melt(Attitudes)

## Density Plot Of Attitudes
Attitudes_Melted %>% ggplot(aes(x = value, y = variable, group=variable,fill = variable)) + 
  geom_density_ridges()


Attitudes_Melted %>% ggplot(aes(x = value, y = variable, group=variable,fill = variable)) + 
  geom_density_ridges()+ggtitle("Expected Harm By Duration") +
  scale_x_continuous(name = "Expected Harm on a scale -5 to +5.",
                     limits = c(-5, 5),
                     breaks = seq(-5, 5, 1)) +scale_y_discrete(
                       labels = c("Now", "10 Years", "25 Years", "50 Years"),
                       name = "Time Period",
                       breaks = c(0, 1, 2, 3)) 

#------------------------------
# Section 3: Line Plots ####
#------------------------------

Data$VarianceLowerBound = Data$MeanExpectedFuture + Data$VarianceLowerBound
Data$VarianceUpperBound = Data$MeanExpectedFuture - Data$VarianceUpperBound

Expectations <- reshape2::melt(cbind("Best Case Scenario"=Data$VarianceUpperBound,
                     "Mean Expected"=Data$MeanExpectedFuture,
                     "Worst Case Scenario"=Data$VarianceLowerBound))

Expectations %>% ggplot(aes(x = value, y = Var2, group=Var2,fill = Var2)) + 
  geom_density_ridges()+
  ggtitle("Expected Future Harm Of Microplastics") +
  scale_y_discrete(
    name = "Questions.") +
  scale_x_continuous(name = "Harm on a scale -5 to +5.",
                     limits = c(-5, 5),
                     breaks = seq(-5, 5, 1)) + 
  geom_vline(xintercept = 0)


Expectations <- cbind("Current"=Data$MeanExpectedCurrent,
                      "Expected"=Data$MeanExpectedFuture)
Means <- reshape2::melt(Expectations)
Means$Var1[Means$Var2=="Current"] <- 0
Means$Var1[Means$Var2=="Expected"] <- 1


Expectations <- cbind("Best"=Data$VarianceUpperBound,
                     "Current"=Data$MeanExpectedCurrent,
                     "Expected"=Data$MeanExpectedFuture,
                     "Worst"=Data$VarianceLowerBound)
Means <- reshape2::melt(Expectations)
Means$Time <- Means$Var1
Means$Time[Means$Var2=="Current"] <- 0
Means$Time[Means$Var2!="Current"] <- 1


# Means$Var1<-0
Means$Var2 <- recode(Means$Var2,"Best Case Scenario"=2,
                         "Mean Current"=0,
                                    "Mean Expected"=1,
                         "Worst Case Scenario"=3)




#---------------------------------------
# Section 3: Q16 Plots of MPs vs CC ####
#----------------------------------------

Attitudes_Melted$variable <- recode(Attitudes_Melted$variable,"Q16_MicroplasticsCurrentSelf"=0,
                                    "Q16_MicroplasticsTen"=1,
                                    "Q16_MicroplasticsTwentyFive"=2,
                                    "Q16_MicroplasticsFifty"=3)

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







Attitudes <- reshape2::melt(Data[,21:27]-6)
Attitudes %>% ggplot(aes(x = value, y = variable, group=variable,fill = variable)) + 
  geom_density_ridges()+ggtitle("How Strongly Do You Agree Or Disagree") +
  scale_x_continuous(name = "Expected Threat From No Threat (-5) To Significant Threat (+5).",
                     limits = c(-5, 5),
                     breaks = seq(-5, 5, 1))



## Isolate variables here:
### Minusing 6 to center the variables around zero
Q16Variables <- (Data[,21:27]-6)

## Reorder column for the plot to make more sense:
Q16Variables <- Q16Variables[,c(1,3,2,4,5,6,7)]


## Define text for Y axis to make more sense:
YAxisLabels = c(
  "Climate Change:\n Current effect on the environment",
  "Microplastics:\n Current effect on the environment",
  "Climate Change:\n Current effect on self",
  "Microplastics:\n Current effect on self",
  "Microplastics:\n Effect on humanity in 10 Years",
  "Microplastics:\n Effect on humanity in 25 Years",
  "Microplastics:\n Effect on humanity in 50 Years"
  )

## Some nice explanatory text for X axis to make more sense:
XAxisLabels = c(
  "-5\n (No Threat)",
  -4,-3,-2,-1,0,
  1,2,3,4,"5\n (Significant Threat)"
)


## Probably a better way but adding summary stats to the legend.
LegendLabels = c(
  paste0("Climate Change: Environment\n (Mean: ",round(mean(Data$Q16_ClimateCurrentEnvironment),2),",\n SD: ",round(sd(Data$Q16_ClimateCurrentEnvironment),2),") \n"),
  paste0("Microplastics: Environment\n (Mean: ",round(mean(Data$Q16_MicroplasticsCurrentEnvironment),2),",\n SD: ",round(sd(Data$Q16_MicroplasticsCurrentEnvironment),2),") \n"),
  paste0("Climate Change: Self\n (Mean: ",round(mean(Data$Q16_ClimateCurrentSelf),2),",\n SD: ",round(sd(Data$Q16_ClimateCurrentSelf),2),") \n"),
  paste0("Microplastics: Self\n (Mean: ",round(mean(Data$Q16_MicroplasticsCurrentSelf),2),",\n SD: ",round(sd(Data$Q16_MicroplasticsCurrentSelf),2),") \n"),
  paste0("Microplastics: 10 Years\n (Mean: ",round(mean(Data$Q16_MicroplasticsTen),2),",\n SD: ",round(sd(Data$Q16_MicroplasticsTen),2),") \n"),
  paste0("Microplastics: 25 Years\n (Mean: ",round(mean(Data$Q16_MicroplasticsTwentyFive),2),",\n SD: ",round(sd(Data$Q16_MicroplasticsTwentyFive),2),") \n"),
  paste0("Microplastics: 50 Years\n (Mean: ",round(mean(Data$Q16_MicroplasticsFifty),2),",\n SD: ",round(sd(Data$Q16_MicroplasticsFifty),2),") \n")
)


## Creating plot:
Attitudes <- reshape2::melt(Q16Variables)
PlotOfAttitudes <- Attitudes %>% ggplot(aes(x = value, y = variable, group=variable,fill = variable)) + 
  stat_histinterval(side="top",position="dodgejust",outline_bars = TRUE,slab_color="gray45")+
  geom_vline(xintercept = 0)+
  theme_bw()+
  ggtitle(paste0("How Strongly Do You Agree Or Disagree \nWith The Following Statements:")) +
  scale_x_continuous(name = "Expected Threat.",
                     limits = c(-5, 5),labels = XAxisLabels,
                     breaks = seq(-5, 5, 1))+
  scale_y_discrete(labels = YAxisLabels,
                 name = "Statement")+
  scale_fill_brewer(name="Item:",
                    label=LegendLabels,
                    guide=guide_legend(reverse = TRUE))+
  theme(legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"))



ggsave(PlotOfAttitudes,device = "jpeg",
       filename = "PlotOfAttitudes_2022_07_11.jpeg",
       width=20,height=15,units = "cm",dpi=1000)



#---------------------------------------------
# Section 3B: Plotting Effect Of Pandemic ####
#---------------------------------------------


## Define text for Y axis to make more sense:
YAxisLabels_Pandemic = c(
  "Pandemic:\n Changed Thoughts On\n Conserving The Environment",
  "Pandemic:\n Changed Thoughts On\n Microplastics"
)


## Probably a better way but adding summary stats to the legend.
LegendLabels_Pandemic = c(
  paste0("Pandemic: Environment\n (Mean: ",round(mean(Data$Q17_PandemicEnvironment),2),",\n SD: ",round(sd(Data$Q17_PandemicEnvironment),2),") \n"),
  paste0("Pandemic: Microplastics\n (Mean: ",round(mean(Data$Q17_PandemicMicroplastics),2),",\n SD: ",round(sd(Data$Q17_PandemicMicroplastics),2),") \n")
)


## Some nice explanatory text for X axis to make more sense:
XAxisLabels_Pandemic = c(
  "1\n (Less Concerned)",2,"3\n (Unchanged)",4,"5\n (More Concerned)"
)


PlotOfAttitudes_Pandemic <- reshape2::melt(Data[,28:29])%>% ggplot(aes(x = value, y = variable, group=variable,fill = variable)) + 
  stat_halfeye()+
  theme_bw()+
  ggtitle(paste0("How Strongly Do You Agree Or Disagree \nWith The Following Statements:")) +
  scale_x_continuous(name = "Beliefs.",labels = XAxisLabels_Pandemic,
                     limits = c(1, 5),
                     breaks = seq(1, 5, 1))+
  scale_y_discrete(name = "Statement",
                    labels=YAxisLabels_Pandemic)+
  scale_fill_brewer(name="Item:",labels=LegendLabels_Pandemic,
                    guide=guide_legend(reverse = TRUE))+
  theme(legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"))


ggsave(PlotOfAttitudes_Pandemic,device = "jpeg",
       filename = "PlotOfAttitudes_Pandemic_2022_07_11.jpeg",
       width=20,height=15,units = "cm",dpi=1000)




#------------------------------
# Section 4: Map Of Responses ####
#------------------------------



# Import Data: #------------------------------

# GB <- st_read("Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp")
# GB <- st_transform(GB,crs=4326) ## Comes in BNG so convert to LatLon
# 
# 
# # Join Data: #------------------------------
# colnames(Data)[which(names(Data)=="X")] <- "ID"
# Data_Spatial <- as.data.frame(Data) %>% 
#   st_as_sf(coords=c("Longitude","Latitude"), crs=4326, remove=FALSE)  
# 
# GB_Pilot <- st_join(GB, left = FALSE, Data_Spatial)
# # GB_Pilot <- st_join(Data_Spatial, left = FALSE, GB) # join points


# Plot Respondents Location: #------------------------------
# ggplot() +
#   geom_sf(data=GB) +
#   geom_point(data=GB_Pilot,
#              aes(x = Longitude, y = Latitude,colour="Response"),
#              size = 5,shape=4
#   ) +
#   coord_sf(
#     xlim = c(st_bbox(GB)["xmin"], st_bbox(GB)["xmax"]),
#     ylim = c(st_bbox(GB)["ymin"], st_bbox(GB)["ymax"]),
#     expand = FALSE
#   )+theme(aspect.ratio = 5/5)+
#   labs(colour="Respondents")+
#   xlab(label="Longitude")+
#   ylab(label="Latitude")+
#   scale_color_manual(values = c("Response" = 'black'))+
#   ggtitle("Location Of Responses.")
# 
# 
# ## Colour polygons by income:
# ggplot() +
#   geom_sf(data=GB)+
#   geom_sf(data = GB_Pilot, aes(fill = Income))





#------------------------------
# Section 5: Histograms ####
#------------------------------
fun = function(x,mean,sd,n){
  n*dnorm(x=x,mean=mean,sd=sd)
}

ggplot(Data,aes(Understanding))+
  geom_histogram(aes(y=..density..),color="black",fill="white",binwidth = 1)+
  stat_function(fun=fun,
                args=with(Data,c(mean(Understanding),
                                 sd=sd(Understanding),n=1)))+
  ggtitle("Histogram Of Responses To Survey Understanding.")+
  scale_x_continuous(name="Survey Understanding 1-10",
                     limits = c(1,11),
                     breaks = seq(1, 10, 1))


