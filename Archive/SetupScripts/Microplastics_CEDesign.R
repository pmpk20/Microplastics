#### DEFRA: Microplastics ####
## Function: Creates an efficient Choice Experiment Design 
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 12/04/2022
## TODO: explore more complex designs



#------------------------------
# Replication Information: ####
# Output of 'sessionInfo()'
#------------------------------

# R version 4.1.2 (2021-11-01)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)
# [1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252   
# other attached packages:
#   [1] idefix_1.0.3 shiny_1.7.1 
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.8       magrittr_2.0.2   MASS_7.3-55      munsell_0.5.0   
# [5] tidyselect_1.1.1 colorspace_2.0-2 xtable_1.8-4     R6_2.5.1        
# [9] rlang_0.4.11     fastmap_1.1.0    fansi_1.0.2      dplyr_1.0.7     
# [13] tools_4.1.2      rbibutils_2.2.7  utf8_1.2.2       DBI_1.1.2       
# [17] htmltools_0.5.2  ellipsis_0.3.2   assertthat_0.2.1 digest_0.6.27   
# [21] tibble_3.1.6     lifecycle_1.0.1  crayon_1.4.2     purrr_0.3.4     
# [25] later_1.3.0      vctrs_0.3.8      promises_1.2.0.1 Rdpack_2.1.3    
# [29] glue_1.6.1       mime_0.12        pillar_1.7.0     compiler_4.1.2  
# [33] scales_1.1.1     generics_0.1.2   httpuv_1.6.5     pkgconfig_2.0.3 


#------------------------------
# Setup Environment: ####
#------------------------------


## Remove prior data in workspace:
rm(list=ls())

## Load IDEFIX package for designs
library(idefix)

## To enable replication
set.seed(123) 


#------------------------------
# Design: ####'
#------------------------------


## Specify the prior means of the parameters here
### We have 6 parameters from previous work
### Price: Continuous with 4 levels 
### Performance: Categorical dummy coded with 3 levels (low, medium, high) 
### Emission: Categorical dummy coded with 3 levels (low, medium, high) 
### Values are c(ASC, price, perf medium, perf high, em medium, em high)
Mean <- c(1.143,-0.532, 0.461, 1.966,0.651,-1.16)
Priors <- c(1,-1,0,0,0,0,0,0)

## Specify variance matrix
Variance <- diag(length(Priors))


## Generate N draws from distribution
N <- 100 ## Specify 10 draws for ease but can increase
ProbDist <- MASS::mvrnorm(n = N,mu = Priors,Sigma = Variance)


## Transform draws into different object
### Following idefix package documentation here
ProbDistList <- list(matrix(ProbDist[,1],
                            ncol=1),
                     ProbDist[,2:8])

## Use the Coordinate-Exchange Algorithm (CEA) to generate a design:
Design <- CEA(
  lvls = c(5, 4, 4), ## Number of levels for price, perf, em attributes
  coding = c("C", "D", "D"), ## Coding dummy (D) or effects (E) or continuous (E)
  par.draws = ProbDistList, ## Specify generated draws
  n.alts = 3, ## Number of alternatives per set: 3 here as more efficient than 2
  n.sets = 8, ## Total number of choice sets
  parallel = FALSE, ## Set to TRUE for faster computation
  alt.cte = c(0,0,1), ## Is an ASC necessary for each alternative?
  c.lvls = list(c(0,0.5,1,2.5,5)),
  no.choice = TRUE ## Include an opt-out scenario
)
Design$design ## Print design output

## Return DB-Error:
DBerr(par.draws = ProbDist,des = Design$design,n.alts = 3)





## Generate all possible profiles
Profs <- Profiles(lvls = c(4, 4,5),
        coding = c("D", "D", "C"),
        c.lvls=list(c(0,0.5, 1, 2.5, 5)))


## Generate design using Modified-Federov Algorithm Instead:
### All options the same as CEA() but just with profiles this time
ModfedDesign <- Modfed(
  cand.set = Profs, ## Candidate profiles generated with Profiles()
  n.sets = 8,
  n.alts = 3,
  alt.cte = c(1, 0, 0),
  parallel = TRUE,
  par.draws = ProbDistList
)

## Choice Set Outputs
ModfedDesign$design

Sets <- data.frame(ModfedDesign$design)

## Recoding names and values to make more sense:
colnames(Sets) <- c("ASC","PerfLow","PerfMedium","PerfHigh","EmLow","EmMedium","EmHigh","Price")
Sets$PerfLow <- ifelse(Sets$PerfLow==0,NA,5)
Sets$PerfMedium <- ifelse(Sets$PerfMedium==0,NA,10)
Sets$PerfHigh <- ifelse(Sets$PerfHigh==0,NA,50)
Sets$EmLow <- ifelse(Sets$EmLow==0,NA,10)
Sets$EmMedium <- ifelse(Sets$EmMedium==0,NA,40)
Sets$EmHigh <- ifelse(Sets$EmHigh==0,NA,90)

Sets2 <- cbind(Sets$ASC,coalesce(Sets$PerfLow,Sets$PerfMedium,Sets$PerfHigh),
      coalesce(Sets$EmLow,Sets$EmMedium,Sets$EmHigh),
      Sets$Price)
rownames(Sets2) = rownames(Sets)
Sets2 <- ifelse(is.na(Sets2)==TRUE,0,Sets2)
colnames(Sets2) <- c("ASC","Performance","Emission","Price")


## Block 1 Only:
Sets2[1:12,]

## Block 2 Only:
Sets2[13:24,]


## Question by question:
t(Sets2[1:3,]) ## 9A
t(Sets2[4:6,])## 9B
t(Sets2[7:9,])## 10A
t(Sets2[10:12,])## 10B
t(Sets2[13:15,])## 11A
t(Sets2[16:18,])## 11B
t(Sets2[19:21,])## 12A
t(Sets2[22:24,])## 12B
