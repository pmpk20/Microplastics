#### Microplastics: IOP Paper ####
## Function: 2 stage GLM
## Author: PK
## Last change: 05/06/24
# Changes:
## - No covariates model



# *****************************
# Replication Information: ####
# Selected output of 'sessionInfo()'
# *****************************
# R version 4.3.0 (2023-04-21)
# Platform: x86_64-apple-darwin20 (64-bit)
# Running under: macOS Ventura 13.5
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: Europe/Helsinki
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] AER_1.2-10     survival_3.5-7 sandwich_3.0-2 lmtest_0.9-40  zoo_1.8-12     car_3.1-2      carData_3.0-5 
# [8] boot_1.3-28.1  betareg_3.1-4 
# 
# loaded via a namespace (and not attached):
#   [1] gridExtra_2.3        inline_0.3.19        rlang_1.1.2          magrittr_2.0.3       multcomp_1.4-25     
# [6] matrixStats_1.1.0    compiler_4.3.0       flexmix_2.3-19       loo_2.6.0            callr_3.7.3         
# [11] vctrs_0.6.4          reshape2_1.4.4       stringr_1.5.1        pkgconfig_2.0.3      crayon_1.5.2        
# [16] fastmap_1.1.1        backports_1.4.1      ellipsis_0.3.2       utf8_1.2.4           threejs_0.3.3       
# [21] cmdstanr_0.6.1.9000  promises_1.2.1       markdown_1.11        ps_1.7.5             xfun_0.41           
# [26] modeltools_0.2-23    jsonlite_1.8.7       later_1.3.1          parallel_4.3.0       prettyunits_1.2.0   
# [31] R6_2.5.1             dygraphs_1.1.1.6     stringi_1.8.2        StanHeaders_2.26.28  estimability_1.4.1  
# [36] Rcpp_1.0.11          rstan_2.32.3         knitr_1.45           base64enc_0.1-3      bayesplot_1.10.0    
# [41] httpuv_1.6.12        Matrix_1.6-3         splines_4.3.0        nnet_7.3-19          igraph_1.5.1        
# [46] tidyselect_1.2.0     rstudioapi_0.15.0    abind_1.4-5          codetools_0.2-19     miniUI_0.1.1.1      
# [51] curl_5.1.0           processx_3.8.2       pkgbuild_1.4.2       lattice_0.22-5       tibble_3.2.1        
# [56] plyr_1.8.9           shiny_1.8.0          bridgesampling_1.1-2 posterior_1.5.0      coda_0.19-4         
# [61] RcppParallel_5.1.7   xts_0.13.1           pillar_1.9.0         tensorA_0.36.2       checkmate_2.3.0     
# [66] DT_0.30              stats4_4.3.0         shinyjs_2.1.0        distributional_0.3.2 generics_0.1.3      
# [71] ggplot2_3.4.4        rstantools_2.3.1.1   munsell_0.5.0        scales_1.2.1         gtools_3.9.5        
# [76] xtable_1.8-4         glue_1.6.2           emmeans_1.8.9        tools_4.3.0          shinystan_2.6.0     
# [81] colourpicker_1.3.0   mvtnorm_1.2-3        grid_4.3.0           QuickJSR_1.0.7       crosstalk_1.2.1     
# [86] colorspace_2.1-0     nlme_3.1-163         Formula_1.2-5        cli_3.6.1            fansi_1.0.5         
# [91] Brobdingnag_1.2-9    dplyr_1.1.4          V8_4.4.0             gtable_0.3.4         digest_0.6.33       
# [96] TH.data_1.1-2        brms_2.20.4          htmlwidgets_1.6.3    farver_2.1.1         htmltools_0.5.7     
# [101] lifecycle_1.0.4      mime_0.12            shinythemes_1.2.0    MASS_7.3-60   

rm(list=ls())

## Useful for all scripts:
library(data.table)
library(magrittr)
library(dplyr)
library(tidyverse)
library(here)
library(DCchoice)


## Key for this script:
library(betareg)
library(boot)
library(AER)
library(snow)


# ***********************************************************
# Section 1: Import Data ####
# ***********************************************************

Data <-
  here("Data",
       "Microplastics_AllData_Long_Anonymised_2022_06_19.csv") %>%
  fread() %>%
  data.frame()


# 
# 
# ## Start with the latest anonymised data in one-row per one-respondent format
# Data <- read.csv("~/Library/CloudStorage/Dropbox/Project_King/Microplastics_AllData_Long_Anonymised_2022_06_19.csv")


# ***********************************************************
# Section 2: Create  additional variables ####
# ***********************************************************


# ## Rescale to check
# Data$Income[Data$Income == 5000] <- 7500


## Income and income weighted bid
Data$Income_Annual <- Data$Income %>% multiply_by(12)
Data$LogBidIncome <-
  log((Data$Income_Annual - Data$Bid) / (Data$Income_Annual))


## Transform Mean expected future
Data$MEF <- (Data$MeanExpectedFuture + 5.001) / 10.002
# summary(Data$MEF)


## Transform mean expected current
Data$MEC <-
  (Data$MeanExpectedCurrent + 5.001) / 10.002
# summary(Data$MEC)


## Verify that transforms don't change relationships
# plot((Data$MeanExpectedFuture + Data$MeanExpectedCurrent),
#      (Data$MEF + Data$MEC))


# For consistency with boot function
Data$NewMean <- ((Data$MEF + Data$MEC) / 2)


## Half differences between variance bounds
Data$Uncertainty <-
  ((Data$VarianceLowerBound - Data$VarianceUpperBound) / 2)

## Transform to Cameron (2005) measure
Data$var.cameron <- (0.5 * Data$Uncertainty) ^ 2

## Change to negative
Data$var.cameron_negative <- (Data$Uncertainty * -1)

## Negative correlation so Uncertainty up means CV down
##
# cor.test(Data$var.cameron, Data$CV)

# ****************************************
# Misc variable transformations


Data$Education_HigherEd <- ifelse(Data$Education == 5,
                                  1, ## 1 = higher education
                                  0) ## 0 = all other

## Drop "other" due to small sample
Data_Trim <- Data[Data$Gender < 2, ]
Data$Gender_Female <- ifelse(Data$Gender == 0,
                             "Male",
                             "Female")

Data$Gender_Dummy <- ifelse(Data$Gender_Female == "Female",
                             0,
                             1)

# ********************************************
# Section 2: Define functions ####
# ********************************************



AddStars <- function(ZValues) {
  
  ## 2(1 - pnorm(Z))
  Converted <-  (1 - pnorm(abs(ZValues))) %>% multiply_by(2)
  Converted_trimmed <- Converted %>% round(3) %>% sprintf("%.3f", .)
  ifelse(
    Converted < 0.01,
    paste0(Converted_trimmed, " ***"),
    ifelse(
      Converted < 0.05,
      paste0(Converted_trimmed, " **"),
      ifelse(
        Converted < 0.1,
        paste0(Converted_trimmed, " *"),
        paste0(Converted_trimmed, " "))))
}

# ********************************************
# Section 3: Simple models ####
# ********************************************



stage_1_Naive <- betareg(
  MEF ~
    -1 +
    AgeBracket +
    EthnicityDummy +
    Gender_Dummy +
    Charity +
    Q16_ClimateCurrentEnvironment +
    Q16_ClimateCurrentSelf +
    Q16_MicroplasticsCurrentEnvironment +
    Q16_MicroplasticsCurrentSelf +
    Q16_MicroplasticsTen +
    Q16_MicroplasticsTwentyFive +
    Q16_MicroplasticsFifty  |
    -1 +
    Uncertainty +
    Consequentiality +
    Education_HigherEd,
  Data,
  type = "BC"
)


stage_2_Naive <- glm(CV ~ -1 +
                        LogBidIncome + 
                        I((predict(stage_1_Naive,type="response")+MEC)/2) + 
                        I(0- predict(stage_1_Naive, type = "variance")),
                      family = binomial(link = "probit"), 
                      Data)


# ********************************************
# Section 3: Proper 2stage, consistent SE ####
# ********************************************


boot.function <- function(data, indices){
  d <- data[indices,]
  
  
  stage_1_bootstrapped <- betareg(
    MEF ~
      -1 +
      AgeBracket +
      EthnicityDummy +
      Gender_Dummy +
      Charity +
      Q16_ClimateCurrentEnvironment +
      Q16_ClimateCurrentSelf +
      Q16_MicroplasticsCurrentEnvironment +
      Q16_MicroplasticsCurrentSelf +
      Q16_MicroplasticsTen +
      Q16_MicroplasticsTwentyFive +
      Q16_MicroplasticsFifty  |
      -1 +
      Uncertainty +
      Consequentiality +
      Education_HigherEd,
    d,
    type = "BC"
  )
  
  
  stage_2_bootstrapped <- glm(CV ~ -1 +
                   LogBidIncome + 
                   I((predict(stage_1_bootstrapped,type="response")+MEC)/2) + 
                   I(0- predict(stage_1_bootstrapped, type = "variance")),
                 family = binomial(link = "probit"), 
                 d)
  
  
  return(c(summary(stage_1_bootstrapped)$coefficients$mean[ , c(1) ],
           summary(stage_1_bootstrapped)$coefficients$precision[ , c(1) ],
           summary(stage_2_bootstrapped)$coefficients[ ,c(1) ],
           summary(stage_1_bootstrapped)$coefficients$mean[ , c(2) ],
           summary(stage_1_bootstrapped)$coefficients$precision[ , c(2) ],
           summary(stage_2_bootstrapped)$coefficients[ ,c(2) ]))

  # return(c(summary(stage_1_bootstrapped)$coefficients$mean[ , c(1) ],
  #          summary(stage_1_bootstrapped)$coefficients$precision[ , c(1) ],
  #          summary(stage_2_bootstrapped)$coefficients[ ,c(1) ],
  #          summary(stage_1_bootstrapped)$coefficients$mean[ , c(2) ],
  #          summary(stage_1_bootstrapped)$coefficients$precision[ , c(2) ],
  #          summary(stage_2_bootstrapped)$coefficients[ ,c(2) ],
  # 
  #          stage_1_bootstrapped %>% AIC() %>% round(3) ,
  #          stage_1_bootstrapped %>% logLik() %>% as.numeric() %>% round(3) ,
  #          stage_1_bootstrapped$pseudo.r.squared %>% as.numeric() %>% round(3) ,
  # 
  #          stage_2_bootstrapped %>% AIC() %>% round(3) ,
  #          stage_2_bootstrapped %>% logLik() %>% as.numeric() %>% round(3) ,
  #          stage_2_bootstrapped$deviance %>% as.numeric() %>% round(3)
  # ))
}

boot.results <- boot(data = Data,
                     statistic = boot.function,
                     R = 100,
                     parallel = "snow")
#see https://www.datacamp.com/tutorial/bootstrap-r for definition of boot() outcome
## we might have to change "multicore" to "snow" for windows machines


l <- length(boot.results$t0)



diagnostics <- boot.results$t0[(l - 5):l]

diagnostics_formatted <- c(
  "S1AIC" = diagnostics[1],
  "S1LogLik" = diagnostics[2],
  "S1pseudo.r.squared" = diagnostics[3],
  "S2AIC" = diagnostics[4],
  "S2LogLik" = diagnostics[5],
  "S2Deviance" = diagnostics[6]
)

## Changing original to use pipe which is easier to read
results_raw <-
  cbind(
    boot.results[1]$t0[1:(l / 2)], ## Recover estimates
    boot.results$t[, (l / 2 + 1):l] %>% colMeans(), ## standard errors
    boot.results[1]$t0[1:(l / 2)] %>% 
      divide_by(
        boot.results$t[, (l / 2 + 1):l] %>% ## so dividing by colmeans()
          colMeans())
  ) 

results <- results_raw[1:(nrow(results_raw) -3), ]

## rename columns
colnames(results) <- c("Estimate", "Std. Error", "z value")



## defining a seperate object that is rounded
## in case we need to operate on a non-rounded version
results_rounded <- results %>% round(3)


## View with P values
results_rounded_withP <- cbind(
  results_rounded, 
  "P values" = results[, 3] %>% AddStars()) %>% 
  data.frame() 


#compare to naive SE
summary(stage_1_Naive)$coefficients$mean[,c(1:2)]
summary(stage_1_Naive)$coefficients$precision[,c(1:2)]
summary(stage_2_Naive)$coefficients[,c(1:2)]


# *************************************************
# Section 4: Expected Option Prices ####
# *************************************************


## Beta_0 being the parameter on scaled income
B0 <- results["LogBidIncome", "Estimate"] %>% as.numeric()


## Define Y == gross monthly income * 12
Y <- Data$Income_Annual


## Delta parameters are the mean and var parameters recovered from stage_2
Delta_0 <- results['I((predict(stage_1_bootstrapped, type = "response") + MEC)/2)', "Estimate"] %>% as.numeric()
Delta_1 <- results['I(0 - predict(stage_1_bootstrapped, type = "variance"))', "Estimate"] %>% as.numeric() 


## Delta0 * (t* - E[t])
## Delta1 * (- Var[t])
### NOTE: here using (0 - mean(predictions)) rather than (-Data$Uncertainty)
### which was blowing up the EOP estimates



A <-
  ((Delta_0 * c(I((predict(stage_1_Naive, type = "response") + Data$MEC)/2)) +
      (Delta_1 * (0 - mean(betareg::predict(stage_1_Naive,type = "variance"))))
  )) %>% as.numeric()

# A <-
#   ((Delta_0*c(Data$NewMean) +
#      (Delta_1 * (0 - mean(betareg::predict(stage_1,type = "variance"))))
#      )) %>% as.numeric()


## Formula here: Y - Y exp(-A/B0)exp(1/2*B0^2)
EOP <- (Y - (Y*exp(- A / B0))) *
  exp(1 %>% divide_by(B0 %>% raise_to_power(2) %>% multiply_by(2)))


## Summary of EOP
cbind(
  "min" = EOP %>% min() %>% round(2),
  "mean" = EOP %>% mean() %>% round(2),
  "median" = EOP %>% median() %>% round(2),
  "sd" = EOP %>% sd() %>% round(2),
  "max" = EOP %>% max() %>% round(2)) %>% write.csv(quote = FALSE)

# EOP %>% hist() %>% plot()


## Output summary stats of Income (monthly), EOP (monthly), Percent 
cbind(
  "Monthly income" = (Data$Income_Annual/12) %>% 
    summary() %>% 
    round(3) %>% 
    sprintf("%.3f", .),
  "EOP" = (EOP/12) %>% 
    summary() %>% 
    round(3) %>% 
    sprintf("%.3f", .),
  
  "EOP as percent of income"  = (100 / (Data$Income_Annual/12) * (EOP/12)) %>% 
    summary() %>% 
    round(3) %>% 
    sprintf("%.3f", .)
) %>% write.csv(quote = FALSE)



Data$EOP <- EOP
Data %>% fwrite(sep = ",",
                here("Data",
                     "Data_PlusTestEOP.csv"))




# *************************************************
# Section 5: EOP summary by variable ####
# *************************************************


## Income
T1 <- 
  Data %>% 
  reframe("EOP" = quantile(EOP, c(0.025, 0.25, 0.5, 0.75, 0.975)), 
          .by = Income) %>%  ## Quantiles of EOP by income level
  mutate(QuantilesOfEOP = c(0.025, 0.25, 0.5, 0.75, 0.975) %>% 
           rep(times = Data$Income %>% unique() %>% length())) %>% ## mutate makes column names only
  pivot_wider(names_from = QuantilesOfEOP, 
              values_from = EOP) %>% ## rearrange data to wider format for column per quantile
  slice(match(Income %>% unique() %>% sort(), Income)) %>%  ## reorder variables
  mutate(Count = Data$Income %>% table())  %>% ## add number of respondents column
  data.frame() %>%  ## Convert to DF for ease of manipulation
  arrange(desc(Income)) ## rearrange column order


## By new mean
T2 <- 
  Data %>% 
  reframe("EOP" = quantile(EOP, c(0.025, 0.25, 0.5, 0.75, 0.975)), 
          .by = NewMean) %>% 
  mutate(QuantilesOfEOP = c(0.025, 0.25, 0.5, 0.75, 0.975) %>% 
           rep(times = Data$NewMean %>% unique() %>% length())) %>% 
  pivot_wider(names_from = QuantilesOfEOP, 
              values_from = EOP) %>% 
  slice(match(NewMean %>% unique() %>% sort(), NewMean)) %>% 
  mutate(Count = Data$NewMean %>% table()) %>% 
  data.frame() %>%  
  arrange(desc(NewMean))


## By variance
T2 <- 
  Data %>% 
  reframe("EOP" = quantile(EOP, c(0.025, 0.25, 0.5, 0.75, 0.975)), 
          .by = NewMean) %>% 
  mutate(QuantilesOfEOP = c(0.025, 0.25, 0.5, 0.75, 0.975) %>% 
           rep(times = Data$NewMean %>% unique() %>% length())) %>% 
  pivot_wider(names_from = QuantilesOfEOP, 
              values_from = EOP) %>% 
  slice(match(NewMean %>% unique() %>% sort(), NewMean)) %>% 
  mutate(Count = Data$NewMean %>% table()) %>% 
  data.frame() %>%  
  arrange(desc(NewMean))



Data$Income_Annual_Quintile <- ifelse(Data$Income_Annual < 21000, 1, 
                                      ifelse(Data$Income_Annual <= 30000, 2, 
                                             ifelse(Data$Income_Annual <= 42000, 3, 
                                                    ifelse(Data$Income_Annual <= 54000, 4, 5))))



Data %>% reframe("Fitted" = quantile(EOP, c(0.025, 0.25, 0.5, 0.75, 0.975)), .by = NewMean) %>% mutate(
  Quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975) %>% rep(times = Data$NewMean %>% unique() %>% length())
) %>%
  ggplot(aes(x = NewMean %>% as.numeric(),
             group = Quantiles %>% as.factor())) +
  
  geom_line(aes(y = Fitted, color = Quantiles %>% as.factor()),
            linewidth = 1) +
  
  geom_point(aes(y = Fitted, color = Quantiles %>% as.factor())) +
  
  theme_bw()




# *************************************************
# Section 6A: Plot EOP, Means, Vars, Income  ####
# *************************************************


PlotData <- bind_cols(
  "Means" = I((predict(stage_1_Naive,type="response")+Data$MEC)/2),
  "Variances" = I(0- predict(stage_1_Naive, type = "variance")),
  "Income" = Data$Income_Annual_Quintile,
  "EOP" = findInterval(Data$EOP, 
                       quantile(Data$EOP, 
                                c(0.025, 0.25, 0.5, 0.75, 0.975)),
                       rightmost.closed = TRUE) %>% as.factor() 
) 


## Plot mean, variance relationship by income quintile
PlotData %>% 
  ggplot(aes(x = Means %>% as.numeric(),
             y = Variances %>% as.numeric(),
             group = Income %>% as.factor())) +
  
  stat_smooth(aes(colour = Income %>% as.factor())) + 
  geom_point(aes(colour = Income %>% as.factor())) +
  scale_x_continuous(breaks = seq.int(from = 0.1, to = 1, by = 0.1)) +
  scale_y_continuous(breaks = seq.int(from = -0.06056, to = -0.01390, by = 0.01)) +
  theme_bw() +
  facet_wrap( ~ Income %>% as.factor()) +
  scale_color_brewer(palette = "Reds")



# *************************************************
# Section 6B: Plot EOP, Means, Vars, Income  ####
# *************************************************


## Bin variances into quintiles
PlotData2 <- bind_cols(
  "Means" = PlotData$Means,
  "Variances" = findInterval(PlotData$Variances, 
                             quantile(PlotData$Variances, 
                                      c(0.025, 0.25, 0.5, 0.75, 0.975)),
                             rightmost.closed = TRUE) %>% as.factor(),
  "Income" = Data$Income_Annual_Quintile,
  "EOP" = PlotData$EOP
) 


## Points(Mean, Variance) with relationship to EOP by quintile
PlotData2 %>% 
  ggplot(aes(x = Means %>% as.numeric(),
             y = Variances %>% as.factor(),
             group = EOP %>% as.factor())) +
  
  stat_smooth(aes(colour = EOP %>% as.factor())) + 
  geom_point(aes(colour = EOP %>% as.factor())) +
  scale_x_continuous(breaks = seq.int(from = 0.1, to = 1, by = 0.1)) +
  theme_bw() +
  facet_wrap( ~ EOP %>% as.factor()) +
  scale_color_brewer(palette = "Reds")


# END OF SCRIPT ************************