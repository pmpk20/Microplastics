#### Microplastics: IOP Paper ####
## Function: In-text specification
## Author: PK
## Last change: 14/02/2025
# Changes:
# - Here we simulate then plot EOP
# - key difference is var[q] not 0-var[q]
# - this affects stage_2, Delta_1, A, and EOP
# - Now replacing return <- boot.results$t0 with
# - - colmeans (one per person) or boot.results$t




# ***********************************************************
# Replication Information: ####
# ***********************************************************
# This script performs in-text specification.
# Session information is saved to a separate file 'session_info.txt'

# R version 4.4.1 (2024-06-14 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 22631)
# Matrix products: default
# locale:
#   [1] LC_COLLATE=English_United Kingdom.utf8 
# [2] LC_CTYPE=English_United Kingdom.utf8   
# [3] LC_MONETARY=English_United Kingdom.utf8
# [4] LC_factor=C                           
# [5] LC_TIME=English_United Kingdom.utf8    
# 
# time zone: Europe/London
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] snow_0.4-4        AER_1.2-14        survival_3.6-4    sandwich_3.1-1   
# [5] lmtest_0.9-40     zoo_1.8-12        car_3.1-2         carData_3.0-5    
# [9] boot_1.3-30       betareg_3.2-1     DCchoice_0.2.0    here_1.0.1       
# [13] lubridate_1.9.3   forcats_1.0.0     stringr_1.5.1     purrr_1.0.2      
# [17] readr_2.1.5       tidyr_1.3.1       tibble_3.2.1      ggplot2_3.5.1    
# [21] tidyverse_2.0.0   dplyr_1.1.4       magrittr_2.0.3    data.table_1.16.0
# 
# loaded via a namespace (and not attached):
#   [1] utf8_1.2.4        generics_0.1.3    stringi_1.8.4     lattice_0.22-6   
# [5] hms_1.1.3         grid_4.4.1        timechange_0.3.0  Matrix_1.7-0     
# [9] rprojroot_2.0.4   nnet_7.3-19       Formula_1.2-5     Icens_1.76.0     
# [13] fansi_1.0.6       scales_1.3.0      modeltools_0.2-23 abind_1.4-8      
# [17] cli_3.6.3         rlang_1.1.4       munsell_0.5.1     splines_4.4.1    
# [21] withr_3.0.1       parallel_4.4.1    tools_4.4.1       flexmix_2.3-19   
# [25] tzdb_0.4.0        interval_1.1-1.0  colorspace_2.1-1  vctrs_0.6.5      
# [29] R6_2.5.1          stats4_4.4.1      lifecycle_1.0.4   MASS_7.3-60.2    
# [33] MLEcens_0.1-7.1   pkgconfig_2.0.3   pillar_1.9.0      gtable_0.3.5     
# [37] glue_1.7.0        tidyselect_1.2.1  rstudioapi_0.16.0 perm_1.0-0.4     
# [41] compiler_4.4.1    


# Clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(here)
library(DCchoice)
library(janitor)
library(data.table)
library(betareg)
library(boot)
library(AER)
library(snow)
library(speedglm)
library(sessioninfo)
library(magrittr)
library(ggtext)


# ***********************************************************
# Section 1: Import Data ####
# ***********************************************************

# Load data from the specified path using data.table::fread
Data <- here("Data", "Microplastics_AllData_Wide_Anonymised.csv") %>%
  fread() %>%
  data.frame()



Data_Filtered <- Data %>% dplyr::select(c(
  "CV",
  "MEC",
  "MEF", 
  "AgeDummy",
  "EthnicityDummy",
  "Gender_Dummy",  
  "Charity",
  "Education_HigherEd",
  "Q16_ClimateCurrentEnvironment",
  "Q16_ClimateCurrentSelf",
  "Q16_MicroplasticsCurrentEnvironment", 
  "Q16_MicroplasticsCurrentSelf",
  "Q16_MicroplasticsTen", 
  "Q16_MicroplasticsTwentyFive", 
  "Q16_MicroplasticsFifty", 
  "Uncertainty",
  "LogBidIncome",
  "Income_Annual"
))


# ***********************************************************
# Section 2: Define Functions ####
# ***********************************************************

# Function to convert z-values to p-values
PvalueConverter <- function(ZValues) {
  2 * (1 - pnorm(abs(ZValues)))
}

# Function to add significance stars to an estimate based on p-value
PvalueLabeller <- function(estimate, p_values) {
  estimate_rounded <- round(estimate, 3)
  case_when(
    p_values < 0.001 ~ paste0(estimate_rounded, "***"),
    p_values < 0.01  ~ paste0(estimate_rounded, "**"),
    p_values < 0.05 ~ paste0(estimate_rounded, "*"),
    p_values < 0.1 ~ paste0(estimate_rounded, "."),
    TRUE ~ as.character(estimate_rounded)
  )
}

# Function to format p-values for paper
PvalueLabeller_Paper <- function(p_values) {
  case_when(
    p_values < 0.001 ~ "<0.001***",
    p_values < 0.01 ~ sprintf("%.3f***", p_values),
    p_values < 0.05 ~ sprintf("%.3f**", p_values),
    p_values < 0.1 ~ sprintf("%.3f*", p_values),
    TRUE ~ sprintf("%.3f", p_values)
  )
}

# Function to format model output for printing
ModelOutput <- function(Estimates, Identifier) {
  Estimates %>%
    data.frame() %>%
    dplyr::mutate(
      Variable = rownames(Estimates),
      Estimate = paste0(PvalueLabeller(Estimate, P.values), " (", round(Std..Error, 3), ")"),
      Model = Identifier
    ) %>%
    dplyr::select(Variable, Estimate, Model)
}

# Function to format model output for paper
ModelOutput_Paper <- function(Estimates, Identifier) {
  Estimates %>%
    data.frame() %>%
    dplyr::mutate(
      Variable = rownames(Estimates),
      Estimate = Estimates$Estimate,
      Std..Error = Estimates$Std..Error,
      z.value = Estimates$z.value,
      P.values = PvalueLabeller_Paper(P.values)
    ) %>%
    dplyr::select(Variable, Estimate, Std..Error, z.value, P.values)
}


summary_function <- function(EOP) {
  cbind(
    "2.5%" = EOP %>% quantile(c(0.025)) %>% round(2) %>% sprintf("%.2f", .) %>%  paste0("£", .),
    Median = EOP %>% median(na.rm = TRUE) %>% round(2) %>%  sprintf("%.2f", .) %>% paste0("£", .), 
    Mean = EOP %>% mean(na.rm = TRUE) %>% round(2) %>%  sprintf("%.2f", .) %>% paste0("£", .), 
    SD = EOP %>% sd(na.rm = TRUE) %>% round(2) %>% sprintf("%.2f", .) %>%  paste0("£", .),
    "97.5%" = EOP %>% quantile(c(0.975)) %>% round(2) %>% sprintf("%.2f", .) %>%  paste0("£", .),
    "Percent" = (100/Data$Income_Annual*abs(EOP)) %>% 
      mean(na.rm = TRUE) %>% 
      round(2) %>% 
      sprintf("%.2f", .) %>% 
      paste0(., "%")
  )
}



# ***********************************************************
# Section 3: Simulator Function ####
# ***********************************************************

# Function to simulate data and perform the two-stage modelling
Simulator <- function(data,
                      formula_stage_1,
                      formula_stage_2,
                      R = R) {
  
  # Function to perform a single bootstrap replicate
  boot.function <- function(data, indices) {
    d <- data[indices, ]
    
    # Stage 1: Beta regression of MEF on covariates
    stage_1 <- betareg(formula_stage_1, d, type = "BC")
    
    # Stage 2: Probit model of CV on LogBidIncome, predicted MEF, predicted variance and control variables
    stage_2 <- speedglm(
      paste0(
        "CV ~ ",
        formula_stage_2,
        # " + I((predict(stage_1, type = 'response') + MEC) / 2) + I(0 - predict(stage_1, type = 'variance'))"
        " + I((predict(stage_1, type = 'response') + MEC) / 2) + I(predict(stage_1, type = 'variance'))"
      ) %>% as.formula(),
      family = binomial(link = "probit"),
      data = d
    )
    
    
    B0 <- stage_2$coefficients["LogBidIncome"] %>% as.numeric()
    Delta_0 <- stage_2$coefficients['I((predict(stage_1, type = "response") + MEC)/2)'] %>% as.numeric()
    # Delta_1 <- stage_2$coefficients['I(0 - predict(stage_1, type = "variance"))'] %>% as.numeric()
    Delta_1 <- stage_2$coefficients['I(predict(stage_1, type = "variance"))'] %>% as.numeric()
    
    Means <- c(I((predict(stage_1, type = "response") + d$MEC)/2))
    Variances <- (betareg::predict(stage_1, type = "variance"))
    ## Define Y == gross monthly income * 12
    Y <- d$Income_Annual
    A <- ((Delta_0 * Means +
            # (Delta_1 * (0 - Variances))
            (Delta_1 * (Variances))
    )) %>% as.numeric()
    ## Formula here: Y - Y exp(-A/B0)exp(1/2*B0^2)
    # EOP <- (Y - (Y*exp(- A / B0))) *
    #   exp(1 %>% divide_by(B0 %>% raise_to_power(2) %>% multiply_by(2)))
    EOP <- (Y - Y * exp(-A / B0) * exp(1 / (2 * B0 ^ 2)))
    
    # EOP <- (d$Income_Annual - (d$Income_Annual * exp(-A / B0))) *
    #   exp(0.5 * (B0^-2))
    
    return(EOP)
    
  }
  
  # Run the bootstrap
  boot.results <- boot(data = data,
                       statistic = boot.function,
                       R = R,
                       parallel = "snow")
  
  # Extracting the results
  # l <- length(boot.results$t0)
  results <- boot.results$t0
  
  # results <- boot.results$t %>% as.matrix() %>% Rfast::colmeans()
  
  ## Here just the raw data
  results %>% return() 
  # results %>% summary_function() %>% return()  
}

# ***********************************************************
# Section 4: Model Output ####
# ***********************************************************

# Define number of bootstrap iterations
R <- 10
# R <- 1000

## WATCH OUT VERY LARGE AND LONG RUNNING
# R <- 100000

# Define your formula for stage_1 and stage_2 models
Model1_stage1_formula <- as.formula(
  MEF ~
    1 + ## intercept here
    AgeDummy + 
    EthnicityDummy +
    Gender_Dummy  + 
    Charity +
    Education_HigherEd +
    Q16_ClimateCurrentEnvironment +
    Q16_ClimateCurrentSelf +
    Q16_MicroplasticsCurrentEnvironment + 
    Q16_MicroplasticsCurrentSelf +
    Q16_MicroplasticsTen + 
    Q16_MicroplasticsTwentyFive + 
    Q16_MicroplasticsFifty |
    1 +  # intercept here
    as.factor(Uncertainty)
)



# Define your formula for stage_1 and stage_2 models
Model1_stage2_formula <- "-1 + LogBidIncome"


# Call the simulator function
Model1_simulation <- Simulator(data = Data_Filtered,
                               formula_stage_1 = Model1_stage1_formula,
                               formula_stage_2 = Model1_stage2_formula,
                               R = R
)  


# *****************************
# Section x: Export Data ####
# *****************************

Data$EOP <- Model1_simulation

## Changing to test2
Data %>%
  data.frame() %>%
  fwrite(sep = ",",
         here("Data", "Microplastics_AllData_Wide_Anonymised_WithEOP_TEST1000.csv"))





# *****************************
# Section 2: Data Setup ####
# *****************************


Data$Mean_Change <- Data$MeanExpectedCurrent + Data$MeanExpectedFuture
Data$Variance_ConfidenceAsSD <- ifelse(Data$Variance == 1, 0, 
                                       ifelse(Data$Variance == 2, 2, 
                                              ifelse(Data$Variance == 3, 6, 10))) %>% 
  magrittr::divide_by(4)


Data$Variance_ConfidenceAsVariance <- Data$Variance_ConfidenceAsSD %>% 
  magrittr::raise_to_power(2)



## Isolate variables we want only
PlotData <- Data[, c("MeanExpectedFuture", 
                     "Uncertainty",
                     "EOP")]

## Force to order we want
PlotData <- PlotData %>% arrange(Uncertainty) 

PlotData$Variance <- factor(PlotData$Uncertainty,
                            levels = unique(PlotData$Uncertainty))




## This is convoluted but it adds nice formatting for x axis
Labels_X <- c(
  "-10\n(Better)",
  seq.int(from = -9, to = -1, by = 1),
  "0\n(No change)",
  paste0("+", seq.int(
    from = 1, to = 9, by = 1
  )),
  "+10\n(Worse)"
)


Labels_X_2 <- c(
  "-10\n(Better)",
  -5,
  0,
  paste0("+5"),
  "+10\n(Worse)"
)




## Set here for ease
TextSetup <- element_text(
  size = 14,
  colour = "black",
  family = "serif"
)


Quartile_breaks <- quantile(
  Data$Income_Annual,
  probs = seq(0, 1, by = 1/4),
  na.rm = TRUE
)

Data$Income_Quartile <-
  cut(
    Data$Income_Annual,
    breaks = Quartile_breaks,
    labels = 1:4,
    include.lowest = TRUE
  ) %>%
  as.numeric()


## Summarise data
Data %>%
  group_by(Income_Quartile, Uncertainty) %>%
  summarise(
    Mean_EOP = paste0("£",mean(EOP, na.rm = TRUE) %>% round(2), " (£",
                      sd(EOP, na.rm = TRUE) %>% round(2), ")")
  ) %>% 
  pivot_wider(names_from = Income_Quartile, values_from = Mean_EOP)


# *****************************
# PLOT MEAN VS VARIANCE BY INCOME ####
# *****************************


# Create a custom labeller function
# custom_labeller <- function(variable, value) {
#   if (variable == "Variance") {
#     labels <- c(
#       "0" = "**A**: Highly certain: +/- zero points (N = 151)",
#       "1" = "**B**: Mostly certain: +/- one point (N = 995)",
#       "3" = "**C**: Mostly uncertain: (+/- three points (N = 307)",
#       "5" = "**D**: Highly uncertain: (+/- five points (N = 111)"
#     )
#     return(labels[value])
#   }
# }


## New approach to avoid deprecation issues
custom_labeller <- as_labeller(
  c(
    "0" = "**A**: Highly certain: +/- zero points (N = 151)",
    "1" = "**B**: Mostly certain: +/- one point (N = 995)",
    "3" = "**C**: Mostly uncertain: (+/- three points (N = 307)",
    "5" = "**D**: Highly uncertain: (+/- five points (N = 111)"
  )
)


Figure_2 <- Data[, c("Mean_Change", 
                       "Uncertainty",
                       "EOP",
                       "Income_Quartile")] %>%
  arrange(Uncertainty) %>% 
  mutate(Variance = factor(Uncertainty, levels = unique(Uncertainty)),
         Income_Quartile = as.factor(Income_Quartile)) %>% 
  ggplot(aes(y = EOP, 
             x = Mean_Change, 
             colour = Income_Quartile, 
             fill = Income_Quartile)) +  
  
  # Smooth curves with alpha for SE shading
  stat_smooth(aes(fill = Income_Quartile, colour = Income_Quartile), alpha = 0.25,  linewidth = 1.25) +
  
  theme_bw() +
  
  facet_wrap( ~ Variance,
              nrow = 2,
              ncol = 2,
              labeller = custom_labeller
  ) + 
  
  # Axes and Labels
  ylab("Expected option prices") +
  
  # Add red reference lines
  geom_vline(xintercept = 0, linetype = 'dotted', col = 'red') +
  geom_hline(yintercept = 0, linetype = 'dotted', col = 'red') +
  
  scale_x_continuous(name = "Mean expected harmfulness\n[Better (-10), Worse (+10)]", limits = c(-10, 10)) + 
  scale_y_continuous(breaks = seq(-500, 1500, 10)) +
  
  # Colours and fills for Income Quintile
  scale_colour_manual(
    name = "Income Quartile",  # Legend title for clarity
    values = c("black", "blue", "#008080", "skyblue"),  # Custom colours
    labels = c(
      "Q1 (Lowest Income)",
      "Q2",
      "Q3",
      "Q4 (Highest Income)"
    )) +
  scale_fill_manual(
    name = "Income Quartile",  # Ensure legend aligns with colour scale
    values = c("black", "blue", "#008080",  "skyblue"),
    labels = c(
      "Q1 (Lowest Income)",
      "Q2",
      "Q3",
      "Q4 (Highest Income)"
    )) +
  
  guides(colour = guide_legend(title = "Income Quartile")) +
  
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_markdown(size = TextSetup$size, 
                                  colour = TextSetup$colour, 
                                  family = TextSetup$family),
    legend.position = "bottom",
    legend.text = TextSetup,
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = TextSetup,
    axis.title.x = TextSetup,
    ## Change text to be clearer for reader
    axis.text.y = TextSetup,
    axis.title.y = TextSetup,
    legend.title = TextSetup
  ) +
  coord_cartesian(ylim = c(320, 350))



## Export and save in the right location
ggsave(
  Figure_2,
  device = "png",
  filename = here("CVoutput", "Figure_2_Smooth_Income_EOP_TEST2.png"),
  width = 25,
  height = 15,
  units = "cm",
  dpi = 500
)


# *****************************
# NEW PLOT MEAN VS INCOME BY VARIANCE ####
# *****************************



## New approach to avoid deprecation issues
custom_labeller_2 <- as_labeller(
  c(
    "1" = "**A**: Quartile 1 (Lowest Income)",
    "2" = "**B**: Quartile 2",
    "3" = "**C**: Quartile 3",
    "4" = "**D**: Quartile 4 (Highest Income)"
  )
)


Figure_2B <- Data[, c("Mean_Change", 
                       "Uncertainty",
                       "EOP",
                       "Income_Quartile")] %>%
  
  arrange(Uncertainty) %>% 
  
  mutate(Variance = factor(Uncertainty, levels = unique(Uncertainty)),
         Income_Quartile = as.factor(Income_Quartile)) %>% 
  
  ggplot(aes(y = EOP, 
             x = Mean_Change, 
             colour = Variance, 
             fill = Variance)) +  
  
  # Smooth curves with alpha for SE shading
  stat_smooth(aes(fill = Variance, 
                  colour = Variance), 
              alpha = 0.25,  
              linewidth = 1.25) +
  
  theme_bw() +
  
  facet_wrap( ~ Income_Quartile,
              nrow = 2,
              ncol = 2,
              labeller = custom_labeller_2
  ) + 
  
  # Axes and Labels
  ylab("Expected option prices") +
  
  # Add red reference lines
  geom_vline(xintercept = 0, linetype = 'dotted', col = 'red') +
  geom_hline(yintercept = 0, linetype = 'dotted', col = 'red') +
  
  scale_x_continuous(name = "Mean expected harmfulness\n[Better (-10), Worse (+10)]", limits = c(-10, 10)) + 
  scale_y_continuous(breaks = seq(-500, 1500, 10)) +
  
  # Colours and fills for Income Quintile
  scale_colour_manual(
    name = "Income Quartile",  # Legend title for clarity
    values = c("black", "blue", "#008080", "skyblue"),  # Custom colours
    labels = c(
      "Highly certain: +/- zero points (N = 151)",
      "Mostly certain: +/- one point (N = 995)",
      "Mostly uncertain: (+/- three points (N = 307)",
      "Highly uncertain: (+/- five points (N = 111)"
    )) +
  scale_fill_manual(
    name = "Income Quartile",  # Ensure legend aligns with colour scale
    values = c("black", "blue", "#008080",  "skyblue"),
    labels = c(
      "Highly certain: +/- zero points (N = 151)",
      "Mostly certain: +/- one point (N = 995)",
      "Mostly uncertain: (+/- three points (N = 307)",
      "Highly uncertain: (+/- five points (N = 111)"
    )) +
  
  guides(colour = guide_legend(title = "Income Quartile", 
                               nrow = 2, 
                               ncol = 2))+
  
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_markdown(size = TextSetup$size, 
                                  colour = TextSetup$colour, 
                                  family = TextSetup$family),
    legend.position = "bottom",
    legend.text = TextSetup,
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = TextSetup,
    axis.title.x = TextSetup,
    ## Change text to be clearer for reader
    axis.text.y = TextSetup,
    axis.title.y = TextSetup,
    legend.title = TextSetup
  ) +
  coord_cartesian(ylim = c(310, 360))



## Export and save in the right location
ggsave(
  Figure_2B,
  device = "png",
  filename = here("CVOutput", "Figure_2B_Smooth_Income_EOP_TEST2.png"),
  width = 25,
  height = 15,
  units = "cm",
  dpi = 500
)


# *****************************
# BOXPLOT EOP BY MEAN + VARIANCE   ####
# *****************************


ggplot(Data, aes(
  x = factor(Mean_Change),
  y = EOP,
  fill = factor(Variance)
)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outliers = FALSE) +
  facet_wrap( ~ Variance, ncol = 2) +
  labs(title = "EOP by Mean Change, Facetted by Variance",
       x = "Mean Change",
       y = "EOP") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_markdown(size = TextSetup$size, 
                                  colour = TextSetup$colour, 
                                  family = TextSetup$family),
    legend.position = "none",
    legend.text = TextSetup,
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = TextSetup,
    axis.title.x = TextSetup,
    ## Change text to be clearer for reader
    axis.text.y = TextSetup,
    axis.title.y = TextSetup,
    legend.title = TextSetup
  )




# End Of Script # ********************************************