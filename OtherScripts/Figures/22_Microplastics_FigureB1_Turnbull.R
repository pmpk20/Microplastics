#### Microplastics: IOP Paper ####
## Function: Turnbull estimators of CV ~ Bid | Uncertainty × PaymentVehicle
## Author: PK
## Last change: 16/02/2026
## Outputs: Figure B1 (faceted by payment vehicle)

# **********************************************************************************
#### Section Zero: Libraries ####
# **********************************************************************************
rm(list = ls())
library(data.table)
library(tidyverse)
library(here)
library(DCchoice)

# **********************************************************************************
#### Section One: Import Data ####
# **********************************************************************************


Data <- here("Data", "Microplastics_AllData_Wide_Anonymised.csv") %>%
  fread() %>%
  as.data.frame()


# **********************************************************************************
#### Section Two: Fit Turnbull by Uncertainty × PaymentVehicle_Dummy ####
# **********************************************************************************


# ── Helper: fit Turnbull and extract curve + summary stats ──
fit_turnbull <- function(sub, uncertainty, vehicle) {
  tb_sum <- turnbull.sb(CV ~ Bid, data = sub) %>% summary()
  list(
    curve = tb_sum$estimates %>%
      as.data.frame() %>%
      setNames(c("Bid", "Prob")) %>%
      mutate(Uncertainty = uncertainty,
             PaymentVehicle_Dummy = as.character(vehicle)),
    mean_wtp             = tb_sum$meanWTP,
    median_lower         = tb_sum$medianWTP[1],
    Uncertainty          = uncertainty,
    PaymentVehicle_Dummy = as.character(vehicle)
  )
}


# ── Fit by Uncertainty × PaymentVehicle_Dummy ──
fits <- Data %>%
  distinct(Uncertainty, PaymentVehicle_Dummy) %>%
  arrange(Uncertainty, PaymentVehicle_Dummy) %>%
  pmap(function(Uncertainty, PaymentVehicle_Dummy) {
    Data %>%
      filter(Uncertainty == !!Uncertainty,
             PaymentVehicle_Dummy == !!PaymentVehicle_Dummy) %>%
      fit_turnbull(Uncertainty, PaymentVehicle_Dummy)
  })


# ── Fit pooled (by Uncertainty only) ──
pooled_fits <- Data %>%
  distinct(Uncertainty) %>%
  arrange(Uncertainty) %>%
  pmap(function(Uncertainty) {
    Data %>%
      filter(Uncertainty == !!Uncertainty) %>%
      fit_turnbull(Uncertainty, "2")
  })


all_fits <- c(fits, pooled_fits)


# ── Helper: build annotation df from a list of fits ──
build_annotations <- function(fit_list) {
  fit_list %>%
    map_dfr(~ tibble(
      Uncertainty          = factor(.x$Uncertainty, levels = c(5, 3, 1, 0)),
      PaymentVehicle_Dummy = .x$PaymentVehicle_Dummy,
      mean_wtp             = .x$mean_wtp,
      median_wtp           = .x$median_lower
    )) %>%
    mutate(PaymentVehicle_Dummy = factor(PaymentVehicle_Dummy, levels = c("0", "1", "2"))) %>%
    arrange(PaymentVehicle_Dummy, desc(as.numeric(as.character(Uncertainty)))) %>%
    group_by(PaymentVehicle_Dummy) %>%
    mutate(
      label = sprintf("%s: \u00A3%.0f / \u00A3%.0f",
                      c("\u00B15", "\u00B13", "\u00B11", "\u00B10")[row_number()],
                      mean_wtp, median_wtp),
      y_pos = seq(0.20, 0.05, length.out = n())
    ) %>%
    ungroup()
}


# **********************************************************************************
#### Section Three: Plot setup ####
# **********************************************************************************


uncertainty_colours <- c(`0` = "skyblue", `1` = "purple",
                         `3` = "darkblue", `5` = "black")


uncertainty_labels <- c(`0` = "Highly certain (\u00B10 pts)",
                        `1` = "Mostly certain (\u00B11 pt)",
                        `3` = "Mostly uncertain (\u00B13 pts)",
                        `5` = "Highly uncertain (\u00B15 pts)")


TextSize <- 20


TextSetup <- element_text(size = TextSize,
                          colour = "black",
                          family = "serif")


# **********************************************************************************
#### Section Four: Plot ####
# **********************************************************************************


Plot_Turnbull <- all_fits %>%
  map_dfr("curve") %>%
  mutate(Uncertainty = factor(Uncertainty, levels = c(5, 3, 1, 0)),
         PaymentVehicle_Dummy = factor(PaymentVehicle_Dummy, levels = c("0", "1", "2"))) %>%
  ggplot(aes(x = Bid, y = Prob, colour = Uncertainty)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  geom_text(
    data = build_annotations(all_fits),
    aes(x = 5, y = y_pos, label = label),
    inherit.aes = FALSE,
    hjust = 0, size = 6, family = "serif"
  ) +
  geom_text(
    data = tibble(
      PaymentVehicle_Dummy = factor(c("0", "1", "2"), levels = c("0", "1", "2")),
      x = 5, y = 0.25, label = "WTP (Mean / Median)"
    ),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0, size = 6, family = "serif", fontface = "bold"
  ) +
  facet_wrap(~PaymentVehicle_Dummy,
             labeller = labeller(PaymentVehicle_Dummy = c(
               `0` = "Council tax (N = 228)",
               `1` = "Water bills (N = 1336)",
               `2` = "Pooled (N = 1564)"
             ))) +
  scale_colour_manual(
    values = uncertainty_colours,
    labels = uncertainty_labels,
    name   = "How confident are you in this prediction?"
  ) +
  scale_x_continuous(
    name   = "Bid level (additional GBP per HH per year)",
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80),
    labels = scales::label_dollar(prefix = "\u00A3", accuracy = 1)
  ) +
  scale_y_continuous(
    name   = "Pr(Yes)",
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  ) +
  theme_bw() +
  theme(
    legend.position   = "bottom",
    strip.background  = element_rect(fill = "white"),
    legend.background = element_blank(),
    panel.grid        = element_blank(),
    legend.text       = TextSetup,
    axis.text.x  = TextSetup,
    axis.text.y  = TextSetup,
    axis.title.x =  TextSetup,
    axis.title.y =  TextSetup,
    legend.title = TextSetup,
    strip.text.x = TextSetup
  ) +
  guides(colour = guide_legend(title.position = "top"))


# **********************************************************************************
#### Section Five: Export Plot ####
# **********************************************************************************



ggsave(
  Plot_Turnbull,
  filename = here("Figures", "FigureB1_KMTurnbull.png"),
  device   = "png",
  width    = 40,
  height = 30,
  units = "cm",
  dpi = 500
)

