library(simcausal)
#remotes::install_github("nt-williams/lcmmtp@overhaul")
library(tidyverse)
library(lcmmtp)
library(mlr3superlearner)
library(mlr3extralearners)
library(xgboost)
library(earth)

#imports
# library(checkmate)
# library(data.table)
# library(glue)
# library(ife)
# library(origami)
# library(R6)
# library(slider)
# 
# files <- list.files("lcmmtp/R/", pattern = "\\.R$", full.names = TRUE)
# sapply(files, source)

dag <-
  DAG.empty() +
  # baseline (pre-exposure)
  node("L0", distr = "rbern", prob = 0.3) +
  # time 1
  node("L1", distr = "rbern", prob = 0.5) +
  node("A1", distr = "rbern", prob = 0.4 + L1*0.09 + L0 * 0.01) +
  # node("A1", distr = "rbern", prob = 1) +
  # node("C1", distr = "rbern", prob = 0, EFU = TRUE) +
  node("M1", distr = "rbern", prob = 0.6 - A1*0.1 + L1*0.1 - L0 * 0.00005) +
  node("D2", distr = "rbern", prob = 0.01 + M1*0.05 + A1*0.02 - L1*0.001 + L0 * 0.000002, EFU = TRUE) +
  node("Y2", distr = "rbern", prob = D2*0 + (1-D2)*(0.3 + M1*0.05 + A1*0.2 - L1*0.001 + L0 * 0.0002), EFU = TRUE) +
  # time 2
  node("L2", distr = "rbern", prob = 0.5) +
  node("A2", distr = "rbern", prob = 0.4 + L2*0.1) +
  # node("A2", distr = "rbern", prob = 1) +
  # node("C2", distr = "rbern", prob = 0, EFU = TRUE) +
  node("M2", distr = "rbern", prob = D2*0 + (1-D2)*(0.6 - A2*0.1 + L2*0.1)) +
  node("D3", distr = "rbern", prob = D2*1 + (1-D2)*(0.01 + M2*0.05 + A2*0.02 - L2*0.001), EFU = TRUE) +
  node("Y3", distr = "rbern", prob = D3*0 + (1-D3)*(0.3 + M2*0.05 + A2*0.2 - L2*0.001), EFU = TRUE) +
  # time 3
  node("L3", distr = "rbern", prob = 0.5) +
  node("A3", distr = "rbern", prob = 0.4 + L3*0.1) +
  # node("A3", distr = "rbern", prob = 1) +
  # node("C3", distr = "rbern", prob = 0, EFU = TRUE) +
  node("M3", distr = "rbern", prob = D3*0 + (1-D3)*(0.6 - A3*0.1 + L3*0.1)) +
  node("D4", distr = "rbern", prob = D3*1 + (1-D3)*(0.01 + M3*0.05 + A3*0.03 - L3*0.001), EFU = TRUE) +
  node("Y4", distr = "rbern", prob = D3*0 + (1-D3)*(0.3 + M3*0.05 + A3*0.3 - L3*0.001), EFU = TRUE)

dag <- set.DAG(dag)

data <- sim(dag, n = 1e4, LTCF = "Y")

# carry forward D and Y

data <- data |>
  mutate(
    # across(starts_with("C"), ~ifelse(. == 1, 0, 1)),
    # C2 = ifelse(C1 == 0, C1, C2),
    # C3 = ifelse(C2 == 0, C2, C3),
    D3 = case_when(D2 == 1 ~ 1, 
                   Y2 == 1 ~ 0,
                   Y3 == 1 ~ 0,
                   TRUE ~ D3),
    D4 = case_when(D3 == 1 ~ 1, 
                   Y2 == 1 ~ 0,
                   Y3 == 1 ~ 0,
                   Y4 == 1 ~ 0,
                   TRUE ~ D4),
    Y2 = ifelse(D2 == 1, 0, Y2),
    Y3 = ifelse(D3 == 1, 0, Y3),
    Y4 = ifelse(D4 == 1, 0, Y4),
    Y3 = ifelse(Y2 == 1, 1, Y3),
    Y4 = ifelse(Y3 == 1, 1, Y4),
    M2 = ifelse(Y2 == 1 | D2 == 1, NA, M2),
    M3 = ifelse(Y3 == 1 | D3 == 1, NA, M3)
  )

d_ap <- function(data, trt) rep(1, length(data[[trt]]))
d_as <- function(data, trt) rep(1, length(data[[trt]]))

# single timepoint

lcmmtp(data = data,   
       treatment = c("A1"),
       outcome = c("Y2"),
       mediator = c("M1"),
       competingRisks = c("D2"), 
       baselineConfounders = "L0", 
       mediatorOutcomeConfounders = list(c("L1")),
       timeVaryConfounders = list(NULL),
       censoring = NULL,
       d_prime = d_ap, 
       d_star = d_as,
       control = .lcmmtp_control(folds = 1))


# 2 timepoints (single A)

# everything after D2 is missing except for Dt and Yt.
data |> filter(D2 == 1) |> summary()

data |> filter(D2 == 0, Y2 == 0) |> summary()

lcmmtp(data = data,   
       treatment = "A1",
       outcome = c("Y2", "Y3"),
       mediator = c("M1", "M2"),
       competingRisks = c("D2", "D3"), 
       baselineConfounders = "L0", 
       mediatorOutcomeConfounders = list(c("L1"), c("L2")),
       timeVaryConfounders = list(NULL, NULL),
       censoring = NULL,
       d_prime = d_ap, 
       d_star = d_as,
       control = .lcmmtp_control(folds = 1))

# 2 timepoints (multiple A)

lcmmtp(data = data,   
       treatment = c("A1", "A2"),
       outcome = c("Y2", "Y3"),
       mediator = c("M1", "M2"),
       competingRisks = c("D2", "D3"), 
       baselineConfounders = "L0", 
       mediatorOutcomeConfounders = list(c("L1"), c("L2")),
       timeVaryConfounders = list(NULL, NULL),
       censoring = NULL,
       d_prime = d_ap, 
       d_star = d_as,
       control = .lcmmtp_control(folds = 1))

# 3 timepoints
lcmmtp(data = data,   
       treatment = c("A1", "A2", "A3"),
       outcome = c("Y2", "Y3", "Y4"),
       mediator = c("M1", "M2", "M3"),
       competingRisks = c("D2", "D3", "D4"), 
       baselineConfounders = "L0", 
       mediatorOutcomeConfounders = list(c("L1"), c("L2"), c("L3")),
       timeVaryConfounders = list(NULL, NULL, NULL),
       censoring = NULL,
       d_prime = d_ap, 
       d_star = d_as,
       control = .lcmmtp_control(folds = 1))



