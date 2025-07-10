#remotes::install_github("nt-williams/lcmmtp@overhaul")
library(tidyverse)
library(mlr3superlearner)
library(mlr3extralearners)
library(xgboost)
library(earth)
library(checkmate)
library(lcmmtp)

# NOTES: competing risk indicated with 1 (once competing risk occurs, set all subsequent outcome nodes to 0 and subsequent competing risk nodes to 1)
# NOTES: censoring indicated with 0 (once censoring occurs, set all subsequent outcome nodes to NA)

dat <- readRDS(here::here("...")) 

# baselne covariates
W <- c( "W_1",
        "W_2",
        "W_3"
)

# mediators -- one per timepoint
M <- c(c("M_1"),
       c("M_2"),
       c("M_3"),
       c("M_4"),
       c("M_5")
)

# treatment
A <- "TRT"

# time-varying covariates
L <- list(c("L1_1", "L2_1"), # t=1
          c("L1_2", "L2_2"), # t=2
          c("L1_3", "L2_3"), # t=3
          c("L1_4", "L2_4"), # t=4
          c("L1_5", "L2_5")  # t=5
)

# mediator-outcome confounders
Z <- list(c("Z1_1", "Z2_1"), # t=1
          c("Z1_2", "Z2_2"), # t=2
          c("Z1_3", "Z2_3"), # t=3
          c("Z1_4", "Z2_4"), # t=4
          c("Z1_5", "Z2_5")  # t=5
)

# adjust these
learners <- list("mean",
                 "glm", 
                 "earth",
                 list("xgboost",
                      lambda = 2,
                      id = "xgboost1")
)

# function for running lcmmtp
run_lcmmtp <-  function(data, t = 5, x = 1, y = 1) # x = 0 and y = 0, x = 1 and y = 0, ATE/ITE = 1,1 - 0,0, IIE = 1,1 - 1,0, IDE = 1,0 - 0,0
{
  if (t <= 5) {
    outcome_nodes <- 1:t
  } else {
    outcome_nodes <- c(1, 2, 3, 4, t)
  }
  
  num_folds <- 10L # adjust depending on size of data
  
  result <- lcmmtp(
    data = data, 
    treatment = A,
    outcome = paste0("Y_", outcome_nodes), 
    mediator = M, # character -- one mediator per timeperiod (can be few-valued, discrete)
    competingRisks = paste0("D_", outcome_nodes), # or NULL if you don't have this
    baselineConfounders = W,
    timeVaryConfounders = L,
    mediatorOutcomeConfounders = Z,
    censoring = NULL, # or set this if you have it
    d_prime = function(data, treatment) rep(x, length(data[[treatment]])), # intervention on A to see counterfactual outcome on Y
    d_star = function(data, treatment) rep(y, length(data[[treatment]])), # intervention on A to see counterfactual M
    control = .lcmmtp_control(folds = num_folds,
                              folds_trt = NULL,
                              folds_mediator = NULL,
                              folds_QL = NULL,
                              folds_QZ = NULL,
                              folds_QM = NULL,
                              learners_trt = learners,
                              learners_mediator = learners,
                              learners_QL = learners,
                              learners_QZ = learners,
                              learners_QM = learners),
    id = NULL)
  
  result
}

dat |> select(PROTSEG, max_cows_1, adj_1, C_1, Y_1, 
              max_cows_2, adj_2, C_2, Y_2, 
              max_cows_3, adj_3, C_3, Y_3, 
              max_cows_4, adj_4, C_4, Y_4, 
              max_cows_5, adj_5, C_5, Y_5) |>
  summary()

set.seed(9)
for (x in c(1))
{
  for (y in c(0))
  {
    if (x == 0 && y == 1) {
      next
    }
    for (i in 1:5) 
    {
      
      set.seed(9)
      res <- run_lcmmtp(data = dat,
                        t = i,
                        x = x,
                        y = y
      )
      
      saveRDS(res, here::here(paste0("...", x, "_", y, "_", i, ".rds")))
    }
  }
}