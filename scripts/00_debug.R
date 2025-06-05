#remotes::install_github("nt-williams/lcmmtp@overhaul")
library(tidyverse)
library(mlr3superlearner)
library(mlr3extralearners)
library(xgboost)
library(earth)
library(checkmate)
library(data.table)
library(lcmmtp)

# folder_path <- "lcmmtp/R"
# r_files <- list.files(folder_path, pattern = "\\.R$", full.names = TRUE)
# invisible(lapply(r_files, source))
# 
dat <- readRDS(here::here("data/analysis_data/analysis_data_alt_shift.rds")) |>
  as.data.frame()

dat <- dat |>
  mutate(across(starts_with("C_"), ~ ifelse(. == 0, 1, ifelse(. == 1, 0, .)))) # alternating to match competing risks format

W <- c("days_from_admission_to_consent",
       # demographics
       "DESEX",
       "age",
       "is_hispanic",
       #"is_hispanic_missing",
       "DEWHITE",
       "DEBLACK",
       #"DEAMEIND",
       #"DEAAPI",
       "DEOTHER",
       #"DERACE_missing",
       #"PROTSEG",
       # substance use
       "alcohol_use_disorder", #missing
       #"alcohol_use_disorder_missing",
       "amphetamine_use_disorder", #missing
       #"amphetamine_use_disorder_missing",
       "cannabis_use_disorder", #missing
       #"cannabis_use_disorder_missing",
       "cocaine_use_disorder", #missing
       #"cocaine_use_disorder_missing",
       "sedative_use_disorder", #missing
       #"sedative_use_disorder_missing",
       "injection_opioid_use",
       "injection_opioid_use_missing",
       "years_since_first_opioid_use",
       "years_since_first_opioid_use_missing",
       # mental health
       "anxiety", #missing
       #"anxiety_missing",
       "bipolar", #missing,
       #"bipolar_missing",
       "depression"#, #missing
       #"depression_missing"
)

M <- c(c("adj_1"),
       c("adj_2"),
       c("adj_3"),
       c("adj_4"),
       c("adj_5")
)

A <- "PROTSEG"


L <- list(c("max_cows_1", 
            #"max_cows_eligible_1", # always 1
            "max_cows_missing_indicator_1"), #benzo
          c("max_cows_2", 
            #"max_cows_eligible_2", # always 1
            "max_cows_missing_indicator_2"), #benzo
          c("max_cows_3", 
            "max_cows_eligible_3", 
            "max_cows_missing_indicator_3"), #benzo
          c("max_cows_4", 
            "max_cows_eligible_4", 
            "max_cows_missing_indicator_4"), #benzo
          c("max_cows_5", 
            "max_cows_eligible_5", 
            "max_cows_missing_indicator_5") #benzo
)

learners <- list("mean",
                 "glm")

# function for running lmtp
run_lcmmtp <-  function(data, day = 5, x = 1, y = 1) # x = 0 and y = 0, x = 1 and y = 0, ATE = 1,1 - 0,0, IIE = 1,1 - 1,0, IDE = 1,0 - 0,0
{
  if (day <= 5) {
    outcome_nodes <- 1:day
  } else {
    outcome_nodes <- c(1, 2, 3, 4, day)
  }
  
  num_folds <- 2L
  
  result <- lcmmtp(
    data = data, 
    treatment = "PROTSEG",
    outcome = paste0("Y_", outcome_nodes), 
    mediator = M, # character -- one mediator per timeperiod
    competingRisks = paste0("C_", outcome_nodes),
    baselineConfounders = W,
    timeVaryConfounders = list(NULL, NULL, NULL, NULL, NULL),
    mediatorOutcomeConfounders = L,
    censoring = NULL,
    d_prime = function(data, treatment) rep(x, length(data[[treatment]])), # intervention on A to see counterfactual outcome on Y
    d_star = function(data, treatment) rep(y, length(data[[treatment]])), # intervention on A to see counterfactual M
    control = .lcmmtp_control(folds = num_folds,
                              #folds_trt = num_folds,
                              #folds_mediator = num_folds,
                              #folds_QL = num_folds,
                              #folds_QZ = num_folds,
                              #folds_QM = num_folds,
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
x <- 0
y <- 0
    if (x == 0 && y == 1) {
      next
    }
    for (i in 14:14)
    {
      
      set.seed(9)
      res <- run_lcmmtp(data = dat,
                        day = i,
                        x = x,
                        y = y
      )
      
      saveRDS(res, here::here(paste0("results/mediation_", x, "_", y, "_", i, "_lambda_100.rds")))
    }