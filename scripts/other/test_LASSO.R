#remotes::install_github("nt-williams/lcmmtp@overhaul")
library(tidyverse)
library(mlr3superlearner)
library(mlr3extralearners)
library(xgboost)
library(earth)
library(checkmate)
library(lcmmtp)

dat <- readRDS(here::here("data/analysis_data/analysis_data_alt_shift.rds")) |>
  as.data.frame() |>
  mutate(dose_total_clonazepam_and_benzo_1 = dose_total_clonazepam_1 + dose_total_benzo_1,
         dose_total_clonazepam_and_benzo_2 = dose_total_clonazepam_2 + dose_total_benzo_2,
         dose_total_clonazepam_and_benzo_3 = dose_total_clonazepam_3 + dose_total_benzo_3,
         dose_total_clonazepam_and_benzo_4 = dose_total_clonazepam_4 + dose_total_benzo_4,
         dose_total_clonazepam_and_benzo_5 = dose_total_clonazepam_5 + dose_total_benzo_5
  ) |>
  mutate(
    Group_1 = case_when(dose_total_clonidine_1 >= 0.1 & dose_total_clonazepam_and_benzo_1 >= 3 & max_cows_eligible_1 == 1 ~ 3,
                        (dose_total_clonidine_1 >= 0.1 | dose_total_clonazepam_and_benzo_1 >= 2) & max_cows_eligible_1 == 1 ~ 2,
                        (dose_total_clonidine_1 >= 0.1 | dose_total_clonazepam_and_benzo_1 > 0) & max_cows_eligible_1 == 1 ~ 1,
                        TRUE ~ 0),
    Group_2 = case_when(is.na(adj_2) ~ as.numeric(NA),
                        dose_total_clonidine_2 >= 0.1 & dose_total_clonazepam_and_benzo_2 >= 3 & max_cows_eligible_2 == 1 ~ 3,
                        (dose_total_clonidine_2 >= 0.1 | dose_total_clonazepam_and_benzo_2 >= 2) & max_cows_eligible_2 == 1 ~ 2,
                        (dose_total_clonidine_2 >= 0.1  | dose_total_clonazepam_and_benzo_2 > 0) & max_cows_eligible_2 == 1 ~ 1,
                        TRUE ~ 0),
    Group_3 = case_when(is.na(adj_3) ~ as.numeric(NA),
                        dose_total_clonidine_3 >= 0.1 & dose_total_clonazepam_and_benzo_3 >= 3 & max_cows_eligible_3 == 1 ~ 3,
                        (dose_total_clonidine_3 >= 0.1 | dose_total_clonazepam_and_benzo_3 >= 2) & max_cows_eligible_3 == 1 ~ 2,
                        (dose_total_clonidine_3 >= 0.1 | dose_total_clonazepam_and_benzo_3 > 0) & max_cows_eligible_3 == 1 ~ 1,
                        TRUE ~ 0),
    Group_4 = case_when(is.na(adj_4) ~ as.numeric(NA),
                        dose_total_clonidine_4 >= 0.1 & dose_total_clonazepam_and_benzo_4 >= 3 & max_cows_eligible_4 == 1 ~ 3,
                        (dose_total_clonidine_4 >= 0.1 | dose_total_clonazepam_and_benzo_4 >= 2) & max_cows_eligible_4 == 1 ~ 2,
                        (dose_total_clonidine_4 >= 0.1 | dose_total_clonazepam_and_benzo_4 > 0) & max_cows_eligible_4 == 1 ~ 1,
                        TRUE ~ 0),
    Group_5 = case_when(is.na(adj_5) ~ as.numeric(NA),
                        dose_total_clonidine_5 >= 0.1 & dose_total_clonazepam_and_benzo_5 >= 3 & max_cows_eligible_5 == 1 ~ 3,
                        (dose_total_clonidine_5 >= 0.1 | dose_total_clonazepam_and_benzo_5 >= 2) & max_cows_eligible_5 == 1 ~ 2,
                        (dose_total_clonidine_5>= 0.1 | dose_total_clonazepam_and_benzo_5 > 0) & max_cows_eligible_5 == 1 ~ 1,
                        TRUE ~ 0)  
  )

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

M <- c(c("Group_1"),
       c("Group_2"),
       c("Group_3"),
       c("Group_4"),
       c("Group_5")
)

A <- "PROTSEG"

L <- list(c("max_cows_1", 
            "max_cows_missing_indicator_1"), #benzo
          c("max_cows_2", 
            "max_cows_missing_indicator_2"), #benzo
          c("max_cows_3", 
            "max_cows_missing_indicator_3"), #benzo
          c("max_cows_4", 
            "max_cows_missing_indicator_4"), #benzo
          c("max_cows_5", 
            "max_cows_missing_indicator_5") #benzo
)

learners <- list("cv_glmnet"
)

# function for running lmtp
run_lcmmtp <-  function(data, day = 5, x = 1, y = 1) # x = 0 and y = 0, x = 1 and y = 0, ATE = 1,1 - 0,0, IIE = 1,1 - 1,0, IDE = 1,0 - 0,0
{
  if (day <= 5) {
    outcome_nodes <- 1:day
  } else {
    outcome_nodes <- c(1, 2, 3, 4, day)
  }
  
  num_folds <- 5L
  
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
                              folds_trt = 1,
                              folds_mediator = 1,
                              folds_QL = 1,
                              folds_QZ = 1,
                              folds_QM = 1,
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
for (x in c(0))
{
  for (y in c(0))
  {
    if (x == 0 && y == 1) {
      next
    }
    for (i in 14:14) 
    {
      
      set.seed(9)
      res <- progressr::with_progress(run_lcmmtp(data = dat,
                                                 day = i,
                                                 x = x,
                                                 y = y
      ))
      
      saveRDS(res, here::here(paste0("results_061725/mediation_", x, "_", y, "_", i, "LASSO.rds")))
    }
  }
}