# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(tidyverse)
library(data.table)
library(parallel)
library(xgboost)
library(earth)
library(data.table)
library(medoutcon)
library(sl3)

## LEARNERS

mean_lrnr <- Lrnr_mean$new()
fglm_lrnr <- Lrnr_glm_fast$new(family = binomial())
lasso_lrnr <- Lrnr_glmnet$new(alpha = 1, family = "binomial", nfolds = 3)
enet_lrnr <- Lrnr_glmnet$new(alpha = 0.5, family = "binomial", nfolds = 3)
rf_lrnr <- Lrnr_ranger$new(num.trees = 200)
xgboost_lrnr <- Lrnr_xgboost$new()

hal_gaussian_lrnr <- Lrnr_hal9001$new(
  family = "gaussian",
  fit_control = list(
    max_degree = 3,
    n_folds = 3,
    use_min = TRUE,
    type.measure = "mse"
  )
)
bound_lrnr <- Lrnr_bound$new(bound = 1e-6)
hal_bounded_lrnr <- Pipeline$new(hal_gaussian_lrnr, bound_lrnr)

lrnr_lib <- Stack$new(
  mean_lrnr, fglm_lrnr, enet_lrnr, lasso_lrnr,
  rf_lrnr, hal_bounded_lrnr
)
sl_lrnr <- Lrnr_sl$new(learners = lrnr_lib, metalearner = Lrnr_nnls$new())

lrnr_lib <- Stack$new(mean_lrnr, fglm_lrnr, lasso_lrnr, rf_lrnr, xgboost_lrnr)
sl_lrnr <- Lrnr_sl$new(learners = lrnr_lib, metalearner = Lrnr_nnls$new())

## DATA

dat <- readRDS("data/analysis_data/analysis_data_alt_shift.rds") |>
  as.data.frame() |>
  mutate(dose_total_clonazepam_and_benzo_1 = dose_total_clonazepam_1 + dose_total_benzo_1,
         dose_total_clonazepam_and_benzo_2 = dose_total_clonazepam_2 + dose_total_benzo_2,
         dose_total_clonazepam_and_benzo_3 = dose_total_clonazepam_3 + dose_total_benzo_3,
         dose_total_clonazepam_and_benzo_4 = dose_total_clonazepam_4 + dose_total_benzo_4,
         dose_total_clonazepam_and_benzo_5 = dose_total_clonazepam_5 + dose_total_benzo_5
  ) |>
  rowwise() |>
  mutate(M1 = sum(dose_total_clonazepam_and_benzo_1 + dose_total_clonazepam_and_benzo_2 + dose_total_clonazepam_and_benzo_3, na.rm = TRUE),
         M2 = sum(dose_total_clonidine_1 + dose_total_clonidine_2 + dose_total_clonidine_3, na.rm = TRUE))

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
       "depression", #missing
       #"depression_missing"
       "D97NPOPI",
       "D97NPOPI_missing"
)


L <- c("max_cows_1", 
            "max_cows_missing_indicator_1")

M <- c("M1", 
       "M2")

dat <- dat |>
  select(W, unlist(L), PROTSEG, M, Y_14)


tmle_de <- medoutcon(
  W = dat[, c(W, L)],
  A = dat$PROTSEG,
  Z = NULL,
  M = dat[, M],
  Y = dat$Y_14,
  g_learners = sl_lrnr,
  h_learners = sl_lrnr,
  b_learners = sl_lrnr,
  q_learners = sl_lrnr,
  r_learners = sl_lrnr,
  effect = "direct",
  estimator = "tmle",
  estimator_args = list(cv_folds = 10)
)

tmle_ie <- medoutcon(
  W = dat[, c(W, L)],
  A = dat$PROTSEG,
  Z = NULL,
  M = dat[, M],
  Y = dat$Y_14,
  g_learners = sl_lrnr,
  h_learners = sl_lrnr,
  b_learners = sl_lrnr,
  q_learners = sl_lrnr,
  r_learners = sl_lrnr,
  effect = "indirect",
  estimator = "tmle",
  estimator_args = list(cv_folds = 10)
)

te_var <- var(tmle_ie$eif + tmle_de$eif)/nrow(dat)

te <- tmle_ie$theta + tmle_de$theta

df <- data.frame(lwr_ci = te - qnorm(0.975) * sqrt(te_var),
                 param_est = te,
                 upr_ci = te + qnorm(0.975) * sqrt(te_var),
                 var_est = te_var,
                 eif_mean = mean(tmle_ie$eif + tmle_de$eif),
                 estimator = "tmle",
                 param = "total_natural"
                 )

df <- df |>
  rbind(summary(tmle_ie)) |>
  rbind(summary(tmle_de))