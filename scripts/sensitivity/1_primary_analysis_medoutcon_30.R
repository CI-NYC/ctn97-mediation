# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
#remotes::install_github("shodaiinose/medoutcon@nocrossfit")

## NOTE: need to add ridge for stability for the sensitivity analysis (see commented code in medoutcon)
library(tidyverse)
library(data.table)
library(parallel)
library(xgboost)
library(earth)
library(data.table)
library(medoutconnocrossfit)
library(sl3)

dat <- readRDS("data/analysis_data/analysis_data_alt_shift.rds") |>
  as.data.frame() |>
  mutate(across(starts_with("C_"), ~ ifelse(. == 0, 1, ifelse(. == 1, 0, .)))) |>
  mutate(dose_total_clonazepam_and_benzo_1 = dose_total_clonazepam_1 + dose_total_benzo_1,
         dose_total_clonazepam_and_benzo_2 = dose_total_clonazepam_2 + dose_total_benzo_2,
         dose_total_clonazepam_and_benzo_3 = dose_total_clonazepam_3 + dose_total_benzo_3,
         dose_total_clonazepam_and_benzo_4 = dose_total_clonazepam_4 + dose_total_benzo_4,
         dose_total_clonazepam_and_benzo_5 = dose_total_clonazepam_5 + dose_total_benzo_5
  ) |>
  rowwise() |>
  mutate(M1 = mean(c(dose_total_clonazepam_and_benzo_1, dose_total_clonazepam_and_benzo_2, dose_total_clonazepam_and_benzo_3), na.rm = TRUE),
         M2 = mean(c(dose_total_clonidine_1, dose_total_clonidine_2, dose_total_clonidine_3), na.rm = TRUE)) |>
  mutate(M1 = case_when(M1 < 1 ~ 0,
                        M1 < 2 ~ 1,
                        M1 < 3 ~ 2,
                        TRUE ~ 3),
         M2 = case_when(M2 <= 0.1 ~ 0,
                        TRUE ~ 1))

# total effect should be around 0.27
dat |> group_by(PROTSEG) |> summarize(initiated = sum(ifelse(Y_30 == 1, 1, 0)), count = n(), perc = initiated/count)

W <- c("days_from_admission_to_consent",
       # demographics
       "DESEX",
       "age",
       "is_hispanic",
       "DEWHITE",
       "DEBLACK",
       "DEOTHER",
       # substance use
       "alcohol_use_disorder",
       "amphetamine_use_disorder",
       "cannabis_use_disorder",
       "cocaine_use_disorder",
       "sedative_use_disorder",
       "injection_opioid_use",
       "injection_opioid_use_missing",
       "years_since_first_opioid_use",
       "years_since_first_opioid_use_missing",
       # mental health
       "anxiety",
       "bipolar",
       "depression",
       "D97NPOPI",
       "D97NPOPI_missing")

L <- c("max_cows_1",
       "max_cows_missing_indicator_1")

M <- c("M1",
       "M2")

dat <- dat |>
  select(W, L, PROTSEG, M, C_30, Y_30)

learners_txt <- "mean_glm_lasso1SE_interactions_crossfit"

s <- 1
set.seed(s)

i <- 1

    tmle_de <- medoutcon(
      W = dat[, c(W, L)],
      A = dat$PROTSEG,
      Z = NULL,
      M = dat[, M],
      Y = dat$Y_30,
      g_learners = Lrnr_mean$new(),
      h_learners = NULL,
      b_learners = NULL,
      q_learners = NULL,
      r_learners = NULL,
      effect = "direct",
      estimator = "tmle",
      g_bounds = c(0, 1),
      estimator_args = list(cv_folds = i)
    )
    
    tmle_ie <- medoutcon(
      W = dat[, c(W, L)],
      A = dat$PROTSEG,
      Z = NULL,
      M = dat[, M],
      Y = dat$Y_30,
      g_learners = Lrnr_mean$new(),
      h_learners = NULL,
      b_learners = NULL,
      q_learners = NULL,
      r_learners = NULL,
      effect = "indirect",
      estimator = "tmle",
      g_bounds = c(0, 1),
      estimator_args = list(cv_folds = i)
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
    
saveRDS(tmle_de, paste0("results_medoutcon_101025/tmle_de_learners_", learners_txt, "_seed_", s, "_", i, "_benzo_and_clon_M_sens.rds"))
saveRDS(tmle_ie, paste0("results_medoutcon_101025/tmle_ie_learners_", learners_txt, "_seed_", s, "_", i, "_benzo_and_clon_M_sens.rds"))
saveRDS(df, paste0("results_medoutcon_101025/res_learners_", learners_txt, "_seed_", s, "_", i, "_benzo_and_clon_M_sens.rds"))
