#devtools::install_github("nt-williams/lmtp@competing-risks")
library(tidyverse)

# loading data and making into wide format

DMA <- read_csv("data/DMA.csv") |>
  select(PATID,
         DMAMDDT,
         DMOTBZO
  ) |>
  filter(!is.na(PATID)) |>
  filter(!is.na(DMOTBZO)) |>
  mutate(DMOTBZO = as.numeric(DMOTBZO))

dat_long <- readRDS(here::here("data/analysis_data/max_cows_data_alt.rds")) |>
  left_join(DMA, by = c("PATID" = "PATID",
                        "DMAMDDT" = "DMAMDDT")) |>
  mutate(A1 = case_when(day_post_consent == naltrexone_injection_day & pre_injection_clonidine >= 0.1 ~ 1, # if injection day, only look at pre-injection 
                        DMCLDDTL >= 0.1 ~ 1, # looking at all clonidine in a day
                        TRUE ~ 0), # otherwise, did not receive medication
         A2 = case_when(day_post_consent == naltrexone_injection_day & pre_injection_clonazepam >= 1 ~ 1,  # if injection day, only look at pre-injection 
                        DMCZPDTL >= 1 ~ 1, # looking at all clonazepam doses in a day
                        TRUE ~ 0), # otherwise, did not receive medication
         A3 = case_when(DMBZODTL > 0 ~ 1, # any benzo
                        TRUE ~ 0),
         L1 = case_when(DMBUPDTL > 0 ~ 1, # any bup
                        TRUE ~ 0), 
         L2 = ifelse(DMDROWSY == 3 | DMDIZZY == 3, 1, 0)) |> # severely dizzy or drowsy (not using)
  #mutate(both_inelig = rowSums(cbind(both_inelig, L2), na.rm = TRUE)) |>
  mutate(dose_total_clonidine = ifelse(is.na(DMCLDDTL), 0, DMCLDDTL),
         dose_total_clonazepam = ifelse(is.na(DMCZPDTL), 0, DMCZPDTL),
         # converting to clonazepam units (see p 114 https://www.healthquality.va.gov/guidelines/MH/sud/VA-DoD-SUD-CPG_Final_for-508_v3.pdf)
         dose_total_benzo = case_when(is.na(DMBZODTL) ~ 0, 
                                      DMOTBZO == 1 ~ DMBZODTL/10, #diazepam
                                      DMOTBZO == 2 ~ DMBZODTL/25, #chlordiazepoxide
                                      DMOTBZO == 3 ~ DMBZODTL/2, #lorazepam
                                      DMOTBZO == 4 ~ DMBZODTL, #alprazolam
                                      DMOTBZO == 5 ~ DMBZODTL/15, #tempazepam
                                      DMOTBZO == 6 ~ DMBZODTL/30, #oxazepam
                                      DMOTBZO == 99 ~ DMBZODTL #other (alprazolam)
         )) |>
  filter(day_post_consent <= 15) |>
  mutate_at(vars(starts_with("L")), ~ if_else(is.na(.), 0, .)) |>
  rename("max_cows_ineligible" = "both_inelig") |>
  mutate(end_shift = case_when(naltrexone_injection_day == day_post_consent & is.na(max_cows_time) & pre_injection_clonidine == 0 & pre_injection_clonazepam == 0 & is.na(DMBZODTL) ~ 1, # if no cows/trt on day x, then injection counted as previous day
                               end_induction_day == day_post_consent & day_post_consent != 1 & is.na(max_cows_time) & is.na(DMCLDDTL) & is.na(DMCZPDTL) & is.na(DMBZODTL) ~ 1, # if no cows/trt on day x, then leaving counted as previous day
                               TRUE ~ 0),
         adj = ifelse(rowSums(cbind(A1, A2, A3), na.rm = TRUE) >= 1, 1, 0)) |>
  select(PATID, PROTSEG, naltrexone_injection_day, naltrexone_injection_time, end_induction_day, 
         received_naltrexone_injection, days_from_admission_to_consent, day_post_consent, max_cows, max_cows_time, max_cows_ineligible, ends_with("inelig"), A1, A2, A3, adj, starts_with("dose_total"), L1, L2, end_shift) |>
  group_by(PATID) |>
  mutate(patient_shifted = any(end_shift == 1)) |>
  mutate(naltrexone_injection_day = ifelse(patient_shifted == TRUE, naltrexone_injection_day - 1, naltrexone_injection_day),
         end_induction_day = ifelse(patient_shifted == TRUE, end_induction_day - 1, end_induction_day)) |>
  select(-c(end_shift, patient_shifted)) 

dat_long <- dat_long |>
  filter(day_post_consent <= 14, day_post_consent <= end_induction_day) |>
  mutate(max_cows_missing_indicator = ifelse(is.na(max_cows), 1, 0),
         max_cows = ifelse(is.na(max_cows), 0, max_cows), # replacing missing max cows with 0 
         max_cows_ineligible = ifelse(is.na(max_cows_ineligible), 0, max_cows_ineligible)) # replacing missing max cows eligible with 0

dat_long |> select(PATID, PROTSEG, naltrexone_injection_day) |> group_by(PROTSEG) |> summarize(mean_day = mean(naltrexone_injection_day, na.rm = TRUE))

saveRDS(dat_long, "data/analysis_data/dat_long_alt_shift.rds")

dat <- dat_long |>
  pivot_wider(names_from = day_post_consent, values_from = c(max_cows, max_cows_time, max_cows_missing_indicator, max_cows_ineligible, ends_with("inelig"), A1, A2, A3, L1, L2, adj, starts_with("dose_total"))) |>
  mutate(PROTSEG = ifelse(PROTSEG == "C", 0, 1))

# making outcome variables

dat <- dat |>
  mutate(Y_1 = 0,
         Y_2 = case_when(is.na(naltrexone_injection_day) ~ 0,
                         naltrexone_injection_day == 2 ~ 1,
                         TRUE ~ 0),
         Y_3 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                         Y_2 == 1 ~ 1,
                         naltrexone_injection_day == 3 ~ 1,
                         TRUE ~ 0),
         Y_4 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                         Y_3 == 1 ~ 1,
                         naltrexone_injection_day == 4 ~ 1,
                         TRUE ~ 0),
         Y_5 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                         Y_4 == 1 ~ 1,
                         naltrexone_injection_day == 5 ~ 1,
                         TRUE ~ 0),
         Y_6 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                         Y_5 == 1 ~ 1,
                         naltrexone_injection_day == 6 ~ 1,
                         TRUE ~ 0),
         Y_7 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                         Y_6 == 1 ~ 1,
                         naltrexone_injection_day == 7 ~ 1,
                         TRUE ~ 0),
         Y_8 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                         Y_7 == 1 ~ 1,
                         naltrexone_injection_day == 8 ~ 1,
                         TRUE ~ 0),
         Y_9 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                         Y_8 == 1 ~ 1,
                         naltrexone_injection_day == 9 ~ 1,
                         TRUE ~ 0),
         Y_10 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                          Y_9 == 1 ~ 1,
                          naltrexone_injection_day == 10 ~ 1,
                          TRUE ~ 0),
         Y_11 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                          Y_10 == 1 ~ 1,
                          naltrexone_injection_day == 11 ~ 1,
                          TRUE ~ 0),
         Y_12 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                          Y_11 == 1 ~ 1,
                          naltrexone_injection_day == 12 ~ 1,
                          TRUE ~ 0),
         Y_13 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                          Y_12 == 1 ~ 1,
                          naltrexone_injection_day == 13 ~ 1,
                          TRUE ~ 0),
         Y_14 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                          Y_13 == 1 ~ 1,
                          naltrexone_injection_day == 14 ~ 1,
                          TRUE ~ 0),
         Y_30 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                          Y_14 == 1 ~ 1,
                          naltrexone_injection_day > 14 & naltrexone_injection_day <= 30 ~ 1,
                          TRUE ~ 0),
         )

# making censoring variables

dat <- dat |>
  mutate(C_1 = ifelse(end_induction_day == 1 & received_naltrexone_injection == 0, 0, 1),
         C_2 = case_when(C_1 == 0 ~ 0,
                         end_induction_day == 2 & received_naltrexone_injection == 0 ~ 0,
                         TRUE ~ 1),
         C_3 = case_when(C_2 == 0 ~ 0,
                         end_induction_day == 3 & received_naltrexone_injection == 0 ~ 0,
                         TRUE ~ 1),
         C_4 = case_when(C_3 == 0 ~ 0,
                         end_induction_day == 4 & received_naltrexone_injection == 0 ~ 0, 
                         TRUE ~ 1),
         C_5 = case_when(C_4 == 0 ~ 0,
                         end_induction_day == 5 & received_naltrexone_injection == 0 ~ 0, 
                         TRUE ~ 1),
         C_6 = case_when(C_5 == 0 ~ 0,
                         end_induction_day == 6 & received_naltrexone_injection == 0 ~ 0, 
                         TRUE ~ 1),
         C_7 = case_when(C_6 == 0 ~ 0,
                         end_induction_day == 7 & received_naltrexone_injection == 0 ~ 0, 
                         TRUE ~ 1),
         C_8 = case_when(C_7 == 0 ~ 0,
                         end_induction_day == 8 & received_naltrexone_injection == 0 ~ 0, 
                         TRUE ~ 1),
         C_9 = case_when(C_8 == 0 ~ 0,
                         end_induction_day == 9 & received_naltrexone_injection == 0 ~ 0, 
                         TRUE ~ 1),
         C_10 = case_when(C_9 == 0 ~ 0,
                          end_induction_day == 10 & received_naltrexone_injection == 0 ~ 0, 
                          TRUE ~ 1),
         C_11 = case_when(C_10 == 0 ~ 0,
                          end_induction_day == 11 & received_naltrexone_injection == 0 ~ 0, 
                          TRUE ~ 1),
         C_12 = case_when(C_11 == 0 ~ 0,
                          end_induction_day == 12 & received_naltrexone_injection == 0 ~ 0, 
                          TRUE ~ 1),
         C_13 = case_when(C_12 == 0 ~ 0,
                          end_induction_day == 13 & received_naltrexone_injection == 0 ~ 0, 
                          TRUE ~ 1),
         C_14 = case_when(C_13 == 0 ~ 0,
                          end_induction_day == 14 & received_naltrexone_injection == 0 ~ 0, 
                          TRUE ~ 1),
         C_30 = case_when(C_14 == 0 ~ 0,
                          end_induction_day > 14 & end_induction_day <= 30 & received_naltrexone_injection == 0 ~ 0, 
                          TRUE ~ 1))  |>
  select(PATID, PROTSEG, received_naltrexone_injection,
         days_from_admission_to_consent,
         starts_with("max_cows_"),
         starts_with("adj_"),
         starts_with("dose_total"),
         starts_with("L"),
         starts_with("C_"),
         starts_with("Y_"),
  )

# carry-forward previous COWS if missing, we are now imputing with 0

# dat <- dat |>
#   mutate(max_cows_2 = ifelse(is.na(max_cows_2) & C_2 == 1, max_cows_1, max_cows_2),
#          max_cows_3 = ifelse(is.na(max_cows_3) & C_3 == 1, max_cows_2, max_cows_3),
#          max_cows_4 = ifelse(is.na(max_cows_4) & C_4 == 1, max_cows_3, max_cows_4),
#          max_cows_5 = ifelse(is.na(max_cows_5) & C_5 == 1, max_cows_4, max_cows_5),
#          max_cows_6 = ifelse(is.na(max_cows_6) & C_6 == 1, max_cows_5, max_cows_6),
#          max_cows_7 = ifelse(is.na(max_cows_7) & C_7 == 1, max_cows_6, max_cows_7),
#          max_cows_8 = ifelse(is.na(max_cows_8) & C_8 == 1, max_cows_7, max_cows_8),
#          max_cows_9 = ifelse(is.na(max_cows_9) & C_9 == 1, max_cows_8, max_cows_9),
#          max_cows_10 = ifelse(is.na(max_cows_10) & C_10 == 1, max_cows_9, max_cows_10),
#          max_cows_11 = ifelse(is.na(max_cows_11) & C_11 == 1, max_cows_10, max_cows_11),
#          max_cows_12 = ifelse(is.na(max_cows_12) & C_12 == 1, max_cows_11, max_cows_12),
#          max_cows_13 = ifelse(is.na(max_cows_13) & C_13 == 1, max_cows_12, max_cows_13),
#          max_cows_14 = ifelse(is.na(max_cows_14) & C_14 == 1, max_cows_13, max_cows_14),
#          max_cows_ineligible_1 = ifelse(is.na(max_cows_ineligible_1), 0, max_cows_ineligible_1), 
#          max_cows_ineligible_2 = ifelse(is.na(max_cows_ineligible_2) & C_2 == 1, max_cows_ineligible_1, max_cows_ineligible_2),
#          max_cows_ineligible_3 = ifelse(is.na(max_cows_ineligible_3) & C_3 == 1, max_cows_ineligible_2, max_cows_ineligible_3),
#          max_cows_ineligible_4 = ifelse(is.na(max_cows_ineligible_4) & C_4 == 1, max_cows_ineligible_3, max_cows_ineligible_4),
#          max_cows_ineligible_5 = ifelse(is.na(max_cows_ineligible_5) & C_5 == 1, max_cows_ineligible_4, max_cows_ineligible_5),
#          max_cows_ineligible_6 = ifelse(is.na(max_cows_ineligible_6) & C_6 == 1, max_cows_ineligible_5, max_cows_ineligible_6),
#          max_cows_ineligible_7 = ifelse(is.na(max_cows_ineligible_7) & C_7 == 1, max_cows_ineligible_6, max_cows_ineligible_7),
#          max_cows_ineligible_8 = ifelse(is.na(max_cows_ineligible_8) & C_8 == 1, max_cows_ineligible_7, max_cows_ineligible_8)
#   )

# demographic vars
DEM <- read.csv(here::here("data/DEM.csv"), colClasses = c(PATID = "character"), na.strings = "") |>
  mutate(is_hispanic = case_when(DEHISPNC == 1 ~ 1,
                                 DEHISPNC == 0 ~ 0,
                                 DEHISPNC == 97 ~ as.numeric(NA), # don't know
                                 DEHISPNC == 98 ~ as.numeric(NA), # refused
                                 TRUE ~ as.numeric(NA)),
         DERACE_missing = case_when(DERACEDK == 1 | DERACERF == 1 ~ 1, # don't know or refuse
                                    TRUE ~ 0),
         DEWHITE = case_when(DEWHITE == 1 ~ 1,
                             DERACE_missing == 1 ~ as.numeric(NA),
                             TRUE ~ 0),
         DEBLACK = case_when(DEBLACK == 1 ~ 1,
                             DERACE_missing == 1 ~ as.numeric(NA),
                             TRUE ~ 0),
         DEOTHER = case_when(DEHAWAII == 1 | DEASIAN == 1 | DESAMOAN == 1 | DEAMEIND == 1| DERACEOT == 1 ~ 1, # other 
                             DERACE_missing == 1 ~ as.numeric(NA),
                             TRUE ~ 0)
  ) |>
  select(PATID, age, DESEX, is_hispanic, DEWHITE, DEBLACK, DEOTHER, DERACE_missing)

# mental health vars
MHX <- read.csv(here::here("data/MHX.csv"), colClasses = c(PATID = "character"), na.strings = "") |>
  select(PATID, MHANXH, MHBPLRH, MHMDDH) |>
  rename("anxiety" = "MHANXH",
         "bipolar" = "MHBPLRH",
         "depression" = "MHMDDH")

# substance use vars, 4 is none, we are considering mild, moderate, or severe
DSM <- read.csv(here::here("data/DSM.csv"), colClasses = c(PATID = "character"), na.strings = "") |>
  mutate(alcohol_use_disorder = ifelse(DSALCSCO < 4, 1, 0),
         amphetamine_use_disorder = ifelse(DSAMPSCO < 4, 1, 0),
         cannabis_use_disorder = ifelse(DSTHCSCO < 4, 1, 0),
         cocaine_use_disorder = ifelse(DSCOCSCO < 4, 1, 0),
         sedative_use_disorder = ifelse(DSSEDSCO < 4, 1, 0)
  ) |>
  select(PATID, ends_with("disorder"))

ASU <- read.csv(here::here("data/ASU.csv"), colClasses = c(PATID = "character"), na.strings = "") |>
  mutate(age_first_opioid_use = case_when(is.na(AUPNKAGE) == FALSE & is.na(AUHERAGE) == FALSE & AUPNKAGE < AUHERAGE ~ AUPNKAGE, # if painkiller age is lower than heroin age, take lower
                                          is.na(AUPNKAGE) == FALSE & is.na(AUHERAGE) == FALSE & AUPNKAGE >= AUHERAGE ~ AUHERAGE, # if painkiller age is greater than heroin age, take lower
                                          is.na(AUPNKAGE) == FALSE & is.na(AUHERAGE) == TRUE ~ AUPNKAGE,
                                          is.na(AUPNKAGE) == TRUE & is.na(AUHERAGE) == FALSE ~ AUHERAGE,
                                          TRUE ~ as.numeric(NA)),
         injection_opioid_use = case_when(AUPNKRTE == 4 | AUPNKRTE == 5 | AUHERRTE == 4 | AUHERRTE == 5 ~ 1, #route of painkiller/heroin use: IV or non-IV injection 
                                          (AUPNKLFT == 1 & is.na(AUPNKRTE)) | ((AUHERLFT == 1 | is.na(AUHERLFT)) & is.na(AUHERRTE)) ~ as.numeric(NA), # if lifetime painkiller/heroin use (or unknown status) but route unknown, then missing
                                          TRUE ~ 0)) |> # if no lifetime painkiller/heroin use then none
  select(PATID, age_first_opioid_use, injection_opioid_use)

D97 <- read.csv(here::here("data/D97.csv"), colClasses = c(PATID = "character"), na.strings = "") |>
  select(PATID, D97NPOPI)

SITE <- read.csv(here::here("data/EC0097C.csv"), colClasses = c(PATID = "character", SITE = "character"), na.strings = "") |>
  select(PATID, SITE) |>
  merge(read.csv(here::here("data/EC0097D.csv"), colClasses = c(PATID = "character", SITE = "character"), na.strings = "") |>
          select(PATID, SITE), all = TRUE)

process_column <- function(text) {
  text <- iconv(text, from = "latin1", to = "UTF-8", sub = "byte")
  text <- tolower(text)
  split_values <- unlist(strsplit(text, ",\\s*"))
  unique_values <- unique(split_values)
  return(unique_values)
}

# finding all types of pain killers
unique_values_vector <- process_column(ASU$AUPNKLSP)

# non-opioid pain killers - ativan, tylenol, valium -- people with these had opioids prescribed anyways

# joining data and imputing missing values
dat <- dat |>
  left_join(SITE) |>
  left_join(DEM) |>
  left_join(MHX) |>
  left_join(DSM) |>
  left_join(ASU) |>
  left_join(D97) |>
  mutate(DESEX = ifelse(DESEX == 2, 1, 0),
         years_since_first_opioid_use = case_when(is.na(age_first_opioid_use) == FALSE ~ age - age_first_opioid_use,
                                                  TRUE ~ as.numeric(NA))) |>
  select(-age_first_opioid_use,
         -starts_with("max_cows_time")) |>
  ungroup()

dat_demographics <- dat |>
  select(PATID, PROTSEG, received_naltrexone_injection, days_from_admission_to_consent, age:last_col())

dat_time_vary <- dat |>
  select(PATID, PROTSEG, received_naltrexone_injection, max_cows_1:Y_30)

# once experienced competing risk, all subsequent values should be missing (except cr/outcome which will always be 1/0 - this is manually changed later)
cols_to_modify <- c("C_1", "C_2", "C_3", "C_4", "C_5", "C_6", "C_7",
                    "C_8", "C_9", "C_10", "C_11", "C_12", "C_13", "C_14", "C_30")
for (col in cols_to_modify) {
  cols_after <- which(names(dat_time_vary) == col):ncol(dat_time_vary)
  cols_after <- cols_after[-1]
  dat_time_vary <- dat_time_vary |>
    mutate(across(all_of(cols_after), ~ if_else(dat_time_vary[[col]] == 0, NA, .))) |>
    mutate(across(starts_with("C_"), ~replace_na(.x, 0))) |>
    mutate(across(starts_with("Y_"), ~replace_na(.x, 0)))
}


# once experienced outcome, all subsequent values should be missing (except cr/outcome which will always be 0/1 - this is manually changed later)
cols_to_modify <- c("Y_1", "Y_2", "Y_3", "Y_4", "Y_5", "Y_6", "Y_7",
                    "Y_8", "Y_9", "Y_10", "Y_11", "Y_12", "Y_13", "Y_14", "Y_30")
for (col in cols_to_modify) {
  cols_after <- which(names(dat) == col):ncol(dat_time_vary)
  cols_after <- cols_after[-1]
  dat_time_vary <- dat_time_vary |>
    mutate(across(all_of(cols_after), ~ if_else(dat_time_vary[[col]] == 1, NA, .))) |>
    mutate(across(starts_with("C_"), ~replace_na(.x, 1))) |>
    mutate(across(starts_with("Y_"), ~replace_na(.x, 1)))
}

dat <- dat_demographics |>
  left_join(dat_time_vary)

dat <- dat |>
  mutate(C_1 = ifelse(is.na(C_1), 0, C_1),
         C_2 = ifelse(is.na(C_2), 0, C_2),
         C_3 = ifelse(is.na(C_3), 0, C_3),
         C_4 = ifelse(is.na(C_4), 0, C_4),
         C_5 = ifelse(is.na(C_5), 0, C_5),
         C_6 = ifelse(is.na(C_6), 0, C_6),
         C_7 = ifelse(is.na(C_7), 0, C_7),
         C_8 = ifelse(is.na(C_8), 0, C_8),
         C_9 = ifelse(is.na(C_9), 0, C_9),
         C_10 = ifelse(is.na(C_10), 0, C_10),
         C_11 = ifelse(is.na(C_11), 0, C_11),
         C_12 = ifelse(is.na(C_12), 0, C_12),
         C_13 = ifelse(is.na(C_13), 0, C_13),
         C_14 = ifelse(is.na(C_14), 0, C_14),
         C_30 = ifelse(is.na(C_30), 0, C_30)) |>
  mutate(max_cows_eligible_1 = case_when(max_cows_ineligible_1 == 1 ~ 0,
                                         max_cows_ineligible_1 == 0 ~ 1,
                                         TRUE ~ as.numeric(NA)),
         adj_2 = ifelse(C_1 == 0, NA, adj_2),
         max_cows_2 = ifelse(C_1 == 0, NA, max_cows_2),
         max_cows_eligible_2 = case_when(max_cows_ineligible_2 == 1 ~ 0,
                                         max_cows_ineligible_2 == 0 ~ 1,
                                         TRUE ~ as.numeric(NA)),
         L1_2 = ifelse(C_1 == 0, NA, L1_2),
         adj_3 = ifelse(C_2 == 0 | Y_2 == 1, NA, adj_3),
         max_cows_3 = ifelse(C_2 == 0 | Y_2 == 1, NA, max_cows_3),
         max_cows_eligible_3 = case_when(max_cows_ineligible_3 == 1 ~ 0,
                                         max_cows_ineligible_3 == 0 ~ 1,
                                         TRUE ~ as.numeric(NA)),
         L1_3 = ifelse(C_2 == 0 | Y_2 == 1, NA, L1_3),
         adj_4 = ifelse(C_3 == 0 | Y_2 == 1, NA, adj_4),
         max_cows_4 = ifelse(C_3 == 0 | Y_3 == 1, NA, max_cows_4),
         max_cows_eligible_4 = case_when(max_cows_ineligible_4 == 1 ~ 0,
                                         max_cows_ineligible_4 == 0 ~ 1,
                                         TRUE ~ as.numeric(NA)),
         L1_4 = ifelse(C_3 == 0 | Y_3 == 1, NA, L1_4),
         adj_5 = ifelse(C_4 == 0 | Y_4 == 1, NA, adj_5),
         max_cows_5 = ifelse(C_4 == 0 | Y_4 == 1, NA, max_cows_5),
         max_cows_eligible_5 = case_when(max_cows_ineligible_5 == 1 ~ 0,
                                         max_cows_ineligible_5 == 0 ~ 1,
                                         TRUE ~ as.numeric(NA)),
         L1_5 = ifelse(C_4 == 0 | Y_4 == 1, NA, L1_5)
  )

# imputing values with mode/median

Mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

saveRDS(dat, here::here("data/analysis_data/pre_imputed_analysis_data_alt_shift.rds"))

dat <- dat |>  
  mutate(DEBLACK = ifelse(is.na(DEBLACK), Mode(DEBLACK), DEBLACK),
         DEWHITE = ifelse(is.na(DEWHITE), Mode(DEWHITE), DEWHITE),
         DEOTHER = ifelse(is.na(DEOTHER), Mode(DEOTHER), DEOTHER),
         is_hispanic_missing = ifelse(is.na(is_hispanic), 1, 0),
         is_hispanic = ifelse(is_hispanic_missing == 1, Mode(is_hispanic), is_hispanic),
         alcohol_use_disorder_missing = ifelse(is.na(alcohol_use_disorder), 1, 0),
         alcohol_use_disorder = ifelse(alcohol_use_disorder_missing == 1, Mode(alcohol_use_disorder), alcohol_use_disorder),
         amphetamine_use_disorder_missing = ifelse(is.na(amphetamine_use_disorder), 1, 0),
         amphetamine_use_disorder = ifelse(amphetamine_use_disorder_missing == 1, Mode(amphetamine_use_disorder), amphetamine_use_disorder),
         cannabis_use_disorder_missing = ifelse(is.na(cannabis_use_disorder), 1, 0),
         cannabis_use_disorder = ifelse(cannabis_use_disorder_missing == 1, Mode(cannabis_use_disorder), cannabis_use_disorder),
         cocaine_use_disorder_missing = ifelse(is.na(cocaine_use_disorder), 1, 0),
         cocaine_use_disorder = ifelse(cocaine_use_disorder_missing == 1, Mode(cocaine_use_disorder), cocaine_use_disorder),
         sedative_use_disorder_missing = ifelse(is.na(sedative_use_disorder), 1, 0),
         sedative_use_disorder = ifelse(sedative_use_disorder_missing == 1, Mode(sedative_use_disorder), sedative_use_disorder),
         anxiety_missing = ifelse(is.na(anxiety), 1, 0),
         anxiety = ifelse(anxiety_missing == 1, Mode(anxiety), anxiety),
         bipolar_missing = ifelse(is.na(bipolar), 1, 0),
         bipolar = ifelse(bipolar_missing == 1, Mode(bipolar), bipolar),
         depression_missing = ifelse(is.na(depression), 1, 0),
         depression = ifelse(depression_missing == 1, Mode(depression), depression),
         years_since_first_opioid_use_missing = ifelse(is.na(years_since_first_opioid_use), 1, 0),
         years_since_first_opioid_use = ifelse(years_since_first_opioid_use_missing == 1, median(years_since_first_opioid_use, na.rm = TRUE), years_since_first_opioid_use),
         injection_opioid_use_missing = ifelse(is.na(injection_opioid_use), 1, 0),
         injection_opioid_use = ifelse(injection_opioid_use_missing == 1, Mode(injection_opioid_use), injection_opioid_use),
         D97NPOPI_missing = ifelse(is.na(D97NPOPI), 1, 0),
         D97NPOPI = ifelse(D97NPOPI_missing == 1, Mode(D97NPOPI), D97NPOPI)
) 

saveRDS(dat, here::here("data/analysis_data/analysis_data_alt_shift.rds"))