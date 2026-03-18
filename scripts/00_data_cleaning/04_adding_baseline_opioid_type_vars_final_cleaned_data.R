library(tidyverse)

dat_long <- readRDS(here::here("data/analysis_data/max_cows_data_alt.rds")) |>
  select(PATID, consent_DMAMDDT) |>
  distinct() |>
  mutate(flag = 1)

T97 <- read_csv("data/T97.csv") |>
  select(PATID,
         starts_with("TLDATE"),
         starts_with("TLHER"), #heroin
         starts_with("TLOPIR"), #opioid analgesics
         starts_with("TLMTDR"), #methadone
         starts_with("TLBUPR") #buprenorphine
         ) |>
  mutate(across(starts_with("TL"), as.numeric)) |>
  left_join(dat_long) |>
  filter(flag == 1) |>
  select(-flag)

# add underscore between the day number and the name
T97 <- T97 |>
  rename_with(~ sub("(TL[A-Za-z]+)(\\d+)$", "\\1_\\2", .),
              matches("^TL"))

# pivot to long format so there is one row per day per patient
T97 <- T97 |>
  pivot_longer(
    cols = matches("^TL"),
    names_to = c(".value", "day"),
    names_pattern = "^(TL[^_]+)_(\\d+)$"
  ) |>
  select(-day) |>
  filter(is.na(TLDATE) == FALSE) |>
  # look 30 days before consent
  mutate(thirty_days_before_consent = consent_DMAMDDT - 30) |>
  filter(consent_DMAMDDT > TLDATE, # must be before consent day
         TLDATE >= thirty_days_before_consent # mut be on or after 30 day prior date
         )

# per Kara's instructions: if they have all missing then treat as missing; if they have partial data then assume not used for missing days

T97 <- T97 |>
  mutate(across(everything(), ~ replace_na(.x, 0)))


T97_for_grouping <- T97 |>
  select(-c(consent_DMAMDDT, TLDATE, thirty_days_before_consent))

# if any use of any opioid, mark as 1
T97_grouped <- T97_for_grouping |>
  group_by(PATID) |>
  summarise(across(c(TLHERR, TLOPIR, TLMTDR, TLBUPR),
                   ~ as.integer(any(.x > 0, na.rm = TRUE)),
                   .names = "{.col}_indicator"))

# adding in opioid type (pre-impute)
preimputed_data <- readRDS(here::here("data/analysis_data/pre_imputed_analysis_data_alt_shift.rds")) |>
  left_join(T97_grouped)

saveRDS(preimputed_data, here::here("data/analysis_data/pre_imputed_analysis_data_alt_shift_R1.rds"))

# adding in opioid type (impute with mode)
cleaned_data <- readRDS(here::here("data/analysis_data/analysis_data_alt_shift.rds")) |>
  left_join(T97_grouped) |>
  mutate(TLHERR_indicator_missing = ifelse(is.na(TLHERR_indicator), 1, 0),
         TLOPIR_indicator_missing = ifelse(is.na(TLOPIR_indicator), 1, 0),
         TLMTDR_indicator_missing = ifelse(is.na(TLMTDR_indicator), 1, 0),
         TLBUPR_indicator_missing = ifelse(is.na(TLBUPR_indicator), 1, 0),
         TLHERR_indicator = ifelse(is.na(TLHERR_indicator), 1, TLHERR_indicator),
         TLOPIR_indicator = ifelse(is.na(TLOPIR_indicator), 0, TLOPIR_indicator),
         TLMTDR_indicator = ifelse(is.na(TLMTDR_indicator), 0, TLMTDR_indicator),
         TLBUPR_indicator = ifelse(is.na(TLBUPR_indicator), 0, TLBUPR_indicator)
         )

saveRDS(cleaned_data, here::here("data/analysis_data/analysis_data_alt_shift_R1.rds"))
