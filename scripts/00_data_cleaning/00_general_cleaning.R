library(tidyverse)
library(lubridate)
library(hms)

# enrollment data
enrollment <- read.csv("data/EC0097C.csv", colClasses = c(PATID = "character")) |>
  mutate(PROTSEG = "C") |> # standard procedure
  select(PATID, E97ADMDT, PROTSEG) |>
  merge(read.csv("data/EC0097D.csv", colClasses = c(PATID = "character")) |>
          mutate(PROTSEG = "D") |> # rapid procedure
          select(PATID, E97ADMDT, PROTSEG), all = TRUE) |>
  rename("admission_date" = "E97ADMDT")

# daily medication data

DMA <- read.csv("data/DMA.csv", colClasses = c(PATID = "character"), na.strings = "") |>
  # only keep the standard and rapid arms of the pre-initiation study (not the post-initiation)
  filter(PROTSEG == "D" | PROTSEG == "C") |>
  select(-PROTSEG) |>
  full_join(enrollment, by = c("PATID" = "PATID"))

# fixing issues of patients with 2 rows per day (most are due to extra vitals -- does not affect medication counts)

DMA <- DMA |>
  filter(!(PATID == "02221009700150" & VISNO == "I01A"), # extra vitals for that day
         !(PATID == "02221009700210" & VISNO == "I03A"), # extra vitals for that day
         !(PATID == "02221009700210" & VISNO == "I04A"), # extra vitals for that day
         !(PATID == "02221009700210" & VISNO == "I05A"), # extra vitals for that day
         !(PATID == "02221009700253" & VISNO == "I02A"), # extra vitals for that day
         !(PATID == "02221009700253" & VISNO == "I03A"), # extra vitals for that day
         !(PATID == "02221009700253" & VISNO == "I04A") # extra vitals for that day
  )

## BE CAREFUL OF PATID 02217009700004 -- has 2 entries for DMAMDDT -2 -- manually combining into 1

DMA <- DMA |> 
  mutate(DMBUPD07 = ifelse(PATID == "02217009700004" & VISNO == "IN03", 1, as.numeric(NA)),
         DMBUPT07 = ifelse(PATID == "02217009700004" & VISNO == "IN03", "19:50:00", NA)) |>
  mutate(DMBUPT07 = hms::as_hms(DMBUPT07)) |> # adding a 7th bup column for this indivdual 
  rowwise() |>
  mutate(DMBUPDTL = sum(DMBUPD01, DMBUPD02, DMBUPD03, DMBUPD04, DMBUPD05, DMBUPD06, DMBUPD07, na.rm = TRUE)) |> # making sum of all bup over the day to include 7th column (only changes the one patient)
  relocate(DMBUPT07, .after = DMBUPT06) |>
  filter(!(PATID == "02217009700004" & VISNO == "I03A"))|>
  relocate(admission_date, .after = SITE) |>
  mutate(DMAMDDT = ifelse(is.na(DMAMDDT), admission_date, DMAMDDT)) |> # 3 instances of missing date -- make this the admission date
  complete(PATID, DMAMDDT) |> # checking for missing days between admission and end of initiation date
  group_by(PATID) |>
  fill(c(admission_date), .direction = "downup") |> # admission date doesn't change -- can impute
  filter(DMAMDDT >= admission_date) |> 
  arrange(PATID, DMAMDDT) 

# consent data
consent <- read.csv("data/EC0097B_0730.csv", colClasses = c(PATID = "character")) |>
  filter(is.na(STARTDT) == FALSE) |>
  select(PATID, STARTDT) |>
  rename("consent_DMAMDDT" = "STARTDT") # day of signed consent

# COWS data
COW <- read.csv("data/COW.csv", colClasses = c(PATID = "character")) |>
  filter(PROT == "0097") |>
  mutate(across(c(COPULSE, COSWEAT, CORESTLS, COPUPIL, COBONJNT, CONOSEYE, COGIUPST, COTREMOR,
                  COYAWN, COANXITY, COGOOSKN), ~ coalesce(., 0))) |> # missing values = 0
  rowwise() |>
  mutate(cows_score = case_when(is.na(COCOWSCR) & is.na(COWSCRRT) == FALSE ~ COWSCRRT, # if missing COWS but retrospective available, use that
                                # patient notes indicating that these patients did not receive/finish COWS assessments at the visits
                                PATID == "02201009700118" & VISNO == "IN02" ~ as.numeric(NA),
                                PATID == "02076009700045" & VISNO == "IN08" ~ as.numeric(NA),
                                PATID == "02076009700335" & VISNO == "B00" ~ as.numeric(NA),
                                TRUE ~ sum(c(COPULSE, COSWEAT, CORESTLS, COPUPIL, COBONJNT, CONOSEYE, COGIUPST, COTREMOR, # some categories missing information but still taking sum (these were imputed with 0)
                                             COYAWN, COANXITY, COGOOSKN)) # issues with COCOWSCR variable -- sometimes doesn't match sum -- we will take sum for now (only 7 instances)
  )) |>
  filter(!((PATID == "02201009700118" & COWASMDT == 0 & COASMTM == "02:57") | COASMTM == "")) |>
  left_join(enrollment, by = c("PATID" = "PATID")) |>
  rename("cows_time" = "COASMTM") |>
  filter(COWASMDT >= admission_date) |>
  select(PATID, COWASMDT, cows_time, cows_score) |>
  arrange(PATID, COWASMDT, cows_time) |>
  group_by(PATID, COWASMDT) |> 
  mutate(number = row_number(),
         number = as.character(number)) |>
  pivot_wider(names_from = c(number), 
              values_from = c(cows_time, cows_score)) |>
  rename("DMAMDDT" = "COWASMDT") 

full_data <- DMA |> # joining COWS data
  left_join(COW, by = c("PATID" = "PATID",
                        "DMAMDDT" = "DMAMDDT"))

full_data <- full_data |>
  left_join(consent, by = c("PATID" = "PATID"))  |>
  relocate(admission_date, .after = PATID) |>
  mutate(DMAMDDT = ifelse(is.na(DMAMDDT), admission_date, DMAMDDT)) |> # for 2 cases of missing day (only day), then use as day 1
  filter(admission_date <= DMAMDDT) |>
  # calculating day number
  mutate(day = case_when(admission_date == DMAMDDT ~ 1,
                         admission_date < DMAMDDT ~ DMAMDDT - admission_date + 1,
                         TRUE ~ NA
  )) |>
  relocate(day, .after = admission_date) |>
  arrange(PATID, day)

#max_dates_df <- full_data |>
#  group_by(PATID) |>
#  summarize(max_day = max(day))

#min_dates_df <- full_data |>
#  group_by(PATID) |>
#  summarize(min_day = min(DMAMDDT))

#full_data <- full_data |>
#  left_join(max_dates_df, by = c("PATID" = "PATID")) |>
#  left_join(min_dates_df, by = c("PATID" = "PATID"))

# end of induction data
EOI <- read.csv("data/EOI.csv", colClasses = c(PATID = "character")) |>
  rowwise() |> 
  mutate(EITERMDT = case_when(EINTXIND == 1 ~ EITERMDT, # if initiated XR-NTX, keep as is (NA)
                              TRUE ~ min(EITERMDT, EOIASMDT, EILEFTDT, na.rm = TRUE))) |> # if did not initiated, use minimum of induction termination date, assessment date (1 patient who switched to buprenorphine before end of induction), or left unit date (2 people left before end of induction)
  ungroup() |>
  select(PATID, EINTXIND, EOIINJDT, EOIINJTM, EITERMDT)

full_data <- full_data |>
  left_join(EOI, by = c("PATID" = "PATID"))

induction_end <- full_data |>
  filter(EOIINJDT == DMAMDDT) |>
  mutate(EOIINJDT = day) |>
  select(PATID, EOIINJDT, EOIINJTM) |>
  rename("naltrexone_injection_day" = "EOIINJDT") |>
  rename("naltrexone_injection_time" = "EOIINJTM") |>
  mutate(naltrexone_injection_time = as_hms(paste0(naltrexone_injection_time, ":00"))) |>
  full_join(full_data |>
              filter(EITERMDT == DMAMDDT) |>
              mutate(EITERMDT = day) |>
              select(PATID, EITERMDT) |>
              rename("end_induction_day" = "EITERMDT")) |>
  mutate(end_induction_day = ifelse(is.na(end_induction_day), naltrexone_injection_day, end_induction_day)) |>
  mutate(received_naltrexone_injection = ifelse(is.na(naltrexone_injection_day) == FALSE, 1, 0))


full_data <- full_data |>
  left_join(induction_end, by = c("PATID" = "PATID")) |>
  group_by(PATID) |>
  mutate(max_day = max(day, na.rm = TRUE)) |>
  ungroup() |>
  #complete(PATID, day) |>
  group_by(PATID) |>
  fill(c(PROTSEG, consent_DMAMDDT, SITE, end_induction_day, received_naltrexone_injection, naltrexone_injection_day, naltrexone_injection_time), .direction = "downup") |> # these variables never change
  ungroup() |>
  select(-max_day) |>
  filter(day >= 1) |>
  select(PATID, PROTSEG, admission_date, day, DMAMDDT, SITE, consent_DMAMDDT, received_naltrexone_injection, naltrexone_injection_day, naltrexone_injection_time, end_induction_day, # key information
         DMBUPD01, DMBUPD02, DMBUPD03, DMBUPD04, DMBUPD05, DMBUPD06, DMBUPD07, DMBUPDTL, DMBUPT01, DMBUPT02, DMBUPT03, DMBUPT04, DMBUPT05, DMBUPT06, DMBUPT07, # BUP
         DMCLDD01, DMCLDD02, DMCLDD03, DMCLDD04, DMCLDD05, DMCLDD06, DMCLDDTL, DMCLDT01, DMCLDT02, DMCLDT03, DMCLDT04, DMCLDT05, DMCLDT06, #CL
         DMCZPD01, DMCZPD02, DMCZPD03, DMCZPD04, DMCZPD05, DMCZPD06, DMCZPDTL, DMCZPT01, DMCZPT02, DMCZPT03, DMCZPT04, DMCZPT05, DMCZPT06, # CZ
         DMBZODTL, # other benzos
         DMEFF24H,
         DMDROWSY, DMDIZZY, # side effects
         cows_score_1, cows_score_2, cows_score_3, cows_score_4, cows_score_5, cows_score_6, cows_score_7, cows_score_8, cows_score_9, cows_score_10, cows_score_11, cows_score_12, # COWS score
         cows_time_1, cows_time_2, cows_time_3, cows_time_4, cows_time_5, cows_time_6, cows_time_7, cows_time_8, cows_time_9, cows_time_10, cows_time_11, cows_time_12 # COWS time
  )

time_columns <- c("DMBUPT01", "DMBUPT02", "DMBUPT03", "DMBUPT04", "DMBUPT05", "DMBUPT06", #DMBUPT07, #BUP
                  "DMCLDT01", "DMCLDT02", "DMCLDT03", "DMCLDT04", "DMCLDT05", "DMCLDT06", #CL
                  "DMCZPT01", "DMCZPT02", "DMCZPT03", "DMCZPT04", "DMCZPT05", "DMCZPT06", # CZ
                  "cows_time_1", "cows_time_2", "cows_time_3", "cows_time_4", "cows_time_5", "cows_time_6", 
                  "cows_time_7", "cows_time_8", "cows_time_9", "cows_time_10", "cows_time_11", "cows_time_12" # COWS time
)

for (col in time_columns) {
  full_data[[col]] <- as.POSIXct(full_data[[col]], format = "%H:%M")
}

change_time <- function(df, time_columns) {
  for (col in time_columns) {
    df[[col]] <- as_hms(df[[col]])
  }
  return(df)
}

# Apply the function to extract time components
full_data <- change_time(full_data, time_columns)


# urinary drug screening

UDS <- read.csv("data/UDS.csv", colClasses = c(PATID = "character"), na.strings = "") |>
  filter(PROTSEG == "C" | PROTSEG == "D") |>
  filter(PATID != "02207009701249") # look at note -- patient who failed induction but returned one time for medication

full_data <- full_data |>
  full_join(UDS |> select(PATID, UDSASMDT, UDTEST1), by = c("PATID" = "PATID",
                                                            "DMAMDDT" = "UDSASMDT")) |>
  mutate(UDTEST1 = ifelse(is.na(UDTEST1), 0, UDTEST1)) |>
  distinct()

full_data_final <- full_data |>
  filter(day <= end_induction_day)

saveRDS(full_data_final, here::here("data/analysis_data/ctn97_analysis_data_final.rds"))


#  full_data |>
#  filter(PROTSEG == "C") |> # filter to rapid induction
#  complete(PATID, day) |>
#  group_by(day) |> # group by VISNO day
#  summarize(buprenorphine = sum(ifelse(DMBUPDTL > 0, 1, 0), na.rm = TRUE),  # if pat received any buprenorphine then yes
#            buprenorphine2 = sum(ifelse(DMBUPD01 > 0 |
#                                          DMBUPD02 > 0 |
#                                          DMBUPD03 > 0 |
#                                          DMBUPD04 > 0 |
#                                          DMBUPD05 > 0 |
#                                          DMBUPD06 > 0, 1, 0), na.rm = TRUE),
#            clonidine = sum(ifelse(DMCLDDTL > 0, 1, 0), na.rm = TRUE), # if pat received any clonidine then yes
#            clonazepam = sum(ifelse(DMCZPDTL > 0, 1, 0), na.rm = TRUE)) # if pat received any clonazepam then yes

## Checking if DMCZPDTL is accurate
#full_data |>
#  rowwise() |>
#  mutate(clonazepam_sum = sum(DMCZPD01, DMCZPD02, DMCZPD03, DMCZPD04, DMCZPD05, DMCZPD06, na.rm = TRUE),
#         clonazepam_diff = ifelse(DMCZPDTL == clonazepam_sum, 0, 1))  |>
#  select(PATID, day, DMCZPDTL,clonazepam_sum,  clonazepam_diff) |>
#  filter(clonazepam_diff == 1)

