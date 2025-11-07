library(tidyverse)
library(gtsummary)
library(ggplot2)
library(patchwork)

dat <- readRDS(here::here("data/analysis_data/pre_imputed_analysis_data_alt_shift.rds")) |>
  # mutate(max_cows_1 = ifelse(max_cows_missing_indicator_1 == 1, as.numeric(NA), max_cows_1),
  #        max_cows_2 = ifelse(max_cows_missing_indicator_2 == 1, as.numeric(NA), max_cows_2),
  #        max_cows_3 = ifelse(max_cows_missing_indicator_3 == 1, as.numeric(NA), max_cows_3),
  #        max_cows_4 = ifelse(max_cows_missing_indicator_4 == 1, as.numeric(NA), max_cows_4),
  #        max_cows_5 = ifelse(max_cows_missing_indicator_5 == 1, as.numeric(NA), max_cows_5)
  #        ) |>
  mutate(across(starts_with("C_"), ~ ifelse(. == 0, 1, ifelse(. == 1, 0, .)))) |>
  mutate(max_cows_eligible_1 = ifelse(max_cows_missing_indicator_1 == 1, as.numeric(NA), max_cows_eligible_1),
         max_cows_eligible_2 = ifelse(max_cows_missing_indicator_2 == 1, as.numeric(NA), max_cows_eligible_2),
         max_cows_eligible_3 = ifelse(max_cows_missing_indicator_3 == 1, as.numeric(NA), max_cows_eligible_3),
         max_cows_eligible_4 = ifelse(max_cows_missing_indicator_4 == 1, as.numeric(NA), max_cows_eligible_4),
         max_cows_eligible_5 = ifelse(max_cows_missing_indicator_5 == 1, as.numeric(NA), max_cows_eligible_5)
  )

W <- c("days_from_admission_to_consent",
       # demographics
       "DESEX",
       "age",
       "is_hispanic",
       "DEWHITE",
       "DEBLACK",
       "DEOTHER",
       "alcohol_use_disorder", #missing
       "amphetamine_use_disorder", #missing
       "cannabis_use_disorder", #missing
       "cocaine_use_disorder", #missing
       "sedative_use_disorder", #missing
       "injection_opioid_use",
       "years_since_first_opioid_use",
       "anxiety", #missing
       "bipolar", #missing,
       "depression", #missing
       "D97NPOPI"
)

L <- list(c("max_cows_1", 
            "max_cows_eligible_1", 
            "max_cows_missing_indicator_1"
), 
c("max_cows_2", 
  "max_cows_eligible_2", 
  "max_cows_missing_indicator_2"
), 
c("max_cows_3", 
  "max_cows_eligible_3", 
  "max_cows_missing_indicator_3"
), 
c("max_cows_4", 
  "max_cows_eligible_4", 
  "max_cows_missing_indicator_4"
), 
c("max_cows_5", 
  "max_cows_eligible_5", 
  "max_cows_missing_indicator_5"
)
)

table1 <- dat |>
  select(PROTSEG, W, unlist(L), "C_14", "Y_14", "C_30", "Y_30") |>
  tbl_summary(
    statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = list(days_from_admission_to_consent ~ "continuous"),
    missing = "ifany",
    by = PROTSEG
  )

table1

latex_table <- table1 |>
  as_kable(format = "latex")

cat(latex_table)

## MEDIATOR TABLE

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
  mutate(M1_sum = sum(dose_total_clonazepam_and_benzo_1 + dose_total_clonazepam_and_benzo_2 + dose_total_clonazepam_and_benzo_3, na.rm = TRUE),
         M2_sum = sum(dose_total_clonidine_1 + dose_total_clonidine_2 + dose_total_clonidine_3, na.rm = TRUE),
         M1_mean = mean(c(dose_total_clonazepam_and_benzo_1, dose_total_clonazepam_and_benzo_2, dose_total_clonazepam_and_benzo_3), na.rm = TRUE),
         M2_mean = mean(c(dose_total_clonidine_1, dose_total_clonidine_2, dose_total_clonidine_3), na.rm = TRUE),
         M1_median = median(c(dose_total_clonazepam_and_benzo_1, dose_total_clonazepam_and_benzo_2, dose_total_clonazepam_and_benzo_3), na.rm = TRUE),
         M2_median = median(c(dose_total_clonidine_1, dose_total_clonidine_2, dose_total_clonidine_3), na.rm = TRUE),
         M1_max = max(c(dose_total_clonazepam_and_benzo_1, dose_total_clonazepam_and_benzo_2, dose_total_clonazepam_and_benzo_3), na.rm = TRUE),
         M2_max = max(c(dose_total_clonidine_1, dose_total_clonidine_2, dose_total_clonidine_3), na.rm = TRUE),
         ) |>
  mutate(
    Group_1 = case_when((dose_total_clonidine_1 > 0.1 | dose_total_clonazepam_and_benzo_1 >= 2) & max_cows_eligible_1 == 1 ~ 2,
                        (dose_total_clonidine_1 > 0 | dose_total_clonazepam_and_benzo_1 > 0) & max_cows_eligible_1 == 1 ~ 1,
                        TRUE ~ 0),
    Group_2 = case_when(is.na(adj_2) ~ as.numeric(NA),
                        (dose_total_clonidine_2 > 0.1 | dose_total_clonazepam_and_benzo_2 >= 2) & max_cows_eligible_2 == 1 ~ 2,
                        (dose_total_clonidine_2 > 0  | dose_total_clonazepam_and_benzo_2 > 0) & max_cows_eligible_2 == 1 ~ 1,
                        TRUE ~ 0),
    Group_3 = case_when(is.na(adj_3) ~ as.numeric(NA),
                        (dose_total_clonidine_3 > 0.1 | dose_total_clonazepam_and_benzo_3 >= 2) & max_cows_eligible_3 == 1 ~ 2,
                        (dose_total_clonidine_3 > 0 | dose_total_clonazepam_and_benzo_3 > 0) & max_cows_eligible_3 == 1 ~ 1,
                        TRUE ~ 0),
    Group_4 = case_when(is.na(adj_4) ~ as.numeric(NA),
                        (dose_total_clonidine_4 > 0.1 | dose_total_clonazepam_and_benzo_4 >= 2) & max_cows_eligible_4 == 1 ~ 2,
                        (dose_total_clonidine_4 > 0 | dose_total_clonazepam_and_benzo_4 > 0) & max_cows_eligible_4 == 1 ~ 1,
                        TRUE ~ 0),
    Group_5 = case_when(is.na(adj_5) ~ as.numeric(NA),
                        (dose_total_clonidine_5 > 0.1 | dose_total_clonazepam_and_benzo_5 >= 2) & max_cows_eligible_5 == 1 ~ 2,
                        (dose_total_clonidine_5 > 0 | dose_total_clonazepam_and_benzo_5 > 0) & max_cows_eligible_5 == 1 ~ 1,
                        TRUE ~ 0)  
  ) |>
  mutate(M1 = case_when(M1_mean < 1 ~ 0,
                        M1_mean < 2 ~ 1,
                        M1_mean < 3 ~ 2,
                        TRUE ~ 3),
         M2 = case_when(M2_mean <= 0.1 ~ 0,
                        TRUE ~ 1))

table2 <- dat |>
  select(PROTSEG, M1, M2, M1_mean, M2_mean, starts_with("Group_"),
         dose_total_clonidine_1, dose_total_clonidine_2, dose_total_clonidine_3, dose_total_clonidine_4, dose_total_clonidine_5,
         dose_total_clonazepam_and_benzo_1, dose_total_clonazepam_and_benzo_2, dose_total_clonazepam_and_benzo_3, dose_total_clonazepam_and_benzo_4, dose_total_clonazepam_and_benzo_5) |>
  tbl_summary(
    statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = list(M1_mean ~ "continuous",
                M2_mean ~ "continuous",
                dose_total_clonidine_1 ~ "continuous", 
                dose_total_clonidine_2 ~ "continuous", 
                dose_total_clonidine_3 ~ "continuous", 
                dose_total_clonidine_4 ~ "continuous", 
                dose_total_clonidine_5 ~ "continuous",
                dose_total_clonazepam_and_benzo_1 ~ "continuous", 
                dose_total_clonazepam_and_benzo_2 ~ "continuous", 
                dose_total_clonazepam_and_benzo_3 ~ "continuous", 
                dose_total_clonazepam_and_benzo_4 ~ "continuous", 
                dose_total_clonazepam_and_benzo_5 ~ "continuous"),
    missing = "ifany",
    by = PROTSEG
  )

table2

latex_table_mediator <- table2 |>
  as_kable(format = "latex")

cat(latex_table_mediator)

joint_mediators_sum <- ggplot(dat, aes(x = M1_sum, y = M2_sum)) +
  geom_jitter(width = 0.5, height = 0, alpha = 0.7) +
  labs(
    x = "Sum of Benzodiazepines Days 1-3 (mg)",
    y = "Sum of Clonidine Days 1-3 (mg)"
  ) +
  theme_minimal()

joint_mediators_mean <- ggplot(dat, aes(x = M1_mean, y = M2_mean, color = factor(PROTSEG))) +
  geom_point(alpha = 0.7) +
  labs(
    x = "Mean Dose of Benzodiazepines Days 1-3 (mg)",
    y = "Mean Dose of Clonidine Days 1-3 (mg)",
    color = "Randomization Arm"
  ) +
  scale_color_discrete(labels = c(
    "0" = "Standard",
    "1" = "Rapid"
  )) + 
  theme_minimal() +
  theme(legend.position = c(0.2, 0.75),
        legend.box.background = element_rect(
          color = "black", fill = "gray95", linewidth = 1
        ))

joint_mediators_median <- ggplot(dat, aes(x = M1_median, y = M2_median)) +
  geom_jitter(width = 0.5, height = 0, alpha = 0.7) +
  labs(
    x = "Median of Benzodiazepines Days 1-3 (mg)",
    y = "Median Clonidine Days 1-3 (mg)"
  ) +
  theme_minimal()

joint_mediators_max <- ggplot(dat, aes(x = M1_max, y = M2_max)) +
  geom_jitter(width = 0.5, height = 0, alpha = 0.7) +
  labs(
    x = "Max of Clonidine Days 1-3 (mg)",
    y = "Max of Benzos Days 1-3 (mg)"
  ) +
  theme_minimal()

four_plot <- joint_mediators_sum + joint_mediators_mean + joint_mediators_median + joint_mediators_max

ggsave(filename = "figures/four_mediator.pdf",
       width = 9,
       height = 6,
       units = "in",
       plot = four_plot)

ggsave(filename = "figures/joint_mediators_mean.pdf",
       width = 9,
       height = 6,
       units = "in",
       plot = joint_mediators_mean)