library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggpubr)

df_no_benzo <- readRDS(here::here("data/analysis_data/analysis_data_alt_shift_no_benzo.rds")) |>
  select(PATID,
         adj_1,
         adj_2,
         adj_3,
         adj_4,
         adj_5) |>
  rename("adj_no_benzo_1" = "adj_1",
         "adj_no_benzo_2" = "adj_2",
         "adj_no_benzo_3" = "adj_3",
         "adj_no_benzo_4" = "adj_4",
         "adj_no_benzo_5" = "adj_5"
  )

df <- readRDS(here::here("data/analysis_data/analysis_data_alt_shift.rds")) |>
  as.data.frame() |>
  mutate(dose_total_clonazepam_and_benzo_1 = dose_total_clonazepam_1 + dose_total_benzo_1,
         dose_total_clonazepam_and_benzo_2 = dose_total_clonazepam_2 + dose_total_benzo_2,
         dose_total_clonazepam_and_benzo_3 = dose_total_clonazepam_3 + dose_total_benzo_3,
         dose_total_clonazepam_and_benzo_4 = dose_total_clonazepam_4 + dose_total_benzo_4,
         dose_total_clonazepam_and_benzo_5 = dose_total_clonazepam_5 + dose_total_benzo_5
  ) |>
  mutate(
    Group_1 = case_when(dose_total_clonidine_1 >= 0.1 & dose_total_clonazepam_and_benzo_1 >= 3 & max_cows_eligible_1 == 1 ~ 2,
                        dose_total_clonidine_1 > 0 | dose_total_clonazepam_and_benzo_1 >= 2 & max_cows_eligible_1 == 1 ~ 1,
                        TRUE ~ 0),
    Group_2 = case_when(is.na(adj_2) ~ as.numeric(NA),
                        dose_total_clonidine_2 >= 0.1 & dose_total_clonazepam_and_benzo_2 >= 3 & max_cows_eligible_2 == 1 ~ 2,
                        dose_total_clonidine_2 > 0 | dose_total_clonazepam_and_benzo_2 >= 2 & max_cows_eligible_2 == 1 ~ 1,
                        TRUE ~ 0),
    Group_3 = case_when(is.na(adj_3) ~ as.numeric(NA),
                        dose_total_clonidine_3 >= 0.1 & dose_total_clonazepam_and_benzo_3 >= 3 & max_cows_eligible_3 == 1 ~ 2,
                        dose_total_clonidine_3 > 0 | dose_total_clonazepam_and_benzo_3 >= 2 & max_cows_eligible_3 == 1 ~ 1,
                        TRUE ~ 0),
    Group_4 = case_when(is.na(adj_4) ~ as.numeric(NA),
                        dose_total_clonidine_4 >= 0.1 & dose_total_clonazepam_and_benzo_4 >= 3 & max_cows_eligible_4 == 1 ~ 2,
                        dose_total_clonidine_4 > 0 | dose_total_clonazepam_and_benzo_4 >= 2 & max_cows_eligible_4 == 1 ~ 1,
                        TRUE ~ 0),
    Group_5 = case_when(is.na(adj_5) ~ as.numeric(NA),
                        dose_total_clonidine_5 >= 0.1 & dose_total_clonazepam_and_benzo_5 >= 3 & max_cows_eligible_5 == 1 ~ 2,
                        dose_total_clonidine_5 > 0 | dose_total_clonazepam_and_benzo_5 >= 2 & max_cows_eligible_5 == 1 ~ 1,
                        TRUE ~ 0)  
  ) |>
  mutate(Protocol = ifelse(PROTSEG == 1, "Rapid", "Standard"),
         Outcome = case_when(Y_14 == 1 ~ "Experienced outcome",
                             TRUE ~ "Did not experience outcome/censored")) |>
  select(PATID,
         Protocol,
         Outcome,
         Group_1,
         Group_2,
         Group_3,
         Group_4,
         Group_5,
         C_1,
         C_2,
         C_3,
         C_4,
         C_5,
         Y_1,
         Y_2,
         Y_3,
         Y_4,
         Y_5)


df_joined <- df |>
  inner_join(df_no_benzo) |>
  mutate(adjunctive_group_1 = case_when(Group_1 == 2 ~ "High",
                                        Group_1 == 1 ~ "Moderate",
                                        Group_1 == 0 ~ "None"
                                        ),
         adjunctive_group_2 = case_when(Group_2 == 2 ~ "High",
                                        Group_2 == 1 ~ "Moderate",
                                        Group_2 == 0 ~ "None",
                                        is.na(Group_2) & C_1 == 0 ~ "Censored",
                                        is.na(Group_2) & Y_1 == 1 ~ "Experienced Outcome"
         ),
         adjunctive_group_3 = case_when(Group_3 == 2 ~ "High",
                                        Group_3 == 1 ~ "Moderate",
                                        Group_3 == 0 ~ "None",
                                        is.na(Group_3) & C_2 == 0 ~ "Censored",
                                        is.na(Group_3) & Y_2 == 1 ~ "Experienced Outcome"
         ),
         adjunctive_group_4 = case_when(Group_4 == 2 ~ "High",
                                        Group_4 == 1 ~ "Moderate",
                                        Group_4 == 0 ~ "None",
                                        is.na(Group_4) & C_3 == 0 ~ "Censored",
                                        is.na(Group_4) & Y_3 == 1 ~ "Experienced Outcome"
         ),
         adjunctive_group_5 = case_when(Group_5 == 2 ~ "High",
                                        Group_5 == 1 ~ "Moderate",
                                        Group_5 == 0 ~ "None",
                                        is.na(Group_5) & C_4 == 0 ~ "Censored",
                                        is.na(Group_5) & Y_4 == 1 ~ "Experienced Outcome"
         )
  )

df_long <- df_joined |>
  pivot_longer(cols = starts_with("adjunctive_group_"), 
               names_to = "day", 
               names_prefix = "adjunctive_group_", 
               values_to = "Adjunctive") |>
  mutate(day = as.integer(day)) |>
  filter(day <= 5) |>
  mutate(day = factor(day, levels = 1:5)) |>
  mutate(ID = factor(as.numeric(as.factor(PATID))),
         Adjunctive = factor(Adjunctive, levels = c("High", "Moderate", "None", "Experienced Outcome", "Censored")))

df_long_outcome <- df_long |>
  filter(Outcome == "Experienced outcome")

df_long_no_outcome <- df_long |>
  filter(Outcome == "Did not experience outcome/censored")

plot_outcome <- ggplot(df_long_outcome, aes(x = day, y = ID, fill = Adjunctive)) +
  geom_tile(color = "white") +
  facet_wrap(~Protocol, scales = "free_y", ncol = 2) +
  labs(x = "Day", y = "Subject ID", fill = "Adjunctive", title = "Initiated by Day 14") +
  scale_fill_manual(values = c("High" = "red", "Moderate" = "blue", "None" = "grey",
                               "Censored" = "white", "Experienced Outcome" = "gold"), na.value = "white") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 10),       
    axis.title = element_text(size = 8),     
    axis.text = element_text(size = 6),          
    legend.title = element_text(size = 8),       
    legend.text = element_text(size = 7)       
  )

plot_no_outcome <- ggplot(df_long_no_outcome, aes(x = day, y = ID, fill = Adjunctive)) +
  geom_tile(color = "white") +
  facet_wrap(~Protocol, scales = "free_y", ncol = 2) +
  labs(x = "Day", y = "Subject ID", fill = "Adjunctive", title = "Did Not Initiate by Day 14") +
  scale_fill_manual(values = c("High" = "red", "Moderate" = "blue", "None" = "grey",
                               "Censored" = "white", "Experienced Outcome" = "gold"), na.value = "white") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 10),       
    axis.title = element_text(size = 8),     
    axis.text = element_text(size = 6),          
    legend.title = element_text(size = 8),       
    legend.text = element_text(size = 7)       
  )

divider <- cowplot::ggdraw() + 
  cowplot::draw_line(x = c(0, 1), y = c(0.5, 0.5), size = 1, color = "black")

sequence_plot <- ggarrange(plot_outcome, divider, plot_no_outcome, ncol = 1)

hline <- ggplot() +
  geom_segment(aes(x = -2, xend = 2, y = 0.5, yend = 0.5), inherit.aes = FALSE) +
  theme_void() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

sequence_plot <- (plot_outcome / hline / plot_no_outcome) +
  plot_layout(heights = c(1, 0.015, 1)) 