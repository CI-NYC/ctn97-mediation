# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
arm <- readRDS("data/analysis_data/analysis_data_alt_shift.rds")$PROTSEG

mediation_1_1_14 <- readRDS("results_mediation/mediation_1_1_14.rds")
mediation_1_0_14 <- readRDS("results_mediation/mediation_1_0_14.rds")
mediation_0_0_14 <- readRDS("results_mediation/mediation_0_0_14.rds")

eif <- data.frame(PROTSEG = arm,
                  eif_11 = mediation_1_1_14[[1]]@eif,
                  eif_10 = mediation_1_0_14[[1]]@eif,
                  eif_00 = mediation_0_0_14[[1]]@eif) |>
  mutate(PROTSEG = ifelse(PROTSEG == 1, "Rapid", "Standard"))

summary(eif)

eif_long <- eif |>
  pivot_longer(cols = -c(PROTSEG),
               names_to = "variable", values_to = "value") |>
  mutate(variable = factor(variable, levels = c("eif_11", "eif_10", "eif_00")))  

stats <- eif_long |>
  group_by(PROTSEG, variable) |>
  summarise(
    min = min(value),
    max = max(value),
    median = median(value),
    mean = mean(value),
    sd = sd(value),
    .groups = "drop"
  )

stats <- stats |>
  mutate(
    label = paste0("Min = ", round(min,2),
                   "\nMax = ", round(max,2),
                   "\nMedian = ", round(median,2),
                   "\nMean = ", round(mean,2),
                   "\nSD= ", round(sd,2))
    
  ) |>
  mutate(x = case_when(variable == "eif_11" & PROTSEG == "Rapid" ~ 25,
                       variable == "eif_11" & PROTSEG == "Standard" ~ 25,
                       variable == "eif_10" & PROTSEG == "Rapid" ~ -20,
                       variable == "eif_10" & PROTSEG == "Standard" ~ -2,
                       variable == "eif_00" & PROTSEG == "Rapid" ~ 0,
                       variable == "eif_00" & PROTSEG == "Standard" ~ -75
  ),
  y = case_when(variable == "eif_11" & PROTSEG == "Rapid" ~ 70,
                variable == "eif_11" & PROTSEG == "Standard" ~ 100,
                variable == "eif_10" & PROTSEG == "Rapid" ~ 100,
                variable == "eif_10" & PROTSEG == "Standard" ~ 40,
                variable == "eif_00" & PROTSEG == "Rapid" ~ 40,
                variable == "eif_00" & PROTSEG == "Standard" ~ 60))

ggplot(eif_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  #geom_density(fill = "steelblue", color = "white") +
  facet_wrap(PROTSEG ~ variable, scales = "free") +
  geom_text(
    data = stats,
    size = 2.5,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = 1,
    show.legend = FALSE) +
  theme_minimal() + 
  labs(title = "EIF Distributions by Arm",
       y = "") +
  theme(panel.border = element_rect(color = "black", fill = NA))

## Plotting density ratios

dr_11 <- mediation_1_1_14[[2]] |> select(PROTSEG, starts_with("lcmmtp_G_M")) |>
  mutate(PROTSEG = ifelse(PROTSEG == 1, "Rapid", "Standard"))
dr_10 <- mediation_1_0_14[[2]] |> select(PROTSEG, starts_with("lcmmtp_G_M")) |>
  mutate(PROTSEG = ifelse(PROTSEG == 1, "Rapid", "Standard"))
dr_00 <- mediation_0_0_14[[2]] |> select(PROTSEG, starts_with("lcmmtp_G_M")) |>
  mutate(PROTSEG = ifelse(PROTSEG == 1, "Rapid", "Standard"))
## THESE ARE ALL THE SAME

for (i in c("dr_11"))
{
  
  df <- get(i) 
  
  df_long <- df |>
    pivot_longer(
      cols = -PROTSEG,
      names_to = "variable",
      values_to = "value"
    )
  
  stats <- df_long |>
    group_by(PROTSEG, variable) |>
    summarise(
      n = n(),
      mean = mean(value),
      median = median(value),
      sd = sd(value),
      min = min(value),
      max = max(value),
      .groups = "drop"
    ) |>
    mutate(
      label = paste0(
        "Min = ", round(min, 2),
        "\nMax = ", round(max, 2),
        "\nMedian = ", round(median, 2),
        "\nMean = ", round(mean, 2),
        "\nSD = ", round(sd, 2)
      ),
      x = max,   
      y = 125000    
    )
  
  p <- ggplot(df_long, aes(x = value)) +
    geom_histogram(
      bins = 30,
      color = "black",
      alpha = 0.6,
      position = "identity"
    ) +
    facet_wrap(PROTSEG ~ variable, scales = "free", nrow = 2) +
    geom_text(
      data = stats,
      size = 2.5,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 1,
      vjust = 1,
      show.legend = FALSE
    ) +
    theme_minimal() +
    labs(
      #title = paste0("Truncated (97.5th percentile) Mediator Density Ratios by Arm When A' = ", ifelse(i %in% c("dr_11", "dr_10"), "1", "0"), " and A* = ", ifelse(i %in% c("dr_10", "dr_00"), "0", "1")),
      title = "Truncated (97.5th percentile) Mediator Density Ratios by Arm",
      x = "Value",
      y = "Count"
    ) +
    theme(plot.title = element_text(size = 12),
          panel.border = element_rect(color = "black", fill = NA)
    )
  
  print(p)
}
