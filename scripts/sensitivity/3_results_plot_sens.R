library(tidyverse)
library(ggplot2)
library(knitr)

# reading primary results
primary_results <- readRDS(here::here("results_medoutcon_101025/res_learners_mean_glm_lasso1SE_interactions_crossfit_seed_1_1_benzo_and_clon_M_sens.rds")) |>
  mutate(effect_type = case_when(param == "total_natural" ~ "Total",
                                 param == "indirect_natural" ~ "Indirect",
                                 param == "direct_natural" ~ "Direct"),
         mediation_type = "Natural") |>
  select(lwr_ci, param_est, upr_ci, mediation_type, effect_type)

# reading secondary results
mediation_1_1<- readRDS(here::here("results_mediation_final_secondary/mediation_1_1withglmnetranger_sens.rds"))
mediation_1_0<- readRDS(here::here("results_mediation_final_secondary/mediation_1_0withglmnetranger_sens.rds"))
mediation_0_0 <- readRDS(here::here("results_mediation_final_secondary/mediation_0_0withglmnetranger_sens.rds"))

secondary_results <- data.frame(lwr_ci = numeric(),
                                param_est = numeric(),
                                upr_ci = numeric(),
                                mediation_type = character(),
                                effect_type = character())

secondary_results[1, 2] <- ((1 - mediation_1_1) -  (1 - mediation_0_0))@x
secondary_results[2, 2] <- ((1 - mediation_1_1) - (1 - mediation_1_0))@x
secondary_results[3, 2] <- ((1 - mediation_1_0) - (1 - mediation_0_0))@x

secondary_results[1, 1] <- (((1 - mediation_1_1) -  (1 - mediation_0_0))@conf_int)[1]
secondary_results[2, 1] <- (((1 - mediation_1_1) - (1 - mediation_1_0))@conf_int)[1]
secondary_results[3, 1] <- (((1 - mediation_1_0) - (1 - mediation_0_0))@conf_int)[1]

secondary_results[1, 3] <- (((1 - mediation_1_1) -  (1 - mediation_0_0))@conf_int)[2]
secondary_results[2, 3] <- (((1 - mediation_1_1) - (1 - mediation_1_0))@conf_int)[2]
secondary_results[3, 3] <- (((1 - mediation_1_0) - (1 - mediation_0_0))@conf_int)[2]

secondary_results[1, 4] <- "Interventional"
secondary_results[2, 4] <- "Interventional"
secondary_results[3, 4] <- "Interventional"

secondary_results[1, 5] <- "Total"
secondary_results[2, 5] <- "Indirect"
secondary_results[3, 5] <- "Direct"

results <- primary_results |>
  merge(secondary_results, all = TRUE) |>
  mutate(mediation_type = factor(mediation_type, levels = c("Natural", "Interventional")),
         effect_type = factor(effect_type, levels = c("Total", "Indirect", "Direct"))) |>
  mutate(param_est = 100 * param_est,
         lwr_ci = 100 * lwr_ci,
         upr_ci = 100 * upr_ci
  ) |>
  arrange(effect_type, mediation_type)

res_plot <- ggplot(results, aes(x = mediation_type, y = param_est)) +
  geom_point(size = 2, color = "black") +
  geom_errorbar(aes(ymin = lwr_ci, ymax = upr_ci), width = 0.2, color = "black") +
  facet_wrap(~ effect_type, ncol = 3) + 
  theme_minimal(base_size = 14) +
  labs(
    x = "Effect Type",
    y = "Effect Estimate (in Percentage Points)"
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")

res_plot

ggsave(filename = "figures/res_plot_sens.pdf",
       width = 9,
       height = 6,
       units = "in",
       plot = res_plot)

results_for_tbl <- results |>
  mutate(estimate = paste0(round(param_est, 1), " (", round(lwr_ci, 1), ", ", round(upr_ci, 1), ")")) |>
  select(effect_type, mediation_type, estimate)

kable(results_for_tbl, format = "latex", booktabs = TRUE, digits = 2,
      caption = "Point estimates and 95\\% confidence intervals")