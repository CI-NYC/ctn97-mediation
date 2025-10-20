library(tidyverse)
library(ggplot2)

set.seed(123)

learners_txt <- c(paste0("glm_lrnr", "_", "lasso_lrnr", "_", "earth_lrnr", "_", "mean_lrnr"), paste0("glm_lrnr", "_", "lasso_lrnr", "_", "earth_lrnr", "_", "mean_lrnr", "_", "ridge_lrnr", "_", "enet_lrnr"))

learners_vec <- c(learners_txt)
seeds_vec <- c(1:20)
folds_vec <- c(20)

all_results <- expand.grid(
  learners_txt = learners_vec,
  s = seeds_vec,
  I = folds_vec
) |>
  pmap_dfr(function(learners_txt, s, I) {
    file_path <- paste0("results_medoutcon_new/res_learners_", learners_txt, "_seed_", s, "_", I, "_fold.rds")
    
    if(file.exists(file_path)){
      res <- readRDS(file_path)   
      res$learners_txt <- learners_txt
      res$s <- s
      res$I <- I
      return(res)
    } else return(NULL)
  })

plot_data <- all_results |>
  mutate(
    effect_type = recode(param,
                         "total_natural" = "Total Effect",
                         "indirect_natural" = "Indirect Effect"),
    learners_txt = factor(learners_txt, levels = learners_vec),
    fold = factor(I),
    seed = factor(s)
  ) |>
  rename(
    effect = param_est,
    ci_lower = lwr_ci,
    ci_upper = upr_ci
  ) |>
  mutate(fold_num = as.numeric(fold),
         se_squared = var_est/415) 

fold_avg <- plot_data |>
  group_by(learners_txt, fold, effect_type) |>
  summarise(
    M = n(),
    theta_bar = mean(effect), 
    U_bar = mean(se_squared),
    B = var(effect),           
    .groups = "drop"
  ) |>
  mutate(
    T = U_bar + (1 + 1/M) * B,
    ci_lower = theta_bar - 1.96 * sqrt(T),
    ci_upper = theta_bar + 1.96 * sqrt(T),
    fold_num = as.numeric(fold)
  )

plot_data <- plot_data |> mutate(fold_num = as.numeric(fold))

pd <- position_dodge(width = 0.7)

ggplot(plot_data, aes(x = fold_num, y = effect, color = seed, group = seed)) +
  geom_point(position = pd, size = 2) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = pd) +
  geom_point(data = fold_avg, aes(x = fold_num, y = theta_bar), color = "black", size = 3, inherit.aes = FALSE) +
  geom_errorbar(data = fold_avg, aes(x = fold_num, ymin = ci_lower, ymax = ci_upper), 
                color = "black", width = 0.2, inherit.aes = FALSE) +
    geom_text(
    data = fold_avg,
    aes(x = fold_num, 
        y = ci_upper + 0.2,   
        label = paste0("θ=", round(theta_bar, 2), 
                       " [", round(ci_lower, 2), ", ", round(ci_upper, 2), "]")),
    inherit.aes = FALSE,
    size = 4,   
    color = "black"
  ) +
  
  scale_x_continuous(breaks = unique(plot_data$fold_num),
                     labels = levels(plot_data$fold)) +
  facet_grid(learners_txt ~ effect_type, scales = "free_y") +
  labs(x = "Number of Folds", y = "Effect", color = "Seed") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold", size = 7),
    legend.position = "none"
  ) +
  ylim(-0.5, 1.0) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")
