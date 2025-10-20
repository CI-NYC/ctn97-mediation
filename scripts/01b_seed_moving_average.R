library(tidyverse)
library(tidyr)
library(ggplot2)

set.seed(123)

learners_txt <- c("mean_glm_lasso1SE_interactions_ridge_crossfit")

learners_vec <- c(learners_txt)
seeds_vec <- c(1:10)
folds_vec <- c(1)

all_results <- expand.grid(
  learners_txt = learners_vec,
  s = seeds_vec,
  I = folds_vec
) |>
  pmap_dfr(function(learners_txt, s, I) {
    file_path <- paste0("results_medoutcon_101025/res_learners_", learners_txt, "_seed_", s, "_", I, "_benzo_and_clon_M.rds")
    
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
  mutate(
    fold_num = as.numeric(fold),
    se_squared = var_est
  )

cumulative_rubin <- plot_data |>
  group_by(learners_txt, effect_type) |>
  arrange(seed) |>
  group_modify(~{
    df <- .x
    map_dfr(1:nrow(df), function(i){
      subset_df <- df[1:i, ]
      M <- nrow(subset_df)
      theta_bar <- mean(subset_df$effect)
      U_bar <- mean(subset_df$se_squared)
      B <- if(M > 1) var(subset_df$effect) else 0
      T <- U_bar + (1 + 1/M) * B
      tibble(
        seed_num = as.numeric(subset_df$seed[i]),
        cum_theta = theta_bar,
        cum_se = sqrt(T)
      )
    })
  }) |>
  ungroup()

ggplot(plot_data, aes(x = as.numeric(seed), y = effect)) +
  geom_point(size = 2, color = 'grey') +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = 'grey') +
  geom_line(data = cumulative_rubin,
            aes(x = seed_num, y = cum_theta, group = 1),
            color = "blue", size = 1, inherit.aes = FALSE) +
  geom_ribbon(data = cumulative_rubin,
              aes(x = seed_num,
                  ymin = cum_theta - 1.96 * cum_se,
                  ymax = cum_theta + 1.96 * cum_se,
                  group = 1),
              fill = "blue", alpha = 0.2, inherit.aes = FALSE) +
  facet_grid(learners_txt ~ effect_type, scales = "free_y") +
  labs(x = "Seed", y = "Effect") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") + 
  theme(legend.position = "none")

