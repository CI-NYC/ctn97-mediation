library(tidyverse)
library(lcmmtp)
library(ggpubr)
library(ggplot2)

tidy.lmtp_survival <- function(x, projection = TRUE, ...) {
  
  if (projection == FALSE)
  {
    for (i in seq_along(x)) {
      x[[i]]@x <- 1 - x[[i]]@x
    }
  }
  
  out <- do.call("rbind", lapply(x, ife::tidy))
  out$t <- 1:length(x)
  out[, c(ncol(out), 1:ncol(out) - 1)]
}

tidy.lmtp_survival_non_projection <- function(x, ...) {
  
  
  
  out <- do.call("rbind", lapply(x, ife::tidy))
  out$t <- 1:length(x)
  out[, c(ncol(out), 1:ncol(out) - 1)]
}

# function to apply isotonic projection
isotonic_projection <- function(x) {

  # estim <- data.frame(estimate = c(x[[1]]@x, x[[2]]@x, x[[3]]@x, x[[4]]@x, x[[5]]@x,
  #                                  x[[6]]@x, x[[7]]@x, x[[8]]@x, x[[9]]@x, x[[10]]@x))
  
  estim <- tidy.lmtp_survival(x)
  
  iso_fit <- isotone::gpava(1:length(x), 1 - estim$estimate)
  
  for (i in seq_along(x)) {
    x[[i]]@x <- iso_fit$x[i]
  }
  x
}

ife_to_df <- function(x)
{
  # estim <- data.frame(estimate = c(x[[1]]@x, x[[2]]@x, x[[3]]@x, x[[4]]@x, x[[5]]@x,
  #                                  x[[6]]@x, x[[7]]@x, x[[8]]@x, x[[9]]@x, x[[10]]@x),
  #                     std.error = c((sd(x[[1]]@eif))/sqrt(length(x[[1]]@eif)), 
  #                                   (sd(x[[2]]@eif))/sqrt(length(x[[2]]@eif)), 
  #                                   (sd(x[[3]]@eif))/sqrt(length(x[[3]]@eif)), 
  #                                   (sd(x[[4]]@eif))/sqrt(length(x[[4]]@eif)),
  #                                   (sd(x[[5]]@eif))/sqrt(length(x[[5]]@eif)),
  #                                   (sd(x[[6]]@eif))/sqrt(length(x[[6]]@eif)),
  #                                   (sd(x[[7]]@eif))/sqrt(length(x[[7]]@eif)),
  #                                   (sd(x[[8]]@eif))/sqrt(length(x[[8]]@eif)),
  #                                   (sd(x[[9]]@eif))/sqrt(length(x[[9]]@eif)),
  #                                   (sd(x[[10]]@eif))/sqrt(length(x[[10]]@eif))))
  estim <- tidy.lmtp_survival(x)
  
  estim
}

read_results <- function(day, x, y){
  data <- readRDS(here::here(paste0("results_final/mediation_", x, "_", y, "_", day, ".rds")))[[1]]
}

# getting results for each component
results_1_1 <- list()
results_1_0 <- list()
results_0_0 <- list()


for (x in c(1, 0))
{
  for (y in c(1, 0))
  {
    if (x == 0 && y == 1) {
      next
    } else if (x == 1 && y == 1)
    {
    for (j in 5:14)
    {
      results_1_1[[j - 4]] <- read_results(j, x, y) 
    }
      results_1_1_df <- tidy.lmtp_survival(results_1_1, projection = FALSE)  |>
        mutate(type = "1_1")
      
      results_1_1 <- isotonic_projection(results_1_1) 
      } else if (x == 1 && y == 0)
        {
        for (j in 5:14)
          {
          results_1_0[[j - 4]] <- read_results(j, x, y)
        }
        results_1_0_df <- tidy.lmtp_survival(results_1_0, projection = FALSE) |>
          mutate(type = "1_0")
        
        results_1_0 <- isotonic_projection(results_1_0) 
        
        } else if (x == 0 && y == 0)
        {
          for (j in 5:14)
          {
            results_0_0[[j - 4]] <- read_results(j, x, y)
          }
          results_0_0_df <- tidy.lmtp_survival(results_0_0, projection = FALSE) |>
            mutate(type = "0_0")
          
          results_0_0 <- isotonic_projection(results_0_0) 
          
        }
  }
}

merged_pre_projection_df <- results_1_1_df |> 
  merge(results_1_0_df, all = TRUE) |>
  merge(results_0_0_df, all = TRUE) |>
  mutate(type = factor(type, levels = c("1_1", "1_0", "0_0")),
         t = factor(t))

ggplot(merged_pre_projection_df, aes(x = t, y = estimate, group = type)) +
  geom_point(aes(shape = type, color = type, fill = type),
             position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = type),
                position = position_dodge(width = 0.5), width = 0.2) +
  theme_minimal() +
  labs(title = "Estimate Components",
       x = "Day",
       y = "Estimate",
       shape = "Component (prime_star)",
       color = "Component (prime_star)",
       fill = "Component (prime_star)")

ate_results <- list()
iie_results <- list()
ide_results <- list()

for (t in 1:10)
{
  ate_results[[t]] <- results_1_1[[t]] - results_0_0[[t]]
  
  iie_results[[t]] <- results_1_1[[t]] - results_1_0[[t]]
  
  ide_results[[t]] <- results_1_0[[t]] - results_0_0[[t]]
}

share_results <- list()

for (t in 1:10)
{
  share_results[[t]] <- iie_results[[t]] / ate_results[[t]]
}

ate_results_df <- ife_to_df(ate_results) |>
  mutate(type = "ITE") |>
  mutate(day = row_number() + 4)

iie_results_df <- ife_to_df(iie_results) |>
  mutate(type = "IIE") |>
  mutate(day = row_number() + 4)

ide_results_df <- ife_to_df(ide_results) |>
  mutate(type = "IDE") |>
  mutate(day = row_number() + 4)

merged_df <- ate_results_df |>
  merge(iie_results_df, all = TRUE) |>
  merge(ide_results_df, all = TRUE)

share_df <- ife_to_df(share_results) |>
  mutate(day = row_number() + 4)

merged_df <- merged_df |>
  mutate(is_overlap = type %in% c("ITE", "IIE")) |>
  group_by(day, is_overlap) |>
  arrange(day, desc(estimate)) |>
  ungroup()

merged_df <- merged_df |>
  mutate(
    day_bar = case_when(
      type %in% c("ITE", "IIE") ~ day,         
      type == "IDE" ~ day + 0.3               
    ),
    day_error = case_when(
      type %in% c("ITE", "IIE") ~ day,  
      type == "IDE" ~ day + 0.3               
    )
  ) |>
  mutate(type = factor(type, levels = c("ITE", "IIE", "IDE")))

shares <- merged_df |>
  group_by(day) |>
  summarise(
    y_pos = 0.7         
  ) |>
  mutate(share = share_df$estimate,
         share = sprintf("%.2f", share)
  )

ggplot(merged_df) +
  geom_col(aes(x = day_bar, y = estimate, fill = type),
           width = 0.4,
           position = position_identity(),
           alpha = 0.5) +
  geom_errorbar(aes(x = day_bar, ymin = conf.low, ymax = conf.high, color = type),
                width = 0.15,
                position = position_identity(),
                size = 0.8) +
  geom_text(data = shares, aes(x = day, y = y_pos, label = share),
            size = 3, fontface = "bold") +
  scale_fill_manual(values = c("ITE" = "blue2", "IIE" = "brown3", "IDE" = "chartreuse3")) +
  scale_color_manual(values = c("ITE" = "blue2", "IIE" = "brown3", "IDE" = "chartreuse3")) +
  scale_x_continuous(breaks = 5:14) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(x = "Day", y = "Estimate", title = "Total, Indirect, and Direct Effect Estimates",
       fill = "Effect Type", color = "Effect Type") +
  annotate("text", x = -Inf, y = 0.75, label = "IIE/ITE Share:", size = 3, hjust = 0, fontface = "bold")

