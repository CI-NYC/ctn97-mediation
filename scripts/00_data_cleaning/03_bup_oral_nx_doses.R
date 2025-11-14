#devtools::install_github("nt-williams/lmtp@competing-risks")
library(tidyverse)

# loading data and making into wide format

DMA <- read_csv("data/DMA.csv") |>
  select(PATID,
         DMAMDDT,
         DMVIVDTL
  ) |>
  filter(!is.na(PATID)) 

dat_long <- readRDS(here::here("data/analysis_data/max_cows_data_alt.rds")) |>
  left_join(DMA, by = c("PATID" = "PATID",
                        "DMAMDDT" = "DMAMDDT")) |>
  filter(day_post_consent <= 5) |>
  select(PATID, PROTSEG, day_post_consent, DMBUPDTL, DMVIVDTL) |>
  mutate(DMVIVDTL = ifelse(is.na(DMVIVDTL), 0, DMVIVDTL),
         DMBUPDTL = ifelse(is.na(DMBUPDTL), 0, DMBUPDTL)) |>
  mutate(PROTSEG = ifelse(PROTSEG == "C", "Standard", "Rapid"),
         PROTSEG = factor(PROTSEG, levels = c("Standard", "Rapid")))

p_x <- ggplot(dat_long, aes(x = "", y = DMBUPDTL)) +
  geom_boxplot() +
  facet_grid(PROTSEG ~ day_post_consent, scales = "free_y") +
  labs(
    title = "Boxplots of Buprenorphine by Arm and Day",
    y = "Buprenorphine Dose",
    x = NULL
  ) +
  theme_bw()

p_y <- ggplot(dat_long, aes(x = "", y = DMVIVDTL)) +
  geom_boxplot() +
  facet_grid(PROTSEG ~ day_post_consent, scales = "free_y") +
  labs(
    title = "Boxplots of Oral Naltrexone by Arm and Day",
    y = "Oral Naltrexone Dose",
    x = NULL
  ) +
  theme_bw()

p_x
p_y

summary_x <- dat_long |>
  group_by(PROTSEG, day_post_consent) |>
  summarise(
    mean = mean(DMBUPDTL),
    median = median(DMBUPDTL),
    sd = sd(DMBUPDTL),
    min = min(DMBUPDTL),
    max = max(DMBUPDTL)
  )

summary_y <- dat_long|>
  group_by(PROTSEG, day_post_consent) |>
  summarise(
    mean = mean(DMVIVDTL),
    median = median(DMVIVDTL),
    sd = sd(DMVIVDTL),
    min = min(DMVIVDTL),
    max = max(DMVIVDTL)
  )

summary_x
summary_y