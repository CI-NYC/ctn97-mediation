library(tidyverse)
library(ggpointdensity)

dat <- readRDS(here::here("data/analysis_data/analysis_data_alt_shift.rds"))

# benzos in 0.5 mg increments (0-6mg)
# clonazepam in 0.5 mg increments (0-5mg)
# clonidine in mainly 0.1 mg increments but there is an exception (0-1.2 mg)

dat |>
  select(dose_total_clonidine_1,
         dose_total_clonidine_2,
         dose_total_clonidine_3,
         dose_total_clonidine_4,
         dose_total_clonidine_5,
         dose_total_clonazepam_1,
         dose_total_clonazepam_2,
         dose_total_clonazepam_3,
         dose_total_clonazepam_4,
         dose_total_clonazepam_5,
         dose_total_benzo_1,
         dose_total_benzo_2,
         dose_total_benzo_3,
         dose_total_benzo_4,
         dose_total_benzo_5
         ) |>
  summary()

dat <- dat |>
  mutate(dose_total_clonazepam_and_benzo_1 = dose_total_clonazepam_1 + dose_total_benzo_1,
         dose_total_clonazepam_and_benzo_2 = dose_total_clonazepam_2 + dose_total_benzo_2,
         dose_total_clonazepam_and_benzo_3 = dose_total_clonazepam_3 + dose_total_benzo_3,
         dose_total_clonazepam_and_benzo_4 = dose_total_clonazepam_4 + dose_total_benzo_4,
         dose_total_clonazepam_and_benzo_5 = dose_total_clonazepam_5 + dose_total_benzo_5
         )

# look at splitting at 2 mg for benzos and 0.4 mg for clonidine?
hist(dat$dose_total_clonazepam_and_benzo_1)
hist(dat$dose_total_clonidine_1)

ggplot(dat, aes(x = dose_total_clonidine_1, y = dose_total_clonazepam_and_benzo_1)) +
  geom_pointdensity() +
  scale_color_viridis_c() +
  theme_minimal() +
  facet_wrap(~PROTSEG) +
  labs(title = "Scatterplot of adjunctive doses by randomization arm",
       color = "Density")

dat |>
  group_by(PROTSEG,
           dose_total_clonidine_4 >= 0.2,
           dose_total_clonazepam_and_benzo_4 >= 2
  ) |>
  summarize(count = n())

