library(tidyverse)

mediation_1_1_14 <- 1 - readRDS("results/mediation_1_1_14.rds")
mediation_0_0_14 <- 1 - readRDS("results/mediation_0_0_14.rds")
mediation_1_0_14 <- 1 - readRDS("results/mediation_1_0_14.rds")

ATE <- mediation_1_1_14 - mediation_0_0_14

IIE <- mediation_1_1_14 - mediation_1_0_14

IIE/ATE