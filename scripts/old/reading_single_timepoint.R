## Y, Day 14
res_00_1SE_single <- readRDS("res_00_1SE_single.rds")
res_10_1SE_single <- readRDS("res_10_1SE_single.rds")
res_11_1SE_single <- readRDS("res_11_1SE_single.rds")

ITE <- (res_11_1SE_single[[1]]) - (res_00_1SE_single[[1]])
IIE <- (res_11_1SE_single[[1]]) - (res_10_1SE_single[[1]])

ITE
IIE

## Y, Days 1 and 14
res_00_1SE_days_1_2 <- readRDS("res_00_1SE_days_1_14.rds")
res_10_1SE_days_1_2 <- readRDS("res_10_1SE_days_1_14.rds")
res_11_1SE_days_1_2 <- readRDS("res_11_1SE_days_1_14.rds")

ITE <- (1 - res_11_1SE_days_1_2[[1]]) - (1 - res_00_1SE_days_1_2[[1]])
IIE <- (1 - res_11_1SE_days_1_2[[1]]) - (1 - res_10_1SE_days_1_2[[1]])

ITE
IIE