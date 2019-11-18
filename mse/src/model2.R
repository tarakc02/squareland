#
# Authors:     TS
# Maintainers: TS
# Copyright:   2019, HRDAG, GPL v2 or later
# =========================================
# squareland/mse/src/model2.R

library(pacman)
pacman::p_load("argparse", "feather", "dplyr", "brms")

parser <- ArgumentParser()
parser$add_argument("--input", default = "output/cell-counts.feather")
parser$add_argument("--seed", type = "integer", default = 370712)
parser$add_argument("--output", default = "output/model2.rds")
args <- parser$parse_args()

train <- read_feather(args$input)

mod <- brm(n ~ (source1 + source2 + source3 + source4) | cell,
           family = poisson(), data = train,
           chains = 4, iter = 10000, seed = args$seed,
           cores = 4, control = list(max_treedepth = 15,
                                     adapt_delta = .9))

saveRDS(mod, args$output)

# done.

