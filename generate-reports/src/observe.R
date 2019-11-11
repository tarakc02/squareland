#
# Authors:     TS
# Maintainers: TS
# Copyright:   2019, HRDAG, GPL v2 or later
# =========================================
# /Users/tshah/git/squareland/generate-reports/src/observe.R

library(pacman)
pacman::p_load("argparse", "feather", "dplyr", "tidyr", "purrr", "viridis")

parser <- ArgumentParser()
parser$add_argument("--input", default = "../generate-ground-truth/output/truth.feather")
parser$add_argument("--nsource", type = "integer", default = 4)
parser$add_argument("--seed", type = "integer", default = 202070170)
args <- parser$parse_args()

observe_incidents <- function(truth) {
    truth %>%
        group_by(class) %>%
        nest %>%
        ungroup %>%
        mutate(frac = rbeta(n = nrow(.), shape1 = .5, shape2 = 2)) %>%
        mutate(samps = map2(data, frac, sample_frac)) %>%
        select(samps) %>%
        unnest(samps)
}

gen_reports <- function(truth) {
    tibble(source = paste0("source", 1:args$nsource)) %>%
        mutate(report = replicate(nrow(.),
                                  observe_incidents(truth),
                                  simplify = FALSE)) %>%
        unnest(report)
}

set.seed(args$seed)
gt <- read_feather(args$input)
reports <- gen_reports(gt)


gt %>%
    select(cell_x, cell_y, id) %>%
    left_join(reports %>% distinct(id) %>% mutate(obs = TRUE), by = "id") %>%
    replace_na(list(obs = FALSE)) %>%
    group_by(cell_x, cell_y) %>%
    summarise(n_obs = sum(obs),
              n_unobs = sum(1-obs),
              n_total = n(),
              obs_pct = sum(obs)/n()) %>%
    arrange(obs_pct) %>% print(n = Inf)

reports %>%
    ggplot(aes(x, y)) +
    geom_point(size = .2) +
    facet_wrap(~source) +
    coord_fixed() + theme_minimal()

reports %>%
    distinct(x, y, id) %>%
    ggplot(aes(x, y)) +
    geom_point(size = .2) +
    coord_fixed() + theme_minimal()

reports %>%
    bind_rows(gt %>% select(-class) %>% mutate(source = "truth")) %>%
    count(source, cell_x, cell_y) %>%
    group_by(source) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup %>%
    ggplot(aes(x = cell_x, y = cell_y, fill = pct)) +
    geom_tile() + facet_wrap(~source) +
    scale_fill_viridis() +
    theme_minimal() + coord_fixed()

train <- reports %>%
    select(id, source, cell_x, cell_y) %>%
    mutate(ind = 1L) %>%
    spread(source, ind, fill = 0L) %>%
    group_by(cell_x, cell_y, source1, source2, source3, source4) %>%
    summarise(n = n_distinct(id)) %>% ungroup %>%
    mutate(cell = paste0(cell_x, "_", cell_y))

library(brms)
library(tidybayes)

mod <- brm(n ~ (source1 + source2 + source3 + source4) | cell,
           family = poisson(), data = train, cores = 4)

train %>%
    add_predicted_draws(mod, n = 200) %>%
    summarise(lo = quantile(.prediction, .1),
              hi = quantile(.prediction, .9)) %>%
    ungroup %>%
    mutate(good = n >= lo & n <= hi) %>%
    summarise(sum(good))

train %>%
    add_predicted_draws(mod, n = 200) %>%
    group_by(cell_x, cell_y, .draw) %>%
    summarise(n = sum(n), predicted = sum(.prediction)) %>%
    ggplot(aes(x = predicted)) +
    geom_histogram(bins = 40) +
    geom_vline(aes(xintercept = n), colour = "red") +
    facet_grid(cell_y ~ cell_x, scales = "free") +
    theme_minimal()

true <- gt %>%
    group_by(cell_x, cell_y) %>%
    summarise(n = n_distinct(id)) %>%
    ungroup

observed <- reports %>%
    group_by(cell_x, cell_y) %>%
    summarise(n_obs = n_distinct(id)) %>%
    ungroup

unobserved <- observed %>%
    left_join(true, by = c("cell_x", "cell_y")) %>%
    mutate(unobs = n - n_obs)

crossing(cell_x = 1:5, cell_y = 1:5,
         source1 = 0, source2 = 0, source3 = 0, source4 = 0) %>%
    mutate(cell = paste0(cell_x, "_", cell_y)) %>%
    add_predicted_draws(mod, n = 200, allow_new_levels = TRUE) %>%
    group_by(cell_x, cell_y, .draw) %>%
    summarise(predicted = sum(.prediction)) %>%
    ungroup %>%
    left_join(unobserved, by = c("cell_x", "cell_y")) %>%
    replace_na(list(unobs = 0)) %>%
    ggplot(aes(x = predicted)) +
    geom_histogram(bins = 40) +
    geom_vline(aes(xintercept = unobs), colour = "red") +
    facet_grid(cell_y ~ cell_x, scales = "free") +
    theme_minimal()


crossing(cell_x = 1:5, cell_y = 1:5,
         source1 = 0, source2 = 0, source3 = 0, source4 = 0) %>%
    mutate(cell = paste0(cell_x, "_", cell_y)) %>%
    add_predicted_draws(mod, n = 200, allow_new_levels = TRUE) %>%
    group_by(cell_x, cell_y, .draw) %>%
    summarise(predicted = sum(.prediction)) %>%
    group_by(cell_x, cell_y) %>%
    summarise(lo = quantile(predicted, .05),
              hi = quantile(predicted, .95)) %>%
    ungroup %>%
    left_join(unobserved, by = c("cell_x", "cell_y")) %>%
    replace_na(list(unobs = 0)) %>%
    mutate(bad = lo > unobs | hi < unobs) %>%
    mutate(lo = str_pad(round(lo), 3, side = "left", pad = " "),
           hi = str_pad(round(hi), 3, side = "right", pad = " "),
           unobs = str_pad(unobs, 3, side = "left", pad = " ")) %>%
    mutate(res = paste0(lo, ",", hi, " [", unobs, "]"))  %>%
    #     filter(bad) %>%
    mutate(res = ifelse(bad, paste0("X ", res, " X"), paste0("  ", res)),
           res = paste0("|", res)) %>%
    select(cell_x, cell_y, res) %>%
    spread(cell_x, res, fill = "") %>%
    select(-cell_y)

crossing(cell_x = 1:5, cell_y = 1:5,
         source1 = 0, source2 = 0, source3 = 0, source4 = 0) %>%
    mutate(cell = paste0(cell_x, "_", cell_y)) %>%
    add_predicted_draws(mod, n = 200, allow_new_levels = TRUE) %>%
    group_by(.draw) %>%
    summarise(predicted = sum(.prediction)) %>%
    ungroup %>%
    pluck("predicted") %>% quantile(seq(0, 1, by = .1))

    ggplot(aes(x = predicted)) + geom_histogram(bins = 40) +
    geom_vline(xintercept = sum(unobserved$unobs), colour = "red")
