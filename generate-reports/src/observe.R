#
# Authors:     TS
# Maintainers: TS
# Copyright:   2019, HRDAG, GPL v2 or later
# =========================================
# /Users/tshah/git/squareland/generate-reports/src/observe.R

library(pacman)
pacman::p_load("argparse", "feather",
               "dplyr", "tidyr", "purrr",
               "stringr",
               "ggplot2", "viridis")

parser <- ArgumentParser()
parser$add_argument("--input", default = "../generate-ground-truth/output/truth.feather")
parser$add_argument("--nsource", type = "integer", default = 4)
parser$add_argument("--seed", type = "integer", default = 202070170)
parser$add_argument("--reports", default = "output/reports.feather")
# parser$add_argument("--obs_by_source", default = "output/obs-by-source.png")
# parser$add_argument("--obs_total", default = "output/obs-total.png")
args <- parser$parse_args()

observe_incidents <- function(truth) {
    truth %>%
        group_by(class) %>%
        nest %>%
        ungroup %>%
        mutate(frac = rbeta(n = nrow(.), shape1 = .5, shape2 = 2)) %>%
        mutate(samps = map2(data, frac, sample_frac)) %>%
        select(class, samps) %>%
        unnest(samps)
}

gen_reports <- function(truth) {
    tibble(source = paste0("source", 1:args$nsource)) %>%
        mutate(report = replicate(nrow(.),
                                  observe_incidents(truth),
                                  simplify = FALSE)) %>%
        unnest(report)
}

gt <- read_feather(args$input)
set.seed(args$seed)
reports <- gen_reports(gt)

write_feather(reports, args$reports)

# 
# gt %>%
#     select(cell_x, cell_y, id) %>%
#     left_join(reports %>% distinct(id) %>% mutate(obs = TRUE), by = "id") %>%
#     replace_na(list(obs = FALSE)) %>%
#     group_by(cell_x, cell_y) %>%
#     summarise(n_obs = sum(obs),
#               n_unobs = sum(1-obs),
#               n_total = n(),
#               obs_pct = sum(obs)/n()) %>%
#     arrange(obs_pct) %>% print(n = Inf)
# 
# # observed by source
# reports %>%
#     ggplot(aes(x, y)) +
#     geom_point(size = .1) +
#     facet_wrap(~source) +
#     coord_fixed() + theme_minimal() +
#     theme(panel.grid.minor = element_blank())
# 
# # observed (total)
# reports %>%
#     distinct(x, y, id, class) %>%
#     ggplot(aes(x, y)) +
#     geom_point(size = .2) +
#     coord_fixed() + theme_minimal()
# 
# reports %>%
#     mutate(region = case_when(
#                between(cell_x, 1,3) & cell_y == 5 ~ "r1",
#                between(cell_x, 4, 5) & between(cell_y, 2, 5) ~ "r2",
#                between(cell_x, 1, 2) & cell_y == 1 ~ "r3",
#                between(cell_x, 1, 2) & between(cell_y, 2, 3) ~ "r4",
#                TRUE ~ "other")) %>%
#     ggplot(aes(x, y, colour = region)) + geom_point(size = .2) +
#     coord_fixed() + theme_minimal()
# 
# reports %>%
#     bind_rows(gt %>% select(-class) %>% mutate(source = "truth")) %>%
#     count(source, cell_x, cell_y) %>%
#     group_by(source) %>%
#     mutate(pct = n / sum(n)) %>%
#     ungroup %>%
#     ggplot(aes(x = cell_x, y = cell_y, fill = pct)) +
#     geom_tile() + facet_wrap(~source) +
#     scale_fill_viridis() +
#     theme_minimal() + coord_fixed()
# 
# # train <- reports %>%
# #     distinct(source, cell_x, cell_y) %>%
# #     expand(source, cell_x, cell_y) %>%
# #     left_join(reports %>% select(id, source, cell_x, cell_y),
# #               by = c("source", "cell_x", "cell_y")) %>%
# #     filter(is.na(id))
# #     left_join(reports %>%
# #                   group_by(source, cell_x, cell_y) %>%
# #                   summarise(n = n_distinct(id)) %>% ungroup, 
# #               by = c("source", "cell_x", "cell_y")) %>%
# #     replace_na(list(n = 0)) %>%
# #     spread(source, n, fill = 0)
# 
# reports <- reports %>%
#     mutate(region = case_when(
#                between(cell_x, 1,3) & cell_y == 5 ~ "r1",
#                between(cell_x, 4, 5) & between(cell_y, 2, 5) ~ "r2",
#                between(cell_x, 1, 2) & cell_y == 1 ~ "r3",
#                between(cell_x, 1, 2) & between(cell_y, 2, 3) ~ "r4",
#                TRUE ~ "r5"))
# 
# train <- crossing(cell_x = 1:5, cell_y = 1:5,
#                   source1 = c(0, 1), source2 = c(0, 1),
#                   source3 = c(0, 1), source4 = c(0, 1)) %>%
#     filter(source1 + source2 + source3 + source4 > 0) %>%
#     mutate(region = case_when(
#                between(cell_x, 1,3) & cell_y == 5 ~ "r1",
#                between(cell_x, 4, 5) & between(cell_y, 2, 5) ~ "r2",
#                between(cell_x, 1, 2) & cell_y == 1 ~ "r3",
#                between(cell_x, 1, 2) & between(cell_y, 2, 3) ~ "r4",
#                TRUE ~ "r5")) %>%
#     left_join(
#               reports %>%
#                   select(id, source, region, cell_x, cell_y) %>%
#                   mutate(ind = 1L) %>%
#                   spread(source, ind, fill = 0L) %>%
#                   group_by(region, cell_x, cell_y,
#                            source1, source2, source3, source4) %>%
#                   summarise(n = n_distinct(id)) %>% ungroup,
#               by = c("cell_x", "cell_y", "region",
#                      "source1", "source2", "source3", "source4")) %>%
#     replace_na(list(n = 0)) %>%
#     mutate(cell = paste0(cell_x, "_", cell_y))
# 
# library(brms)
# library(tidybayes)
# 
# mod <- brm(n ~ ((source1 + source2 + source3 + source4) | cell) +
#                ((source1 + source2 + source3 + source4) | region),
#            family = poisson(), data = train,
#            chains = 4, iter = 6000,
#            cores = 4, control = list(max_treedepth = 15))
# 
# mod2_form <- bf(n ~ main + interaction,
#                 main ~ (1 + source1 + source2 + source3 + source4) | cell,
#                 interaction ~ 0 + (source1 + source2 + source3 + source4) ^ 4 -
#                                   (source1 + source2 + source3 + source4),
#                 family = poisson(), nl = TRUE)
# 
# mod2_priors <- c(prior(normal(0, 10), nlpar = "main"),
#                  prior(horseshoe(), nlpar = "interaction"))
# 
# mod2 <- brm(mod2_form,
#             data = train, prior = mod2_priors,
#             chains = 4, iter = 5000,
#             cores = 4,
#             control = list(max_treedepth = 15,
#                            adapt_delta = .9))
# 
# train %>%
#     add_predicted_draws(mod, n = 200) %>%
#     summarise(lo = quantile(.prediction, .1),
#               hi = quantile(.prediction, .9)) %>%
#     ungroup %>%
#     mutate(good = n >= lo & n <= hi) %>%
#     summarise(sum(good))
# 
# train %>%
#     add_predicted_draws(mod, n = 200) %>%
#     group_by(cell_x, cell_y, .draw) %>%
#     summarise(n = sum(n), predicted = sum(.prediction)) %>%
#     ggplot(aes(x = predicted)) +
#     geom_histogram(bins = 40) +
#     geom_vline(aes(xintercept = n), colour = "red") +
#     facet_grid(cell_y ~ cell_x, scales = "free") +
#     theme_minimal()
# 
# train %>%
#     add_predicted_draws(mod, n = 200) %>%
#     group_by(cell_x, cell_y, .draw) %>%
#     summarise(n = sum(n), predicted = sum(.prediction)) %>%
#     mutate(prediction_error = predicted - n,
#            err_sq = prediction_error ^ 2) %>%
#     summarise(n = max(n),
#               rms_error = sqrt(mean(err_sq))) %>%
#     ungroup %>%
#     mutate(rms_pct = rms_error / n) %>%
#     ggplot(aes(x = n, y = rms_pct)) + geom_point() + theme_minimal()
# 
#     ggplot(aes(x = n, y = prediction_error)) +
#     geom_point()
# 
# true <- gt %>%
#     group_by(cell_x, cell_y) %>%
#     summarise(n = n_distinct(id)) %>%
#     ungroup
# 
# observed <- reports %>%
#     group_by(cell_x, cell_y) %>%
#     summarise(n_obs = n_distinct(id)) %>%
#     ungroup
# 
# unobserved <- observed %>%
#     left_join(true, by = c("cell_x", "cell_y")) %>%
#     mutate(unobs = n - n_obs)
# 
# crossing(cell_x = 1:5, cell_y = 1:5,
#          source1 = 0, source2 = 0, source3 = 0, source4 = 0) %>%
#     mutate(region = case_when(
#                               between(cell_x, 1,3) & cell_y == 5 ~ "r1",
#                               between(cell_x, 4, 5) & between(cell_y, 2, 5) ~ "r2",
#                               between(cell_x, 1, 2) & cell_y == 1 ~ "r3",
#                               between(cell_x, 1, 2) & between(cell_y, 2, 3) ~ "r4",
#                               TRUE ~ "other")) %>%
#     mutate(cell = paste0(cell_x, "_", cell_y)) %>%
#     add_predicted_draws(mod, n = 200, allow_new_levels = TRUE) %>%
#     group_by(cell_x, cell_y, .draw) %>%
#     summarise(predicted = sum(.prediction)) %>%
#     ungroup %>%
#     left_join(unobserved, by = c("cell_x", "cell_y")) %>%
#     replace_na(list(unobs = 0)) %>%
#     ggplot(aes(x = predicted)) +
#     geom_histogram(bins = 40) +
#     geom_vline(aes(xintercept = unobs), colour = "red") +
#     facet_grid(cell_y ~ cell_x, scales = "free") +
#     theme_minimal()
# 
# library(ggbeeswarm)
# 
# crossing(cell_x = 1:5, cell_y = 1:5,
#          source1 = 0, source2 = 0, source3 = 0, source4 = 0) %>%
#     mutate(region = case_when(
#                               between(cell_x, 1,3) & cell_y == 5 ~ "r1",
#                               between(cell_x, 4, 5) & between(cell_y, 2, 5) ~ "r2",
#                               between(cell_x, 1, 2) & cell_y == 1 ~ "r3",
#                               between(cell_x, 1, 2) & between(cell_y, 2, 3) ~ "r4",
#                               TRUE ~ "other")) %>%
#     mutate(cell = paste0(cell_x, "_", cell_y)) %>%
#     add_predicted_draws(mod, n = 100, allow_new_levels = TRUE) %>%
#     group_by(cell_x, cell_y, .draw) %>%
#     summarise(predicted = sum(.prediction)) %>%
#     ungroup %>%
#     left_join(unobserved, by = c("cell_x", "cell_y")) %>%
#     replace_na(list(unobs = 0)) %>%
#     mutate(sq_err = (predicted - unobs)^2) %>%
#     group_by(cell_x, cell_y) %>%
#     summarise(n_obs     = max(n_obs),
#               n_unobs   = max(unobs),
#               n         = max(n),
#               pred_lo   = quantile(predicted, .08),
#               pred      = mean(predicted),
#               pred_hi   = quantile(predicted, .92),
#               rms_error = sqrt(mean(sq_err))) %>%
#     mutate(cell = paste(cell_x, cell_y, sep = ",")) %>%
#     ggplot(aes(x = reorder(cell, n_unobs))) +
#     geom_linerange(aes(ymin = pred_lo, ymax = pred_hi)) +
#     geom_point(aes(y = n_unobs), colour = "red") +
#     geom_line(aes(y = n_unobs), colour = "red", group = NA) +
#     #     coord_flip() +
#     theme_minimal()
# 
# 
#     ggplot(aes(x = cell_y, y = predicted)) +
#     geom_quasirandom(size = .2) + facet_wrap(~cell_x, nrow = 1) +
#     coord_flip() + theme_minimal()
# 
# 
#     ggplot(aes(x = predicted)) +
#     geom_dotplot(size = .1, bins = 25) +
#     #     geom_histogram(bins = 25) +
#     facet_grid(cell_y ~ cell_x, scales = "free") +
#     geom_vline(aes(xintercept = unobs), colour = "red") +
#     theme_minimal()
# 
# 
# 
# 
# crossing(cell_x = 1:5, cell_y = 1:5,
#          source1 = 0, source2 = 0, source3 = 0, source4 = 0) %>%
#     mutate(region = case_when(
#                               between(cell_x, 1,3) & cell_y == 5 ~ "r1",
#                               between(cell_x, 4, 5) & between(cell_y, 2, 5) ~ "r2",
#                               between(cell_x, 1, 2) & cell_y == 1 ~ "r3",
#                               between(cell_x, 1, 2) & between(cell_y, 2, 3) ~ "r4",
#                               TRUE ~ "other")) %>%
#     mutate(cell = paste0(cell_x, "_", cell_y)) %>%
#     add_predicted_draws(mod, n = 200, allow_new_levels = TRUE) %>%
#     group_by(cell_x, cell_y, .draw) %>%
#     summarise(predicted = sum(.prediction)) %>%
#     group_by(cell_x, cell_y) %>%
#     summarise(lo = quantile(predicted, .05),
#               hi = quantile(predicted, .95)) %>%
#     ungroup %>%
#     left_join(unobserved, by = c("cell_x", "cell_y")) %>%
#     replace_na(list(unobs = 0)) %>%
#     mutate(bad = lo > unobs | hi < unobs) %>%
#     mutate(lo = str_pad(round(lo), 3, side = "left", pad = " "),
#            hi = str_pad(round(hi), 3, side = "right", pad = " "),
#            unobs = str_pad(unobs, 3, side = "left", pad = " ")) %>%
#     mutate(res = paste0(lo, ",", hi, " [", unobs, "]"))  %>%
#     #     filter(bad) %>%
#     mutate(res = ifelse(bad, paste0("X ", res, " X"), paste0("  ", res)),
#            res = paste0("|", res)) %>%
#     select(cell_x, cell_y, res) %>%
#     spread(cell_x, res, fill = "") %>%
#     select(-cell_y)
# 
# crossing(cell_x = 1:5, cell_y = 1:5,
#          source1 = 0, source2 = 0, source3 = 0, source4 = 0) %>%
#     mutate(region = case_when(
#                               between(cell_x, 1,3) & cell_y == 5 ~ "r1",
#                               between(cell_x, 4, 5) & between(cell_y, 2, 5) ~ "r2",
#                               between(cell_x, 1, 2) & cell_y == 1 ~ "r3",
#                               between(cell_x, 1, 2) & between(cell_y, 2, 3) ~ "r4",
#                               TRUE ~ "other")) %>%
#     mutate(cell = paste0(cell_x, "_", cell_y)) %>%
#     add_predicted_draws(mod, n = 200, allow_new_levels = TRUE) %>%
#     group_by(.draw) %>%
#     summarise(predicted = sum(.prediction)) %>%
#     ungroup %>% ggplot(aes(predicted)) + geom_histogram(bins = 75) + theme_minimal()
#     pluck("predicted") %>% quantile(seq(0, 1, by = .1))
# 
#     ggplot(aes(x = predicted)) + geom_histogram(bins = 40) +
#     geom_vline(xintercept = sum(unobserved$unobs), colour = "red")
