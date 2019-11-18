#
# Authors:     TS
# Maintainers: TS
# Copyright:   2019, HRDAG, GPL v2 or later
# =========================================
# squareland/mse/src/summarize_reports.R

library(pacman)
pacman::p_load("argparse", "feather", "dplyr", "tidyr")

parser <- ArgumentParser()
parser$add_argument("--input", default = "../generate-reports/output/reports.feather")
parser$add_argument("--output", default = "output/cell-counts.feather")
args <- parser$parse_args()

observed_data <- read_feather(args$input)

train <- crossing(cell_x = 1:5, cell_y = 1:5,
                  source1 = c(0, 1), source2 = c(0, 1),
                  source3 = c(0, 1), source4 = c(0, 1)) %>%
    filter(source1 + source2 + source3 + source4 > 0) %>%
    left_join(
              observed_data %>%
                  select(id, source, cell_x, cell_y) %>%
                  mutate(ind = 1L) %>%
                  spread(source, ind, fill = 0L) %>%
                  group_by(cell_x, cell_y,
                           source1, source2, source3, source4) %>%
                  summarise(n = n_distinct(id)) %>% ungroup,
              by = c("cell_x", "cell_y",
                     "source1", "source2", "source3", "source4")) %>%
    replace_na(list(n = 0)) %>%
    mutate(cell = paste0(cell_x, "_", cell_y))

stopifnot(sum(train$n) == length(unique(observed_data$id)))

write_feather(train, args$output)

# done.
