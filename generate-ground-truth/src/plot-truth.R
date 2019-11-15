#
# Authors:     TS
# Maintainers: TS
# Copyright:   2019, HRDAG, GPL v2 or later
# =========================================
# squareland/generate-ground-truth/src/plot-truth.R

library(pacman)
pacman::p_load("ggplot2", "argparse", "feather")

parser <- ArgumentParser()
parser$add_argument("--input", default = "output/c7l3i1000.feather")
parser$add_argument("--output", default = "output/c7l3i1000.png")
args <- parser$parse_args()

plot_incidents <- function(incidents) {
    ggplot(incidents, aes(x = x, y = y)) +
        geom_point(size = .1) +
        coord_fixed(xlim = c(0, 5),
                    ylim = c(0, 5)) +
        scale_x_continuous(breaks = 0:5) +
        scale_y_continuous(breaks = 0:5) +
        theme_minimal() +
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank())
}

truth <- read_feather(args$input)
png(filename = args$output, width = 650, height = 650)
plot_incidents(truth)
dev.off()

# plot.observed_incidents <- function(reps) {
#     plot.incidents(reps) +
#         ggplot2::facet_wrap(~reporter)
# }
# 
# grid_cut <- function(vect) {
#     cut(vect, breaks = seq(0, 5, by = 1),
#         labels = 1:5,
#         right = FALSE,
#         include.lowest = TRUE) %>%
#     as.character %>% as.integer
# }

# done.

