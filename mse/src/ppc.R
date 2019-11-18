#
# Authors:     TS
# Maintainers: TS
# Copyright:   2019, HRDAG, GPL v2 or later
# =========================================
# squareland/mse/src/ppc.R

library(pacman)
pacman::p_load("argparse", "feather",
               "rlang",
               "dplyr", "tidybayes",
               "ggplot2", "ggbeeswarm")

library(argparse)

parser <- ArgumentParser()
parser$add_argument("--data", default = "output/cell-counts.feather")
parser$add_argument("--model", default = "output/model1.rds")
parser$add_argument("--modelname", default = "model_1")
parser$add_argument("--outputdir", default = "output")
args <- parser$parse_args()

cell_counts <- read_feather(args$data)
model <- readRDS(args$model)

save_plot <- function(gg, plotname,
                      height, width) {
    filename <- paste0(friendly_name(args$modelname),
                       "-ppc-", plotname, ".png")
    filename <- file.path(args$outputdir, filename)
    png(filename = filename, width = width, height = height, units = "px")
    print(gg)
    dev.off()
}

friendly_name <- function(txt) {
    gsub("_", "-", txt)
}

summarise_counts <- function(..., cells = cell_counts,
                             mod = model, nsim = 200) {
    cells %>%
        add_predicted_draws(model, n = nsim) %>%
        group_by(..., .draw) %>%
        summarise(n = sum(n),
                  predicted = sum(.prediction)) %>%
        ungroup
}

ppc_geoms <- function(post_samples, level_var) {
    levs <- enquo(level_var)
    ggplot(post_samples, aes(!!levs)) +
        geom_quasirandom(aes(y = predicted),
                         size = .1, colour = "gray70") +
        geom_point(aes(y = n),
                   colour = "red", size = 1) +
        coord_flip() +
        theme_minimal() +
        theme(panel.grid.minor = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank())
}

geo <- summarise_counts(cell_x, cell_y) %>%
    ppc_geoms(level_var = cell_y) +
    facet_wrap(~cell_x, nrow = 1) +
    ggtitle(paste0("Observed counts vs. posterior predictions, ",
                   args$modelname),
            "by grid cell")

save_plot(geo, plotname = "gridcell",
          height = 400, width = 1750)

docpattern <- summarise_counts(source1, source2, source3, source4) %>%
    mutate(documentation_pattern = paste0(source1, source2, source3, source4)) %>%
    mutate(documentation_pattern = reorder(documentation_pattern, n)) %>%
    ppc_geoms(level_var = documentation_pattern) +
    ggtitle(paste0("Observed counts vs. posterior predictions, ",
                   args$modelname),
            "by documentation pattern")

save_plot(docpattern, plotname = "docpattern",
          height = 500, width = 800)

# done.
