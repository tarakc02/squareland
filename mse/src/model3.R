#
# Authors:     TS
# Maintainers: TS
# Copyright:   2019, HRDAG, GPL v2 or later
# =========================================
# squareland/mse/src/model3.R

library(pacman)
pacman::p_load("argparse", "feather", "dplyr", "brms")

parser <- ArgumentParser()
parser$add_argument("--input", default = "output/cell-counts.feather")
parser$add_argument("--reports", default = "../generate-reports/output/reports.feather")
parser$add_argument("--seed", type = "integer", default = 67020723)
parser$add_argument("--regionplot", default = "output/model-3-regions.png")
parser$add_argument("--regioncells", default = "output/cell-counts-region.feather")
parser$add_argument("--output", default = "output/model3.rds")
args <- parser$parse_args()

train <- read_feather(args$input)
reports <- read_feather(args$reports)

add_region <- function(dat) {
    dat %>%
        mutate(region = case_when(
            between(cell_x, 1,3) & cell_y == 5 ~ "r1",
            between(cell_x, 4, 5) & between(cell_y, 1, 5) ~ "r2",
            cell_x == 3 & between(cell_y, 1, 4) ~ "r2",
            between(cell_x, 1, 2) & between(cell_y, 1, 4) ~ "r3",
            TRUE ~ "other"))
}

region_plot <- reports %>%
    add_region %>%
    ggplot(aes(x = x, y = y)) +
    geom_jitter(aes(colour = region)) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    coord_fixed() +
    ggtitle("ad hoc region definitions",
            "based on data")

png(filename = args$regionplot, width = 700, height = 750)
print(region_plot)
dev.off()

train <- train %>% add_region
write_feather(train, args$regioncells)

mod <- brm(n ~ ((source1 + source2 + source3 + source4) | cell)+
               ((source1 + source2 + source3 + source4) | region),
           family = poisson(), data = train,
           chains = 4, iter = 10000, seed = args$seed,
           cores = 4, control = list(max_treedepth = 15,
                                     adapt_delta = .9))

saveRDS(mod, args$output)

# done.


