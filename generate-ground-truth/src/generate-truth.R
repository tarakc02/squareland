#
# Authors:     TS
# Maintainers: TS
# Copyright:   2019, HRDAG, GPL v2 or later
# =========================================
# squareland/generate-ground-truth/src/generate-truth.R

library(pacman)
pacman::p_load("MASS", "dplyr", "tidyr", "argparse", "feather")

parser <- ArgumentParser()
parser$add_argument("--seed", type = "integer", default = 19481210)
parser$add_argument("--n_centers", type = "integer", default = 7)
parser$add_argument("--mean_incidents", type = "integer", default = 1000)
parser$add_argument("--output", type = "character", default = "output/truth.feather")
args <- parser$parse_args()

is_pos_def <- function(Sigma) {
    if (is.null(Sigma)) return(FALSE)
    tol = 1e-06

    eS <- eigen(Sigma, symmetric = TRUE)
    ev <- eS$values

    all(ev >= -tol * abs(ev[1L]))
}

gen_incidents <- function(n = rnorm(1, mean = args$mean_incidents,
                                    sd = .1 * args$mean_incidents) %>% round,
                          means = runif(2, min = 0, max = 10),
                          x_sd = runif(1, min = .3, max = 2),
                          y_sd = runif(1, min = .3, max = 2)) {

    try_cov_mat <- function() {
        covariance <- runif(1, min = .1, max = .5)
        matrix(c(
                 x_sd^2, covariance,
                 covariance, y_sd^2
                 ),
               byrow = TRUE, nrow = 2, ncol = 2)
    }

    covariance_matrix <- NULL
    while (!is_pos_def(covariance_matrix)) covariance_matrix <- try_cov_mat()

    res <- mvrnorm(n = n, mu = means, Sigma = covariance_matrix)

    tibble(x = res[, 1, drop = TRUE],
           y = res[, 2, drop = TRUE]) %>%
        filter(x >= 0, x <= 10, y >= 0, y <= 10) %>%
        mutate(x = x/2, y = y/2)
}

grid_cut <- function(vect) {
    cut(vect, breaks = seq(0, 5, by = 1),
        labels = 1:5,
        right = FALSE,
        include.lowest = TRUE) %>%
    as.character %>% as.integer
}

gen_truth <- function(n_centers = args$n_centers,
                      classes = LETTERS[seq(1, args$n_centers)],
                      seed = args$seed,...) {
    set.seed(seed)
    gen <- function() gen_incidents(...)

    tibble(class = classes) %>%
        mutate(dat = replicate(nrow(.),
                               gen_incidents(),
                               simplify = FALSE)) %>%
        unnest(dat) %>%
        mutate(cell_x = grid_cut(x), cell_y = grid_cut(y)) %>%
        mutate(id = seq_len(nrow(.)))
}

write_feather(gen_truth(), args$output)

# done.
