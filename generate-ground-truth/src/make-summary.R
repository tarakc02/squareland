make_summary <- function(incidents) UseMethod("make_summary")

make_summary.incidents <- function(incidents) {
    cells <- incidents %>%
        dplyr::mutate(x = grid_cut(x), y = grid_cut(y)) %>%
        dplyr::group_by(x, y) %>%
        dplyr::summarise(n = dplyr::n_distinct(id)) %>%
        dplyr::ungroup()

    tidyr::crossing(x = 1:5, y = 1:5) %>%
        dplyr::left_join(cells, by = c("x", "y")) %>%
        tidyr::replace_na(list(n = 0)) %>%
        mutate(cell = paste(x, y, sep = "_"))
}

make_summary.observed_incidents <- function(reps) {
    cells <- reps %>%
        dplyr::mutate(x = grid_cut(x), y = grid_cut(y)) %>%
        dplyr::group_by(x, y, reporter) %>%
        dplyr::summarise(n = dplyr::n_distinct(id)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(reporter = paste0("r", reporter)) %>%
        dplyr::mutate(pl = 1L) %>%
        tidyr::spread(reporter, pl, fill = 0)

    expandcols <- names(cells)[names(cells) != "n"]
    expandcols <- rlang::syms(expandcols)

    cells %>%
        tidyr::complete(!!!expandcols, fill = list(n = 0)) %>%
        dplyr::filter_at(dplyr::vars(dplyr::starts_with("r")),
                         dplyr::any_vars(. > 0)) %>%
        mutate(cell = paste(x, y, sep = "_"))
}

# mse_cells <- reps %>%
#     mutate(x = grid_cut(x), y = grid_cut(y),
#            placeholder = 1L, reporter = paste0("r", reporter)) %>%
#     spread(reporter, placeholder, fill = 0) %>%
#     group_by_at(vars(-id)) %>%
#     summarise(n = dplyr::n_distinct(id)) %>%
#     ungroup %>%
#     mutate(cell = paste(x, y, sep = "_"))
# 
# mc <- crossing(x = 1:5, y = 1:5,
#                r1 = 0:1, r2 = 0:1, r3 = 0:1, r4 = 0:1) %>%
#     filter(r1 + r2 + r3 + r4 > 0) %>%
#     left_join(mse_cells, by = c("x", "y", "r1", "r2", "r3", "r4")) %>%
#     replace_na(list(n = 0)) %>%
#     mutate(cell = paste(x, y, sep = "_"))
# 
# 
