reports <- function(incidents, location = runif(2, min = 0, max = 10)) {
    p <- runif(1, min = .1, max = .3)
    n <- nrow(incidents) * p

    inc <- incidents %>%
        dplyr::mutate(distance = (x - location[1])^2 + (y - location[2])^2)

    res <- tibble::tibble(id = sample(inc$id,
                                      prob = 1 / inc$distance,
                                      size = n,
                                      replace = TRUE)) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(incidents, by = "id")
    #     dplyr::mutate(locx = location[1], locy = location[2])

    structure(res,
              location = location,
              class = c("observed_incidents", "incidents", class(res)))
}

#' @export
gen_reports <- function(incidents, n_reports = 4L, ...) {
    reps <- replicate(n_reports, reports(incidents), simplify = FALSE)
    locations <- lapply(reps, attr, which = "location")

    reps <- dplyr::bind_rows(reps, .id = "reporter")
    structure(reps,
              locations = locations,
              class = c("observed_incidents", "incidents", class(reps)))
}
