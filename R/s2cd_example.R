#' Simple example `s2cd` object
#'
#' `s2cd_example()` returns a very simple example
#'
#' @return A `s2cd` object
#' @export
#' @examples
#' s2cd_example()
#'
#' str(s2cd_example())
s2cd_example <- function() {
  s2cd(
    s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
    dates = list(
      as.Date("2024-01-01"),
      c(as.Date("2024-09-13"), as.Date("2024-09-20"))
    )
  )
}

#' More complex example `s2cd` object
#'
#' `s2cd_example_cincy()` randomly generates an example s2cd object by sampling locations
#' from the center of level 15 cells covering the s2 cell "8841" and
#' sampling dates from 2024.
#' @param n_locations integer; number of s2 locations to simulate
#' @param n_dates_each integer; number of dates to simulate per location
#' @param n_dates_variation "poisson+1" randomly draws the number of
#' dates per location from a Poisson distribution with lambda equal
#' to `n_dates`; "fixed" randomly draws `n_dates` for every location
#' @export
#' @examples
#' set.seed(923)
#' s2cd_example_cincy()
#' s2cd_example_cincy(10L, 5L, "poisson+1")
s2cd_example_cincy <- function(
  n_locations = 100L,
  n_dates_each = 3L,
  n_dates_variation = c("fixed", "poisson+1")
) {
  stopifnot(
    "n_locations must be an integer" = is.integer(n_locations),
    "n_dates_each must be an integer" = is.integer(n_dates_each),
    "n_dates_variation must be a character" = is.character(n_dates_variation)
  )
  n_dates_variation <- match.arg(n_dates_variation)
  the_s2 <- s2::s2_covering_cell_ids(
    s2::s2_cell_polygon(s2::as_s2_cell("8841")),
    min_level = 15,
    max_level = 15
  ) |>
    unlist() |>
    sample(size = n_locations) |>
    s2::s2_cell_center() |>
    s2::as_s2_cell()
  if (n_dates_variation == "poisson+1") {
    n_dates_each <- stats::rpois(n_locations, lambda = n_dates_each + 1)
  } else if (n_dates_variation == "fixed") {
    n_dates_each <- rep(n_dates_each, n_locations)
  }
  the_dates <-
    lapply(n_dates_each, \(x) {
      sample(
        seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = 1),
        size = x
      )
    }) |>
    lapply(sort)
  s2cd(the_s2, the_dates)
}
