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

#' Test if a `s2cd` object is within a date range or s2 cell union
#'
#' Dates within the s2cd object are compared to the date range,
#' inclusively. An s2_cell vector or s2_cell_union object
#' is used to test for intersection with the s2 cells.
#' If both a date range and s2 cell union are provided,
#' `s2cd_within()` will return TRUE only if both are within.
#' @param x an s2_cell_dates object
#' @param date_range length two Date vector representing a
#' minimum and maximum allowable date
#' @param s2_cell a s2_cell vector, often a covering represented by
#' many cells of different resolutions; see examples
#' @return logical
#' @export
#' @examples
#' # create compact representations of arbitrary polygons:
#' # s2::s2_covering_cell_ids_agg(
#' #   codec::cincy_city_geo(),
#' #   max_level = 12, max_cells = 100)
s2cd_within <- function(x, date_range = NULL, s2_cell = NULL) {
  if (!is.null(date_range)) {
    covers_dates <- s2cd_within_dates(x = x, date_range = date_range)
  }
  if (!is.null(s2_cell)) {
    covers_s2 <- s2cd_within_s2_cells(x = x, s2_cell = s2_cell)
  }
  if (is.null(date_range) && is.null(s2_cell)) {
    return(FALSE)
  }
  if (!is.null(date_range) && is.null(s2_cell)) {
    return(covers_dates)
  }
  if (is.null(date_range) && !is.null(s2_cell)) {
    return(covers_s2)
  }
  if (!is.null(date_range) && !is.null(s2_cell)) {
    return(covers_dates && covers_s2)
  }
}

s2cd_within_dates <- function(x, date_range) {
  stopifnot(
    "x must be a s2cd object" = is_s2cd(x),
    "date_range must be a Date vector" = inherits(date_range, "Date"),
    "date_range must be length 2" = length(date_range) == 2,
    "date_range must not have any missing values" = !anyNA(date_range),
    "date_range must be chronologically sorted" = sort(date_range) == date_range
  )
  if (
    any(sapply(x@dates, \(.) any(. <= date_range[1]))) ||
      any(sapply(x@dates, \(.) any(. >= date_range[2])))
  ) {
    return(FALSE)
  }
  TRUE
}

s2cd_within_s2_cells <- function(x, s2_cell) {
  stopifnot(
    "x must be a s2cd object" = is_s2cd(x),
    "s2_cell must be a s2_cell vector" = inherits(s2_cell, "s2_cell"),
    "s2_cell must have a non-zero length" = length(s2_cell) > 0,
    "s2_cell must not having any missing values" = !any(is.na(s2_cell))
  )
  s2_bounds <- s2::s2_cell_union(list(s2_cell))
  does_contain <- s2::s2_cell_union_contains(s2_bounds, s2::as_s2_cell(x))
  all(does_contain)
}
