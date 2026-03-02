.data.frame <- S7::new_S3_class("data.frame")

.s2_cell <- S7::new_S3_class(
  "s2_cell",
  constructor = function(.data) {
    structure(.data, class = c("s2_cell", "wk_vctr"))
  },
  validator = function(x) {
    if (!inherits(x, "s2_cell")) {
      "Not an s2_cell."
    }
  }
)

#' Create a new s2_cell_dates (`s2cd`) object
#'
#' A `s2cd` object (short for **s2_cell_dates**)
#' extends the `s2::s2_cell` class with a list of date vectors,
#' where each vector of chronologically, non-missing dates corresponds
#' to each (valid, level 30) s2 cell. Create a new `s2cd` object
#' with `s2cd()` or coerce another object into a `s2cd` object
#' with `as_s2cd()`.
#'
#' Each position in the s2_cells vector corresponds to
#' the same-indexed element in dates, allowing multiple
#' dates to be associated with a single level-30 S2 cell.
#'
#' At level 30, S2 cells have an approximate spatial
#' resolution of *1 centimeter*; in this context,
#' they are intended to function as point-like
#' representations rather than true spatial areas.
#'
#' @seealso as_s2cd
#' @param .data an s2_cell object that is valid and is at level 30 resolution
#' @param dates a list of date vectors,
#' each in chronological order and without missing values
#' @return A `s2cd` object
#' @export
#' @examples
#'
#' # create directly with s2_cell vector and list of Date vectors
#' d <- s2cd(
#'   s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
#'   dates = list(
#'     as.Date("2026-01-01"),
#'     c(as.Date("2024-09-13"), as.Date("2024-09-20"))
#'   )
#' )
#'
#' # check if object is a s2cd object
#' is_s2cd(d)
#'
#' # create using data.frame with s2_cell and dates columns
#' data.frame(
#'   s2_cell = s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
#'   dates = I(list(
#'     as.Date("2026-01-01"),
#'     c(as.Date("2024-09-13"), as.Date("2024-09-20"))
#'   ))
#' ) |>
#'   as_s2cd()
s2cd <- S7::new_class(
  "s2cd",
  parent = .s2_cell,
  package = NULL,
  properties = list(dates = S7::class_list),
  validator = function(self) {
    if (!inherits(self, "s2_cell")) {
      "`s2cd` must extend an `s2_cell` vector."
    } else if (!all(s2::s2_cell_is_valid(self))) {
      "s2 cells must be valid (`s2::s2_cell_is_valid()`)."
    } else if (!all(s2::s2_cell_level(self) == 30)) {
      "all s2 cells must be level 30"
    } else if (!is.list(self@dates)) {
      "`dates` must be a list."
    } else if (!length(self@dates) == length(self)) {
      "`dates` must have same length as `cells`."
    } else if (!all(sapply(self@dates, inherits, "Date"))) {
      "all elements in the dates list must be Date vectors."
    } else if (any(sapply(self@dates, anyNA))) {
      "dates must not contain missing values"
    } else if (!all(sapply(self@dates, is_non_decreasing))) {
      "each Date vector must be in chronological order"
    }
  }
)

#' @export
#' @method [ s2cd
`[.s2cd` <- function(x, i, ...) {
  if (missing(i)) {
    return(x)
  }
  s2cd(S7::S7_data(x)[i], x@dates[i])
}

#' Convert another object into a s2_cell_dates (`s2cd`) object
#'
#' Convert other R objects into s2cd objects
#' @section Methods implemented for:
#' - `data.frame`: must have columns called "s2_cell" and "dates"
#' - `s2cd`: returned as-is
#' @param x an object to convert
#' @param ... passed to methods
#' @export
#' @examples
#' data.frame(
#'      s2_cell = s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
#'      dates = I(list(
#'        as.Date("2026-01-01"),
#'        c(as.Date("2024-09-13"), as.Date("2024-09-20"))
#'      ))
#'    ) |>
#'   as_s2cd()
#'
#' as_s2cd(s2cd_example())
as_s2cd <- S7::new_generic("as_s2cd", dispatch_args = "x")

S7::method(as_s2cd, .data.frame) <- function(x, ...) {
  stopifnot(
    "data.frame must have column named `s2_cell`" = "s2_cell" %in% names(x),
    "data.frame must have column named `dates`" = "dates" %in% names(x)
  )
  s2cd(.data = x$s2_cell, dates = x$dates)
}

S7::method(as_s2cd, s2cd) <- function(x, ...) x

# TODO add method to create s2cd object given data.frame with s2_cell, start_date, and end_date (and id??? or do we store one s2cd object per person??)

# then, show examples how to use impute_date_ranges to go from address history (or address reported dates) to imputed address history and then use new function to add to convert to s2cd object

#' @importFrom s2 as_s2_cell
NULL

S7::method(as_s2_cell, s2cd) <- function(x, ...) {
  s2::new_s2_cell(S7::S7_data(x))
}

S7::method(print, s2cd) <- function(x, ...) {
  cat(sprintf("<s2_cell_dates[%d]>", length(x)))
  the_s2 <- structure(x, class = c("s2_cell", "wk_vctr"))
  cat("\n<s2>\n")
  print(as.character(the_s2))
  print(x@dates)
}

#' @export
as.data.frame.s2cd <- function(x, ...) {
  data.frame(
    s2_cell = structure(S7::S7_data(x), class = c("s2_cell", "wk_vctr")),
    dates = I(x@dates),
    row.names = NULL,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Test if an object is a `s2cd` object
#'
#' Tests if an object is of class `sc2d`
#' @param x any object to test
#' @return logical
#' @export
#' @examples
#' is_s2cd(s2cd_example())
#'
#' is_s2cd(letters)
is_s2cd <- function(x) {
  inherits(x, "s2cd")
}

#' Get unique years from s2cd object
#'
#' @param x a s2_cell_dates object (see `?s2cd`)
#' @returns a sorted character vector of years present in x
#' @export
#' @examples
#' s2cd_years(s2cd_example())
s2cd_years <- function(x) {
  stopifnot(
    "x must be a s2_cell_dates object" = is_s2cd(x)
  )
  x@dates |>
    do.call(c, args = _) |>
    format("%Y") |>
    unique() |>
    sort()
}


s2_cell_to_vect <- function(x) {
  x <- s2::as_s2_cell(x)
  stopifnot(
    "x must be (coercible to) a s2_cell object" = inherits(x, "s2_cell")
  )
  x |>
    s2::s2_cell_to_lnglat() |>
    as.data.frame() |>
    terra::vect(geom = c("x", "y"), crs = "+proj=longlat +datum=WGS84")
}


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
