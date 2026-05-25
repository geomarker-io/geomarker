#' Create a new s2_cell_dates (`s2cd`) object
#'
#' A `s2cd` object (short for **s2_cell_dates**)
#' stores an `s2::s2_cell` vector alongside a list of date vectors,
#' where each vector of chronologically, non-missing dates corresponds to each
#' valid, level 30 s2 cell. Create a new `s2cd` object with `s2cd()` or coerce
#' another object into a `s2cd` object with `as_s2cd()`.
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
s2cd <- function(.data, dates = list()) {
  .data <- s2::as_s2_cell(.data)
  validate_s2cd(.data, dates)
  new_s2cd(.data, dates)
}

new_s2cd <- function(.data = s2::new_s2_cell(double()), dates = list()) {
  dates <- as.list(dates)
  date_field <- do.call(
    vctrs::list_of,
    c(dates, list(.ptype = as.Date(character())))
  )
  vctrs::new_rcrd(
    list(
      s2_cell = .data,
      dates = date_field
    ),
    class = "s2cd"
  )
}

validate_s2cd <- function(.data, dates) {
  if (!inherits(.data, "s2_cell")) {
    stop("`s2cd` must contain an `s2_cell` vector.", call. = FALSE)
  }
  if (!all(s2::s2_cell_is_valid(.data))) {
    stop("s2 cells must be valid (`s2::s2_cell_is_valid()`).", call. = FALSE)
  }
  if (!all(s2::s2_cell_level(.data) == 30)) {
    stop("all s2 cells must be level 30", call. = FALSE)
  }
  if (!is.list(dates)) {
    stop("`dates` must be a list.", call. = FALSE)
  }
  if (length(dates) != length(.data)) {
    stop("`dates` must have same length as `cells`.", call. = FALSE)
  }
  if (!all(vapply(dates, inherits, logical(1), "Date"))) {
    stop("all elements in the dates list must be Date vectors", call. = FALSE)
  }
  if (any(vapply(dates, anyNA, logical(1)))) {
    stop("dates must not contain missing values", call. = FALSE)
  }
  if (!all(vapply(dates, is_non_decreasing, logical(1)))) {
    stop("each Date vector must be in chronological order", call. = FALSE)
  }
  invisible(NULL)
}

s2cd_s2_cell <- function(x) {
  stopifnot("x must be a s2_cell_dates object" = is_s2cd(x))
  vctrs::field(x, "s2_cell")
}

#' Get date vectors from a s2_cell_dates (`s2cd`) object
#'
#' @param x a s2_cell_dates object (see `?s2cd`)
#' @returns a list of Date vectors
#' @export
#' @examples
#' s2cd_dates(s2cd_example())
s2cd_dates <- function(x) {
  stopifnot("x must be a s2_cell_dates object" = is_s2cd(x))
  as.list(vctrs::field(x, "dates"))
}

#' @export
format.s2cd <- function(x, ...) {
  as.character(s2cd_s2_cell(x))
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
as_s2cd <- function(x, ...) {
  UseMethod("as_s2cd")
}

#' @export
as_s2cd.data.frame <- function(x, ...) {
  stopifnot(
    "data.frame must have column named `s2_cell`" = "s2_cell" %in% names(x),
    "data.frame must have column named `dates`" = "dates" %in% names(x)
  )
  s2cd(.data = x$s2_cell, dates = x$dates)
}

#' @export
as_s2cd.s2cd <- function(x, ...) x

#' @export
as_s2cd.default <- function(x, ...) {
  stop(
    "can't convert object of class `",
    paste(class(x), collapse = "/"),
    "` to a s2cd object",
    call. = FALSE
  )
}

# TODO add method to create s2cd object given data.frame with s2_cell, start_date, and end_date (and id??? or do we store one s2cd object per person??)

# then, show examples how to use impute_date_ranges to go from address history (or address reported dates) to imputed address history and then use new function to add to convert to s2cd object

#' @importFrom s2 as_s2_cell
#' @importFrom vctrs vec_cast vec_ptype2 vec_ptype_abbr vec_ptype_full
NULL

#' @export
as_s2_cell.s2cd <- function(x, ...) {
  s2cd_s2_cell(x)
}

#' @export
print.s2cd <- function(x, ...) {
  cat(sprintf("<s2_cell_dates[%d]>", length(x)))
  cat("\n<s2>\n")
  print(as.character(s2cd_s2_cell(x)))
  print(s2cd_dates(x))
  invisible(x)
}

#' @export
as.data.frame.s2cd <- function(x, ...) {
  data.frame(
    s2_cell = s2cd_s2_cell(x),
    dates = I(s2cd_dates(x)),
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

#' @export
as.character.s2cd <- function(x, ...) {
  as.character(s2cd_s2_cell(x))
}

#' @export
as.numeric.s2cd <- function(x, ...) {
  as.numeric(s2cd_s2_cell(x))
}

#' @export
as.double.s2cd <- function(x, ...) {
  as.double(s2cd_s2_cell(x))
}

#' @export
vec_ptype_abbr.s2cd <- function(x, ...) {
  "s2cd"
}

#' @export
vec_ptype_full.s2cd <- function(x, ...) {
  "s2_cell_dates"
}

#' @export
vec_ptype2.s2cd.s2cd <- function(x, y, ...) {
  new_s2cd()
}

#' @export
vec_cast.s2cd.s2cd <- function(x, to, ...) {
  x
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
  s2cd_dates(x) |>
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
