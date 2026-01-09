#' Example `s2cd` objects
#'
#' `s2cd_example()` returns example `s2cd` objects for use in examples
#' and vignettes.
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
      as.Date("2026-01-01"),
      c(as.Date("2024-09-13"), as.Date("2024-09-20"))
    )
  )
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
#' @param s2_cell_union a s2_cell_union object
#' @return logical
#' @export
#' @examples
#' # create compact representations of arbitrary polygons:
#' # s2::s2_covering_cell_ids_agg(
#' #   codec::cincy_city_geo(),
#' #   max_level = 12, max_cells = 100)
s2cd_within <- function(x, date_range = NULL, s2_cell_union = NULL) {
  if (!is.null(date_range)) {
    covers_dates <- s2cd_within_dates(x = x, date_range = date_range)
  }
  if (!is.null(s2_cell_union)) {
    covers_s2 <- s2cd_within_s2_cells(x = x, s2_cell_union = s2_cell_union)
  }
  if (is.null(date_range) && is.null(s2_cell_union)) {
    return(FALSE)
  }
  if (!is.null(date_range) && is.null(s2_cell_union)) {
    return(covers_dates)
  }
  if (is.null(date_range) && !is.null(s2_cell_union)) {
    return(covers_s2)
  }
  if (!is.null(date_range) && !is.null(s2_cell_union)) {
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

s2cd_within_s2_cells <- function(x, s2_cell_union) {}
