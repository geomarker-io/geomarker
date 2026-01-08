#' Are all dates within a date range?
#' @param x an s2_cell_dates object
#' @param date_range length two Date vector representing a minimum and maximum allowable date
#' @export
is_within_date_range <- function(x, date_range) {
  stopifnot(
    "x must be a s2_cell_dates object" = inherits(
      x,
      "geomarker::s2_cell_dates"
    ),
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

#' Are all s2 cells located within another set of s2_cells?
#' @param x an s2_cell_dates object
#' @param s2_cells a collection of s2_cells
#' @export
#' @examples
#' # create compact representations of arbitrary polygons:
#' s2::s2_covering_cell_ids_agg(
#'   codec::cincy_city_geo(),
#'   max_level = 12, max_cells = 100)
#'
# is_within_s2_cells <- function(x, s2_cells) {}
