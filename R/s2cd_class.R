#' Create a new s2_cell_dates (`s2cd`) vector
#'
#' A `s2cd` vector (short for **s2_cell_dates**)
#' stores an `s2::s2_cell` vector alongside a list of date vectors,
#' where each vector of chronologically, non-missing dates corresponds to each
#' valid, level 30 s2 cell. Create a new `s2cd` vector with `s2cd()` or coerce
#' another object into a `s2cd` vector with `as_s2cd()`.
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
#' @param .data an s2_cell vector that is valid and is at level 30 resolution
#' @param dates a list of date vectors,
#' each in chronological order and without missing values
#' @param sort_dates logical; sort each date vector chronologically when needed.
#' This defaults to `FALSE` so accidentally unordered dates continue to error.
#' @return A `s2cd` vector
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
#' # check if d is a s2cd vector
#' is_s2cd(d)
#'
#' # use a s2cd vector as a tibble column
#' # the display includes each cell's number of dates
#' tibble::tibble(id = seq_along(d), location = d)
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
s2cd <- function(.data, dates = list(), sort_dates = FALSE) {
  .data <- s2::as_s2_cell(.data)
  validate_s2cd(.data, dates, sort_dates = sort_dates)
  unordered_dates <- !vapply(dates, is_non_decreasing, logical(1))
  if (any(unordered_dates)) {
    warning(
      "Some Date vectors were not in chronological order. ",
      "Because `sort_dates = TRUE`, `s2cd()` sorted each Date vector ",
      "chronologically before constructing the vector. ",
      "Silence this warning by sorting each date vector ahead of time with ",
      "`lapply(dates, sort)`.",
      call. = FALSE
    )
    dates <- lapply(dates, sort)
  }
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

validate_s2cd <- function(.data, dates, sort_dates = FALSE) {
  if (!is.logical(sort_dates) || length(sort_dates) != 1 || is.na(sort_dates)) {
    stop("`sort_dates` must be TRUE or FALSE.", call. = FALSE)
  }
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
  if (!sort_dates && !all(vapply(dates, is_non_decreasing, logical(1)))) {
    stop(
      "each Date vector must be in chronological order. ",
      "Set `sort_dates = TRUE` to sort automatically.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' @export
format.s2cd <- function(x, ...) {
  as.character(s2cd_s2_cell(x))
}

#' @importFrom s2 as_s2_cell
#' @importFrom vctrs vec_cast vec_ptype2 vec_ptype_abbr vec_ptype_full
NULL

#' @export
as_s2_cell.s2cd <- function(x, ...) {
  s2cd_s2_cell(x)
}

#' @export
print.s2cd <- function(x, ...) {
  cat(sprintf("<s2_cell_dates[%d]>\n", length(x)))
  print(
    paste0(
      as.character(s2cd_s2_cell(x)),
      "[",
      lengths(s2cd_dates(x)),
      "]"
    ),
    quote = FALSE
  )
  invisible(x)
}

#' @exportS3Method pillar::pillar_shaft
pillar_shaft.s2cd <- function(x, ...) {
  pillar::new_pillar_shaft_simple(
    paste0(
      format(x),
      "[",
      lengths(s2cd_dates(x)),
      "]"
    )
  )
}

#' Test whether input is a `s2cd` vector
#'
#' Tests if x inherits from class `s2cd`
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
