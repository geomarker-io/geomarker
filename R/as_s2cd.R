#' Convert other objects into a s2_cell_dates (`s2cd`) vector
#'
#' Convert other R objects into s2cd vectors
#' @section Methods implemented for:
#' - `data.frame`: must have column "s2_cell" and list-column "dates"
#' - `s2cd`: returned as-is
#' @param x an object to convert
#' @param ... passed to methods; for the `data.frame` method, passed to
#'   `s2cd()`
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
  s2cd(.data = x$s2_cell, dates = x$dates, ...)
}

#' @export
as_s2cd.s2cd <- function(x, ...) x

#' @export
as_s2cd.default <- function(x, ...) {
  stop(
    "can't convert object of class `",
    paste(class(x), collapse = "/"),
    "` to a s2cd vector",
    call. = FALSE
  )
}

# TODO add method to create s2cd vector given data.frame with s2_cell, start_date, and end_date (and id??? or do we store one s2cd vector per person??)

# then, show examples how to use impute_date_ranges to go from address history (or address reported dates) to imputed address history and then use new function to add to convert to s2cd vector
