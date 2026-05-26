s2cd_s2_cell <- function(x) {
  stopifnot("x must be a s2_cell_dates vector" = is_s2cd(x))
  vctrs::field(x, "s2_cell")
}

#' Get date vectors from a s2_cell_dates (`s2cd`) vector
#'
#' @param x a s2_cell_dates vector (see `?s2cd`)
#' @returns a list of Date vectors
#' @export
#' @examples
#' s2cd_dates(s2cd_example())
s2cd_dates <- function(x) {
  stopifnot("x must be a s2_cell_dates vector" = is_s2cd(x))
  as.list(vctrs::field(x, "dates"))
}

#' Get unique years from s2cd vector
#'
#' @param x a s2_cell_dates vector (see `?s2cd`)
#' @returns a sorted character vector of years present in x
#' @export
#' @examples
#' s2cd_years(s2cd_example())
s2cd_years <- function(x) {
  stopifnot(
    "x must be a s2_cell_dates vector" = is_s2cd(x)
  )
  s2cd_dates(x) |>
    do.call(c, args = _) |>
    format("%Y") |>
    unique() |>
    sort()
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
