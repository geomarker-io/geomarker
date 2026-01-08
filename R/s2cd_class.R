S3_s2_cell <- S7::new_S3_class(
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

is_non_decreasing <- function(x) {
  if (length(x) <= 1) {
    return(TRUE)
  }
  all(diff(x) >= 0)
}

#' s2_cell_dates (s2cd) object
#'
#' An s2cd object (short for **s2_cell_dates**)
#' extends the s2::s2_cell class with a list of date vectors,
#' where each vector of chronologically, non-missing dates corresponds
#' to each (valid, level 30) s2 cell.
#' At level 30, S2 cells have an approximate spatial
#' resolution of *1 centimeter*; in this context,
#' they are intended to function as point-like
#' representations rather than true spatial areas.
#'
#' @param .data an s2_cell object that is valid and is at level 30 resolution
#' @param dates a list of date vectors, each in chronological order and without missing values
#' @param ... reserved for future extensions
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
#' my_d <-
#'   data.frame(
#'     s2_cell = s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")),
#'     dates = I(list(
#'       as.Date("2026-01-01"),
#'       c(as.Date("2024-09-13"), as.Date("2024-09-20"))
#'     ))
#'  ) |>
#'   as_s2cd()
s2cd <- S7::new_class(
  "s2cd",
  parent = S3_s2_cell,
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

#' @rdname s2cd
#' @export
is_s2cd <- function(x) {
  inherits(x, "s2cd")
}


#' @rdname s2cd
#' @param x an object of class s2cd or a data.frame with
#' a column called "s2_cell" containing an s2_cell vector
#' and a column called "dates" containing a list of Date vectors
#' @export
as_s2cd <- S7::new_generic("as_s2cd", dispatch_args = "x")

S7::method(as_s2cd, S7::new_S3_class("data.frame")) <- function(x, ...) {
  stopifnot(
    "data.frame must have column named `s2_cell`" = "s2_cell" %in% names(x),
    "data.frame must have column named `dates`" = "dates" %in% names(x)
  )
  s2cd(.data = x$s2_cell, dates = x$dates)
}

S7::method(as_s2cd, s2cd) <- function(x, ...) x


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

# #' @export
# if (requireNamespace("tibble", quietly = TRUE)) {
#   as_tibble.s2cd <- function(x, ...) {
#     tibble::tibble(
#       s2_cell = structure(x, class = c("s2_cell", "wk_vctr")),
#       dates = x@dates
#     )
#   }
# }
