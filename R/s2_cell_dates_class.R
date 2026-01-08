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

# s2dates extends the s2::s2_cell class with a list of date vectors,
# where each vector of chronologically, non-missing dates corresponds
# to each (valid, level 30) s2 cell
# add the estimated meter resolution on level 30 cell and describe more as a point
s2_cell_dates <- S7::new_class(
  "s2_cell_dates",
  parent = S3_s2_cell,
  properties = list(dates = S7::class_list),
  validator = function(self) {
    if (!inherits(self, "s2_cell")) {
      "`s2dates` must extend an `s2_cell` vector."
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

#' @importFrom s2 as_s2_cell
NULL

S7::method(as_s2_cell, s2_cell_dates) <- function(x, ...) {
  s2::new_s2_cell(S7::S7_data(x))
}

S7::method(print, s2_cell_dates) <- function(x, ...) {
  cat(sprintf("<s2_cell_dates[%d]>", length(x)))
  the_s2 <- structure(x, class = c("s2_cell", "wk_vctr"))
  cat("\n<s2>\n")
  print(as.character(the_s2))
  print(x@dates)
}

#' @export
as.data.frame.s2_cell_dates <- function(x, ...) {
  data.frame(
    s2_cell = structure(x, class = c("s2_cell", "wk_vctr")),
    dates = I(x@dates),
    row.names = NULL,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' @export
if (requireNamespace("tibble", quietly = TRUE)) {
  as_tibble.s2_cell_dates <- function(x, ...) {
    tibble::tibble(
      s2_cell = structure(x, class = c("s2_cell", "wk_vctr")),
      dates = x@dates
    )
  }
}
