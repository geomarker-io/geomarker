# parent S3 class for S7
#' @export
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

# s2dates extends the s2::s2_cell class with a list of date vectors,
# where each vector of chronologically, non-missing dates corresponds
# to each (valid) s2 cell
s2_cell_dates <- S7::new_class(
  "s2_cell_dates",
  parent = S3_s2_cell,
  properties = list(dates = S7::class_list),
  validator = function(self) {
    if (!inherits(self, "s2_cell")) {
      "`s2dates` must extend an `s2_cell` vector."
    }
    if (!all(s2::s2_cell_is_valid(self))) {
      "s2 cells are invalid"
    }
    if (!is.list(self@dates)) {
      "`dates` must be a list."
    }
    if (!length(self@dates) == length(self)) {
      "`dates` must have same length as `cells`."
    }
    for (i in seq_along(self@dates)) {
      di <- self@dates[[i]]
      if (!inherits(di, "Date")) {
        sprintf("`dates[[%d]]` must be a Date vector.", i)
      }
      if (anyNA(di)) {
        sprintf("`dates[[%d]]` must not contain missing values.", i)
      }
      if (!is_non_decreasing(di)) {
        sprintf("`dates[[%d]]` must be in chronological order.", i)
      }
    }
  }
)

#' @importFrom s2 as_s2_cell
NULL

S7::method(as_s2_cell, s2_cell_dates) <- function(x, ...) {
  s2::new_s2_cell(S7::S7_data(x))
}

# format method?

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
