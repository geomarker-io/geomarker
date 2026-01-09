check_installed <- function(pkg, reason = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- sprintf("The package `%s` is required", pkg)
    if (!is.null(reason)) {
      msg <- paste0(msg, " ", reason)
    }
    msg <- paste0(msg, ". Please install it first.")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

is_non_decreasing <- function(x) {
  if (length(x) <= 1) {
    return(TRUE)
  }
  all(diff(x) >= 0)
}
