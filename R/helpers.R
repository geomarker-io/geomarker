is_non_decreasing <- function(x) {
  if (length(x) <= 1) {
    return(TRUE)
  }
  all(diff(x) >= 0)
}
