#' Impute date ranges from a chronological sequence of dates
#'
#' In observational data, the date associated with an address
#' is often the date of the observation instead of the date of
#' a change in address.
#' For each interior observation, the imputed start date is the midpoint
#' between that observation date and the previous observation date.
#' The imputed end date is the next imputed start date.
#' The first start date and last end date are optionally extended by
#' `start_early` and `end_late`.
#' @param x a chronologically sorted Date vector
#' @param start_early start the first imputed date range this many days early
#' as a length-one integer
#' @param end_late end the last imputed date range this many days late
#' as a length-one integer
#' @param expand logical; if `TRUE`, return a list of daily Date sequences,
#' one for each input date, instead of the default `start` and `end` vectors.
#' @returns a list of `start` and `end` Date vectors for the imputed ranges
#' for each input date in x, or, if `expand = TRUE`, a list of Date vectors
#' where each element is the daily sequence from the imputed start date to the
#' imputed end date.
#' @details Use this function to impute an effective date range for a set of
#' addresses or location identifiers that were collected on unrelated days.
#' For example, residential addresses collected during a specific healthcare
#' encounter do not reflect when a patient actually changed addresses.
#' Imputing address date ranges can reduce reliance on observation dates
#' alone and may help avoid differential assumptions about when address
#' changes occurred.
#' @export
#' @examples
#' impute_date_ranges(as.Date(c("2024-01-01", "2024-03-17", "2024-09-21")))
#'
#' impute_date_ranges(as.Date(c("2024-01-01", "2024-03-17", "2024-09-21")),
#'   start_early = 30L, end_late = 60L
#' )
#'
#' impute_date_ranges(
#'   as.Date(c("2024-01-01", "2024-03-17", "2024-09-21")),
#'   expand = TRUE
#' )
#'
#' # use within a data.frame with multiple individuals
#' tibble::tibble(
#'   id = rep(c("A", "B"), each = 3),
#'   encounter = rep(1:3, 2),
#'   date = as.Date(c(
#'     "2024-01-01", "2024-03-17", "2024-09-21",
#'     "2023-11-29", "2024-09-22", "2024-09-29"
#'   ))
#' ) |>
#'   dplyr::arrange(id, date) |>
#'   dplyr::mutate(
#'     imputed_start_date = impute_date_ranges(date)$start,
#'     imputed_end_date = impute_date_ranges(date)$end,
#'     .by = "id"
#'   )
#'
#' # expand to daily dates for use with s2cd()
#' s2cd(
#'   s2::as_s2_cell(rep("8841b39a7c46e25f", 3)),
#'   dates = impute_date_ranges(
#'     as.Date(c("2024-01-01", "2024-03-17", "2024-09-21")),
#'     expand = TRUE
#'   )
#' )
impute_date_ranges <- function(
  x,
  start_early = 0L,
  end_late = 0L,
  expand = FALSE
) {
  stopifnot(
    "`x` must be a Date vector" = inherits(x, "Date"),
    "`x` must contain at least one date" = length(x) > 0L,
    "`x` must not contain missing values" = !anyNA(x),
    "`start_early` must be a length-one integer" = is.integer(start_early) &&
      length(start_early) == 1L &&
      !is.na(start_early),
    "`end_late` must be a length-one integer" = is.integer(end_late) &&
      length(end_late) == 1L &&
      !is.na(end_late),
    "`expand` must be TRUE or FALSE" = is.logical(expand) &&
      length(expand) == 1L &&
      !is.na(expand),
    "date vectors must be ordered chronologically" = is_non_decreasing(x)
  )
  if (length(x) == 1) {
    out <- list("start" = x - start_early, "end" = x + end_late)
    if (expand) {
      out <- Map(seq.Date, out$start, out$end, MoreArgs = list(by = "day"))
    }
    return(out)
  }
  x_ts <- stats::ts(as.numeric(x))
  lag_diff <- as.difftime(
    as.numeric(stats::lag(x_ts, -1) - x_ts),
    units = "days"
  )
  i_start <- x
  i_start[-1] <- x[-1] + (lag_diff / 2)
  i_start[1] <- x[1] - start_early
  i_end <- c(i_start[-1], x[length(x)] + end_late)
  out <- list("start" = i_start, "end" = i_end)
  if (expand) {
    out <- Map(seq.Date, out$start, out$end, MoreArgs = list(by = "day"))
  }
  return(out)
}
