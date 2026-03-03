#' Get wildfire smoke plume data
#'
#' NOAA's Hazard Mapping System (HMS) operates daily in near real-time by outlining
#' the smoke polygon of each distinct smoke plume and classifying it as "Heavy",
#' "Light", or "Medium" based on its apparent thickness.
#' @param x Date vector; files are organized and retrieved by date for all of CONUS
#' @param ... passed to `geomarker_download_file()`
#' @returns a list of daily tibbles, each with columns for geometry and density
#' @details Daily files for HMS smoke data are used again instead of
#' re-downloading, unless there is an updated version of the daily data
#' available. For more details, see <https://www.ospo.noaa.gov/products/land/hms.html#about>.
#' @export
#' @examples
#'  withr::local_envvar(
#'    R_USER_DATA_DIR = fs::path_package(
#'      "geomarker",
#'      "gmrkr--8841"
#'    ),
#'    R_GEOMARKER_NO_DOWNLOAD = "true"
#'  )
#' get_daily_smoke_data(as.Date(c("2024-02-09", "2024-06-10", "2024-06-11", "2024-06-12")))
get_daily_smoke_data <- function(x, ...) {
  stopifnot("x must be a Date vector" = inherits(x, "Date"))
  if (length(x) == 0) {
    stop("length zero dates detected!")
  }

  smoke_url <-
    sprintf(
      "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/%s/%s/hms_smoke%s.zip",
      format(x, "%Y"),
      format(x, "%m"),
      format(x, "%Y%m%d")
    )

  # if (cloud) {
  #   smoke_file <- paste0("/vsizip//vsicurl/", smoke_url)
  # } else {
  smoke_file <- vapply(
    smoke_url,
    geomarker_download_file,
    FUN.VALUE = character(1),
    subdir = "hms",
    ...
  ) |>
    paste0("/vsizip/", the_files = _)
  # }
  out <- lapply(smoke_file, \(.) {
    xx <- sf::st_read(., quiet = TRUE)
    tibble::tibble(
      geometry = sf::st_as_s2(xx$geometry, rebuild = TRUE),
      density = factor(
        xx$Density,
        levels = c("None", "Light", "Medium", "Heavy"),
        ordered = TRUE
      )
    )
  }) |>
    stats::setNames(x)
  return(out)
}

#' Summarize wildfire smoke plume exposures
#'
#' For each s2_cell location and Date vector, the intersections with
#' NOAA's HMS daily smoke polygons (see `?get_daily_smoke_data`) are calculated
#' and summarized as the maximum intensity ("Light", "Medium", "Heavy").
#' If no smoke polygons are intersected, "None" is used to summarize the maximum
#' intensity.
#' @param x a s2_cell_dates object (see `?s2cd`)
#' @param ... passed to `get_daily_smoke_data()
#' (and on to `geomarker_download_file()`)
#' @returns a list of ordered factors (Levels: None > Light > Medium > Heavy)
#' @export
#' @examples
#'  withr::local_envvar(
#'    R_USER_DATA_DIR = fs::path_package(
#'      "geomarker",
#'      "gmrkr--8841"
#'    ),
#'    R_GEOMARKER_NO_DOWNLOAD = "true"
#'  )
#' s2cd(s2::as_s2_cell(c("8841b39a7c46e25f","8841a45555555555")),
#'   dates = list(as.Date(c("2024-05-18", "2024-11-06")),
#'                as.Date(c("2024-06-22", "2024-08-15", "2024-12-30")))
#' ) |>
#'   get_smoke_summary()
get_smoke_summary <- function(x, ...) {
  stopifnot("x must be a s2_cell_dates object" = is_s2cd(x))
  dsd <- get_daily_smoke_data(as.Date(unique(unlist(x@dates))), ...)
  lapply(seq_along(x), \(i) {
    lapply(dsd[as.character(x@dates[[i]])], \(.) {
      safe_max_factor(
        .[
          s2::s2_intersects(s2::s2_cell_center(x[i]), .$geometry),
          "density",
          drop = TRUE
        ]
      )
    }) |>
      do.call(c, args = _)
  })
}


safe_max_factor <- function(x) {
  if (length(x) == 0L || all(is.na(x))) {
    return(factor(
      "None",
      c("None", "Light", "Medium", "Heavy"),
      ordered = TRUE
    ))
  }
  max(x, na.rm = TRUE)
}
