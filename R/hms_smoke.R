#' Get wildfire smoke plume data
#'
#' NOAA's Hazard Mapping System (HMS) operates daily in near real-time by outlining
#' the smoke polygon of each distinct smoke plume and classifying it as "Heavy",
#' "Light", or "Medium" based on its apparent thickness.
#' @param x Date vector; files are organized and retrieved by date for all of CONUS
#' @param ... passed to [stow::stow()]. The `package` and `subdir` arguments
#'   are fixed by geomarker.
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
  smoke_file <- vapply(smoke_url, function(url) {
    geomarker_stow(url, "get_daily_smoke_data", ...)
  }, character(1)) |>
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
#' @param x a s2_cell_dates vector (see `?s2cd`)
#' @param ... passed to `get_daily_smoke_data()` and then [stow::stow()].
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
  stopifnot("x must be a s2_cell_dates vector" = is_s2cd(x))
  x_dates <- s2cd_dates(x)
  dsd <- get_daily_smoke_data(as.Date(unique(unlist(x_dates))), ...)
  lapply(seq_along(x), \(i) {
    lapply(dsd[as.character(x_dates[[i]])], \(.) {
      safe_max_factor(
        .[
          s2::s2_intersects(
            s2::s2_cell_center(s2::as_s2_cell(x[i])),
            .$geometry
          ),
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

install_hms_smoke_geomarker_fixture <- function(cell, dates, output_dir) {
  check_installed("sf", "to create HMS smoke fixture data.")
  check_installed("terra", "to create HMS smoke fixture data.")
  cell <- geomarker_fixture_cell(cell)
  dates <- geomarker_fixture_dates(dates)
  output_dir <- geomarker_fixture_output_dir(output_dir)
  hms_dir <- geomarker_fixture_cache_dir(
    output_dir,
    "get_daily_smoke_data"
  )

  lapply(dates, \(date) {
    stopifnot(inherits(date, "Date"))
    cat("\rprocessing HMS smoke daily files: ", format(date, "%Y-%m-%d"))
    smoke_url <-
      sprintf(
        "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/%s/%s/hms_smoke%s.zip",
        format(date, "%Y"),
        format(date, "%m"),
        format(date, "%Y%m%d")
      )
    dest_path <- file.path(
      hms_dir,
      geomarker_stow_filename(smoke_url)
    ) |>
      tools::file_path_sans_ext() |>
      paste0(path_sans_ext = _, ".shz")
    final_path <- paste0(tools::file_path_sans_ext(dest_path), ".zip")
    unlink(c(dest_path, final_path), recursive = TRUE, force = TRUE)
    paste0(
      "/vsizip/",
      geomarker_stow(
        smoke_url,
        "get_daily_smoke_data",
        quiet = TRUE,
        .etag = FALSE
      )
    ) |>
      sf::read_sf(quiet = TRUE) |>
      terra::vect() |>
      geomarker_fixture_crop_to_cell(cell = cell) |>
      sf::st_as_sf() |>
      sf::st_write(dest_path, driver = "Esri Shapefile", quiet = TRUE)
    file.rename(dest_path, final_path)
    invisible(NULL)
  }) |>
    invisible()
  utils::flush.console()
  cat("\n")
  invisible(output_dir)
}
