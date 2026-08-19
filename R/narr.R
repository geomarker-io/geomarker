#' NARR surface meteorological data
#'
#' Links daily, 0.3 degrees gridded (~ 32 sq km) meteorological monolevel data
#' based on the location of each s2_cell and dates.
#'
#' Data is downloaded from the
#' [NCEP North American Regional Reanalysis](https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/)
#' project page and is available for the entire US from 1979 - near present.
#' Data is updated monthly.
#' @param x a s2_cell_dates vector (see `?s2cd`)
#' @param narr_var character; name of NARR variable
#' @param ... passed to [stow::stow()]. The `package` and `subdir` arguments
#'   are fixed by geomarker.
#' @return a list of numeric vectors of NARR values
#' @export
#' @examples
#'  withr::local_envvar(
#'    R_USER_DATA_DIR = fs::path_package(
#'      "geomarker",
#'      "gmrkr--8841"
#'    ),
#'    R_GEOMARKER_NO_DOWNLOAD = "true"
#'  )
#' get_narr_data(s2cd_example_cincy(2L), "air.2m")
get_narr_data <- function(
  x,
  narr_var = c(
    "air.2m",
    "hpbl",
    "acpcp",
    "rhum.2m",
    "vis",
    "pres.sfc",
    "uwnd.10m",
    "vwnd.10m"
  ),
  ...
) {
  stopifnot(
    "x must be a s2_cell_dates vector" = is_s2cd(x),
    "all dates in x must be after 1979-01-01" = s2cd_within(
      x,
      date_range = c(as.Date("1979-01-01"), as.Date("9999-01-01"))
    )
  )
  narr_var <- match.arg(narr_var)
  check_installed("terra", "to read data from NARR rasters")

  narr_urls <- sprintf(
    "https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/%s.%s.nc",
    narr_var,
    s2cd_years(x)
  )

  narr_files <- vapply(
    narr_urls,
    function(url) {
      geomarker_stow(url, "get_narr_data", ...)
    },
    character(1)
  )
  narr_rasters <- suppressWarnings(lapply(narr_files, terra::rast))
  narr_rasters <- mapply(
    function(.x, .y) {
      stats::setNames(
        .x,
        seq(
          as.Date(sprintf("%s-01-01", .y)),
          as.Date(sprintf("%s-12-31", .y)),
          by = 1
        )[1:terra::nlyr(.x)]
      )
    },
    narr_rasters,
    s2cd_years(x),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  narr_raster <- Reduce(c, narr_rasters)
  x_vect <-
    s2_cell_to_vect(x) |>
    terra::project(narr_raster)
  narr_cells <- terra::cells(narr_raster[[1]], x_vect)[, "cell"]
  xx <- as.data.frame(t(suppressWarnings(terra::extract(
    narr_raster,
    narr_cells
  ))))
  out <- mapply(
    function(.x, .y) xx[as.character(.y), .x],
    seq_len(ncol(xx)),
    s2cd_dates(x),
    SIMPLIFY = FALSE
  )
  stats::setNames(out, as.character(x))
}

install_narr_geomarker_fixture <- function(
  cell,
  dates,
  output_dir,
  narr_var = "air.2m"
) {
  check_installed("terra", "to create NARR fixture data.")
  cell <- geomarker_fixture_cell(cell)
  years <- geomarker_fixture_years(dates)
  output_dir <- geomarker_fixture_output_dir(output_dir)
  narr_urls <- sprintf(
    "https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/%s.%s.nc",
    narr_var,
    years
  )
  fixture_dir <- geomarker_fixture_cache_dir(output_dir, "get_narr_data")

  lapply(narr_urls, \(url) {
    geomarker_stow(
      url,
      "get_narr_data",
      quiet = TRUE,
      .etag = FALSE
    ) |>
      terra::rast() |>
      geomarker_fixture_crop_to_cell(cell = cell) |>
      terra::writeCDF(
        file.path(fixture_dir, geomarker_stow_filename(url)),
        overwrite = TRUE
      )
  }) |>
    invisible()
  invisible(output_dir)
}
