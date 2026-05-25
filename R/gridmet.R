#' gridMET surface meteorological data
#'
#' Links daily, high spatial resolution (~4-km)
#' meteorological data based on the location of each s2_cell
#' and dates.
#'
#' Data is downloaded from the
#' [Climatology Lab](https://www.climatologylab.org/gridmet.html)
#' and is available for the contiguous US from 1979-yesterday.
#' Data for dates within the last 60 days are considered
#' preliminary and subject to change.
#' @param x a s2_cell_dates object (see `?s2cd`)
#' @param gridmet_var character; name of gridMET variable
#' @param ... passed to geomarker_download_file()
#' @return a list of numeric vectors of gridMET values
#' @export
#' @examples
#'  withr::local_envvar(
#'    R_USER_DATA_DIR = fs::path_package(
#'      "geomarker",
#'      "gmrkr--8841"
#'    ),
#'    R_GEOMARKER_NO_DOWNLOAD = "true"
#'  )
#' get_gridmet_data(s2cd_example_cincy(2L), "tmmx")
get_gridmet_data <- function(
  x,
  gridmet_var = c(
    "tmmx",
    "tmmn",
    "pr",
    "srad",
    "vs",
    "th",
    "rmax",
    "rmin",
    "sph"
  ),
  ...
) {
  stopifnot(
    "x must be a s2_cell_dates object" = is_s2cd(x),
    "all dates in x must be after 1979-01-01" = s2cd_within(
      x,
      date_range = c(as.Date("1979-01-01"), as.Date("9999-01-01"))
    )
  )
  gridmet_var <- match.arg(gridmet_var)
  check_installed("terra", "to read data from gridMET rasters")

  gridmet_urls <- sprintf(
    "https://www.northwestknowledge.net/metdata/data/%s_%s.nc",
    gridmet_var,
    s2cd_years(x)
  )

  gridmet_files <- vapply(
    gridmet_urls,
    geomarker_download_file,
    FUN.VALUE = character(1),
    ...
  )
  gridmet_rasters <- suppressWarnings(lapply(gridmet_files, terra::rast))
  gridmet_rasters <- mapply(
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
    gridmet_rasters,
    s2cd_years(x),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  gridmet_raster <- Reduce(c, gridmet_rasters)
  x_vect <-
    s2_cell_to_vect(x) |>
    terra::project(gridmet_raster)
  gridmet_cells <- terra::cells(gridmet_raster[[1]], x_vect)[, "cell"]
  xx <- as.data.frame(t(suppressWarnings(terra::extract(
    gridmet_raster,
    gridmet_cells
  ))))
  out <- mapply(
    function(.x, .y) xx[as.character(.y), .x],
    seq_len(ncol(xx)),
    s2cd_dates(x),
    SIMPLIFY = FALSE
  )
  stats::setNames(out, as.character(x))
}
