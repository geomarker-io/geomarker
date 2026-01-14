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
#' @param overwrite logical; overwrite files if already downloaded?
#' @param quiet logical; show download progress messages?
#' @return a list of numeric vectors of gridMET values
#' @export
#' @examples
#' get_gridmet_data(s2cd_example(), "tmmx")
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
  overwrite = FALSE,
  quiet = FALSE
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

  gridmet_years <-
    x@dates |>
    do.call(c, args = _) |>
    format("%Y") |>
    unique() |>
    sort()

  gridmet_urls <- sprintf(
    "https://www.northwestknowledge.net/metdata/data/%s_%s.nc",
    gridmet_var,
    gridmet_years
  )

  gridmet_files <- vapply(
    gridmet_urls,
    geomarker_download_file,
    overwrite = overwrite,
    quiet = quiet,
    FUN.VALUE = character(1)
  )
  gridmet_rasters <- lapply(gridmet_files, terra::rast)
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
    gridmet_years,
    SIMPLIFY = FALSE
  )
  gridmet_raster <- Reduce(c, gridmet_rasters)
  x_vect <-
    s2::s2_cell_to_lnglat(s2::as_s2_cell(x)) |>
    as.data.frame() |>
    terra::vect(geom = c("x", "y"), crs = "+proj=longlat +datum=WGS84") |>
    terra::project(gridmet_raster)
  gridmet_cells <- terra::cells(gridmet_raster[[1]], x_vect)[, "cell"]
  xx <- as.data.frame(t(terra::extract(gridmet_raster, gridmet_cells)))
  out <- mapply(
    function(.x, .y) xx[as.character(.y), .x],
    seq_len(ncol(xx)),
    x@dates,
    SIMPLIFY = FALSE
  )
  stats::setNames(out, as.character(x))
}
