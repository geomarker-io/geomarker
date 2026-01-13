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
#' @param gridmet_year character; year of gridMET file
#' @return a list of numeric vectors of gridMET values
#' @export
#' @examples
#' get_gridmet_data(s2cd_example(), "tmmx")
#' get_gridmet_data(s2cd_example(), "pr")
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
  )
) {
  stopifnot(
    "x must be a s2_cell_dates object" = is_s2cd(x),
    "all dates in x must be after 1979-01-01" = s2cd_within(
      x,
      date_range = c(as.Date("1979-01-01"), as.Date("9999-01-01"))
    )
  )
  gridmet_var <- rlang::arg_match(gridmet_var)

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

  gridmet_raster <-
    purrr::map_chr(gridmet_urls, geomarker_download_file) |>
    purrr::map(terra::rast) |>
    purrr::map2(gridmet_years, \(.x, .y) {
      stats::setNames(
        .x,
        seq(
          as.Date(glue::glue("{.y}-01-01")),
          as.Date(glue::glue("{.y}-12-31")),
          by = 1
        )[1:terra::nlyr(.x)]
      )
    }) |>
    purrr::reduce(c)
  x_vect <-
    s2::s2_cell_to_lnglat(s2::as_s2_cell(x)) |>
    as.data.frame() |>
    terra::vect(geom = c("x", "y"), crs = "+proj=longlat +datum=WGS84") |>
    terra::project(gridmet_raster)
  gridmet_cells <- terra::cells(gridmet_raster[[1]], x_vect)[, "cell"]
  xx <- as.data.frame(t(terra::extract(gridmet_raster, gridmet_cells)))
  purrr::map2(seq_len(ncol(xx)), x@dates, \(.x, .y) xx[as.character(.y), .x]) |>
    stats::setNames(as.character(x))
}
