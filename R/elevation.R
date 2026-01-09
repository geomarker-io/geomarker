#' Summarize nearby elevation
#'
#' Summarizes elevations within
#' the buffer distance of each s2 cell;
#' dates are not used to link to elevation data.
#'
#' Elevation data is downloaded from the PRISM Group at Oregon
#' State University as a digital elevation model in BIL format at an 800 m grid.
#' See <https://prism.oregonstate.edu/normals/> for more details.
#' @param x a s2_cell_dates object (see `?s2cd`)
#' @param fun function to summarize extracted values
#' @param buffer distance from s2 cell (in meters) to summarize data
#' @return numeric vector of elevation summaries
#' @export
#' @examples
#' get_elevation_summary(s2cd_example())
#' get_elevation_summary(s2cd_example(), stats::sd, 1400)
get_elevation_summary <- function(x, fun = stats::median, buffer = 800) {
  stopifnot(
    "x must be a s2_cell_dates object" = is_s2cd(x),
    "fun must be a function" = is.function(fun),
    "buffer must be numeric" = is.numeric(buffer),
    "buffer must be length one" = length(buffer) == 1,
    "buffer must be non-missing" = !is.na(buffer)
  )
  check_installed("terra", "to read elevation raster data.")
  check_installed("sf", "to buffer s2 cells.")
  elevation_raster <-
    geomarker_download_file(
      "https://prism.oregonstate.edu/downloads/data/PRISM_us_dem_800m_bil.zip"
    ) |>
    paste0("/vsizip/", url = _, "/PRISM_us_dem_800m_bil.bil") |>
    terra::rast()

  x_vect <-
    tibble::tibble(
      s2 = unique(s2::as_s2_cell(x)),
      s2_geography = s2::s2_buffer_cells(
        s2::s2_cell_to_lnglat(unique(s2::as_s2_cell(x))),
        distance = buffer
      )
    ) |>
    sf::st_as_sf() |>
    terra::vect() |>
    terra::project(elevation_raster)
  elevations <-
    terra::extract(elevation_raster, x_vect, fun = fun)$PRISM_us_dem_800m_bil |>
    as.list() |>
    stats::setNames(x_vect$s2)
  elevations[as.character(x)] |>
    as.numeric()
}
