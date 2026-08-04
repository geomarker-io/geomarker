#' Summarize nearby elevation
#'
#' Summarizes elevations within
#' the buffer distance of each s2 cell;
#' dates are not used to link to elevation data.
#'
#' Elevation data come from a reproducible snapshot of the U.S. Geological
#' Survey 3D Elevation Program (3DEP) Bare Earth DEM Dynamic service. The
#' service snapshot, reflecting 3DEP data published through July 20, 2026, was
#' exported directly as a single-band signed 16-bit GeoTIFF in EPSG:5070 at an
#' 800 m grid and is distributed as a versioned geomarker release asset.
#' See <https://www.usgs.gov/3d-elevation-program/about-3dep-products-services>
#' and
#' <https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer>.
#' Data courtesy of the U.S. Geological Survey.
#' @param x a s2_cell_dates vector (see `?s2cd`)
#' @param fun function to summarize extracted values
#' @param buffer distance from s2 cell (in meters) to summarize data
#' @param ... passed to `geomarker_download_file()`. The `etag` argument is
#'   always set to `FALSE` for this versioned release asset.
#' @return numeric vector of elevation summaries
#' @export
#' @examples
#'  withr::local_envvar(
#'    R_USER_DATA_DIR = fs::path_package(
#'      "geomarker",
#'      "gmrkr--8841"
#'    ),
#'    R_GEOMARKER_NO_DOWNLOAD = "true"
#'  )
#' get_elevation_summary(s2cd_example())
#' get_elevation_summary(s2cd_example(), stats::sd, 1400)
get_elevation_summary <- function(
  x,
  fun = stats::median,
  buffer = 800,
  ...
) {
  stopifnot(
    "x must be a s2_cell_dates vector" = is_s2cd(x),
    "fun must be a function" = is.function(fun),
    "buffer must be numeric" = is.numeric(buffer),
    "buffer must be length one" = length(buffer) == 1,
    "buffer must be non-missing" = !is.na(buffer)
  )
  check_installed("terra", "to read elevation raster data.")
  check_installed("sf", "to buffer s2 cells.")
  elevation_url <- paste0(
    "https://github.com/geomarker-io/geomarker/releases/download/",
    "v0.0.1/usgs_3dep_conus_800m.tif"
  )
  download_args <- list(...)
  download_args$etag <- FALSE
  elevation_raster <- do.call(
    geomarker_download_file,
    c(list(url = elevation_url), download_args)
  ) |>
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
    terra::extract(
      elevation_raster,
      x_vect,
      fun = fun,
      ID = FALSE
    )[[1]] |>
    as.list() |>
    stats::setNames(x_vect$s2)
  elevations[as.character(x)] |>
    as.numeric()
}

install_elevation_geomarker_fixture <- function(
  cell,
  dates,
  output_dir,
  source_file = NULL
) {
  check_installed("terra", "to create elevation fixture data.")
  cell <- geomarker_fixture_cell(cell)
  geomarker_fixture_dates(dates)
  output_dir <- geomarker_fixture_output_dir(output_dir)
  elevation_url <- paste0(
    "https://github.com/geomarker-io/geomarker/releases/download/",
    "v0.0.1/usgs_3dep_conus_800m.tif"
  )
  if (is.null(source_file)) {
    source_file <- geomarker_download_file(elevation_url, etag = FALSE)
  }
  source_file |>
    terra::rast() |>
    geomarker_fixture_crop_to_cell(cell = cell) |>
    terra::writeRaster(
      file.path(output_dir, url_to_filename(elevation_url, etag = FALSE)),
      overwrite = TRUE,
      filetype = "GTiff",
      datatype = "INT2S",
      gdal = "COMPRESS=LZW"
    )
  invisible(output_dir)
}
