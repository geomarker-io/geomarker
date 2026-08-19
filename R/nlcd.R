#' Summarize nearby imperviousness
#'
#' Summarizes fraction imperviousness (fractional surface area
#' covered with artificial substrate or structures)
#' within the buffer distance of each s2 cell;
#' the year of each date is used to link to the corresponding
#' annual snapshot.
#'
#' Fractional Impervious Surface rasters from Annual NLCD Collection 1.2 are
#' retrieved as full conterminous U.S. GeoTIFFs from the MRLC direct-download
#' bundles at a 30 m grid.
#' The currently available conterminous U.S. data span 1985 through 2025; dates
#' outside the available years return `NA_real_` values.
#' See <https://www.usgs.gov/centers/eros/science/annual-nlcd-data-access> for
#' data-access details and
#' <https://www.usgs.gov/centers/eros/science/usgs-eros-archive-land-cover-annual-nlcd-collection-1-fractional-impervious>
#' for product details.
#' @param x a s2_cell_dates vector (see `?s2cd`)
#' @param fun function to summarize extracted data
#' @param buffer distance from s2 cell (in meters) to summarize data
#' @param ... passed to [stow::stow()]. The `package` and `subdir` arguments
#'   are fixed by geomarker.
#' @return a list of numeric vectors of NLCD fraction imperviousness summaries.
#' Dates in unavailable years return `NA_real_`.
#' @export
#' @examples
#' get_nlcd_fct_imp_data(s2cd_example_cincy(2L))
get_nlcd_fct_imp_data <- function(
  x,
  fun = mean,
  buffer = 800,
  ...
) {
  stopifnot("x must be a s2_cell_dates vector" = is_s2cd(x))

  available_years <- as.character(1985:2025)
  date_years <- lapply(s2cd_dates(x), format, "%Y")
  out <- lapply(s2cd_dates(x), \(dates) rep(NA_real_, length(dates)))
  years <- intersect(
    unique(unlist(date_years, use.names = FALSE)),
    available_years
  )
  if (length(years) == 0) {
    return(out)
  }

  check_installed("terra", "to summarize Annual NLCD data.")
  x_vect <- s2_cell_to_vect(x)

  for (year in years) {
    location_index <- which(vapply(
      date_years,
      \(years) year %in% years,
      logical(1)
    ))
    nlcd_file <- geomarker_stow(
      paste0(
        "https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/",
        "Annual_NLCD_FctImp_",
        year,
        "_CU_C1V2.zip"
      ),
      "get_nlcd_fct_imp_data",
      ...
    )
    nlcd_raster <- terra::rast(paste0(
      "/vsizip/",
      nlcd_file,
      "/",
      "Annual_NLCD_FctImp_",
      year,
      "_CU_C1V2.tif"
    ))
    x_buffer <-
      terra::project(x_vect, nlcd_raster) |>
      terra::buffer(width = buffer)
    summaries <- terra::extract(
      nlcd_raster,
      x_buffer[location_index, ],
      fun = fun,
      ID = FALSE
    )[[1]]
    for (i in seq_along(location_index)) {
      index <- location_index[[i]]
      out[[index]][date_years[[index]] == year] <- summaries[[i]]
    }
  }

  out
}

install_nlcd_geomarker_fixture <- function(
  cell,
  dates,
  output_dir,
  buffer = 800
) {
  check_installed("terra", "to create Annual NLCD fixture data.")
  cell <- geomarker_fixture_cell(cell)
  buffer <- geomarker_fixture_buffer(buffer)
  years <- intersect(geomarker_fixture_years(dates), as.character(1985:2025))
  output_dir <- geomarker_fixture_output_dir(output_dir)
  fixture_dir <- geomarker_fixture_stow_dir(
    output_dir,
    "get_nlcd_fct_imp_data"
  )

  lapply(years, \(year) {
    filename <- paste0("Annual_NLCD_FctImp_", year, "_CU_C1V2")
    url <- paste0(
      "https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/",
      filename,
      ".zip"
    )
    tif <- file.path(tempdir(), paste0(filename, ".tif"))
    zip <- tempfile(fileext = ".zip")
    on.exit(unlink(c(tif, zip)), add = TRUE)

    geomarker_stow(
      url,
      "get_nlcd_fct_imp_data",
      quiet = TRUE,
      .etag = FALSE
    ) |>
      paste0("/vsizip/", url = _, "/", filename, ".tif") |>
      terra::rast() |>
      geomarker_fixture_crop_to_cell(cell = cell, buffer = buffer) |>
      terra::writeRaster(
        tif,
        filetype = "GTiff",
        datatype = "INT1U",
        overwrite = TRUE
      )
    utils::zip(zipfile = zip, files = tif, flags = "-j")
    file.copy(
      zip,
      file.path(fixture_dir, geomarker_stow_filename(url)),
      overwrite = TRUE
    )
  })

  invisible(output_dir)
}
