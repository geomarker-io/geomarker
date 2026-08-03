traffic_data_manifest <- function() {
  manifest_path <- system.file("traffic-data.dcf", package = "geomarker")
  if (!nzchar(manifest_path)) {
    manifest_path <- file.path("inst", "traffic-data.dcf")
  }
  manifest <- as.list(read.dcf(manifest_path)[1, ])
  required <- c(
    "HPMS-Year",
    "HPMS-Source-Item-ID",
    "Transformation-ID",
    "Package-Release",
    "Asset-Name",
    "Asset-URL",
    "Asset-Bytes",
    "Asset-SHA256",
    "Asset-Rows",
    "Asset-Negative-Passenger-Rows",
    "Asset-Layer",
    "Asset-CRS",
    "Asset-Schema"
  )
  stopifnot(
    "traffic data manifest is incomplete" = all(required %in% names(manifest)),
    "traffic data manifest must describe HPMS 2024" = identical(
      manifest[["HPMS-Year"]],
      "2024"
    )
  )
  manifest
}

traffic_data_url <- function() {
  traffic_data_manifest()[["Asset-URL"]]
}

traffic_validate_download <- function(
  path,
  manifest = traffic_data_manifest()
) {
  expected_bytes <- as.numeric(manifest[["Asset-Bytes"]])
  actual_bytes <- unname(file.info(path)$size)
  if (is.na(actual_bytes) || actual_bytes != expected_bytes) {
    stop(
      "Downloaded traffic data has an unexpected size.\n",
      "Expected: ",
      format(expected_bytes, scientific = FALSE),
      " bytes\n",
      "Observed: ",
      format(actual_bytes, scientific = FALSE),
      " bytes\n",
      "File: ",
      path,
      call. = FALSE
    )
  }
  actual_sha256 <- digest::digest(
    file = path,
    algo = "sha256",
    serialize = FALSE
  )
  expected_sha256 <- manifest[["Asset-SHA256"]]
  if (!identical(actual_sha256, expected_sha256)) {
    stop(
      "Downloaded traffic data failed SHA-256 validation.\n",
      "Expected: ",
      expected_sha256,
      "\n",
      "Observed: ",
      actual_sha256,
      "\n",
      "File: ",
      path,
      call. = FALSE
    )
  }
  invisible(path)
}

traffic_data_file <- function(overwrite = FALSE, quiet = FALSE) {
  url <- traffic_data_url()
  dest <- file.path(
    geomarker_data_dir(),
    url_to_filename(url, etag = FALSE)
  )
  needs_full_validation <- overwrite || !file.exists(dest)
  path <- geomarker_download_file(
    url,
    overwrite = overwrite,
    quiet = quiet,
    etag = FALSE
  )

  # Bundled fixtures are intentionally cropped and therefore do not match the
  # full national asset checksum.
  offline_fixture <- nzchar(Sys.getenv("R_GEOMARKER_NO_DOWNLOAD"))
  if (!offline_fixture) {
    expected_bytes <- as.numeric(traffic_data_manifest()[["Asset-Bytes"]])
    actual_bytes <- unname(file.info(path)$size)
    if (is.na(actual_bytes) || actual_bytes != expected_bytes) {
      unlink(path)
      stop(
        "Cached traffic data has an unexpected size and was removed.\n",
        "Expected: ",
        format(expected_bytes, scientific = FALSE),
        " bytes\n",
        "Observed: ",
        format(actual_bytes, scientific = FALSE),
        " bytes\n",
        "Retry to download a fresh copy.",
        call. = FALSE
      )
    }
    if (needs_full_validation) {
      tryCatch(
        traffic_validate_download(path),
        error = function(err) {
          unlink(path)
          stop(
            conditionMessage(err),
            "\nThe invalid file was removed.",
            call. = FALSE
          )
        }
      )
    }
  }
  path
}

traffic_region_filter_wkt <- function(parent, buffer) {
  parent |>
    s2::s2_cell_polygon() |>
    s2::s2_buffer_cells(
      distance = buffer,
      max_cells = 1000,
      min_level = -1
    ) |>
    sf::st_as_sfc() |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_as_text()
}

traffic_zero_summary <- function() {
  c(
    aadtm_trucks_buses = 0,
    aadtm_tractor_trailer = 0,
    aadtm_passenger = 0
  )
}

#' Summarize nearby traffic
#'
#' Summarizes average annual daily traffic meters (`aadtm`) traveled
#' on F1 and F2 roadways within buffer distance of each
#' s2 cell as totals per vehicle class (`trucks_buses`, `tractor_trailer`,
#' and `passenger`); dates are not used to link to traffic data.
#'
#' Traffic data are derived from the final 2024 Highway Performance Monitoring
#' System (HPMS) File Geodatabase published by the Bureau of Transportation
#' Statistics. Only roads with F_SYSTEM classification of 1 ("interstate") or
#' 2 ("principal arterial - other freeways and expressways") are used.
#' Passenger vehicles (FHWA 1-3) are calculated as the total minus FHWA class
#' 4-7 (single unit) and 8-13 (combination). FHWA documents substantial missing
#' portions of the 2024 data for North Dakota and New Jersey.
#' In addition, 55 retained roadway sections report single-unit plus
#' combination AADT greater than total AADT; the documented passenger-vehicle
#' subtraction is preserved for those source records rather than silently
#' modifying them.
#'
#' @param x a s2_cell_dates vector (see `?s2cd`)
#' @param buffer distance from s2 cell (in meters) to summarize data
#' @param overwrite logical; overwrite cached traffic data?
#' @param quiet logical; suppress download and processing messages?
#' @return data frame with one row per input location and numeric traffic
#'   summaries `aadtm_trucks_buses`, `aadtm_tractor_trailer`, and
#'   `aadtm_passenger`
#' @references
#' <https://www.arcgis.com/home/item.html?id=5e6a977c2d7c4ec1bdc82e684d3384f2>
#' @references
#' <https://github.com/FHWA/HPMS/blob/2024-HPMS/README_2024_HPMS_All.md>
#' @export
#' @examples
#' withr::local_envvar(
#'   R_USER_DATA_DIR = fs::path_package(
#'     "geomarker",
#'     "gmrkr--8841"
#'   ),
#'   R_GEOMARKER_NO_DOWNLOAD = "true"
#' )
#' get_traffic_summary(s2cd_example_cincy(), buffer = 1500)
get_traffic_summary <- function(
  x,
  buffer = 400,
  overwrite = FALSE,
  quiet = FALSE
) {
  stopifnot(
    "x must be a s2_cell_dates vector" = is_s2cd(x),
    "buffer must be numeric" = is.numeric(buffer),
    "buffer must be length one" = length(buffer) == 1,
    "buffer must be finite" = is.finite(buffer),
    "buffer must not be negative" = buffer >= 0,
    "overwrite must be logical" = is.logical(overwrite),
    "overwrite must be length one" = length(overwrite) == 1,
    "quiet must be logical" = is.logical(quiet),
    "quiet must be length one" = length(quiet) == 1
  )
  check_installed("sf", "to summarize traffic data.")

  cells <- s2::as_s2_cell(x)
  if (length(cells) == 0) {
    return(as.data.frame(matrix(
      numeric(),
      nrow = 0,
      ncol = 3,
      dimnames = list(NULL, names(traffic_zero_summary()))
    )))
  }

  data_file <- traffic_data_file(overwrite = overwrite, quiet = quiet)
  manifest <- traffic_data_manifest()

  cell_keys <- as.character(cells)
  unique_cells <- cells[!duplicated(cell_keys)]
  restore <- match(cell_keys, as.character(unique_cells))
  centers <- s2::s2_cell_center(unique_cells)
  parent_cells <- s2::s2_cell_parent(
    s2::as_s2_cell(centers),
    level = 6
  )
  regions <- split(seq_along(unique_cells), as.character(parent_cells))

  if (!quiet) {
    message(
      "Summarizing HPMS 2024 traffic for ",
      length(unique_cells),
      " unique locations in ",
      length(regions),
      " region",
      if (length(regions) == 1) "" else "s",
      "..."
    )
  }

  summaries <- matrix(
    0,
    nrow = length(unique_cells),
    ncol = 3,
    dimnames = list(NULL, names(traffic_zero_summary()))
  )
  n_candidates <- 0L

  for (region_name in names(regions)) {
    region_indices <- regions[[region_name]]
    hpms <- sf::read_sf(
      data_file,
      layer = manifest[["Asset-Layer"]],
      wkt_filter = traffic_region_filter_wkt(
        s2::as_s2_cell(region_name),
        buffer
      ),
      quiet = TRUE
    )
    n_candidates <- n_candidates + nrow(hpms)
    if (nrow(hpms) == 0) {
      next
    }

    road_geographies <- s2::as_s2_geography(sf::st_geometry(hpms))
    hpms <- sf::st_drop_geometry(hpms)
    nearby <- s2::s2_dwithin_matrix(
      centers[region_indices],
      road_geographies,
      distance = buffer
    )

    summarize_one <- function(road_indices, center) {
      if (length(road_indices) == 0) {
        return(traffic_zero_summary())
      }
      intersection_lengths <- s2::s2_intersection(
        road_geographies[road_indices],
        s2::s2_buffer_cells(
          center,
          distance = buffer,
          max_cells = 1000,
          min_level = -1
        )
      ) |>
        s2::s2_length()
      c(
        aadtm_trucks_buses = sum(
          hpms$AADT_SINGLE_UNIT[road_indices] * intersection_lengths
        ),
        aadtm_tractor_trailer = sum(
          hpms$AADT_COMBINATION[road_indices] * intersection_lengths
        ),
        aadtm_passenger = sum(
          (hpms$AADT[road_indices] -
            hpms$AADT_SINGLE_UNIT[road_indices] -
            hpms$AADT_COMBINATION[road_indices]) *
            intersection_lengths
        )
      )
    }

    summaries[region_indices, ] <- do.call(
      rbind,
      Map(
        summarize_one,
        nearby,
        centers[region_indices]
      )
    )
  }

  if (!quiet) {
    message(
      "  ...processed ",
      format(n_candidates, big.mark = ","),
      " regional roadway candidates"
    )
  }
  as.data.frame(summaries[restore, , drop = FALSE])
}

install_traffic_geomarker_fixture <- function(
  cell,
  dates,
  output_dir,
  source_file = NULL
) {
  check_installed("sf", "to create traffic fixture data.")
  check_installed("terra", "to create traffic fixture data.")
  cell <- geomarker_fixture_cell(cell)
  geomarker_fixture_dates(dates)
  output_dir <- geomarker_fixture_output_dir(output_dir)
  if (is.null(source_file)) {
    source_file <- traffic_data_file()
  }
  stopifnot(
    "source_file must be length one" = length(source_file) == 1,
    "source_file must exist" = file.exists(source_file)
  )

  cell_filter <- cell |>
    s2::s2_cell_polygon() |>
    sf::st_as_sfc() |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_as_text()
  output_file <- file.path(
    output_dir,
    url_to_filename(traffic_data_url(), etag = FALSE)
  )
  source_file |>
    sf::read_sf(
      layer = traffic_data_manifest()[["Asset-Layer"]],
      wkt_filter = cell_filter,
      quiet = TRUE
    ) |>
    terra::vect() |>
    geomarker_fixture_crop_to_cell(cell = cell) |>
    sf::st_as_sf() |>
    sf::st_write(
      output_file,
      layer = traffic_data_manifest()[["Asset-Layer"]],
      delete_dsn = TRUE,
      layer_options = "SPATIAL_INDEX=YES",
      quiet = TRUE
    )
  invisible(output_dir)
}
