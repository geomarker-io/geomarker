traffic_data_manifest <- function() {
  manifest_path <- system.file("traffic-data.dcf", package = "geomarker")
  if (!nzchar(manifest_path)) {
    manifest_path <- file.path("inst", "traffic-data.dcf")
  }
  as.list(read.dcf(manifest_path)[1, ])
}

traffic_data_url <- function() {
  url <- traffic_data_manifest()[["Asset-URL"]]
  if (is.null(url) || length(url) != 1 || !nzchar(url)) {
    stop("Traffic metadata does not provide an asset URL.", call. = FALSE)
  }
  url
}

traffic_data_file <- function(...) {
  geomarker_download_file(
    traffic_data_url(),
    ...
  )
}

traffic_data_source <- function(path) {
  required_fields <- c("AADT", "AADT_SINGLE_UNIT", "AADT_COMBINATION")
  layers <- sf::st_layers(path, do_count = FALSE)$name
  preferred_layer <- traffic_data_manifest()[["Asset-Layer"]]
  if (!is.null(preferred_layer) && preferred_layer %in% layers) {
    layers <- c(preferred_layer, setdiff(layers, preferred_layer))
  }

  for (layer in layers) {
    escaped_layer <- gsub('"', '""', layer, fixed = TRUE)
    sample <- tryCatch(
      sf::st_read(
        path,
        query = paste0('SELECT * FROM "', escaped_layer, '" LIMIT 1'),
        quiet = TRUE
      ),
      error = function(err) NULL
    )
    if (is.null(sample)) {
      next
    }
    field_indices <- match(tolower(required_fields), tolower(names(sample)))
    if (!anyNA(field_indices)) {
      return(list(
        layer = layer,
        fields = stats::setNames(names(sample)[field_indices], required_fields),
        crs = sf::st_crs(sample)
      ))
    }
  }

  stop(
    "Traffic data must contain a spatial layer with the fields ",
    paste(required_fields, collapse = ", "),
    ".",
    call. = FALSE
  )
}

traffic_region_filter_wkt <- function(parent, buffer, crs = sf::st_crs(4326)) {
  region <- parent |>
    s2::s2_cell_polygon() |>
    s2::s2_buffer_cells(
      distance = buffer,
      max_cells = 1000,
      min_level = -1
    ) |>
    sf::st_as_sfc() |>
    sf::st_bbox() |>
    sf::st_as_sfc()
  if (!is.na(crs)) {
    region <- sf::st_transform(region, crs)
  }
  sf::st_as_text(region)
}

traffic_read_region <- function(path, source, parent, buffer) {
  hpms <- sf::read_sf(
    path,
    layer = source$layer,
    wkt_filter = traffic_region_filter_wkt(parent, buffer, source$crs),
    quiet = TRUE
  )
  names(hpms)[match(unname(source$fields), names(hpms))] <- names(source$fields)
  if (is.na(sf::st_crs(hpms))) {
    hpms <- sf::st_set_crs(hpms, 4326)
  } else {
    hpms <- sf::st_transform(hpms, 4326)
  }
  for (field in names(source$fields)) {
    hpms[[field]] <- suppressWarnings(as.numeric(hpms[[field]]))
  }
  hpms[
    stats::complete.cases(sf::st_drop_geometry(hpms)[names(source$fields)]),
  ]
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
#' The processed traffic data are distributed separately from the R package as
#' a GeoPackage asset attached to the geomarker package release. The file is
#' downloaded to `geomarker_data_dir()` the first time it is required by
#' `get_traffic_summary()` and reused from the local cache on subsequent calls.
#'
#' @param x a s2_cell_dates vector (see `?s2cd`)
#' @param buffer distance from s2 cell (in meters) to summarize data
#' @param ... passed to `geomarker_download_file()`
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
  ...
) {
  stopifnot(
    "x must be a s2_cell_dates vector" = is_s2cd(x),
    "buffer must be numeric" = is.numeric(buffer),
    "buffer must be length one" = length(buffer) == 1,
    "buffer must be finite" = is.finite(buffer),
    "buffer must not be negative" = buffer >= 0
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

  quiet <- isTRUE(list(...)$quiet)
  data_file <- traffic_data_file(...)
  source <- traffic_data_source(data_file)

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
    hpms <- traffic_read_region(
      data_file,
      source,
      s2::as_s2_cell(region_name),
      buffer
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
  source_file = NULL,
  buffer = 400
) {
  check_installed("sf", "to create traffic fixture data.")
  check_installed("terra", "to create traffic fixture data.")
  cell <- geomarker_fixture_cell(cell)
  buffer <- geomarker_fixture_buffer(buffer)
  geomarker_fixture_dates(dates)
  output_dir <- geomarker_fixture_output_dir(output_dir)
  if (is.null(source_file)) {
    source_file <- traffic_data_file()
  }
  stopifnot(
    "source_file must be length one" = length(source_file) == 1,
    "source_file must exist" = file.exists(source_file)
  )

  output_file <- file.path(
    output_dir,
    url_to_filename(traffic_data_url(), etag = FALSE)
  )
  source <- traffic_data_source(source_file)
  traffic_read_region(source_file, source, cell, buffer) |>
    terra::vect() |>
    geomarker_fixture_crop_to_cell(cell = cell, buffer = buffer) |>
    sf::st_as_sf() |>
    sf::st_write(
      output_file,
      layer = "hpms_2024_f12_aadt",
      delete_dsn = TRUE,
      layer_options = "SPATIAL_INDEX=YES",
      quiet = TRUE
    )
  invisible(output_dir)
}
