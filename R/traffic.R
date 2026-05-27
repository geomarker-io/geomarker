#' Summarize nearby traffic
#'
#' Summarizes average annual daily traffic meters (`aadtm`) traveled
#' on F1 and F2 roadways within buffer distance of each
#' s2 cell as totals per vehicle class (`trucks_buses`, `tractor_trailer`,
#' and `passenger`); dates are not used to link to traffic data.
#'
#' Traffic data from the 2020 Highway Performance Monitoring System (HPMS)
#' is downloaded from the Bureau of Transportation Statistics
#' as a geodatabase of all roads geometries
#' with traffic estimates by vehicle type
#' and roadway class.
#' See <https://geodata.bts.gov/datasets/c199f2799b724ffbacf4cafe3ee03e55/about>
#' for more details.
#' Only roads with F_SYSTEM classification of 1 ("interstate") or 2
#' ("principal arterial - other freeways and expressways").
#' are used. Passenger vehicles (FHWA 1-3) are calculated as the
#' total minus FHWA class 4-7 (single unit) and 8-13 (combo).
#' @param x a s2_cell_dates vector (see `?s2cd`)
#' @param buffer distance from s2 cell (in meters) to summarize data
#' @param overwrite logical;
#' overwrite traffic geodatabase if already downloaded?
#' @param quiet logical; show download progress messages?
#' @return list of numeric vectors of traffic summaries
#' @export
#' @examples
#'  withr::local_envvar(
#'    R_USER_DATA_DIR = fs::path_package(
#'      "geomarker",
#'      "gmrkr--8841"
#'    ),
#'    R_GEOMARKER_NO_DOWNLOAD = "true"
#'  )
#' get_traffic_summary(s2cd_example_cincy(), buffer = 1500)
get_traffic_summary <- function(
  x,
  buffer = 400,
  overwrite = FALSE,
  quiet = FALSE
) {
  stopifnot("x must be a s2_cell_dates vector" = is_s2cd(x))
  message("Loading HPMS data...")
  hpms_d <-
    geomarker_download_file(
      "https://github.com/geomarker-io/appc/releases/download/hpms_2020_f12_aadt-2025-07-16/hpms_2020_f12_aadt.gpkg"
    ) |>
    sf::read_sf(quiet = TRUE)
  hpms_d$s2_geography <- s2::as_s2_geography(hpms_d$geom)
  hpms_d <- sf::st_drop_geometry(hpms_d)

  message(
    "  ...loaded ",
    format(nrow(hpms_d), big.mark = ","),
    " roadway segments with traffic data"
  )

  # always takes around 180 seconds on my machine (with 100, 1000, or 10000 input locations...)
  message(
    "Finding nearby road segments for ",
    length(s2::as_s2_cell(x)),
    " locations..."
  )
  tm <- system.time({
    which_within <- s2::s2_dwithin_matrix(
      s2::s2_cell_center(s2::as_s2_cell(x)),
      hpms_d$s2_geography,
      distance = buffer
    )
  })
  message("  ...in ", sprintf("%.0f", tm[['elapsed']]), "s")

  summarize_road_intersections <- function(which_roads, s2_cell) {
    if (length(which_roads) == 0) {
      return(c(
        aadtm_trucks_buses = 0,
        aadtm_tractor_trailer = 0,
        aadtm_passenger = 0
      ))
    }
    int_roads <- hpms_d[which_roads, ]
    il <- s2::s2_intersection(
      int_roads$s2_geography,
      s2::s2_buffer_cells(
        s2::s2_cell_center(s2::as_s2_cell(s2_cell)),
        distance = buffer,
        max_cells = 1000,
        min_level = -1
      )
    ) |>
      s2::s2_length()
    c(
      aadtm_trucks_buses = sum(int_roads$AADT_SINGLE_UNIT * il),
      aadtm_tractor_trailer = sum(int_roads$AADT_COMBINATION * il),
      aadtm_passenger = sum(
        (int_roads$AADT -
          int_roads$AADT_SINGLE_UNIT -
          int_roads$AADT_COMBINATION) *
          il
      )
    )
  }

  aadtm <- mapply(
    summarize_road_intersections,
    which_roads = which_within,
    s2_cell = s2::as_s2_cell(x),
    SIMPLIFY = FALSE
  )
  as.data.frame(do.call(rbind, aadtm))
}

install_traffic_geomarker_fixture <- function(cell, dates, output_dir) {
  check_installed("sf", "to create traffic fixture data.")
  check_installed("terra", "to create traffic fixture data.")
  cell <- geomarker_fixture_cell(cell)
  geomarker_fixture_dates(dates)
  output_dir <- geomarker_fixture_output_dir(output_dir)
  traffic_url <- "https://github.com/geomarker-io/appc/releases/download/hpms_2020_f12_aadt-2025-07-16/hpms_2020_f12_aadt.gpkg"
  geomarker_download_file(traffic_url) |>
    sf::read_sf(quiet = TRUE) |>
    terra::vect() |>
    geomarker_fixture_crop_to_cell(cell = cell) |>
    sf::st_as_sf() |>
    sf::st_write(
      file.path(output_dir, url_to_filename(traffic_url, etag = FALSE)),
      append = FALSE,
      quiet = TRUE
    )
  invisible(output_dir)
}
