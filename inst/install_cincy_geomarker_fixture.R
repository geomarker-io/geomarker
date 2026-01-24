the_cell <- s2::as_s2_cell("8841")
fixture_dir <- fs::path_package(
  "geomarker",
  paste0("gmrkr--", the_cell),
  "R",
  "geomarker"
)
dir.create(fixture_dir, showWarnings = FALSE, recursive = TRUE)

crop_to_cell <- function(r, cell = the_cell) {
  cover <-
    the_cell |>
    s2::s2_cell_polygon() |>
    sf::st_as_sfc() |>
    sf::st_transform(terra::crs(r)) |>
    terra::vect()
  out <- terra::crop(r, cover)
  if (inherits(r, "SpatRaster")) {
    out <- terra::mask(out, cover)
  }
  out
}

# gridmet: tmmx 2024
gridmet_example_url <- "https://www.northwestknowledge.net/metdata/data/tmmx_2024.nc"
gridmet_example_url |>
  geomarker_download_file() |>
  terra::rast() |>
  crop_to_cell() |>
  terra::writeCDF(
    file.path(fixture_dir, url_to_filename(gridmet_example_url, etag = FALSE)),
    overwrite = TRUE
  )

# elevation
elevation_url <- "https://prism.oregonstate.edu/downloads/data/PRISM_us_dem_800m_bil.zip"
write_dir <- tempfile("elevation")
dir.create(write_dir)
geomarker_download_file(elevation_url) |>
  paste0("/vsizip/", url = _, "/PRISM_us_dem_800m_bil.bil") |>
  terra::rast() |>
  crop_to_cell() |>
  terra::writeRaster(
    file.path(write_dir, "PRISM_us_dem_800m_bil.bil"),
    overwrite = TRUE,
    filetype = "EHdr"
  )
utils::zip(
  file.path(fixture_dir, url_to_filename(elevation_url, etag = FALSE)),
  files = list.files(write_dir, full.names = TRUE),
  flags = "-j"
)

# traffic
traffic_url <- "https://github.com/geomarker-io/appc/releases/download/hpms_2020_f12_aadt-2025-07-16/hpms_2020_f12_aadt.gpkg"
geomarker_download_file(traffic_url) |>
  sf::read_sf(quiet = TRUE) |>
  terra::vect() |>
  crop_to_cell() |>
  sf::st_as_sf() |>
  sf::st_write(file.path(
    fixture_dir,
    url_to_filename(traffic_url, etag = FALSE)
  ))
