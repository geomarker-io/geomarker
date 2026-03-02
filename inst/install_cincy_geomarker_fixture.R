the_cell <- s2::as_s2_cell("8841")
fixture_dir <- fs::path_package(
  "geomarker",
  paste0("gmrkr--", the_cell),
  "R",
  "geomarker"
)
dir.create(fixture_dir, showWarnings = FALSE, recursive = TRUE)

crop_rast_to_cell <- function(r, cell = the_cell) {
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

# nlcd: 2023
nlcd_url <- "https://dataverse.harvard.edu/api/access/datafile/10980930"
nlcd_url |>
  geomarker_download_file() |>
  terra::rast() |>
  crop_rast_to_cell() |>
  terra::writeRaster(
    file.path(
      fixture_dir,
      url_to_filename(nlcd_url, etag = FALSE)
    ),
    filetype = "cog"
  )

# gridmet: tmmx 2024
gridmet_example_url <- "https://www.northwestknowledge.net/metdata/data/tmmx_2024.nc"
gridmet_example_url |>
  geomarker_download_file() |>
  terra::rast() |>
  crop_rast_to_cell() |>
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
  crop_rast_to_cell() |>
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
  crop_rast_to_cell() |>
  sf::st_as_sf() |>
  sf::st_write(file.path(
    fixture_dir,
    url_to_filename(traffic_url, etag = FALSE)
  ))

# census bg 2024
tgr_st_url <- "ftp://ftp2.census.gov/geo/tiger/TIGER2024/STATE/tl_2024_us_state.zip"
tgr_st <- sf::st_read(paste0("/vsizip/", geomarker_download_file(tgr_st_url)))
tgr_st_cropped <- sf::st_crop(
  tgr_st,
  sf::st_transform(
    sf::st_as_sfc(s2::s2_cell_polygon(the_cell)),
    sf::st_crs(tgr_st)
  )
)
tgr_st_write_dir <- tempfile("tiger_state")
dir.create(tgr_st_write_dir)
sf::st_write(
  tgr_st_cropped,
  file.path(tgr_st_write_dir, "tl_2024_us_state.shp")
)
utils::zip(
  file.path(fixture_dir, url_to_filename(tgr_st_url, etag = FALSE)),
  files = list.files(tgr_st_write_dir, full.names = TRUE),
  flags = "-j"
)

lapply(
  tgr_st_cropped$GEOID,
  \(st) {
    tgr_bg_url <- sprintf(
      "ftp://ftp2.census.gov/geo/tiger/TIGER2024/BG/tl_2024_%s_bg.zip",
      st
    )
    tgr_bg <- sf::st_read(paste0(
      "/vsizip/",
      geomarker_download_file(tgr_bg_url)
    ))
    tgr_bg_cropped <- sf::st_crop(
      tgr_bg,
      sf::st_transform(
        sf::st_as_sfc(s2::s2_cell_polygon(the_cell)),
        sf::st_crs(tgr_bg)
      )
    )
    tgr_bg_write_dir <- tempfile(st)
    dir.create(tgr_bg_write_dir)
    sf::st_write(
      tgr_bg_cropped,
      file.path(tgr_bg_write_dir, sprintf("tl_2024_%s_bg.shp", st))
    )
    utils::zip(
      file.path(fixture_dir, url_to_filename(tgr_bg_url, etag = FALSE)),
      files = list.files(tgr_bg_write_dir, full.names = TRUE),
      flags = "-j"
    )
    return("written!")
  }
)
