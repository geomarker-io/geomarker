# Recreate the offline API example without claiming NASA source coverage.
# Run from the repository root: Rscript scripts/make-evi-example-fixture.R
pkgload::load_all(quiet = TRUE)
crs <- paste(
  "+proj=sinu +lon_0=0 +x_0=0 +y_0=0",
  "+R=6371007.181 +units=m +no_defs"
)
cell <- s2::as_s2_cell("8841")
buffer <- 10000
cover <- geomarker:::geomarker_fixture_region(cell, buffer = buffer) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs) |>
  terra::vect()
extent <- terra::ext(cover)
resolution <- 231.656358263958
r <- terra::rast(
  xmin = floor(terra::xmin(extent) / resolution) * resolution,
  xmax = ceiling(terra::xmax(extent) / resolution) * resolution,
  ymin = floor(terra::ymin(extent) / resolution) * resolution,
  ymax = ceiling(terra::ymax(extent) / resolution) * resolution,
  resolution = resolution, crs = crs
)
xy <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
terra::values(r) <- 0.3 + 0.1 * sin(xy[, 1] / 4000) + 0.1 * cos(xy[, 2] / 7000)
r <- terra::mask(r, cover)
directory <- file.path("inst", "gmrkr--8841", "R", "geomarker", "stow", "get_evi_summary")
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
path <- file.path(directory, geomarker:::evi_annual_composite_filename("2024", "h11v05"))
terra::writeRaster(r, path, overwrite = TRUE, wopt = geomarker:::evi_raster_write_options("evi"))
record <- list(
  `Manifest-Version` = "1", `EVI-Year` = "2024", `MODIS-Tile` = "h11v05",
  Collection = "synthetic", Transformation = "synthetic-evi-example-v1",
  `Coverage-Status` = "synthetic-example", `Spatial-Scope` = "fixture",
  `Fixture-Cell` = "8841", `Fixture-Buffer-M` = as.character(buffer),
  Purpose = "Offline API examples only; not scientific data or a MODIS annual composite.",
  Generator = "scripts/make-evi-example-fixture.R",
  Formula = "0.3 + 0.1 * sin(x / 4000) + 0.1 * cos(y / 7000); MODIS sinusoidal coordinates",
  `Asset-Bytes` = as.character(file.info(path)$size),
  `Asset-SHA256` = digest::digest(path, algo = "sha256", file = TRUE)
)
write.dcf(as.data.frame(record, check.names = FALSE), geomarker:::evi_manifest_path(path))
