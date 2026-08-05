devtools::load_all()

the_cell <- s2::as_s2_cell("8841")
fixture_dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = 1)
fixture_buffer <- 10000

fixture_dir <- file.path("inst", paste0("gmrkr--", the_cell), "R", "geomarker")

if (FALSE) {
  unlink(fixture_dir, recursive = TRUE)
}
dir.create(fixture_dir, showWarnings = FALSE, recursive = TRUE)

install_hms_smoke_geomarker_fixture(the_cell, fixture_dates, fixture_dir)
install_nlcd_geomarker_fixture(
  the_cell,
  fixture_dates,
  fixture_dir,
  buffer = fixture_buffer
)
install_nei_point_geomarker_fixture(
  the_cell,
  fixture_dates,
  fixture_dir,
  buffer = fixture_buffer
)
install_gridmet_geomarker_fixture(the_cell, fixture_dates, fixture_dir)
install_narr_geomarker_fixture(the_cell, fixture_dates, fixture_dir)
install_elevation_geomarker_fixture(
  the_cell,
  fixture_dates,
  fixture_dir,
  buffer = fixture_buffer
)
install_traffic_geomarker_fixture(
  the_cell,
  fixture_dates,
  fixture_dir,
  buffer = fixture_buffer
)
install_merra_geomarker_fixture(
  the_cell,
  as.Date(c("2024-01-01", "2024-09-13")),
  fixture_dir
)
install_tiger_geomarker_fixture(the_cell, fixture_dates, fixture_dir)
install_evi_geomarker_fixture(
  the_cell,
  fixture_dates,
  fixture_dir,
  buffer = fixture_buffer
)
