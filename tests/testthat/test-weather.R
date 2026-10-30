test_that("weather extraction preserves missing locations and date alignment", {
  skip_if_not_installed("terra")
  raster <- terra::rast(
    nrows = 2, ncols = 2, nlyrs = 3,
    xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster) <- matrix(as.numeric(1:12), nrow = 4)
  path <- tempfile(fileext = ".nc")
  withr::defer(unlink(path))
  terra::writeCDF(raster, path)
  local_mocked_bindings(geomarker_stow = function(...) path)

  x <- s2cd(
    s2::as_s2_cell(s2::s2_lnglat(c(3, 0.5, 0.5, 3), c(3, 0.5, 0.5, 3))),
    dates = list(
      as.Date(c("2024-01-01", "2024-01-03")),
      as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
      as.Date(c("2024-01-02", "2024-01-02")),
      as.Date("2024-01-02")
    )
  )
  expected <- list(c(NA_real_, NA_real_), c(3, 7, 11), c(7, 7), NA_real_)
  names(expected) <- as.character(x)
  first_year <- s2cd(
    s2::as_s2_cell(x),
    lapply(s2cd_dates(x), function(dates) {
      dates - as.integer(as.Date("2024-01-01") - as.Date("1979-01-01"))
    })
  )

  for (get_data in list(get_gridmet_data, get_narr_data)) {
    expect_no_warning(out <- get_data(x))
    expect_identical(out, expected)
    expect_no_warning(outside <- get_data(x[c(1, 4)]))
    expect_identical(outside, expected[c(1, 4)])
    expect_identical(get_data(x[2]), expected[2])
    expect_identical(get_data(first_year), expected)
  }
})
