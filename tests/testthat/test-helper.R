test_that("check_installed works", {
  check_installed("not.a.real.package") |>
    expect_error("`not.a.real.package` is required")
  check_installed("not.a.real.package", "to test this function out") |>
    expect_error("`not.a.real.package` is required to test this function out")
  check_installed("tools") |>
    expect_invisible() |>
    expect_true()
})

test_that("R_GEOMARKER_NO_DOWNLOAD blocks direct network access", {
  withr::local_envvar(R_GEOMARKER_NO_DOWNLOAD = NA)
  expect_false(geomarker_no_download())
  expect_invisible(geomarker_require_download())

  Sys.setenv(R_GEOMARKER_NO_DOWNLOAD = "false")
  expect_true(geomarker_no_download())
  expect_error(
    geomarker_require_download("contact a test service"),
    "R_GEOMARKER_NO_DOWNLOAD.*contact a test service"
  )
  expect_error(
    url_etag("https://example.com"),
    "R_GEOMARKER_NO_DOWNLOAD"
  )
})

test_that("is_non_decreasing works", {
  expect_true(is_non_decreasing(numeric(0)))
  expect_true(is_non_decreasing(1))
  expect_true(is_non_decreasing(c(1, 1, 2, 3)))
  expect_false(is_non_decreasing(c(1, 3, 2)))
})

test_that("url_to_filename rejects query parameters", {
  expect_error(
    url_to_filename("https://example.com/file.zip?token=abc", etag = FALSE),
    "query parameters"
  )
})

test_that("url_to_filename hashes without etag", {
  url_to_filename(
    "https://example.com/path/file.zip",
    etag = FALSE
  ) |>
    expect_identical("c18913fb--file.zip")
})

test_that("url_etag examples work", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  url_etag(
    paste0(
      "https://www2.census.gov/geo/tiger/TIGER2024/",
      "INTERNATIONALBOUNDARY/tl_2024_us_internationalboundary.zip"
    )
  ) |>
    expect_length(1) |>
    expect_type("character")

  expect_true(is.na(url_etag("https://example.com")))

  expect_error(
    url_etag("https://example1209430932490324032.com")
  )
})

test_that("url_to_filename works", {
  c(
    "https://www2.census.gov/geo/tiger/TIGER2024/INTERNATIONALBOUNDARY/tl_2024_us_internationalboundary.zip",
    "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2025/12/hms_smoke20251225.zip",
    "https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/hpbl.2023.nc",
    "https://github.com/geomarker-io/geomarker/releases/download/v0.0.1/hpms_2024_f12_aadt.gpkg",
    "https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/Annual_NLCD_FctImp_2022_CU_C1V1.zip",
    "https://gaftp.epa.gov/air/nei/2020/data_summaries/Facility%20Level%20by%20Pollutant.zip"
  ) |>
    sapply(url_to_filename, etag = FALSE, USE.NAMES = FALSE) |>
    expect_identical(
      c(
        "d274c988--tl_2024_us_internationalboundary.zip",
        "36b8531e--hms_smoke20251225.zip",
        "afadc45b--hpbl.2023.nc",
        "40376751--hpms_2024_f12_aadt.gpkg",
        "d84440c1--Annual_NLCD_FctImp_2022_CU_C1V1.zip",
        "b883536e--Facility%20Level%20by%20Pollutant.zip"
      )
    )
})

test_that("geomarker fixture cells must be level 6", {
  expect_equal(
    s2::s2_cell_level(geomarker_fixture_cell("8841")),
    6L
  )
  expect_error(
    geomarker_fixture_cell("8841b39a7c46e25f"),
    "fixture cell must be level 6"
  )
})

test_that("geomarker fixture regions include requested buffer halos", {
  cell <- s2::as_s2_cell("8841")
  cell_bbox <- geomarker_fixture_cell_bbox(cell)
  halo_bbox <- geomarker_fixture_cell_bbox(cell, buffer = 10000)

  expect_lt(halo_bbox[[1]], cell_bbox[[1]])
  expect_lt(halo_bbox[[2]], cell_bbox[[2]])
  expect_gt(halo_bbox[[3]], cell_bbox[[3]])
  expect_gt(halo_bbox[[4]], cell_bbox[[4]])
  expect_error(
    geomarker_fixture_region(cell, buffer = -1),
    "must not be negative"
  )
  expect_error(
    geomarker_fixture_region(cell, buffer = Inf),
    "must be finite"
  )
})
