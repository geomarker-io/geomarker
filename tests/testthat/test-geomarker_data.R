test_that("geomarker_data_dir works", {
  geomarker_data_dir() |>
    expect_type("character") |>
    expect_length(1) |>
    grepl("geomarker", x = _) |>
    expect_true()
  geomarker_data_dir("foofy") |>
    expect_type("character") |>
    expect_length(1) |>
    grepl("foofy", x = _) |>
    expect_true()
})

test_that("url_to_filename works", {
  c(
    "https://www2.census.gov/geo/tiger/TIGER2024/INTERNATIONALBOUNDARY/tl_2024_us_internationalboundary.zip",
    "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2025/12/hms_smoke20251225.zip",
    "https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/hpbl.2023.nc",
    "https://github.com/geomarker-io/appc/releases/download/hpms_2020_f12_aadt-2025-07-16/hpms_2020_f12_aadt.gpkg",
    "https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/Annual_NLCD_FctImp_2022_CU_C1V1.zip",
    "https://gaftp.epa.gov/air/nei/2020/data_summaries/Facility%20Level%20by%20Pollutant.zip"
  ) |>
    sapply(url_to_filename, USE.NAMES = FALSE) |>
    expect_identical(
      c(
        "d274c988--tl_2024_us_internationalboundary.zip",
        "3ca2-646d99da5cbc0--hms_smoke20251225.zip",
        "5cf681b-60e71a2e19837--hpbl.2023.nc",
        "0x8DDC4A271EE0EEA--hpms_2020_f12_aadt.gpkg",
        "395e73bf-637402f89f5ee--Annual_NLCD_FctImp_2022_CU_C1V1.zip",
        "194d242-5f81da6dcd0df--Facility%20Level%20by%20Pollutant.zip"
      )
    )
})

test_that("geomarker_download_file works", {
  the_url <- "https://www2.census.gov/geo/tiger/TIGER2024/INTERNATIONALBOUNDARY/tl_2024_us_internationalboundary.zip"
  geomarker_download_file(the_url) |>
    expect_visible() |>
    expect_identical(
      file.path(
        geomarker_data_dir(),
        "d274c988--tl_2024_us_internationalboundary.zip"
      )
    )
  geomarker_download_file(the_url, quiet = TRUE) |>
    expect_invisible()
})
