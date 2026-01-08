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
    "http://thredds.northwestknowledge.net:8080/thredds/fileServer/MET/tmmx/tmmx_2025.nc",
    "https://www2.census.gov/geo/tiger/TIGER2024/INTERNATIONALBOUNDARY/tl_2024_us_internationalboundary.zip",
    "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2025/12/hms_smoke20251225.zip",
    "https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/hpbl.2023.nc",
    "https://github.com/geomarker-io/appc/releases/download/hpms_2020_f12_aadt-2025-07-16/hpms_2020_f12_aadt.gpkg",
    "https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/Annual_NLCD_FctImp_2022_CU_C1V1.zip",
    "https://gaftp.epa.gov/air/nei/2020/data_summaries/Facility%20Level%20by%20Pollutant.zip"
  ) |>
    url_to_filename() |>
    expect_identical(
      c(
        "373b0d2a--tmmx_2025.nc",
        "d274c988--tl_2024_us_internationalboundary.zip",
        "36b8531e--hms_smoke20251225.zip",
        "afadc45b--hpbl.2023.nc",
        "4303dd87--hpms_2020_f12_aadt.gpkg",
        "d84440c1--Annual_NLCD_FctImp_2022_CU_C1V1.zip",
        "b883536e--Facility%20Level%20by%20Pollutant.zip"
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
